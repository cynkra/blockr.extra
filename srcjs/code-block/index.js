/**
 * Blockr.Code — a CodeMirror 6 code editor as a Shiny input binding.
 *
 * Follows the blockr JS-driven-block pattern (see blockr.docs
 * patterns/js-driven-blocks.md, blockr.dplyr/inst/js/filter-block.js).
 *
 * Execution model (see blockr.design/open/function-block-editor):
 *   - The editor reports its document as input$<id> (debounced) — this is the
 *     "editor text", compared server-side against the executed text to decide
 *     whether the dirty-state "Run" button shows.
 *   - Manual typing is PENDING: it only reports the value (→ Run appears).
 *   - Discrete actions on complete code run IMMEDIATELY (no Run):
 *       * AI / external write of `fn`  (R pushes `blockr-code-set` w/ original)
 *       * Reject a hunk (userEvent "revert") → run the reduced doc now
 *       * Ctrl/Cmd-Enter → run the current doc now
 *     Accept a hunk (userEvent "accept") changes nothing to run — the AI code
 *     is already live — so it's a no-op for execution.
 *   - DIFF: a `blockr-code-set` carrying an `original` enters a unifiedMergeView
 *     (inline diff, per-hunk Accept/Reject). Accept-all just closes it; reject-
 *     all restores the merge view's own original and runs it.
 *
 * Bundled to inst/js/blockr-code.js via esbuild (see package.json).
 */
import { Compartment, EditorState } from "@codemirror/state";
import {
  EditorView, keymap, lineNumbers, drawSelection, highlightActiveLine
} from "@codemirror/view";
import { defaultKeymap, history, historyKeymap, indentWithTab } from "@codemirror/commands";
import { StreamLanguage, syntaxHighlighting, defaultHighlightStyle, indentUnit, indentService, getIndentUnit } from "@codemirror/language";
import { r } from "@codemirror/legacy-modes/mode/r";
import { autocompletion, completionKeymap } from "@codemirror/autocomplete";
import { unifiedMergeView, getChunks, getOriginalDoc } from "@codemirror/merge";

(() => {
  "use strict";
  if (!window.Shiny) return;

  const DEBOUNCE_MS = 250;

  // Plain block indentation. Each unclosed `(`, `[` or `{` adds exactly
  // one indent unit (2 spaces) regardless of where the bracket sits on
  // its line. This replaces the legacy R mode's "align continuation
  // lines to the column just after the bracket" behaviour, which pushed
  // the body of `function(...) { expr` far to the right. CodeMirror tries
  // every indentService before its syntax-tree fallback, so returning a
  // number here fully overrides the legacy mode's indent.
  const blockIndent = indentService.of((context, pos) => {
    const unit = getIndentUnit(context.state);
    // `lineAt(pos, -1)` returns only `{text, from}` (no `.to`) when a line
    // break is simulated — i.e. the Enter case — so derive the end from
    // the text length. Using `.to` here counted brackets past the cursor
    // (e.g. a closing `}` below), cancelling the open brace to depth 0.
    const prev = context.lineAt(pos, -1);
    const upto = context.state.sliceDoc(0, prev.from + prev.text.length);
    let depth = 0, inStr = null;
    for (let i = 0; i < upto.length; i++) {
      const c = upto[i];
      if (inStr) {
        if (c === "\\") { i++; continue; }
        if (c === inStr) inStr = null;
        continue;
      }
      if (c === '"' || c === "'" || c === "`") inStr = c;
      else if (c === "#") {
        const nl = upto.indexOf("\n", i);
        if (nl === -1) break;
        i = nl;
      } else if (c === "{" || c === "(" || c === "[") depth++;
      else if (c === "}" || c === ")" || c === "]") depth = Math.max(0, depth - 1);
    }
    // A line that itself starts with a closer dedents one level.
    const after = (context.textAfterPos(pos) || "").replace(/^\s*/, "");
    if (/^[}\])]/.test(after)) depth = Math.max(0, depth - 1);
    return depth * unit;
  });

  // Static completion vocabulary — kept small on purpose (the function body is
  // short and usually AI-written). Column names from the upstream data are
  // added dynamically and boosted above these.
  const R_KEYWORDS = [
    "function", "if", "else", "for", "while", "repeat", "in", "next", "break",
    "TRUE", "FALSE", "NULL", "NA", "Inf", "NaN"
  ];
  const R_BASE = [
    "c", "mean", "sum", "median", "min", "max", "range", "round", "abs", "sqrt",
    "length", "nrow", "ncol", "names", "colnames", "rownames", "seq", "seq_len",
    "seq_along", "rep", "paste", "paste0", "sprintf", "format", "factor",
    "as.numeric", "as.character", "as.factor", "as.integer", "as.logical",
    "is.na", "is.null", "ifelse", "sapply", "lapply", "vapply", "mapply",
    "do.call", "Reduce", "Filter", "Map", "unique", "duplicated", "sort",
    "order", "rev", "which", "table", "tapply", "aggregate", "data.frame",
    "list", "matrix", "rbind", "cbind", "apply", "head", "tail", "subset",
    "transform", "merge", "cut", "quantile", "cumsum"
  ];
  const R_TIDY = [
    "filter", "mutate", "select", "summarise", "summarize", "group_by",
    "ungroup", "arrange", "desc", "rename", "relocate", "distinct", "slice",
    "slice_head", "slice_tail", "slice_max", "slice_min", "pull", "count",
    "tally", "across", "where", "everything", "starts_with", "ends_with",
    "contains", "matches", "num_range", "any_of", "all_of", "left_join",
    "inner_join", "right_join", "full_join", "semi_join", "anti_join",
    "bind_rows", "bind_cols", "case_when", "if_else", "coalesce", "n",
    "n_distinct", "row_number", "lag", "lead", "first", "last", "pivot_longer",
    "pivot_wider", "separate", "unite", "nest", "unnest", "fill", "replace_na",
    "ggplot", "aes", "geom_point", "geom_line", "geom_col", "geom_bar",
    "geom_boxplot", "geom_histogram", "facet_wrap", "facet_grid", "labs",
    "scale_x_continuous", "scale_y_continuous", "theme_minimal", "theme_bw"
  ];

  // R-aware completion source. Closes over the host element so it can read the
  // live column list (pushed from R into el._cmCompletions).
  const makeRSource = (el) => (context) => {
    const word = context.matchBefore(/[A-Za-z0-9._]+/);
    if (!word || (word.from === word.to && !context.explicit)) return null;
    const options = [];
    const cols = el._cmCompletions || [];
    for (const col of cols) {
      options.push({ label: col, type: "variable", detail: "column", boost: 3 });
    }
    for (const fn of R_TIDY) {
      options.push({ label: fn, type: "function", apply: fn + "(", boost: 1 });
    }
    for (const fn of R_BASE) {
      options.push({ label: fn, type: "function", apply: fn + "(", boost: 0 });
    }
    for (const kw of R_KEYWORDS) {
      options.push({ label: kw, type: "keyword", boost: -1 });
    }
    return { from: word.from, options };
  };

  const makeView = (el) => {
    let view;

    // Debounced report of the editor text (the "pending"/dirty path).
    const reportValue = () => {
      if (!el._cmCallback) return;
      clearTimeout(el._cmTimer);
      el._cmTimer = setTimeout(() => el._cmCallback && el._cmCallback(true), DEBOUNCE_MS);
    };

    // Execute the current document immediately: sync the editor value AND fire
    // the run-input so the server commits + runs it now (no Run click needed).
    const runNow = () => {
      if (!view) return;
      const code = view.state.doc.toString();
      Shiny.setInputValue(el.id, code, { priority: "event" });
      if (el.dataset.runInput) {
        Shiny.setInputValue(el.dataset.runInput, code, { priority: "event" });
      }
    };
    el._cmRunNow = runNow;

    // Report the live hunk count to the server (drives the "N edits" label).
    const reportCount = () => {
      if (!el.dataset.countInput || !view) return;
      const c = getChunks(view.state);
      Shiny.setInputValue(el.dataset.countInput, c ? c.chunks.length : 0,
        { priority: "event" });
    };
    el._cmReportCount = reportCount;

    // Tell the server the AI review is finished so it can drop the review
    // footer (Accept-all / Reject-all).
    const reviewDone = () => {
      el._cmMergeActive = false;
      if (el.dataset.reviewDone) {
        el._cmReviewSeq = (el._cmReviewSeq || 0) + 1;
        Shiny.setInputValue(el.dataset.reviewDone, el._cmReviewSeq, { priority: "event" });
      }
    };
    // After a hunk is accepted/rejected: end the review if none remain, else
    // refresh the count.
    const afterChunkChange = () => {
      if (!el._cmMergeActive) return;
      const c = getChunks(view.state);
      if (!c || c.chunks.length === 0) reviewDone(); else reportCount();
    };

    const updateListener = EditorView.updateListener.of((u) => {
      // Classify the change by CodeMirror-merge's user events.
      let kind = null;
      for (const tr of u.transactions) {
        if (tr.isUserEvent("revert")) kind = "revert";
        else if (tr.isUserEvent("accept")) kind = "accept";
      }
      if (kind === "revert") {        // reject hunk → run the reduced doc now
        runNow();
        afterChunkChange();
        return;
      }
      if (kind === "accept") {        // accept hunk → already live; nothing to run
        afterChunkChange();
        return;
      }
      // Programmatic set (AI / initial / reject-all) is handled by its caller.
      if (u.docChanged && !el._cmSelfWrite) {
        // The first manual keystroke ENDS an AI review: the diff was there to
        // review the AI's change, not to mark up your own typing. Clear it and
        // fall back to the normal pending/Run model.
        if (el._cmMergeActive) {
          clearMerge(el);
          reviewDone();
        }
        reportValue();
      }
    });

    const mergeCompartment = new Compartment();
    el._cmMerge = mergeCompartment;

    const state = EditorState.create({
      doc: el.getAttribute("data-value") || "",
      extensions: [
        lineNumbers(),
        history(),
        drawSelection(),
        highlightActiveLine(),
        indentUnit.of("  "),
        blockIndent,
        syntaxHighlighting(defaultHighlightStyle),
        StreamLanguage.define(r),
        autocompletion({ override: [makeRSource(el)], activateOnTyping: true }),
        mergeCompartment.of([]),
        keymap.of([
          { key: "Mod-Enter", run: () => { runNow(); return true; } },
          ...defaultKeymap, ...historyKeymap, ...completionKeymap, indentWithTab
        ]),
        updateListener,
        EditorView.lineWrapping
      ]
    });
    view = new EditorView({ state, parent: el });
    return view;
  };

  // Replace the document. If `original` is a string, enter the inline diff;
  // if null, clear any active diff. Either way the server input is synced.
  const setCode = (el, code, original) => {
    const view = el._cmView;
    if (!view) return;
    el._cmSelfWrite = true;
    view.dispatch({
      changes: { from: 0, to: view.state.doc.length, insert: code == null ? "" : code }
    });
    if (el._cmMerge) {
      const isMerge = typeof original === "string";
      view.dispatch({
        effects: el._cmMerge.reconfigure(
          isMerge ? unifiedMergeView({ original, mergeControls: true }) : []
        )
      });
      el._cmMergeActive = isMerge;
      if (isMerge && el._cmReportCount) el._cmReportCount();
    }
    el._cmSelfWrite = false;
    Shiny.setInputValue(el.id, view.state.doc.toString(), { priority: "event" });
  };

  const clearMerge = (el) => {
    if (el._cmView && el._cmMerge) {
      el._cmView.dispatch({ effects: el._cmMerge.reconfigure([]) });
    }
    el._cmMergeActive = false;
  };

  const binding = new Shiny.InputBinding();
  Object.assign(binding, {
    find: function (scope) {
      return $(scope).find(".blockr-code");
    },
    initialize: function (el) {
      if (el._cmView) return;
      el._cmCallback = null;
      el._cmSelfWrite = false;
      el._cmMergeActive = false;
      el._cmCompletions = [];
      el._cmView = makeView(el);
      // Report the initial value so the server input is populated from mount.
      Shiny.setInputValue(el.id, el._cmView.state.doc.toString());
    },
    getValue: function (el) {
      return el._cmView ? el._cmView.state.doc.toString() : "";
    },
    subscribe: function (el, callback) {
      el._cmCallback = callback;
    },
    unsubscribe: function (el) {
      el._cmCallback = null;
    },
    getType: function () {
      return false;
    }
  });
  Shiny.inputBindings.register(binding, "blockr.extra.code");

  // R -> JS: replace the document (+ optionally enter/exit the inline diff).
  Shiny.addCustomMessageHandler("blockr-code-set", (msg) => {
    const el = document.getElementById(msg.id);
    if (el) setCode(el, msg.code, msg.original);
  });

  // R -> JS: Accept all — keep the AI code (already live), just close the diff.
  Shiny.addCustomMessageHandler("blockr-code-merge-clear", (msg) => {
    const el = document.getElementById(msg.id);
    if (el) clearMerge(el);
  });

  // R -> JS: Reject all — restore the diff's own original, close it, run it.
  Shiny.addCustomMessageHandler("blockr-code-reject-all", (msg) => {
    const el = document.getElementById(msg.id);
    const view = el && el._cmView;
    if (!view) return;
    const orig = getOriginalDoc(view.state).toString();
    el._cmSelfWrite = true;
    view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: orig } });
    clearMerge(el);
    el._cmSelfWrite = false;
    if (el._cmRunNow) el._cmRunNow();   // commit + run the restored original
  });

  // R -> JS: update the column-name completions for a given editor.
  Shiny.addCustomMessageHandler("blockr-code-completions", (msg) => {
    const el = document.getElementById(msg.id);
    if (!el) return;
    el._cmCompletions = Array.isArray(msg.columns)
      ? msg.columns
      : (msg.columns == null ? [] : [msg.columns]);
  });

  // CodeMirror mis-measures when created hidden. Re-measure when revealed.
  window.Blockr = window.Blockr || {};
  window.Blockr.Code = {
    refresh: (id) => {
      const el = document.getElementById(id);
      if (el && el._cmView) el._cmView.requestMeasure();
    }
  };
})();
