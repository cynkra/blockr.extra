// blockr.extra prose block: markdown text with inline R, edited as it reads.
//
// One JS controller owns the markdown (inline R as `r expr`, Quarto's own
// form) and drives a Milkdown editor, with the raw markdown in a collapsed
// section under it. Sync is client-side and guarded against echo loops.
//
// Two clocks:
//   - The inline EXPRESSIONS go to R eagerly on every edit (input
//     <exprs-id>); R evaluates each on its own and pushes the values back
//     (`prose-values`), which are painted into the chips. Never touches the
//     DAG.
//   - The TEXT commits on blur or Ctrl/Cmd+Enter to input <input-id>, never
//     on a keystroke.
//
// R -> JS push (`prose-set`) handles external (AI) writes.

import { Editor, rootCtx, defaultValueCtx } from "@milkdown/kit/core";
import { commonmark } from "@milkdown/kit/preset/commonmark";
import { gfm } from "@milkdown/kit/preset/gfm";
import { listener, listenerCtx } from "@milkdown/kit/plugin/listener";
import { replaceAll } from "@milkdown/kit/utils";
import { inlineRNode, inlineRRemark, inlineRPlugin, collectExprs } from "./inline-r.js";

const instances = new Map(); // el.id -> ProseBlock

// A few calls that come up in sentences, offered under the inputs.
const COMMON = [
  ["nrow()", "rows"],
  ["round(, 1)", "rounded"],
  ["mean()", "average"],
  ["median()", "median"],
  ["format(, big.mark = \",\")", "12,345"],
  ["scales::percent()", "12%"],
];

function setShinyInput(id, value) {
  if (window.Shiny && Shiny.setInputValue) {
    Shiny.setInputValue(id, value, { priority: "event" });
  }
}

const esc = (s) => String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;");

class ProseBlock {
  constructor(el) {
    this.el = el;
    this.inputId = el.dataset.inputId;
    this.exprsId = el.dataset.exprsId;
    this.markdown = el.dataset.initial || "";
    this.committed = this.markdown;
    this.inputs = {}; // input name -> [columns]
    this.values = {}; // expr -> {ok, value}
    this._sentExprs = "";
    this._applyingExternal = false;
    this._rawEditing = false;
    this.editor = null;
    this.field = null; // the open code field

    this._buildDom();
    this._initEditor().catch((e) => console.error("[prose] init failed", e));
    this._bindCommit();
  }

  // ---- DOM -----------------------------------------------------------------

  _buildDom() {
    this.el.classList.add("blockr-prose");

    this.editorHost = document.createElement("div");
    this.editorHost.className = "blockr-prose-editor";

    // The raw markdown, collapsed under the text.
    this.details = document.createElement("details");
    this.details.className = "blockr-prose-source";
    const summary = document.createElement("summary");
    summary.textContent = "Markdown";
    this.textarea = document.createElement("textarea");
    this.textarea.className = "blockr-prose-source-ta";
    this.textarea.rows = 6;
    this.textarea.spellcheck = false;
    this.textarea.value = this.markdown;
    this.textarea.addEventListener("input", () => this._onRawEdit());
    this.details.appendChild(summary);
    this.details.appendChild(this.textarea);

    this.el.appendChild(this.editorHost);
    this.el.appendChild(this.details);
  }

  async _initEditor() {
    const self = this;
    this.editor = await Editor.make()
      .config((ctx) => {
        ctx.set(rootCtx, this.editorHost);
        ctx.set(defaultValueCtx, this.markdown);
        ctx.get(listenerCtx).markdownUpdated((_ctx, md) => self._onWysiwygUpdate(md));
      })
      .use(commonmark)
      .use(gfm)
      .use(listener)
      .use(inlineRRemark)
      .use(inlineRNode)
      .use(inlineRPlugin((view, pos, isNew) => self._openField(view, pos, isNew)))
      .create();

    this._sendExprs();
    this._paint();
  }

  _bindCommit() {
    // Leaving the block commits; the code field and its suggestions are part
    // of it.
    this.el.addEventListener("focusout", (ev) => {
      const to = ev.relatedTarget;
      if (to && (this.el.contains(to) || (this.sug && this.sug.contains(to)))) return;
      if (this.field) return;
      this._commit();
    });
    this.el.addEventListener("keydown", (ev) => {
      if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") {
        ev.preventDefault();
        this._commit();
      }
    });
  }

  // ---- sync ------------------------------------------------------------------

  _onWysiwygUpdate(md) {
    if (this._applyingExternal) return;
    this.markdown = md.replace(/\n+$/, "");
    if (!this._rawEditing && this.textarea.value !== this.markdown) {
      this.textarea.value = this.markdown;
    }
    this._afterChange();
  }

  _onRawEdit() {
    this.markdown = this.textarea.value;
    this._rawEditing = true;
    try {
      this._applyToWysiwyg(this.markdown);
    } finally {
      Promise.resolve().then(() => { this._rawEditing = false; });
    }
    this._afterChange();
  }

  _applyToWysiwyg(md) {
    if (!this.editor) return;
    this._applyingExternal = true;
    try {
      this.editor.action(replaceAll(md));
    } finally {
      Promise.resolve().then(() => { this._applyingExternal = false; });
    }
  }

  _afterChange() {
    this._sendExprs();
    this._paint();
  }

  _commit() {
    if (this.markdown === this.committed) return;
    this.committed = this.markdown;
    setShinyInput(this.inputId, this.markdown);
  }

  // External / AI write: update both views, do NOT commit back to R.
  setMarkdown(md) {
    if (md === this.markdown && md === this.committed) return;
    this.markdown = md;
    this.committed = md;
    if (this.textarea.value !== md) this.textarea.value = md;
    this._applyToWysiwyg(md);
    this._sendExprs();
    Promise.resolve().then(() => this._paint());
  }

  // ---- values ----------------------------------------------------------------

  _sendExprs() {
    if (!this.exprsId) return;
    const exprs = collectExprs(this.markdown);
    const key = JSON.stringify(exprs);
    if (key === this._sentExprs) return;
    this._sentExprs = key;
    setShinyInput(this.exprsId, exprs);
  }

  setValues(values) {
    this.values = values || {};
    this._paint();
  }

  // Every chip from the value store: its value when the server sent one, its
  // code while it has not (no data yet), its code in red when it failed.
  _paint() {
    this.editorHost.querySelectorAll(".blockr-r-chip").forEach((chip) => {
      if (chip.classList.contains("is-editing")) return;
      const expr = chip.getAttribute("data-r") || "";
      const rec = this.values[expr];
      const ok = rec && rec.ok === true;
      chip.classList.toggle("is-err", !!rec && rec.ok === false);
      chip.classList.toggle("is-dormant", !ok && !(rec && rec.ok === false));
      const text = ok ? rec.value : expr;
      if (chip.textContent !== text) chip.textContent = text;
      chip.setAttribute("data-tip", rec && rec.ok === false ? expr + "  ·  " + rec.value : expr);
    });
  }

  setInputs(inputs) {
    this.inputs = inputs || {};
  }

  // ---- the code field --------------------------------------------------------
  //
  // A chip opens in place as a small code field: the line reflows around it,
  // nothing floats over the text but the suggestions. Enter or a closing
  // backtick computes it, Escape leaves it as it was (and drops a new one),
  // Tab takes a suggestion.

  _openField(view, pos, isNew) {
    this._closeField(true);
    const node = view.state.doc.nodeAt(pos);
    const chip = view.nodeDOM(pos);
    if (!node || !chip) return;
    const expr = node.attrs.expr || "";

    chip.classList.add("is-editing");
    chip.textContent = "";
    const r = document.createElement("span");
    r.className = "blockr-r-tag";
    r.textContent = "r";
    const input = document.createElement("input");
    input.type = "text";
    input.className = "blockr-r-input";
    input.spellcheck = false;
    input.value = expr;
    chip.appendChild(r);
    chip.appendChild(input);
    const grow = () => { input.style.width = Math.max(2, input.value.length + 1) + "ch"; };
    grow();
    input.focus();
    input.setSelectionRange(input.value.length, input.value.length);

    this.field = { view, pos, chip, input, expr, isNew, items: [], cur: 0 };

    input.addEventListener("input", () => { grow(); this._suggest(); });
    input.addEventListener("keydown", (e) => {
      e.stopPropagation();
      const f = this.field;
      if (!f) return;
      const open = this.sug && !this.sug.hidden && f.items.length;
      if (e.key === "ArrowDown" && open) { e.preventDefault(); f.moved = true; f.cur = Math.min(f.items.length - 1, f.cur + 1); this._mark(); }
      else if (e.key === "ArrowUp" && open) { e.preventDefault(); f.moved = true; f.cur = Math.max(0, f.cur - 1); this._mark(); }
      else if (e.key === "Tab" && open) { e.preventDefault(); this._take(f.items[f.cur]); }
      else if (e.key === "Enter") {
        e.preventDefault();
        // Enter takes a suggestion only while one is chosen with the arrows
        // or a name is half typed; otherwise it computes the field.
        const word = /[A-Za-z0-9_.:$]*$/.exec(input.value)[0];
        if (open && (f.moved || word) && f.items[f.cur].ins !== input.value) this._take(f.items[f.cur]);
        else this._closeField(true);
      }
      else if (e.key === "`") { e.preventDefault(); this._closeField(true); }
      else if (e.key === "Escape") { e.preventDefault(); this._closeField(false); }
    });
    input.addEventListener("mousedown", (e) => e.stopPropagation());
    input.addEventListener("blur", () => setTimeout(() => {
      if (this.field && this.field.input === input && document.activeElement !== input) this._closeField(true);
    }, 0));
    this._suggest();
  }

  _closeField(keep) {
    const f = this.field;
    if (!f) return;
    this.field = null;
    if (this.sug) this.sug.hidden = true;
    const next = f.input.value.trim();
    f.chip.classList.remove("is-editing");
    const view = f.view;
    const node = view.state.doc.nodeAt(f.pos);
    const still = node && node.type.name === "inline_r";
    if (still && (keep ? !next : f.isNew)) {
      // an empty field, or a new one left with Escape, goes
      view.dispatch(view.state.tr.delete(f.pos, f.pos + node.nodeSize));
    } else if (still && keep && next !== f.expr) {
      // a new expression rebuilds the chip, which sends it for a value
      view.dispatch(view.state.tr.setNodeMarkup(f.pos, undefined, { expr: next }));
    } else {
      f.chip.textContent = f.expr;
      this._paint();
    }
    view.focus();
  }

  // Suggestions: the inputs, then common calls; after `name$`, the columns of
  // that input.
  _suggest() {
    const f = this.field;
    if (!f) return;
    if (!this.sug) {
      this.sug = document.createElement("div");
      this.sug.className = "blockr-r-sug";
      this.sug.hidden = true;
      this.sug.addEventListener("mousedown", (e) => {
        e.preventDefault();
        const row = e.target.closest("[data-i]");
        if (row && this.field) this._take(this.field.items[+row.dataset.i]);
      });
      document.body.appendChild(this.sug);
    }
    const v = f.input.value;
    const word = /[A-Za-z0-9_.:$]*$/.exec(v)[0];
    const items = [];
    const dollar = /([A-Za-z.][A-Za-z0-9_.]*)\$([A-Za-z0-9_.]*)$/.exec(v);
    let head = "";
    if (dollar && this.inputs[dollar[1]]) {
      head = "In " + dollar[1];
      (this.inputs[dollar[1]] || [])
        .filter((c) => c.toLowerCase().startsWith(dollar[2].toLowerCase()))
        .forEach((c) => {
          const name = /^[A-Za-z.][A-Za-z0-9_.]*$/.test(c) ? c : "`" + c + "`";
          items.push({ ins: v.slice(0, v.length - dollar[2].length) + name, label: c, meta: "" });
        });
    } else if (word || !v.trim()) {
      // while a name is being typed, or in an empty field
      Object.keys(this.inputs)
        .filter((n) => n && n.startsWith(word))
        .forEach((n) => items.push({ ins: v.slice(0, v.length - word.length) + n, label: n, meta: "input", grp: "in" }));
      COMMON
        .filter(([c]) => !word || c.startsWith(word))
        .forEach(([c, m]) => items.push({ ins: v.slice(0, v.length - word.length) + c, label: c, meta: m, grp: "fn", fn: true }));
    }
    f.items = items;
    f.cur = 0;
    f.moved = false;
    if (!items.length) { this.sug.hidden = true; return; }
    let html = head ? `<div class="blockr-r-sug-title">${esc(head)}</div>` : "";
    items.forEach((it, i) => {
      if (!head && (i === 0 || items[i - 1].grp !== it.grp)) {
        html += `<div class="blockr-r-sug-title">${it.grp === "in" ? "Inputs" : "Often used"}</div>`;
      }
      html += `<div class="blockr-r-sug-row" data-i="${i}"><code>${esc(it.label)}</code><span>${esc(it.meta)}</span></div>`;
    });
    this.sug.innerHTML = html;
    const r = f.chip.getBoundingClientRect();
    this.sug.style.left = (r.left + window.scrollX) + "px";
    this.sug.style.top = (r.bottom + window.scrollY + 6) + "px";
    this.sug.hidden = false;
    this._mark();
  }

  _mark() {
    if (!this.sug || !this.field) return;
    this.sug.querySelectorAll("[data-i]").forEach((row) =>
      row.classList.toggle("is-cur", +row.dataset.i === this.field.cur));
  }

  _take(it) {
    const f = this.field;
    if (!f || !it) return;
    f.input.value = it.ins;
    f.input.dispatchEvent(new Event("input"));
    f.input.focus();
    // a function puts the cursor inside its parentheses
    if (it.fn) {
      const p = it.ins.lastIndexOf("(") + 1;
      f.input.setSelectionRange(p, p);
    }
  }
}

// ---- init & message handlers --------------------------------------------------

function initEl(el) {
  if (!el || !el.id || instances.has(el.id)) return;
  if (!el.classList.contains("blockr-prose") && !el.dataset.inputId) return;
  instances.set(el.id, new ProseBlock(el));
}

function scan(root) {
  (root || document).querySelectorAll(".blockr-prose[data-input-id]").forEach(initEl);
}

function register() {
  scan(document);

  const mo = new MutationObserver((muts) => {
    for (const m of muts) {
      m.addedNodes.forEach((n) => {
        if (n.nodeType !== 1) return;
        if (n.matches && n.matches(".blockr-prose[data-input-id]")) initEl(n);
        if (n.querySelectorAll) scan(n);
      });
    }
  });
  mo.observe(document.body, { childList: true, subtree: true });

  // A session (re)connect re-announces every instance's expressions: the
  // first send can predate Shiny.setInputValue, and a reconnect wipes the
  // server's inputs. shiny:connected is a jQuery event.
  if (window.jQuery) {
    window.jQuery(document).on("shiny:connected", () => {
      instances.forEach((inst) => {
        inst._sentExprs = "";
        inst._sendExprs();
      });
    });
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler("prose-set", (msg) => {
      const inst = instances.get(msg.id);
      if (inst) inst.setMarkdown(msg.markdown || "");
    });
    Shiny.addCustomMessageHandler("prose-columns", (msg) => {
      const inst = instances.get(msg.id);
      if (inst) inst.setInputs(msg.inputs || {});
    });
    Shiny.addCustomMessageHandler("prose-values", (msg) => {
      const inst = instances.get(msg.id);
      if (inst) inst.setValues(msg.values || {});
    });
  }
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", register);
} else {
  register();
}
