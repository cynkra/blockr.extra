// blockr.extra prose block — WYSIWYG-first markdown notes with glue chips.
//
// One JS controller owns the canonical STORED-FORM markdown (chips as
// `{expr}`, literal braces doubled — see glue-remark.js) and drives two
// views: a Milkdown WYSIWYG editor (primary) and a raw markdown <textarea>
// showing the stored form (advanced, collapsed). Sync is client-side and
// guarded against echo loops.
//
// Two clocks:
//   - Chip EXPRESSIONS go to R eagerly on every edit (input <exprs-id>); R
//     evaluates each on its own and pushes values back (`prose-values`),
//     which patch the chip DOM. Never touches the DAG.
//   - The DOCUMENT commits explicitly — blur, Ctrl/Cmd-Enter, or the Apply
//     footer that appears while dirty — to input <input-id>. Never on a
//     keystroke, never on a timer.
//
// R -> JS push (`prose-set`) handles external (AI) writes. See
// blockr.design/open/prose-block.

import { Editor, rootCtx, defaultValueCtx, editorViewCtx } from "@milkdown/kit/core";
import { commonmark } from "@milkdown/kit/preset/commonmark";
import { gfm } from "@milkdown/kit/preset/gfm";
import { listener, listenerCtx } from "@milkdown/kit/plugin/listener";
import { replaceAll } from "@milkdown/kit/utils";
import { glueNode, glueRemark, glueClickPlugin } from "./glue.js";
import { toStoredForm, collectExprs } from "./glue-remark.js";

const instances = new Map(); // el.id -> ProseBlock

function setShinyInput(id, value) {
  if (window.Shiny && Shiny.setInputValue) {
    Shiny.setInputValue(id, value, { priority: "event" });
  }
}

class ProseBlock {
  constructor(el) {
    this.el = el;
    this.inputId = el.dataset.inputId;
    this.exprsId = el.dataset.exprsId;
    this.markdown = el.dataset.initial || ""; // stored form, always
    this.committed = this.markdown;
    this.inputs = {}; // input name -> [columns]
    this.values = {}; // expr -> {ok, value}
    this._sentExprs = "";
    this._applyingExternal = false;
    this._rawEditing = false;
    this.editor = null;

    this._buildDom();
    this._initEditor();
    this._bindCommit();
  }

  // ---- DOM ---------------------------------------------------------------

  _buildDom() {
    this.el.classList.add("blockr-prose");

    // Toolbar: insert a data field, toggle every chip to its code.
    this.toolbar = document.createElement("div");
    this.toolbar.className = "blockr-prose-toolbar";

    this.fieldBtn = document.createElement("button");
    this.fieldBtn.type = "button";
    this.fieldBtn.className = "btn btn-sm btn-outline-secondary blockr-prose-field-btn";
    this.fieldBtn.textContent = "+ Data field";
    this.fieldMenu = document.createElement("div");
    this.fieldMenu.className = "blockr-prose-field-menu";
    this.fieldMenu.hidden = true;
    this.fieldBtn.addEventListener("click", (e) => {
      e.preventDefault();
      this._toggleFieldMenu();
    });

    this.exprBtn = document.createElement("button");
    this.exprBtn.type = "button";
    this.exprBtn.className = "btn btn-sm btn-outline-secondary blockr-prose-expr-btn";
    this.exprBtn.textContent = "{ } Code";
    this.exprBtn.title = "Show every reference as its R expression (Ctrl+`)";
    this.exprBtn.addEventListener("click", (e) => {
      e.preventDefault();
      this._toggleExpr();
    });

    this.toolbar.appendChild(this.fieldBtn);
    this.toolbar.appendChild(this.fieldMenu);
    this.toolbar.appendChild(this.exprBtn);

    // Milkdown mount point
    this.editorHost = document.createElement("div");
    this.editorHost.className = "blockr-prose-editor";

    // Raw markdown source (collapsed advanced section, STORED form)
    this.details = document.createElement("details");
    this.details.className = "blockr-prose-source";
    const summary = document.createElement("summary");
    summary.textContent = "Markdown source";
    this.textarea = document.createElement("textarea");
    this.textarea.className = "form-control blockr-prose-source-ta";
    this.textarea.rows = 6;
    this.textarea.value = this.markdown;
    this.textarea.addEventListener("input", () => this._onRawEdit());
    this.details.appendChild(summary);
    this.details.appendChild(this.textarea);

    // Dirty footer: appears while the editor holds uncommitted text.
    this.footer = document.createElement("div");
    this.footer.className = "blockr-prose-footer";
    this.footer.hidden = true;
    const note = document.createElement("span");
    note.className = "blockr-prose-footer-note";
    note.textContent = "Edited. Not applied yet.";
    const kbd = document.createElement("span");
    kbd.className = "blockr-prose-footer-kbd";
    kbd.textContent = "Ctrl+Enter";
    this.applyBtn = document.createElement("button");
    this.applyBtn.type = "button";
    this.applyBtn.className = "btn btn-sm btn-primary blockr-prose-apply";
    this.applyBtn.textContent = "Apply";
    this.applyBtn.addEventListener("click", (e) => {
      e.preventDefault();
      this._commit();
    });
    this.footer.appendChild(note);
    this.footer.appendChild(kbd);
    this.footer.appendChild(this.applyBtn);

    this.el.appendChild(this.toolbar);
    this.el.appendChild(this.editorHost);
    this.el.appendChild(this.footer);
    this.el.appendChild(this.details);
  }

  async _initEditor() {
    const self = this;
    this.editor = await Editor.make()
      .config((ctx) => {
        ctx.set(rootCtx, this.editorHost);
        ctx.set(defaultValueCtx, this.markdown);
        ctx.get(listenerCtx).markdownUpdated((_ctx, md) => {
          self._onWysiwygUpdate(md);
        });
      })
      .use(commonmark)
      .use(gfm)
      .use(listener)
      .use(glueRemark)
      .use(glueNode)
      .use(glueClickPlugin((view, node, pos) => self._editChip(view, node, pos)))
      .create();

    // Values for the initial document's chips, and their first paint.
    this._sendExprs();
    this._patchChips();
  }

  _bindCommit() {
    // Leaving the block commits; moving between the WYSIWYG and the raw
    // textarea does not (both live inside this.el).
    this.el.addEventListener("focusout", (ev) => {
      if (ev.relatedTarget && this.el.contains(ev.relatedTarget)) return;
      this._commit();
    });
    this.el.addEventListener("keydown", (ev) => {
      if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") {
        ev.preventDefault();
        this._commit();
      }
      if ((ev.ctrlKey || ev.metaKey) && ev.key === "`") {
        ev.preventDefault();
        this._toggleExpr();
      }
    });
  }

  // ---- sync --------------------------------------------------------------

  // WYSIWYG edits arrive as editor-form markdown (sentinels + raw braces);
  // normalize to stored form. While the raw textarea is being typed in, it is
  // the source of truth and must not be rewritten under the cursor.
  _onWysiwygUpdate(md) {
    if (this._applyingExternal) return;
    const stored = toStoredForm(md);
    this.markdown = stored;
    if (!this._rawEditing && this.textarea.value !== stored) {
      this.textarea.value = stored;
    }
    this._afterChange();
  }

  // The raw textarea IS the stored form: no normalization, power users write
  // their own escapes. The WYSIWYG re-parses it (transformer unescapes and
  // chips R-looking spans).
  _onRawEdit() {
    this.markdown = this.textarea.value;
    this._rawEditing = true;
    try {
      this._applyToWysiwyg(this.markdown);
    } finally {
      Promise.resolve().then(() => {
        this._rawEditing = false;
      });
    }
    this._afterChange();
  }

  _applyToWysiwyg(md) {
    if (!this.editor) return;
    this._applyingExternal = true;
    try {
      this.editor.action(replaceAll(md));
    } finally {
      // markdownUpdated fires during dispatch; clear on next tick.
      Promise.resolve().then(() => {
        this._applyingExternal = false;
      });
    }
  }

  _afterChange() {
    this._setDirty(this.markdown !== this.committed);
    this._sendExprs();
    this._patchChips();
  }

  _setDirty(on) {
    this.footer.hidden = !on;
    this.el.classList.toggle("is-dirty", on);
  }

  // Commit the stored form to R. Cheap when clean; the guard also stops a
  // stale blur from re-sending what an external write just replaced.
  _commit() {
    if (this.markdown === this.committed) {
      this._setDirty(false);
      return;
    }
    this.committed = this.markdown;
    setShinyInput(this.inputId, this.markdown);
    this._setDirty(false);
  }

  // External / AI write: update both views, do NOT commit back to R.
  setMarkdown(md) {
    if (md === this.markdown && md === this.committed) return;
    this.markdown = md;
    this.committed = md;
    if (this.textarea.value !== md) this.textarea.value = md;
    this._applyToWysiwyg(md);
    this._setDirty(false);
    this._sendExprs();
    // The re-parse rebuilds every chip; paint after the dispatch settles.
    Promise.resolve().then(() => this._patchChips());
  }

  // ---- chip values ---------------------------------------------------------

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
    this._patchChips();
  }

  // Paint every chip from the value store: value face when the server sent
  // one, dormant code face when it has not (no data yet, or a chip newer than
  // the last round trip), red code face on an evaluation error.
  _patchChips() {
    const chips = this.editorHost.querySelectorAll(".blockr-glue-chip");
    chips.forEach((chip) => {
      const expr = chip.getAttribute("data-glue") || "";
      const val = chip.querySelector(".blockr-glue-val");
      const rec = this.values[expr];
      if (rec && rec.ok === true) {
        chip.classList.remove("is-dormant", "is-err");
        if (val && val.textContent !== rec.value) val.textContent = rec.value;
        chip.title = "{" + expr + "}";
      } else if (rec && rec.ok === false) {
        chip.classList.add("is-err");
        chip.classList.remove("is-dormant");
        if (val) val.textContent = expr;
        chip.title = rec.value || "error";
      } else {
        chip.classList.add("is-dormant");
        chip.classList.remove("is-err");
        if (val) val.textContent = expr;
        chip.title = "{" + expr + "}";
      }
    });
  }

  _toggleExpr() {
    const on = this.el.classList.toggle("show-expr");
    this.exprBtn.classList.toggle("active", on);
    this.exprBtn.textContent = on ? "{ } Values" : "{ } Code";
  }

  // ---- data-field menu -----------------------------------------------------

  setInputs(inputs) {
    this.inputs = inputs || {};
    this._renderFieldMenu();
  }

  _renderFieldMenu() {
    this.fieldMenu.innerHTML = "";
    const names = Object.keys(this.inputs);
    if (!names.length) {
      const empty = document.createElement("div");
      empty.className = "blockr-prose-field-empty";
      empty.textContent = "No data input connected";
      this.fieldMenu.appendChild(empty);
      return;
    }
    for (const name of names) {
      const cols = this.inputs[name] || [];
      const prefix = /^[A-Za-z._][A-Za-z0-9._]*$/.test(name) ? name + "$" : "";
      const nrows = "nrow(" + name + ")";
      const head = document.createElement("button");
      head.type = "button";
      head.className = "blockr-prose-field-item";
      head.textContent = nrows;
      head.addEventListener("click", (e) => {
        e.preventDefault();
        this._insertChip(nrows);
        this._toggleFieldMenu(false);
      });
      this.fieldMenu.appendChild(head);
      for (const col of cols) {
        const item = document.createElement("button");
        item.type = "button";
        item.className = "blockr-prose-field-item";
        item.textContent = prefix + col;
        item.addEventListener("click", (e) => {
          e.preventDefault();
          this._insertChip(prefix + col);
          this._toggleFieldMenu(false);
        });
        this.fieldMenu.appendChild(item);
      }
    }
  }

  _toggleFieldMenu(force) {
    this.fieldMenu.hidden = force === undefined ? !this.fieldMenu.hidden : !force;
  }

  _insertChip(expr) {
    if (!this.editor) return;
    this.editor.action((ctx) => {
      const view = ctx.get(editorViewCtx);
      const type = view.state.schema.nodes.glue_ref;
      if (!type) return;
      const node = type.create({ expr });
      view.dispatch(view.state.tr.replaceSelectionWith(node).scrollIntoView());
      view.focus();
    });
  }

  // ---- in-place chip edit ----------------------------------------------------
  //
  // The chip's own DOM becomes a small mono input where it sits; Enter or
  // click-away applies (a setNodeMarkup transaction rebuilds the chip), Esc
  // reverts. Nothing floats over the text and the line reflows around the
  // input, so the effect of an edit is visible immediately.

  _editChip(view, node, pos) {
    const chip = view.nodeDOM(pos);
    if (!chip || chip.querySelector("input")) return;
    const expr = node.attrs.expr || "";

    chip.classList.add("is-editing");
    const faces = Array.from(chip.children);
    faces.forEach((f) => (f.style.display = "none"));

    const input = document.createElement("input");
    input.type = "text";
    input.className = "blockr-prose-chip-input";
    input.value = expr;
    input.size = Math.max(expr.length, 6);
    input.setAttribute("spellcheck", "false");
    chip.appendChild(input);
    input.focus();
    input.select();

    let done = false;
    const finish = (commit) => {
      if (done) return;
      done = true;
      const next = input.value.trim();
      input.remove();
      faces.forEach((f) => (f.style.display = ""));
      chip.classList.remove("is-editing");
      if (commit && next && next !== expr) {
        const tr = view.state.tr.setNodeMarkup(pos, undefined, { expr: next });
        view.dispatch(tr);
        // setNodeMarkup rebuilds the chip -> markdownUpdated -> exprs+paint.
      }
    };

    input.addEventListener("input", () => {
      input.size = Math.max(input.value.length, 6);
    });
    input.addEventListener("keydown", (e) => {
      e.stopPropagation();
      if (e.key === "Enter") {
        e.preventDefault();
        finish(true);
      }
      if (e.key === "Escape") {
        e.preventDefault();
        finish(false);
      }
    });
    input.addEventListener("mousedown", (e) => e.stopPropagation());
    input.addEventListener("blur", () => finish(true));
  }
}

// ---- init & message handlers ------------------------------------------

function initEl(el) {
  if (!el || !el.id || instances.has(el.id)) return;
  if (!el.classList.contains("blockr-prose") && !el.dataset.inputId) return;
  instances.set(el.id, new ProseBlock(el));
}

function scan(root) {
  const nodes = (root || document).querySelectorAll(".blockr-prose[data-input-id]");
  nodes.forEach(initEl);
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

  // A session (re)connect re-announces every instance's chip expressions:
  // the initial send can predate the Shiny connection (a deferred dock
  // panel), and a reconnect wipes server-side inputs.
  document.addEventListener("shiny:connected", () => {
    instances.forEach((inst) => {
      inst._sentExprs = "";
      inst._sendExprs();
    });
  });

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
