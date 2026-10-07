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
//
// A host that shows several texts as one document (blockr.page) treats the
// edges of each as passages to the next: at an edge, the arrow keys,
// Backspace and Delete raise `prose-edge` on the element, and a host that
// handles it cancels the event. A "/" typed on an empty line raises
// `prose-slash` the same way, for the host's insert menu. `el.blockrProse`
// is the controller, for focusAt(), join() and splitHere().

import { Editor, rootCtx, defaultValueCtx, editorViewCtx, serializerCtx } from "@milkdown/kit/core";
import { commonmark } from "@milkdown/kit/preset/commonmark";
import { gfm } from "@milkdown/kit/preset/gfm";
import { listener, listenerCtx } from "@milkdown/kit/plugin/listener";
import { replaceAll, getMarkdown } from "@milkdown/kit/utils";
import { Selection, TextSelection } from "@milkdown/kit/prose/state";
import { joinBackward } from "@milkdown/kit/prose/commands";
import { inlineRNode, inlineRRemark, inlineRPlugin, collectExprs } from "./inline-r.js";

const instances = new Map(); // el.id -> ProseBlock

// A few calls that come up in sentences, offered under the inputs.
const COMMON = ["nrow", "round", "mean", "median", "min", "max", "sum", "format", "scales::percent"];

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
    this.offer = {}; // object a host offers -> { name, cols } (blockr.page: the blocks above)
    this.values = {}; // expr -> {ok, value}
    this._sentExprs = "";
    this._applyingExternal = false;
    this._rawEditing = false;
    this.editor = null;
    this.field = null; // the open code field
    el.blockrProse = this;

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
      if (to && (this.el.contains(to) || (this.popup && this.popup.contains(to)))) return;
      if (this.field) return;
      this._commit();
    });
    this.el.addEventListener("keydown", (ev) => {
      if ((ev.ctrlKey || ev.metaKey) && ev.key === "Enter") {
        ev.preventDefault();
        this._commit();
      }
    });
    // before the editor's own keys
    this.editorHost.addEventListener("keydown", (ev) => this._edge(ev), true);
  }

  // ---- edges: the host's passage to the next text ----------------------------

  _view() {
    if (!this.editor) return null;
    try { return this.editor.action((ctx) => ctx.get(editorViewCtx)); } catch (e) { return null; }
  }

  _edge(ev) {
    if (ev.key === "/") return this._slash(ev);
    if (this.field || ev.defaultPrevented || ev.shiftKey || ev.altKey || ev.metaKey || ev.ctrlKey || ev.isComposing) return;
    const view = this._view();
    if (!view) return;
    const st = view.state, sel = st.selection, doc = st.doc;
    if (!sel.empty) return;
    const atStart = sel.head <= Selection.atStart(doc).head;
    const atEnd = sel.head >= Selection.atEnd(doc).head;
    const para = (n) => n && n.type.name === "paragraph";
    let dir = null;
    if (ev.key === "ArrowUp" && sel.$head.index(0) === 0 && view.endOfTextblock("up")) dir = "up";
    else if (ev.key === "ArrowDown" && sel.$head.index(0) === doc.childCount - 1 && view.endOfTextblock("down")) dir = "down";
    else if (ev.key === "ArrowLeft" && atStart) dir = "left";
    else if (ev.key === "ArrowRight" && atEnd) dir = "right";
    // Backspace at the start of a heading or a list lifts it, as it should
    else if (ev.key === "Backspace" && atStart && para(doc.firstChild)) dir = "back";
    else if (ev.key === "Delete" && atEnd && para(doc.lastChild)) dir = "forward";
    if (!dir) return;
    const caret = view.coordsAtPos(sel.head);
    const e = new CustomEvent("prose-edge", {
      bubbles: true,
      cancelable: true,
      detail: { dir: dir, x: caret.left, empty: this.isEmpty() }
    });
    if (!this.el.dispatchEvent(e)) {
      ev.preventDefault();
      ev.stopPropagation();
    }
  }

  // "/" on an empty line of its own, not in a list or a quote
  _slash(ev) {
    if (this.field || ev.defaultPrevented || ev.altKey || ev.metaKey || ev.ctrlKey || ev.isComposing) return;
    const view = this._view();
    if (!view) return;
    const sel = view.state.selection, $h = sel.$head;
    if (!sel.empty || $h.depth !== 1 || $h.parent.type.name !== "paragraph" || $h.parent.content.size) return;
    const r = view.coordsAtPos(sel.head);
    const e = new CustomEvent("prose-slash", {
      bubbles: true,
      cancelable: true,
      detail: { left: r.left, top: r.top, bottom: r.bottom }
    });
    if (!this.el.dispatchEvent(e)) {
      ev.preventDefault();
      ev.stopPropagation();
    }
  }

  // The text around the cursor's line, as markdown: what comes before the
  // line and what comes after it. The line itself is in neither.
  splitHere() {
    const view = this._view();
    if (!view) return null;
    const st = view.state, $h = st.selection.$head, doc = st.doc;
    if ($h.depth < 1) return null;
    const ser = this.editor.action((ctx) => ctx.get(serializerCtx));
    const md = (frag) => frag.size ? ser(doc.type.create(doc.attrs, frag)).replace(/\n+$/, "") : "";
    return { before: md(doc.content.cut(0, $h.before(1))), after: md(doc.content.cut($h.after(1))) };
  }

  // The cursor's line out of the text, unless it is the only one.
  dropLine() {
    const view = this._view();
    if (!view) return;
    const $h = view.state.selection.$head;
    if ($h.depth < 1 || view.state.doc.childCount < 2) return;
    view.dispatch(view.state.tr.delete($h.before(1), $h.after(1)));
  }

  // Text typed in for the host: at the cursor, as if typed.
  typeHere(text) {
    const view = this._view();
    if (!view) return;
    view.dispatch(view.state.tr.insertText(text));
    view.focus();
  }

  // The cursor into the text: "start", "end", or on its first or last line
  // ("first", "last") as close to `x` as the line allows; "here" keeps it
  // where it was.
  focusAt(where, x) {
    const view = this._view();
    if (!view) return false;
    if (where === "here") { view.focus(); return true; }
    const doc = view.state.doc;
    let sel = where === "start" || where === "first" ? Selection.atStart(doc) : Selection.atEnd(doc);
    if ((where === "first" || where === "last") && x != null) {
      const r = view.dom.getBoundingClientRect();
      const y = where === "first" ? r.top + 6 : r.bottom - 6;
      const hit = view.posAtCoords({ left: x, top: y });
      if (hit) sel = TextSelection.near(doc.resolve(hit.pos));
    }
    view.dispatch(view.state.tr.setSelection(sel).scrollIntoView());
    view.focus();
    return true;
  }

  // Another text joined onto the end of this one, as Backspace joins two
  // paragraphs: its first paragraph runs on from this one's last, the cursor
  // at the seam. Commits.
  join(md) {
    const view = this._view();
    if (!view) return;
    md = (md || "").replace(/\n+$/, "");
    if (!md.trim()) { this.focusAt("end"); return; }
    const seam = Selection.atEnd(view.state.doc).head;
    const mine = this.text();
    const both = mine.trim() ? mine + "\n\n" + md : md;
    this.markdown = both;
    this.textarea.value = both;
    this._applyToWysiwyg(both);
    const v = this._view();
    if (this.markdown !== md) {
      // the cursor at the start of the joined text, then join backward
      const $p = v.state.doc.resolve(Math.min(seam + 2, v.state.doc.content.size));
      v.dispatch(v.state.tr.setSelection(TextSelection.near($p)));
      joinBackward(v.state, v.dispatch);
    }
    v.focus();
    this._afterChange();
    this._commit();
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

  // The editor reports its markdown on a debounce; this reads it now, so a
  // commit or a host never sees text that is a few keystrokes old.
  text() {
    if (this.editor && !this._rawEditing) {
      try {
        const md = this.editor.action(getMarkdown()).replace(/\n+$/, "");
        if (md !== this.markdown) {
          this.markdown = md;
          if (this.textarea.value !== md) this.textarea.value = md;
          this._afterChange();
        }
      } catch (e) { /* not ready */ }
    }
    return this.markdown;
  }

  isEmpty() {
    const view = this._view();
    if (!view) return !this.markdown.trim();
    const doc = view.state.doc;
    return doc.childCount === 1 && doc.firstChild.isTextblock && doc.firstChild.content.size === 0;
  }

  _commit() {
    this.text();
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
      // pointing at a value shows its code, in the design system's tooltip
      chip.setAttribute("data-blockr-tooltip",
        "r " + expr + (rec && rec.ok === false ? "  \u00b7  " + rec.value : ""));
    });
  }

  setInputs(inputs) {
    this.inputs = inputs || {};
  }

  setOffer(offer) {
    this.offer = offer || {};
  }

  // What the code can name: the inputs, and what a host offers besides.
  _objects() {
    const out = {};
    Object.entries(this.offer).forEach(([k, o]) => { out[k] = { cols: (o && o.cols) || [], meta: (o && o.name) || "block" }; });
    Object.entries(this.inputs).forEach(([k, cols]) => { if (k) out[k] = { cols: cols || [], meta: (out[k] && out[k].meta) || "input" }; });
    return out;
  }

  // ---- the code field --------------------------------------------------------
  //
  // A chip opens in place as a small code field, a Blockr.Input field: the
  // line reflows around it, and its completions drop down in the field
  // dropdown of the design system (portalled with Blockr.place, a layer with
  // Blockr.layer). Enter or a closing backtick computes it, Escape leaves it
  // as it was (and drops a new one), Tab or Enter takes a completion.

  _openField(view, pos, isNew) {
    this._closeField(true);
    const node = view.state.doc.nodeAt(pos);
    const chip = view.nodeDOM(pos);
    if (!node || !chip) return;
    const expr = node.attrs.expr || "";
    chip.removeAttribute("data-blockr-tooltip");

    chip.classList.add("is-editing");
    chip.textContent = "";
    const box = document.createElement("span");
    box.className = "blockr-input blockr-r-field";
    const input = document.createElement("input");
    input.type = "text";
    input.className = "blockr-input__field";
    input.setAttribute("autocomplete", "off");
    input.setAttribute("aria-label", "R expression");
    input.spellcheck = false;
    input.value = expr;
    box.appendChild(input);
    chip.appendChild(box);
    // ch of the code face, plus the field's padding and border
    const grow = () => { input.style.width = "calc(" + Math.max(3, input.value.length + 1) + "ch + 14px)"; };
    grow();
    input.focus();
    input.setSelectionRange(input.value.length, input.value.length);

    this.field = { view, pos, chip, input, expr, isNew, items: [], cur: -1 };

    input.addEventListener("input", () => { grow(); this._suggest(); });
    input.addEventListener("keydown", (e) => {
      e.stopPropagation();
      const f = this.field;
      if (!f) return;
      const open = this._popupOpen();
      if (e.key === "ArrowDown" && open) { e.preventDefault(); f.cur = Math.min(f.items.length - 1, f.cur + 1); this._mark(); }
      else if (e.key === "ArrowUp" && open) { e.preventDefault(); f.cur = Math.max(0, f.cur - 1); this._mark(); }
      else if ((e.key === "Tab" || e.key === "Enter") && open && f.cur >= 0 &&
               f.items[f.cur].insert !== f.items[f.cur].token) { e.preventDefault(); this._take(f.items[f.cur]); }
      else if (e.key === "Enter") { e.preventDefault(); this._closeField(true); }
      else if (e.key === "`") { e.preventDefault(); this._closeField(true); }
      else if (e.key === "Escape") {
        // the layer closes an open list first; Escape on a closed list leaves
        e.preventDefault();
        if (!f.listJustClosed) this._closeField(false);
      }
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
    this._hidePopup();
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

  // ---- completions: the inputs and a few common calls, as Blockr.Input
  // lists columns and functions; after `name$`, the columns of that input.

  _popupOpen() {
    return !!(this.popup && this.popup.style.display === "block" && this.field && this.field.items.length);
  }

  _hidePopup() {
    if (this.placement) { this.placement.stop(); this.placement = null; }
    if (this.layer) { this.layer.remove(); this.layer = null; }
    if (this.popup) { this.popup.style.display = ""; this.popup.innerHTML = ""; }
  }

  _suggest() {
    const f = this.field;
    if (!f) return;
    const v = f.input.value.slice(0, f.input.selectionStart == null ? f.input.value.length : f.input.selectionStart);
    const word = /[A-Za-z0-9_.:]*$/.exec(v)[0];
    const items = [];
    const dollar = /([A-Za-z.][A-Za-z0-9_.]*)\$([A-Za-z0-9_.]*)$/.exec(v);
    const quote = (c) => (/^[A-Za-z.][A-Za-z0-9_.]*$/.test(c) ? c : "`" + c + "`");
    const objs = this._objects();
    if (dollar && objs[dollar[1]]) {
      const part = dollar[2].toLowerCase();
      (objs[dollar[1]].cols || [])
        .filter((c) => String(c).toLowerCase().startsWith(part))
        .forEach((c) => items.push({ text: String(c), insert: quote(String(c)), token: dollar[2], meta: "column" }));
    } else if (word) {
      Object.keys(objs)
        .filter((n) => n.toLowerCase().startsWith(word.toLowerCase()))
        .forEach((n) => items.push({ text: n, insert: n, token: word, meta: objs[n].meta }));
      COMMON
        .filter((c) => c.toLowerCase().startsWith(word.toLowerCase()))
        .forEach((c) => items.push({ text: c, insert: c, token: word, meta: "often used", fn: true }));
    } else if (!f.input.value.trim()) {
      // an empty field offers the inputs
      Object.keys(objs)
        .forEach((n) => items.push({ text: n, insert: n, token: "", meta: objs[n].meta }));
    }
    f.items = items;
    f.cur = items.length ? 0 : -1;
    if (!items.length) { this._hidePopup(); return; }

    if (!this.popup) {
      this.popup = document.createElement("div");
      this.popup.className = "blockr-input__popup blockr-r-popup";
      this.popup.setAttribute("role", "listbox");
      this.popup.addEventListener("mousedown", (e) => {
        e.preventDefault();
        const row = e.target.closest("[data-i]");
        if (row && this.field) this._take(this.field.items[+row.dataset.i]);
      });
    }
    this.popup.innerHTML = items.map((it, i) =>
      `<div class="blockr-input__item" role="option" data-i="${i}">` +
      `<span class="blockr-input__item-text">${esc(it.text)}</span>` +
      (it.fn ? '<span class="blockr-input__item-parens">()</span>' : "") +
      `<span class="blockr-input__item-meta">${esc(it.meta)}</span></div>`).join("");
    if (this.popup.style.display !== "block") {
      document.body.appendChild(this.popup);
      this.popup.style.display = "block";
      if (window.Blockr && Blockr.place) {
        this.placement = Blockr.place(this.popup, f.chip, { gap: 2, width: { min: 220, max: 320 } });
      }
      if (window.Blockr && Blockr.layer) {
        this.layer = Blockr.layer(this.popup, {
          from: f.chip,
          escape: () => {
            const g = this.field;
            this._hidePopup();
            if (g) { g.listJustClosed = true; setTimeout(() => { g.listJustClosed = false; }, 0); }
          },
          outside: () => this._hidePopup(),
        });
      }
    }
    this._mark();
  }

  _mark() {
    if (!this.popup || !this.field) return;
    this.popup.querySelectorAll("[data-i]").forEach((row) =>
      row.classList.toggle("blockr-input__item--highlighted", +row.dataset.i === this.field.cur));
  }

  _take(it) {
    const f = this.field;
    if (!f || !it) return;
    const inp = f.input;
    const at = inp.selectionStart == null ? inp.value.length : inp.selectionStart;
    const start = at - it.token.length;
    const ins = it.fn ? it.insert + "()" : it.insert;
    inp.value = inp.value.slice(0, start) + ins + inp.value.slice(at);
    // a function puts the cursor between its parentheses
    const caret = start + (it.fn ? ins.length - 1 : ins.length);
    inp.setSelectionRange(caret, caret);
    inp.dispatchEvent(new Event("input"));
    inp.focus();
    // a finished name needs no list
    if (!it.fn) this._hidePopup();
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
    // A host (blockr.page) offers objects the text may name besides the
    // inputs: naming one is how the text comes to read from it.
    Shiny.addCustomMessageHandler("prose-offer", (msg) => {
      const inst = instances.get(msg.id);
      if (inst) inst.setOffer(msg.objects || {});
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
