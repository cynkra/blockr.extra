// blockr.extra prose block — WYSIWYG-first markdown notes with glue chips.
//
// One JS controller owns the canonical markdown string and drives two views:
// a Milkdown WYSIWYG editor (primary) and a raw markdown <textarea> (advanced,
// collapsed). Sync is client-side and guarded against echo loops. The committed
// markdown crosses to R on debounce; R -> JS push (prose-set) handles external
// (AI) writes. See blockr.design/open/prose-block.

import { Editor, rootCtx, defaultValueCtx, editorViewCtx } from "@milkdown/kit/core";
import { commonmark } from "@milkdown/kit/preset/commonmark";
import { gfm } from "@milkdown/kit/preset/gfm";
import { listener, listenerCtx } from "@milkdown/kit/plugin/listener";
import { replaceAll, getMarkdown } from "@milkdown/kit/utils";
import { glueNode, glueRemark, glueClickPlugin } from "./glue.js";

const COMMIT_DELAY = 300;
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
    this.markdown = el.dataset.initial || "";
    this.inputs = {}; // input name -> [columns]
    this._applyingExternal = false;
    this._commitTimer = null;
    this.editor = null;

    this._buildDom();
    this._initEditor();
  }

  _buildDom() {
    this.el.classList.add("blockr-prose");

    // Toolbar with the "insert data field" menu
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
    this.toolbar.appendChild(this.fieldBtn);
    this.toolbar.appendChild(this.fieldMenu);

    // Milkdown mount point
    this.editorHost = document.createElement("div");
    this.editorHost.className = "blockr-prose-editor";

    // Raw markdown source (collapsed advanced section)
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

    this.el.appendChild(this.toolbar);
    this.el.appendChild(this.editorHost);
    this.el.appendChild(this.details);

    // Chip edit popover (hidden until a chip is clicked)
    this._buildPopover();
  }

  _buildPopover() {
    this.popover = document.createElement("div");
    this.popover.className = "blockr-prose-popover";
    this.popover.hidden = true;
    this.popInput = document.createElement("input");
    this.popInput.type = "text";
    this.popInput.className = "form-control form-control-sm";
    const ok = document.createElement("button");
    ok.type = "button";
    ok.className = "btn btn-sm btn-primary";
    ok.textContent = "OK";
    ok.addEventListener("click", () => this._commitPopover());
    this.popInput.addEventListener("keydown", (e) => {
      if (e.key === "Enter") this._commitPopover();
      if (e.key === "Escape") this._hidePopover();
    });
    this.popover.appendChild(this.popInput);
    this.popover.appendChild(ok);
    this.el.appendChild(this.popover);
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
      .use(glueClickPlugin((view, node, pos) => self._openPopover(view, node, pos)))
      .create();
  }

  _onWysiwygUpdate(md) {
    if (this._applyingExternal) return;
    this.markdown = md;
    if (this.textarea.value !== md) this.textarea.value = md;
    this._commit();
  }

  _onRawEdit() {
    const md = this.textarea.value;
    this.markdown = md;
    this._applyToWysiwyg(md);
    this._commit();
  }

  // Push markdown into the WYSIWYG editor without triggering a commit loop.
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

  // External / AI write: update both views, do NOT commit back to R.
  setMarkdown(md) {
    if (md === this.markdown) return;
    this.markdown = md;
    if (this.textarea.value !== md) this.textarea.value = md;
    this._applyToWysiwyg(md);
  }

  _commit() {
    clearTimeout(this._commitTimer);
    this._commitTimer = setTimeout(() => {
      setShinyInput(this.inputId, this.markdown);
    }, COMMIT_DELAY);
  }

  // ---- data-field menu -------------------------------------------------
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

  // ---- chip edit popover ----------------------------------------------
  _openPopover(view, node, pos) {
    this._popView = view;
    this._popPos = pos;
    this.popInput.value = node.attrs.expr || "";
    const coords = view.coordsAtPos(pos);
    const host = this.el.getBoundingClientRect();
    this.popover.style.left = coords.left - host.left + "px";
    this.popover.style.top = coords.bottom - host.top + 4 + "px";
    this.popover.hidden = false;
    this.popInput.focus();
    this.popInput.select();
  }

  _commitPopover() {
    const expr = this.popInput.value;
    const view = this._popView;
    if (view && this._popPos != null) {
      const tr = view.state.tr.setNodeMarkup(this._popPos, undefined, { expr });
      view.dispatch(tr);
    }
    this._hidePopover();
  }

  _hidePopover() {
    this.popover.hidden = true;
    this._popView = null;
    this._popPos = null;
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

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler("prose-set", (msg) => {
      const inst = instances.get(msg.id);
      if (inst) inst.setMarkdown(msg.markdown || "");
    });
    Shiny.addCustomMessageHandler("prose-columns", (msg) => {
      const inst = instances.get(msg.id);
      if (inst) inst.setInputs(msg.inputs || {});
    });
  }
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", register);
} else {
  register();
}
