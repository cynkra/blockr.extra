// Glue reference chip: an atomic inline ProseMirror node holding the raw R
// expression. It renders TWO faces -- `.val` (the evaluated value, patched in
// by the controller when `prose-values` arrives) and `.expr` (the code) --
// and CSS decides which face shows: quiet value at rest, pill on editor
// focus, code under the global `.show-expr` toggle, red code when the
// controller marked it `.is-err`, neutral code while `.is-dormant`.
//
// toDOM seeds `.val` with the expression (the dormant look); the controller's
// patchChips() overwrites it as soon as values exist. PM never re-renders an
// atom's interior on its own, so the direct DOM patch is safe and cheap.

import { $node, $remark, $prose } from "@milkdown/kit/utils";
import { Plugin, PluginKey } from "@milkdown/kit/prose/state";
import { remarkGluePlugin } from "./glue-remark.js";

export const glueRemark = $remark("glueRef", () => remarkGluePlugin);

export const glueNode = $node("glue_ref", () => ({
  group: "inline",
  inline: true,
  atom: true,
  selectable: true,
  draggable: false,
  attrs: { expr: { default: "" } },
  parseDOM: [
    {
      tag: "span[data-glue]",
      getAttrs: (dom) => ({ expr: dom.getAttribute("data-glue") || "" }),
    },
  ],
  toDOM: (node) => {
    const expr = node.attrs.expr || "";
    return [
      "span",
      {
        "data-glue": expr,
        class: "blockr-glue-chip is-dormant",
        title: "{" + expr + "}",
      },
      ["span", { class: "blockr-glue-val" }, expr],
      ["span", { class: "blockr-glue-expr" }, expr],
    ];
  },
  parseMarkdown: {
    match: (node) => node.type === "glueRef",
    runner: (state, node, type) => {
      state.addNode(type, { expr: node.value || "" });
    },
  },
  toMarkdown: {
    match: (node) => node.type.name === "glue_ref",
    runner: (state, node) => {
      state.addNode("glueRef", undefined, node.attrs.expr || "");
    },
  },
}));

// Click handling: when a chip is clicked, invoke the supplied callback with
// the editor view, the node and its position so the host can open the
// in-place editor.
const glueClickKey = new PluginKey("BLOCKR_GLUE_CLICK");

export function glueClickPlugin(onChipClick) {
  return $prose(
    () =>
      new Plugin({
        key: glueClickKey,
        props: {
          handleClickOn: (view, pos, node, nodePos) => {
            if (node.type.name === "glue_ref") {
              onChipClick(view, node, nodePos);
              return true;
            }
            return false;
          },
        },
      })
  );
}
