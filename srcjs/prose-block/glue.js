// Glue reference chip: an atomic inline ProseMirror node that renders `{expr}`
// as a pill and round-trips to `{expr}` markdown via the remark plugin.

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
  toDOM: (node) => [
    "span",
    {
      "data-glue": node.attrs.expr,
      class: "blockr-glue-chip",
      title: node.attrs.expr,
    },
    node.attrs.expr,
  ],
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

// Click handling: when a chip is clicked, invoke the supplied callback with the
// node, its position, and the editor view so the host can open an edit popover.
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
