// Custom remark support for glue references: `{expr}` text <-> `glueRef` mdast.
//
// On parse (mdast transform) we split text nodes on balanced `{...}` and emit
// `glueRef` nodes. On stringify we register a mdast-util-to-markdown handler so
// a `glueRef` node serializes back to `{expr}` verbatim. This is what makes the
// chip round-trip lossless through markdown (and through an LLM write).

const GLUE_RE = /\{([^{}]+)\}/g;

function splitTextNodes(parent) {
  if (!parent || !Array.isArray(parent.children)) return;
  const out = [];
  for (const child of parent.children) {
    if (child.type === "text" && typeof child.value === "string" &&
        child.value.indexOf("{") !== -1) {
      let last = 0;
      let m;
      GLUE_RE.lastIndex = 0;
      while ((m = GLUE_RE.exec(child.value)) !== null) {
        if (m.index > last) {
          out.push({ type: "text", value: child.value.slice(last, m.index) });
        }
        out.push({ type: "glueRef", value: m[1] });
        last = m.index + m[0].length;
      }
      if (last < child.value.length) {
        out.push({ type: "text", value: child.value.slice(last) });
      }
      if (last === 0) out.push(child); // no match after all
    } else {
      splitTextNodes(child);
      out.push(child);
    }
  }
  parent.children = out;
}

// A unified/remark plugin (RemarkPluginRaw). `this` is the processor.
export function remarkGluePlugin() {
  const data = this.data();
  const toMarkdownExtensions =
    data.toMarkdownExtensions || (data.toMarkdownExtensions = []);
  toMarkdownExtensions.push({
    handlers: {
      glueRef: (node) => "{" + (node.value || "") + "}",
    },
  });
  return (tree) => {
    splitTextNodes(tree);
    return tree;
  };
}
