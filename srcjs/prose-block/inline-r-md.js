// The markdown side of inline R, kept free of the editor so it can be tested
// in node: finding the expressions, and the remark plugin that reads
// `r expr` inline code into its own node and writes it back.

const R_CODE = /^r[ \t]+(\S[\s\S]*)$/;

// The inline expressions of a markdown string, in order and without repeats,
// for the value preview. Fenced code is skipped.
export function collectExprs(md) {
  const out = [];
  let fenced = false;
  for (const line of String(md || "").split("\n")) {
    if (/^\s*(```|~~~)/.test(line)) { fenced = !fenced; continue; }
    if (fenced) continue;
    const re = /`r[ \t]+([^`]+)`/g;
    let m;
    while ((m = re.exec(line))) {
      const e = m[1].trim();
      if (e && out.indexOf(e) === -1) out.push(e);
    }
  }
  return out;
}

function transform(parent) {
  if (!parent || !Array.isArray(parent.children)) return;
  parent.children = parent.children.map((child) => {
    if (child.type === "inlineCode" && typeof child.value === "string") {
      const m = R_CODE.exec(child.value);
      if (m) return { type: "inlineR", value: m[1].trim() };
    }
    transform(child);
    return child;
  });
}

// A unified/remark plugin: parse inline R into its own node, write it back as
// inline code.
export function remarkInlineR() {
  const data = this.data();
  const ext = data.toMarkdownExtensions || (data.toMarkdownExtensions = []);
  ext.push({ handlers: { inlineR: (node) => "`r " + (node.value || "") + "`" } });
  return (tree) => {
    transform(tree);
    return tree;
  };
}
