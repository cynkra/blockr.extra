import { test } from "node:test";
import assert from "node:assert/strict";
import { collectExprs, remarkInlineR } from "../../srcjs/prose-block/inline-r-md.js";

test("collectExprs finds inline R once each, outside fenced code", () => {
  const md = "Rows `r nrow(data)`, again `r nrow(data)`, mean `r mean(data$x)`.\n\n```\n`r nope`\n```\n\nCode `x` stays.";
  assert.deepEqual(collectExprs(md), ["nrow(data)", "mean(data$x)"]);
});

test("the remark plugin reads `r expr` into a node and writes it back", () => {
  const data = {};
  const proc = { data: () => data };
  const run = remarkInlineR.call(proc);
  const tree = { type: "root", children: [{ type: "paragraph", children: [
    { type: "text", value: "n = " },
    { type: "inlineCode", value: "r nrow(data)" },
    { type: "inlineCode", value: "plain" }
  ] }] };
  run(tree);
  const kids = tree.children[0].children;
  assert.deepEqual(kids[1], { type: "inlineR", value: "nrow(data)" });
  assert.equal(kids[2].type, "inlineCode");
  const handler = data.toMarkdownExtensions[0].handlers.inlineR;
  assert.equal(handler({ value: "nrow(data)" }), "`r nrow(data)`");
});
