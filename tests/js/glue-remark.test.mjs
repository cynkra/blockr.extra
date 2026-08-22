// Stored-form round-trip rules for the prose block (no DOM needed).
// Run: node --test tests/js
import test from "node:test";
import assert from "node:assert/strict";
import {
  looksLikeR, tokenize, toStoredForm, collectExprs, SENT_OPEN, SENT_CLOSE
} from "../../srcjs/prose-block/glue-remark.js";

test("looksLikeR accepts R, rejects Quarto-isms", () => {
  assert.equal(looksLikeR("nrow(data)"), true);
  assert.equal(looksLikeR("round(mean(data$Sepal.Length), 2)"), true);
  assert.equal(looksLikeR("data$Species"), true);
  assert.equal(looksLikeR("nrow(data) - 1"), true);
  assert.equal(looksLikeR("round(x, digits = 1)"), true);
  assert.equal(looksLikeR("sum(x == 1)"), true);
  assert.equal(looksLikeR(".callout-note"), false); // parses as R! rejected anyway
  assert.equal(looksLikeR("#fig-label"), false);
  assert.equal(looksLikeR("width=50%"), false);
  assert.equal(looksLikeR("tbl-cap"), false);
  assert.equal(looksLikeR("< video >"), false);
  assert.equal(looksLikeR(""), false);
});

test("tokenize: chips only for R-looking single-brace spans", () => {
  assert.deepEqual(tokenize("has {nrow(data)} rows"), [
    { type: "text", value: "has " },
    { type: "glueRef", value: "nrow(data)" },
    { type: "text", value: " rows" },
  ]);
  assert.deepEqual(tokenize("::: {{.callout-note}}"), [
    { type: "text", value: "::: {.callout-note}" },
  ]);
  // Un-doubled literal from foreign markdown: stays text under the R test.
  assert.deepEqual(tokenize("a {.callout-note} b"), [
    { type: "text", value: "a {.callout-note} b" },
  ]);
  assert.deepEqual(tokenize("{{{{< video >}}}}"), [
    { type: "text", value: "{{< video >}}" },
  ]);
});

test("toStoredForm doubles braces and restores chips", () => {
  assert.equal(toStoredForm("code { x }"), "code {{ x }}");
  assert.equal(
    toStoredForm("has " + SENT_OPEN + "nrow(data)" + SENT_CLOSE + " rows"),
    "has {nrow(data)} rows"
  );
  assert.equal(
    toStoredForm(SENT_OPEN + "mean(x)" + SENT_CLOSE + " and {literal}"),
    "{mean(x)} and {{literal}}"
  );
  // remark safe() escapes must not stack with the doubling
  assert.equal(toStoredForm("a \\{b\\}"), "a {{b}}");
});

test("stored -> tokenize -> stored is stable", () => {
  const stored = "Mean is {round(mean(d$x), 2)} cm. ::: {{.callout-note}}";
  const rebuilt = tokenize(stored)
    .map((p) => (p.type === "glueRef"
      ? SENT_OPEN + p.value + SENT_CLOSE
      : p.value))
    .join("");
  assert.equal(toStoredForm(rebuilt), stored);
});

test("collectExprs dedupes and skips literals", () => {
  assert.deepEqual(
    collectExprs("x {nrow(data)} y {{.z}} {nrow(data)} {mean(a$b)}"),
    ["nrow(data)", "mean(a$b)"]
  );
  assert.deepEqual(collectExprs("f <- function() {{ 1 }}"), []);
});
