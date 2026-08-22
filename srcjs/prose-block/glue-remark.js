// Custom remark support for glue references, and the brace escaping that
// keeps the stored markdown a valid glue template.
//
// STORED FORM (what R holds, what the raw textarea shows, what an LLM writes):
// chips are `{expr}`, every literal brace is doubled (`{{` / `}}`) -- glue's
// own escape.
//
// Parse (stored -> editor): text nodes are tokenized; `{{`/`}}` become literal
// braces, and a single-brace span becomes a `glueRef` node only if it LOOKS
// LIKE R (below). Code and inlineCode values are unescaped but never chipped.
//
// Stringify (editor -> stored): a `glueRef` serializes to a sentinel pair
// (private-use codepoints), and the controller post-processes the string:
// double every brace, then turn sentinels back into `{expr}`. Doing it on the
// whole string means fenced code, inline code and raw HTML are covered by one
// rule instead of per-handler wrapping.

export const SENT_OPEN = "\uE000";
export const SENT_CLOSE = "\uE001";

// Is a single-brace span a glue reference or literal text? Editor-authored
// content never hits this (chips are nodes); it decides for markdown written
// elsewhere -- an LLM, the ctor, the raw textarea. A plain R parse test is
// NOT enough: `.callout-note` parses as `.callout - note`. So: reject the
// Quarto-isms (leading `.` or `#`, `=` outside parens, `<`, an unspaced
// hyphen outside parens), then require balanced parens/brackets and a sane
// first character.
export function looksLikeR(s) {
  const t = s.trim();
  if (!t) return false;
  if (/^[.#]/.test(t)) return false; // {.callout-note}, {#fig-label}
  if (t.indexOf("<") !== -1) return false; // {{< shortcode >}} remnants
  let depth = 0;
  for (let i = 0; i < t.length; i++) {
    const c = t[i];
    if (c === "(" || c === "[") depth++;
    else if (c === ")" || c === "]") depth--;
    else if (depth === 0) {
      if (c === "=" && t[i + 1] !== "=" && t[i - 1] !== "=" &&
          t[i - 1] !== "!" && t[i - 1] !== "<" && t[i - 1] !== ">") {
        return false; // {width=50%} attribute syntax
      }
      if (c === "-" && /[A-Za-z0-9]/.test(t[i - 1] || "") &&
          /[A-Za-z]/.test(t[i + 1] || "")) {
        return false; // {tbl-cap}, {fig-align} (unspaced word-word hyphen)
      }
    }
    if (depth < 0) return false;
  }
  if (depth !== 0) return false;
  return /^[A-Za-z0-9_.("'`]/.test(t);
}

// Tokenize one text value into text / glueRef parts under the stored-form
// rules. Exported for tests.
export function tokenize(value) {
  const out = [];
  let text = "";
  let i = 0;
  while (i < value.length) {
    if (value[i] === "{" && value[i + 1] === "{") {
      text += "{";
      i += 2;
    } else if (value[i] === "}" && value[i + 1] === "}") {
      text += "}";
      i += 2;
    } else if (value[i] === "{") {
      const end = value.indexOf("}", i + 1);
      const inner = end === -1 ? null : value.slice(i + 1, end);
      if (inner !== null && inner.indexOf("{") === -1 && looksLikeR(inner)) {
        if (text) {
          out.push({ type: "text", value: text });
          text = "";
        }
        out.push({ type: "glueRef", value: inner });
        i = end + 1;
      } else {
        text += "{";
        i += 1;
      }
    } else {
      text += value[i];
      i += 1;
    }
  }
  if (text) out.push({ type: "text", value: text });
  return out;
}

function unescapeBraces(s) {
  return s.replace(/\{\{/g, "{").replace(/\}\}/g, "}");
}

function transform(parent) {
  if (!parent || !Array.isArray(parent.children)) return;
  const out = [];
  for (const child of parent.children) {
    if (child.type === "text" && typeof child.value === "string" &&
        child.value.indexOf("{") !== -1) {
      out.push(...tokenize(child.value));
    } else if ((child.type === "code" || child.type === "inlineCode") &&
               typeof child.value === "string") {
      child.value = unescapeBraces(child.value);
      out.push(child);
    } else {
      transform(child);
      out.push(child);
    }
  }
  parent.children = out;
}

// A unified/remark plugin (RemarkPluginRaw). `this` is the processor. The
// transformer runs on parse; the toMarkdown handler serializes a glueRef to
// its sentinel form (the controller finishes the job -- see fromEditorMd).
export function remarkGluePlugin() {
  const data = this.data();
  const toMarkdownExtensions =
    data.toMarkdownExtensions || (data.toMarkdownExtensions = []);
  toMarkdownExtensions.push({
    handlers: {
      glueRef: (node) => SENT_OPEN + (node.value || "") + SENT_CLOSE,
    },
  });
  return (tree) => {
    transform(tree);
    return tree;
  };
}

// Editor-serialized string (sentinels + raw braces) -> stored form. Every raw
// brace doubles; sentinels become single-brace refs. Backslash-escapes that
// remark's `safe()` may have put on braces are stripped first so they cannot
// stack with the doubling.
export function toStoredForm(md) {
  return md
    .replace(/\\([{}])/g, "$1")
    .replace(/\{/g, "{{")
    .replace(/\}/g, "}}")
    .replace(new RegExp(SENT_OPEN + "([^" + SENT_OPEN + SENT_CLOSE + "]*)" +
      SENT_CLOSE, "g"), "{$1}")
    .replace(new RegExp("[" + SENT_OPEN + SENT_CLOSE + "]", "g"), "");
}

// Every chip expression in a stored-form string, for the eager value preview.
export function collectExprs(stored) {
  const out = [];
  for (const part of tokenizeAll(stored)) {
    if (part.type === "glueRef" && out.indexOf(part.value) === -1) {
      out.push(part.value);
    }
  }
  return out;
}

// tokenize() per line so fenced code does not need a markdown parse here;
// braces inside code are doubled in stored form, so they never look like refs.
function tokenizeAll(stored) {
  const out = [];
  for (const line of String(stored || "").split("\n")) {
    out.push(...tokenize(line));
  }
  return out;
}
