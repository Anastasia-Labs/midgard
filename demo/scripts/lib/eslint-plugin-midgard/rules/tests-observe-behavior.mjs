// Tests observe behaviour, not source text.
//
// A test that reads a module, a validator or a document as text and greps it
// passes while the behaviour it names is broken, and fails on a harmless
// rename. In a package test file (under a `tests/` directory, or a
// `*.test.*` file) this reports:
//
// - a file read (`readFileSync`, `readFile`) whose path names source or
//   prose: a string ending in .ts/.tsx/.js/.mjs/.cjs/.md/.ak, reached
//   directly, through a constant, through `join`/`resolve`/`new URL`
//   arguments, or as an element of an array literal mapped into the read.
//   JSON data, fixtures and build outputs stay readable. A path the same file
//   also hands to a writer (`writeFileSync(path, ...)`,
//   `writeSummaryMarkdown(path, ...)`: a call named write*, append* or
//   save*), or one built under a temporary directory (`mkdtemp`, `tmpdir()`),
//   is output of the code under test, not source, and is not reported.
// - a regular-expression match over a stringified value:
//   `expect(JSON.stringify(x)).toMatch(...)` or
//   `assert.match(JSON.stringify(x), ...)`. `toContain` over a serialized
//   value stays allowed: `not.toContain(secret)` is a legitimate redaction
//   check.
//
// Tooling tests under scripts/ legitimately read the files the tool under
// test writes, and are out of scope.
//
// Blind spots: a read through a local wrapper (`const read = (p) =>
// readFileSync(p)`) or an import alias (`readFileSync as rf`), a path found by
// walking a directory (`readdirSync(...).filter(...)`), and a path taken from
// a destructured loop over tuples are not followed. A regular expression run
// over text the test read (`PATTERN.test(readFileSync(file))`) is reported
// only when the path itself is.

import { defineRule } from "../baseline.mjs";
import {
  calleeName,
  constantInitializer,
  receiverOf,
  unwrap,
} from "../ast.mjs";

const SOURCE_PATH = /\.(?:[cm]?[jt]sx?|md|ak)(?:\.template)?$/u;
const READS = new Set(["readFileSync", "readFile"]);
// `writeFileSync`, `appendFile`, and the code under test's own writers
// (`writeSummaryMarkdownAtomic(path, ...)`).
const WRITER = /^(?:write|append|save)/u;
const TEMPORARY = new Set(["mkdtempSync", "mkdtemp", "tmpdir"]);
const TEST_FILE =
  /^(?!scripts\/)(?:[^/]+\/)+(?:tests?\/|[^/]+\.test\.[cm]?[jt]sx?$)/u;
const MAX_DEPTH = 4;

const literalText = (node) => {
  if (node?.type === "Literal" && typeof node.value === "string") {
    return node.value;
  }
  if (node?.type === "TemplateLiteral") {
    return node.quasis.map((quasi) => quasi.value.cooked).join("*");
  }
  return undefined;
};

/** The iterable a `for (const x of iterable)` loop binds `identifier` from. */
const loopOver = (sourceCode, identifier) => {
  for (
    let scope = sourceCode.getScope(identifier);
    scope;
    scope = scope.upper
  ) {
    const variable = scope.set.get(identifier.name);
    if (variable === undefined) continue;
    const declaration = variable.defs[0]?.parent;
    const loop = declaration?.parent;
    return loop?.type === "ForOfStatement" && loop.left === declaration
      ? loop.right
      : undefined;
  }
  return undefined;
};

/** Every expression a path argument is built from, through constants. */
const pathParts = (sourceCode, node, depth = 0) => {
  const value = unwrap(node);
  if (value === undefined || value === null || depth > MAX_DEPTH) return [];
  const parts = [value];
  const nested = (child) =>
    parts.push(...pathParts(sourceCode, child, depth + 1));
  switch (value.type) {
    case "Identifier": {
      const init = constantInitializer(sourceCode, value);
      if (init !== undefined) nested(init);
      // A loop variable over an array of paths:
      // for (const path of ["../src/a.ts", ...]) readFileSync(path).
      const loop = loopOver(sourceCode, value);
      if (loop !== undefined) nested(loop);
      // A callback parameter fed from an array literal of paths:
      // ["../src/a.ts", ...].map((path) => readFileSync(path)).
      const fn = sourceCode.getScope(value).block;
      const call = fn?.parent;
      if (
        call?.type === "CallExpression" &&
        ["map", "forEach", "flatMap"].includes(calleeName(call)) &&
        fn.params?.[0]?.type === "Identifier" &&
        fn.params[0].name === value.name
      ) {
        const array = unwrap(receiverOf(call));
        if (array?.type === "ArrayExpression") array.elements.forEach(nested);
      }
      break;
    }
    case "CallExpression":
    case "NewExpression":
      value.arguments.forEach(nested);
      if (value.callee.type === "MemberExpression") nested(value.callee.object);
      break;
    case "MemberExpression":
      nested(value.object);
      break;
    case "ArrayExpression":
      value.elements.forEach(nested);
      break;
    case "BinaryExpression":
      nested(value.left);
      nested(value.right);
      break;
    case "TemplateLiteral":
      value.expressions.forEach(nested);
      break;
    default:
      break;
  }
  return parts;
};

const namesSource = (parts) =>
  parts.some((part) => SOURCE_PATH.test(literalText(part) ?? ""));

const underTemporaryDirectory = (parts) =>
  parts.some(
    (part) => part.type === "CallExpression" && TEMPORARY.has(calleeName(part)),
  );

// JSON.stringify(x), or a string method chained on it (.toLowerCase()).
const isStringified = (node) => {
  const call = unwrap(node);
  if (call?.type !== "CallExpression") return false;
  const callee = unwrap(call.callee);
  if (callee?.type !== "MemberExpression") return false;
  if (
    callee.object.type === "Identifier" &&
    callee.object.name === "JSON" &&
    calleeName(call) === "stringify"
  ) {
    return true;
  }
  return isStringified(callee.object);
};

/** Whether an `expect(subject)` chain ends in `.toMatch(...)`. */
const assertsMatch = (expectCall) => {
  let link = expectCall.parent;
  while (link?.type === "MemberExpression") {
    if (link.property?.name === "toMatch") return true;
    link =
      link.parent?.type === "CallExpression" ? link.parent.parent : link.parent;
  }
  return false;
};

export default defineRule({
  meta: {
    type: "problem",
    docs: {
      description:
        "Tests observe behaviour, not source text, prose or stringified values.",
    },
    schema: [],
    messages: {
      sourceRead:
        "This test reads source or prose as text, so it passes while the behaviour is broken and fails on a rename. Fix: import the module and exercise it; if no behaviour is at stake, delete the test. Cross-language goldens belong in one shared JSON vector file both sides load.",
      stringScan:
        "This assertion matches a regular expression over a stringified value. Fix: assert on the value's fields; if it only guards text, delete the assertion.",
    },
  },
  create(context, report, file) {
    if (!TEST_FILE.test(file)) return {};
    const { sourceCode } = context;
    const reads = [];
    const written = new Set();
    return {
      CallExpression(node) {
        const name = calleeName(node);
        const [first] = node.arguments;
        if (READS.has(name) && first !== undefined) {
          reads.push({ node, path: first });
          return;
        }
        if (WRITER.test(name ?? "") && first !== undefined) {
          written.add(sourceCode.getText(unwrap(first)));
          return;
        }
        const stringScan =
          (name === "expect" && isStringified(first) && assertsMatch(node)) ||
          (name === "match" &&
            unwrap(receiverOf(node))?.name === "assert" &&
            isStringified(first));
        if (stringScan) report({ node, messageId: "stringScan" });
      },
      "Program:exit"() {
        for (const { node, path } of reads) {
          if (written.has(sourceCode.getText(unwrap(path)))) continue;
          const parts = pathParts(sourceCode, path);
          if (namesSource(parts) && !underTemporaryDirectory(parts)) {
            report({ node, messageId: "sourceRead" });
          }
        }
      },
    };
  },
});
