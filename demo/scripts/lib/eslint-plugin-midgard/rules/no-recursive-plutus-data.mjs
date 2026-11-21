// Plutus Data is read, built and written without recursion in production code.
//
// Every carrier (redeemer, inline datum, CEK constant) admits Data nested to
// thousands of levels under its byte cap. A recursive walk over such a value
// overflows the JavaScript stack, and where that throw lands decides whether
// an honest transaction is dropped, a batch wedges, or a node defect is
// committed as a verdict. The repository therefore reads and writes Data with
// its own iterative functions (midgard-core `plutus-data-lucid-iterative`,
// midgard-validation `plutus-data-iterative.*`), and this rule bans the
// recursive library entry points in the production source trees:
//   - `cborg`;
//   - Lucid's `Data.from` / `Data.to` in midgard-validation and midgard-core,
//     where bounded encoders of repository-built values are baselined with a
//     reason each.
// The recursive @harmoniclabs codecs this rule used to single out are no
// longer dependencies; `no-restricted-imports` in eslint.config.mjs refuses
// every @harmoniclabs import. The rule is syntactic: it cannot see a
// recursive walk written by hand. docs/agents/lint-rules.md lists the blind
// spots.

import { defineRule } from "../baseline.mjs";
import { staticName, unwrap } from "../ast.mjs";

export const PRODUCTION_SOURCE_TREES = [
  "midgard-core/src/",
  "midgard-fault-proofs/src/",
  "midgard-node/src/",
  "midgard-sdk/src/",
  "midgard-validation/src/",
  "midgard-watcher/src/",
];

const LUCID_DATA_TREES = ["midgard-core/src/", "midgard-validation/src/"];

const importedName = (specifier) =>
  specifier.type === "ImportSpecifier"
    ? staticName(specifier.imported, false)
    : undefined;

export default defineRule({
  meta: {
    type: "problem",
    docs: {
      description:
        "Ban recursive Plutus Data readers and writers in production source.",
    },
    schema: [],
    messages: {
      cborg:
        "`cborg` decodes and encodes recursively. Fix: use the repository's iterative CBOR readers and writers in midgard-core.",
      lucidData:
        "Lucid `Data.{{name}}` walks Data recursively (through CML for `from`). Fix: use `lucidDataFromCborIterative` / `lucidDataToCborIterative` from @al-ft/midgard-core/plutus-data-lucid-iterative; a bounded encoder of a repository-built value may be baselined with its bound as the reason.",
    },
  },
  create(_context, report, file) {
    if (!PRODUCTION_SOURCE_TREES.some((tree) => file.startsWith(tree))) {
      return {};
    }
    const lucidTree = LUCID_DATA_TREES.some((tree) => file.startsWith(tree));
    const lucidDataNames = new Set();
    return {
      ImportDeclaration(node) {
        const source = node.source.value;
        if (source === "cborg" || source.startsWith("cborg/")) {
          report({ node: node.source, messageId: "cborg" });
          return;
        }
        for (const specifier of node.specifiers) {
          if (specifier.importKind === "type" || node.importKind === "type") {
            continue;
          }
          const name = importedName(specifier);
          if (
            lucidTree &&
            source.startsWith("@lucid-evolution/") &&
            name === "Data"
          ) {
            lucidDataNames.add(specifier.local.name);
          }
        }
      },
      CallExpression(node) {
        const callee = unwrap(node.callee);
        if (callee.type !== "MemberExpression") return;
        const object = unwrap(callee.object);
        const name = staticName(callee.property, callee.computed);
        if (
          object.type === "Identifier" &&
          lucidDataNames.has(object.name) &&
          (name === "from" || name === "to")
        ) {
          report({ node: callee, messageId: "lucidData", data: { name } });
        }
      },
    };
  },
});
