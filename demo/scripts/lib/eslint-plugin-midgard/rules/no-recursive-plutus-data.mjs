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
//   - the recursive `@harmoniclabs/plutus-data` functions (`dataFromCbor`,
//     `dataToCbor`, their `Obj` forms, `eqData`, `cloneData`, the JSON pair);
//   - `@harmoniclabs/plutus-machine` names other than the pinned reference
//     evaluator's (`Machine` evaluates over recursive Data);
//   - `Cbor` from `@harmoniclabs/cbor` (`Cbor.parse` / `Cbor.encode`);
//   - `cborg`;
//   - Lucid's `Data.from` / `Data.to` in midgard-validation and midgard-core,
//     where bounded encoders of repository-built values are baselined with a
//     reason each.
// The rule is syntactic: it cannot see `.toString()`, `.toJson()` or
// `.clone()` called on a harmonic Data value, nor a recursive walk written by
// hand. docs/agents/lint-rules.md lists these blind spots.

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

const RECURSIVE_HARMONIC_DATA = new Set([
  "cloneData",
  "dataFromCbor",
  "dataFromCborObj",
  "dataFromJson",
  "dataToCbor",
  "dataToCborObj",
  "dataToJson",
  "eqData",
]);

const REFERENCE_EVALUATOR_NAMES = new Set([
  "BnCEK",
  "CEKConst",
  "CEKError",
  "ExBudget",
  // Scalar bigint cost functions do not traverse Plutus Data.
  "Linear3InY",
  "PartialBuiltin",
  "costModelV3ToBuiltinCosts",
]);

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
      recursiveHarmonic:
        "`{{name}}` from @harmoniclabs/plutus-data walks Data recursively and overflows the stack on deep values that the carriers admit. Fix: decode with `plutusDataFromCborIterative` (midgard-validation/src/plutus-data-iterative.decode.ts) and encode with `encodeMidgardCekPlutusData` (plutus-data-iterative.encode.ts); compare or copy with an explicit-stack walk.",
      machine:
        "`{{name}}` from @harmoniclabs/plutus-machine is outside the pinned reference evaluator; `Machine` evaluates over recursive Data. Fix: evaluate through the structural CEK executor (cek-executor.ts); only BnCEK, CEKConst, CEKError, ExBudget, Linear3InY, PartialBuiltin and costModelV3ToBuiltinCosts may be imported.",
      harmonicCbor:
        "`Cbor` from @harmoniclabs/cbor parses and encodes recursively. Fix: use the iterative readers in midgard-core (`plutus-data-lucid-iterative`) or midgard-validation (`plutus-data-iterative.decode.ts`).",
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
            source === "@harmoniclabs/plutus-data" &&
            RECURSIVE_HARMONIC_DATA.has(name)
          ) {
            report({
              node: specifier,
              messageId: "recursiveHarmonic",
              data: { name },
            });
          } else if (
            source === "@harmoniclabs/plutus-machine" &&
            !REFERENCE_EVALUATOR_NAMES.has(name)
          ) {
            report({
              node: specifier,
              messageId: "machine",
              data: { name: name ?? specifier.local.name },
            });
          } else if (
            source === "@harmoniclabs/cbor" &&
            (name === "Cbor" || specifier.type !== "ImportSpecifier")
          ) {
            report({ node: specifier, messageId: "harmonicCbor" });
          } else if (
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
