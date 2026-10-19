// W4.5: parameters reach a validator only through `applyBlueprintParams`.
//
// Parameter application applies whatever list it is handed. Too few terms
// leave `validator main(...)` parameters as lambdas, and the ledger's single
// script-context application then reduces to a lambda value: evaluation ends
// without error, so the deployed script always succeeds (#605/#609). Too many
// terms give a well-formed script with the wrong hash.
// `applyBlueprintParams` in midgard-sdk/src/fraud-proof/contracts/blueprint.ts
// refuses a list whose arity or shape does not match the blueprint, and it is
// the one place allowed to apply parameters.
//
// A test's independent oracle is the other: a module named
// `expected-parameter-application.ts` (or `<prefix>.expected-parameter-application.ts`)
// under a package's `tests/` tree recomputes an applied script outside the
// guard on purpose, so that a test can compare the guarded result against it.
// Nothing deploys from it. Every other test file is held to the rule, since a
// script a test deploys to the emulator with the wrong arity makes that test
// vacuous in the same way.

import { defineRule } from "../baseline.mjs";
import { calleeName, calleeNameNode, staticName } from "../ast.mjs";

export const APPLY_FUNCTIONS = new Set([
  "applyParamsToScript",
  "apply_params_to_script",
]);
export const BLUEPRINT_MODULE =
  "midgard-sdk/src/fraud-proof/contracts/blueprint.ts";
export const TEST_ORACLE_MODULE =
  /^[^/]+\/tests\/(?:[^/]+\/)*(?:[^/]+\.)?expected-parameter-application\.ts$/u;

const allowedToApply = (file) =>
  file === BLUEPRINT_MODULE || TEST_ORACLE_MODULE.test(file);

export default defineRule({
  meta: {
    type: "problem",
    docs: {
      description:
        "Allow raw parameter application only inside the blueprint module's shape guard.",
    },
    schema: [],
    messages: {
      rawApplication:
        "Raw parameter application skips the arity and shape guard: too few parameters deploy an always-succeeds script (#609), too many a script with the wrong hash. Fix: build the script with `applyBlueprintParams(blueprint, title, params)` from midgard-sdk/src/fraud-proof/contracts/blueprint.ts, the only production module allowed to apply parameters. A test that needs an independent oracle imports it from its package's tests/**/expected-parameter-application.ts.",
      aliased:
        "Renaming a raw parameter-application function hides every later call from this rule. Fix: build the script with `applyBlueprintParams(blueprint, title, params)` from midgard-sdk/src/fraud-proof/contracts/blueprint.ts instead of importing the raw function.",
    },
  },
  create: (_context, report, file) =>
    allowedToApply(file)
      ? {}
      : {
          CallExpression(node) {
            if (APPLY_FUNCTIONS.has(calleeName(node))) {
              report({
                node: calleeNameNode(node),
                messageId: "rawApplication",
              });
            }
          },
          ImportSpecifier(node) {
            const imported = staticName(node.imported, false);
            if (APPLY_FUNCTIONS.has(imported) && node.local.name !== imported) {
              report({ node, messageId: "aliased" });
            }
          },
          Property(node) {
            if (node.parent.type !== "ObjectPattern") return;
            const key = staticName(node.key, node.computed);
            if (
              APPLY_FUNCTIONS.has(key) &&
              !(node.value.type === "Identifier" && node.value.name === key)
            ) {
              report({ node, messageId: "aliased" });
            }
          },
        },
});
