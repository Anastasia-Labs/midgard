// Midgard's local ESLint plugin: owner rulings that can be checked from the
// syntax tree, reported through a shrink-only baseline (baseline.mjs).
//
// Every rule has fixtures under fixtures/ with `// ruleid:` and `// ok:`
// cases, and ../eslint-plugin-midgard.test.mjs fails a rule without them.
// The rules and their blind spots are documented in
// docs/agents/lint-rules.md.

import applyParamsThroughBlueprint from "./rules/apply-params-through-blueprint.mjs";
import exactFeeNoChangeOutput from "./rules/exact-fee-no-change-output.mjs";
import faultProofReferenceScriptsOnly from "./rules/fault-proof-reference-scripts-only.mjs";
import localUplcEval from "./rules/local-uplc-eval.mjs";
import localeCompareExplicitLocale from "./rules/locale-compare-explicit-locale.mjs";
import nodeCliOperatorCommandsOnly from "./rules/node-cli-operator-commands-only.mjs";
import scopedUtxoOverride from "./rules/scoped-utxo-override.mjs";
import validFromWallClockMargin from "./rules/valid-from-wall-clock-margin.mjs";

export { loadBaseline } from "./baseline.mjs";

export const PLUGIN_NAME = "midgard";

export default {
  meta: { name: "eslint-plugin-midgard" },
  rules: {
    "apply-params-through-blueprint": applyParamsThroughBlueprint,
    "exact-fee-no-change-output": exactFeeNoChangeOutput,
    "fault-proof-reference-scripts-only": faultProofReferenceScriptsOnly,
    "local-uplc-eval": localUplcEval,
    "locale-compare-explicit-locale": localeCompareExplicitLocale,
    "node-cli-operator-commands-only": nodeCliOperatorCommandsOnly,
    "scoped-utxo-override": scopedUtxoOverride,
    "valid-from-wall-clock-margin": validFromWallClockMargin,
  },
};
