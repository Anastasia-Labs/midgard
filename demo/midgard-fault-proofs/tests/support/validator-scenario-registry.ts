/**
 * Which lucid-evolution emulator scenarios prove each validator in both
 * polarities: a passing scenario where the deployed, parameterized validator
 * accepts, and a failing one where it must refuse
 * (docs/agents/contracts.md#scenario-coverage).
 *
 * Two key spaces: every validator in the freshly built blueprint
 * (`<module>.<validator>`, the handler suffix dropped) and every family in
 * `FAMILY_APPLICATION_REGISTRY`. `validator-scenario-registry.test.ts` fails
 * when either has a key that is neither mapped here nor listed as unmapped
 * with a reason, when a mapping names a test that does not exist, and when
 * the unmapped lists change size without the pinned counts changing with
 * them.
 *
 * A scenario is a test file (repository-relative) and the test's title
 * exactly as written in the source, including `it.each` placeholders. A
 * table test whose rows cover both polarities may be named on both sides.
 * The mappings were seeded from test titles and bodies; whether the named
 * failing scenario really reaches this validator's refusal is a review
 * judgement the test cannot make.
 */

import "./validator-scenario-registry.validator-scenarios.js";
import "./validator-scenario-registry.family-scenarios.js";
import "./validator-scenario-registry.unmapped-validators.js";
import "./validator-scenario-registry.unmapped-families.js";

import { FAMILY_SCENARIOS as existingFamilyScenarios } from "./validator-scenario-registry.family-scenarios.js";
import { VALIDATOR_SCENARIOS } from "./validator-scenario-registry.validator-scenarios.js";

export const FAMILY_SCENARIOS = {
  ...existingFamilyScenarios,
  missingNativeScriptUtxo:
    VALIDATOR_SCENARIOS[
      "fraud_proofs/missing_native_script_utxo/step_05.main"
    ]!,
};
export {
  UNMAPPED_FAMILIES,
  UNMAPPED_FAMILY_COUNT,
  UNMAPPED_VALIDATOR_COUNT,
} from "./validator-scenario-registry.unmapped-families.js";
export { UNMAPPED_VALIDATORS } from "./validator-scenario-registry.unmapped-validators.js";
export {
  VALIDATOR_SCENARIOS,
  type ValidatorScenario,
  type ValidatorScenarioPair,
} from "./validator-scenario-registry.validator-scenarios.js";
