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
import "./validator-scenario-registry.validation-trace-scenarios.js";
import "./validator-scenario-registry.script-integrity-hash-mismatch-scenarios.js";
import "./validator-scenario-registry.receive-purpose-language-scenarios.js";
import "./validator-scenario-registry.family-scenarios.js";
import "./validator-scenario-registry.unmapped-validators.js";
import "./validator-scenario-registry.unmapped-families.js";

import { FAMILY_SCENARIOS as SHARED_FAMILY_SCENARIOS } from "./validator-scenario-registry.family-scenarios.js";
import {
  RECEIVE_PURPOSE_LANGUAGE_FAMILY_SCENARIOS,
  RECEIVE_PURPOSE_LANGUAGE_SCENARIOS,
} from "./validator-scenario-registry.receive-purpose-language-scenarios.js";
import {
  SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_SCENARIOS,
  SCRIPT_INTEGRITY_HASH_MISMATCH_SCENARIOS,
} from "./validator-scenario-registry.script-integrity-hash-mismatch-scenarios.js";
import { VALIDATION_TRACE_DISPUTE_FAMILY_SCENARIOS } from "./validator-scenario-registry.validation-trace-scenarios.js";
import { VALIDATOR_SCENARIOS as SHARED_VALIDATOR_SCENARIOS } from "./validator-scenario-registry.validator-scenarios.js";

export const FAMILY_SCENARIOS: typeof SHARED_FAMILY_SCENARIOS = {
  ...SHARED_FAMILY_SCENARIOS,
  validationTraceDispute: VALIDATION_TRACE_DISPUTE_FAMILY_SCENARIOS,
  scriptIntegrityHashMismatch: SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_SCENARIOS,
  receivePurposeLanguage: RECEIVE_PURPOSE_LANGUAGE_FAMILY_SCENARIOS,
};
export const VALIDATOR_SCENARIOS: typeof SHARED_VALIDATOR_SCENARIOS = {
  ...SHARED_VALIDATOR_SCENARIOS,
  ...SCRIPT_INTEGRITY_HASH_MISMATCH_SCENARIOS,
  ...RECEIVE_PURPOSE_LANGUAGE_SCENARIOS,
};
export {
  UNMAPPED_FAMILIES,
  UNMAPPED_FAMILY_COUNT,
  UNMAPPED_VALIDATOR_COUNT,
} from "./validator-scenario-registry.unmapped-families.js";
export { UNMAPPED_VALIDATORS } from "./validator-scenario-registry.unmapped-validators.js";
export {
  type ValidatorScenario,
  type ValidatorScenarioPair,
} from "./validator-scenario-registry.validator-scenarios.js";
