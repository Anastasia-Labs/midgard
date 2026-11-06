/** The validation-trace dispute runner at the phase-A native scripts empty
 * all-or-any container step after state 20 of our trace, over the empty
 * `all` of a native script the failing transaction's native spend
 * witnesses, driven through the phase resolver and the phase-A native
 * scripts empty all-or-any container semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { NO_SIGNER_SET_PROOF } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "phase-A native scripts empty all-or-any container",
  refusedBy:
    "phase_a_native_scripts_all_or_any_empty_container_payload_semantic_v1",
  from: 20,
  phase: "phaseANativeScripts",
  semantic: 4,
  evidence: { auxiliary: 3, fixed: { 2: NO_SIGNER_SET_PROOF } },
  at: "phaseANativeScriptsAllOrAnyEmptySemantic",
  failingNativeSpend: true,
});
