/** The validation-trace dispute runner at the phase-A native scripts
 * signature below first step after state 25 of our trace, over the
 * signature of a key below every signer of the transaction, driven through
 * the phase resolver and the phase-A native scripts signature below first
 * semantic resolver to the award (support/validation-trace-dispute-one-
 * step-phase-step.ts). */
import { EVERY_TOKEN } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "phase-A native scripts signature below first",
  refusedBy: "phase_a_native_scripts_signature_below_first_payload_semantic_v1",
  from: 25,
  phase: "phaseANativeScripts",
  semantic: 10,
  evidence: { auxiliary: 3 },
  at: "phaseANativeScriptsSignatureBelowFirstSemantic",
  ...EVERY_TOKEN,
});
