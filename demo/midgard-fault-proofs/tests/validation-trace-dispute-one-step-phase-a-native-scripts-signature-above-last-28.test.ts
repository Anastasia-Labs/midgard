/** The validation-trace dispute runner at the phase-A native scripts
 * signature above last step after state 28 of our trace, over the signature
 * of a key above every signer of the transaction, driven through the phase
 * resolver and the phase-A native scripts signature above last semantic
 * resolver to the award (support/validation-trace-dispute-one-step-phase-
 * step.ts). */
import { EVERY_TOKEN } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "phase-A native scripts signature above last",
  refusedBy: "phase_a_native_scripts_signature_above_last_payload_semantic_v1",
  from: 28,
  phase: "phaseANativeScripts",
  semantic: 11,
  evidence: { auxiliary: 3 },
  at: "phaseANativeScriptsSignatureAboveLastSemantic",
  ...EVERY_TOKEN,
});
