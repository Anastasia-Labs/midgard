/** The validation-trace dispute runner at the phase-A native scripts
 * signature between step after state 31 of our trace, over the signature of
 * a key between two signers of the transaction, driven through the phase
 * resolver and the phase-A native scripts signature between semantic
 * resolver to the award (support/validation-trace-dispute-one-step-phase-
 * step.ts). */
import { EVERY_TOKEN } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "phase-A native scripts signature between",
  refusedBy: "phase_a_native_scripts_signature_between_payload_semantic_v1",
  from: 31,
  phase: "phaseANativeScripts",
  semantic: 12,
  evidence: { auxiliary: 3 },
  at: "phaseANativeScriptsSignatureBetweenSemantic",
  ...EVERY_TOKEN,
});
