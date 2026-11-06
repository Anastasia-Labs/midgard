/** The validation-trace dispute runner at the phase-A native scripts
 * signature membership step after state 22 of our trace, over the owner's
 * signature, which the transaction carries, driven through the phase
 * resolver and the phase-A native scripts signature membership semantic
 * resolver to the award (support/validation-trace-dispute-one-step-phase-
 * step.ts). */
import { EVERY_TOKEN } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "phase-A native scripts signature membership",
  refusedBy: "phase_a_native_scripts_signature_membership_payload_semantic_v1",
  from: 22,
  phase: "phaseANativeScripts",
  semantic: 8,
  evidence: { auxiliary: 3 },
  at: "phaseANativeScriptsSignatureMembershipSemantic",
  ...EVERY_TOKEN,
});
