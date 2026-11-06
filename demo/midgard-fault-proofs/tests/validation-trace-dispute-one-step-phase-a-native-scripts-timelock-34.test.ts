/** The validation-trace dispute runner at the phase-A native scripts time
 * lock step after state 34 of our trace, over a time lock, driven through
 * the phase resolver and the phase-A native scripts time lock semantic
 * resolver to the award (support/validation-trace-dispute-one-step-phase-
 * step.ts). */
import {
  EVERY_TOKEN,
  NO_SIGNER_SET_PROOF,
} from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "phase-A native scripts time lock",
  refusedBy: "phase_a_native_scripts_timelock_payload_semantic_v1",
  from: 34,
  phase: "phaseANativeScripts",
  semantic: 7,
  evidence: { auxiliary: 3, fixed: { 2: NO_SIGNER_SET_PROOF } },
  at: "phaseANativeScriptsTimelockSemantic",
  ...EVERY_TOKEN,
});
