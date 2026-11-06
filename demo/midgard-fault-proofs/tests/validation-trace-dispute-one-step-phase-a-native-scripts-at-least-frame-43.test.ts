/** The validation-trace dispute runner at the phase-A native scripts at-
 * least container frame step after state 43 of our trace, over a non-empty
 * `atLeast` container, driven through the phase resolver and the phase-A
 * native scripts at-least container frame semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import {
  EVERY_TOKEN,
  NO_SIGNER_SET_PROOF,
} from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "phase-A native scripts at-least container frame",
  refusedBy:
    "phase_a_native_scripts_at_least_container_frame_payload_semantic_v1",
  from: 43,
  phase: "phaseANativeScripts",
  semantic: 5,
  evidence: { auxiliary: 3, fixed: { 2: NO_SIGNER_SET_PROOF } },
  at: "phaseANativeScriptsAtLeastContainerFrameSemantic",
  ...EVERY_TOKEN,
});
