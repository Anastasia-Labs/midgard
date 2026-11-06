/** The validation-trace dispute runner at the phase-A native scripts all-or-
 * any container frame step after state 20 of our trace, over the `any`
 * container that opens the native script reading every kind of token,
 * driven through the phase resolver and the phase-A native scripts all-or-
 * any container frame semantic resolver to the award (support/validation-
 * trace-dispute-one-step-phase-step.ts). */
import {
  EVERY_TOKEN,
  NO_SIGNER_SET_PROOF,
} from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "phase-A native scripts all-or-any container frame",
  refusedBy:
    "phase_a_native_scripts_all_or_any_container_frame_payload_semantic_v1",
  from: 20,
  phase: "phaseANativeScripts",
  semantic: 3,
  evidence: { auxiliary: 3, fixed: { 2: NO_SIGNER_SET_PROOF } },
  at: "phaseANativeScriptsAllOrAnyContainerFrameSemantic",
  ...EVERY_TOKEN,
});
