/** The validation-trace dispute runner at the phase-A native scripts frame
 * step after state 23 of our trace, over the return to the `any`
 * container's frame after a branch, driven through the phase resolver and
 * the phase-A native scripts frame semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { EVERY_TOKEN } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "phase-A native scripts frame",
  refusedBy: "phase_a_native_scripts_frame_semantic_v1",
  from: 23,
  phase: "phaseANativeScripts",
  semantic: 13,
  evidence: { auxiliary: 4 },
  at: "phaseANativeScriptsFrameSemantic",
  ...EVERY_TOKEN,
});
