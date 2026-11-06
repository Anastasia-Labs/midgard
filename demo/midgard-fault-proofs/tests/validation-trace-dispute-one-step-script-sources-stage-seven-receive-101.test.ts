/** The validation-trace dispute runner at the script-sources stage-seven
 * receive step after state 101 of our trace, over the protected output a
 * native script receives, as a script purpose, driven through the phase
 * resolver and the script-sources stage-seven receive semantic resolver to
 * the award (support/validation-trace-dispute-one-step-phase-step.ts). */
import { RECEIVING } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "script-sources stage-seven receive",
  refusedBy: "script_sources_stage_seven_receive_semantic_v1",
  from: 101,
  phase: "scriptSources",
  semantic: 26,
  evidence: { auxiliary: 8 },
  at: "scriptSourcesStageSevenReceiveSemantic",
  ...RECEIVING,
});
