/** The validation-trace dispute runner at the phase-A script-preconditions
 * required observer step after state 27 of our trace, over the observer the
 * failing transaction requires, a native script it witnesses, driven
 * through the phase resolver and the phase-A script-preconditions required
 * observer semantic resolver to the award (support/validation-trace-
 * dispute-one-step-phase-step.ts). */
import { OBSERVED } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "phase-A script-preconditions required observer",
  refusedBy: "phase_a_script_preconditions_item_semantic_v1",
  from: 27,
  phase: "phaseAScriptPreconditions",
  semantic: 1,
  evidence: { fieldIndex: 3, itemIndex: 0 },
  at: "phaseAScriptPreconditionsItemSemantic",
  ...OBSERVED,
});
