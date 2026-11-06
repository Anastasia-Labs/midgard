/** The validation-trace dispute runner at the script-sources stage-seven
 * observer step after state 102 of our trace, over the observer the failing
 * transaction requires, as a script purpose, driven through the phase
 * resolver and the script-sources stage-seven observer semantic resolver to
 * the award (support/validation-trace-dispute-one-step-phase-step.ts). */
import {
  OBSERVED,
  OBSERVER_HASH,
} from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "script-sources stage-seven observer",
  refusedBy: "script_sources_stage_seven_observer_semantic_v1",
  from: 102,
  phase: "scriptSources",
  semantic: 25,
  evidence: {
    fieldIndex: 3,
    itemIndex: 0,
    claims: [OBSERVER_HASH, 1n],
    yieldFields: ["scriptSourcesObserverItem", "scriptSourcesObserverBound"],
  },
  at: "scriptSourcesStageSevenObserverSemantic",
  ...OBSERVED,
});
