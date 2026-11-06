/** The validation-trace dispute runner at the script-sources stage-nine
 * mismatch step after state 112 of our trace, over a witness script that is
 * not the source a scan looks for, driven through the phase resolver and
 * the script-sources stage-nine mismatch semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "script-sources stage-nine mismatch",
  refusedBy: "script_sources_stage_nine_mismatch_semantic_v1",
  from: 112,
  phase: "scriptSources",
  semantic: 10,
  evidence: { auxiliary: 9 },
  at: "scriptSourcesStageNineMismatchSemantic",
  failingNativeSpend: true,
});
