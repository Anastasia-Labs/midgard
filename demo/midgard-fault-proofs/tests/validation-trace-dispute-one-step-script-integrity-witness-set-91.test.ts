/** The validation-trace dispute runner at the script integrity witness set step
 * after state 91 of our trace: driven through the phase resolver and the
 * script integrity witness set semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "script integrity witness set",
  refusedBy: "script_integrity_witness_set_semantic_v1",
  from: 91,
  phase: "scriptIntegrity",
  semantic: 2,
  at: "scriptIntegrityWitnessSetSemantic",
});
