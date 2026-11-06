/** The validation-trace dispute runner at the script integrity finalize step
 * after state 92 of our trace: driven through the phase resolver and the
 * script integrity finalize semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "script integrity finalize",
  refusedBy: "script_integrity_finalize_semantic_v1",
  from: 92,
  phase: "scriptIntegrity",
  semantic: 3,
  at: "scriptIntegrityFinalizeSemantic",
});
