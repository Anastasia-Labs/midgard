/** The validation-trace dispute runner at the script integrity compact step
 * after state 90 of our trace: driven through the phase resolver and the
 * script integrity compact semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "script integrity compact",
  refusedBy: "script_integrity_compact_semantic_v1",
  from: 90,
  phase: "scriptIntegrity",
  semantic: 1,
  at: "scriptIntegrityCompactSemantic",
});
