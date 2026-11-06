/** The validation-trace dispute runner at the script integrity authentication step
 * after state 89 of our trace: driven through the phase resolver and the
 * script integrity authentication semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "script integrity authentication",
  refusedBy: "script_integrity_authentication_semantic_v1",
  from: 89,
  phase: "scriptIntegrity",
  semantic: 0,
  at: "scriptIntegrityAuthenticationSemantic",
});
