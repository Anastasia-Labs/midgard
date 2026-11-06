/** The validation-trace dispute runner at the native scripts step after
 * state 88 of our trace, past the failing transaction's last execution:
 * driven through the phase resolver and the native scripts terminal
 * semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "native scripts terminal",
  refusedBy: "native_scripts_terminal_semantic_v1",
  from: 88,
  phase: "nativeScripts",
  semantic: 0,
  at: "nativeScriptsTerminalSemantic",
});
