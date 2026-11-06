/** The validation-trace dispute runner at the native scripts step after
 * state 128 of our trace, over the native script whose output the failing
 * transaction also spends, whose execution descriptor carries its first
 * chunk's proof: driven through the phase resolver and the native scripts
 * native semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "native scripts native",
  refusedBy: "native_scripts_native_semantic_v1",
  from: 128,
  phase: "nativeScripts",
  semantic: 1,
  evidence: { auxiliary: 33, fixed: { 1: 0n }, unwrapped: [15] },
  at: "nativeScriptsNativeSemantic",
  failingNativeSpend: true,
});
