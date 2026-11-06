/** The validation-trace dispute runner at the native scripts step after
 * state 87 of our trace, over the failing transaction's one script, a
 * Plutus script, whose execution descriptor has neither a first chunk nor
 * signer peaks: driven through the phase resolver and the native scripts
 * effectful semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { Constr } from "@lucid-evolution/lucid";

import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "native scripts effectful",
  refusedBy: "native_scripts_effectful_semantic_v1",
  from: 87,
  phase: "nativeScripts",
  semantic: 2,
  evidence: { auxiliary: 33, fixed: { 15: new Constr(1, []), 16: [] } },
  at: "nativeScriptsEffectfulSemantic",
});
