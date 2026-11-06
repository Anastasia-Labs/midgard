/** The validation-trace dispute runner at the phase-A native scripts
 * signature over no signers step after state 20 of our trace, over the
 * owner's signature, over a transaction nobody signs, driven through the
 * phase resolver and the phase-A native scripts signature over no signers
 * semantic resolver to the award (support/validation-trace-dispute-one-
 * step-phase-step.ts). */
import { UNSIGNED } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep({
  role: "phase-A native scripts signature over no signers",
  refusedBy: "phase_a_native_scripts_signature_empty_payload_semantic_v1",
  from: 20,
  phase: "phaseANativeScripts",
  semantic: 9,
  evidence: { auxiliary: 3 },
  at: "phaseANativeScriptsSignatureEmptySemantic",
  ...UNSIGNED,
});
