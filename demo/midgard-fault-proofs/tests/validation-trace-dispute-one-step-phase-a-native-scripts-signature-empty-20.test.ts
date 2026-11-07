/** The validation-trace dispute runner at the phase-A native scripts
 * signature over no signers step after state 20 of our trace, over the
 * owner's signature, over a transaction nobody signs, driven through the
 * phase resolver and the phase-A native scripts signature over no signers
 * semantic resolver to the award (support/validation-trace-dispute-one-
 * step-phase-step.ts). */
import { it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { UNSIGNED } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep(
  {
    role: "phase-A native scripts signature over no signers",
    from: 20,
    phase: "phaseANativeScripts",
    semantic: 9,
    evidence: { auxiliary: 3 },
    at: "phaseANativeScriptsSignatureEmptySemantic",
    ...UNSIGNED,
  },
  (drive) => {
    it("refuses a transition other than the one the phase resolver committed, at the phase-A native scripts signature over no signers semantic resolver", async () => {
      const refused = await drive.otherTransition();
      await expectOnchainRefusal(refused.build, {
        refusedBy:
          "fraud_proofs/validation_trace/phase_a_native_scripts_signature_empty_payload_semantic_v1",
        check: /^Validator returned false$/u,
      });
      await refused.unmoved();
    }, 600_000);
  },
);
