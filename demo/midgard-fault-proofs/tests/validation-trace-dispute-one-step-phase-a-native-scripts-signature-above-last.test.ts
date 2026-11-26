/** The validation-trace dispute runner at the phase-A native scripts
 * signature above last step after state 28 of our trace, over the signature
 * of a key above every signer of the transaction, driven through the phase
 * resolver and the phase-A native scripts signature above last semantic
 * resolver to the award (support/validation-trace-dispute-one-step-phase-
 * step.ts). */
import { it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { EVERY_TOKEN } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep(
  {
    role: "phase-A native scripts signature above last",
    from: 28,
    phase: "phaseANativeScripts",
    semantic: 11,
    evidence: { auxiliary: 3 },
    at: "phaseANativeScriptsSignatureAboveLastSemantic",
    ...EVERY_TOKEN,
  },
  (drive) => {
    it("refuses a transition other than the one the phase resolver committed, at the phase-A native scripts signature above last semantic resolver", async () => {
      const refused = await drive.otherTransition();
      await expectOnchainRefusal(refused.build, {
        refusedBy:
          "fraud_proofs/validation_trace/phase_a_native_scripts_signature_above_last_payload_semantic_v1",
        check: /^Validator returned false$/u,
      });
      await refused.unmoved();
    }, 600_000);
  },
);
