/** The validation-trace dispute runner at the phase-A native scripts
 * signature between step after state 31 of our trace, over the signature of
 * a key between two signers of the transaction, driven through the phase
 * resolver and the phase-A native scripts signature between semantic
 * resolver to the award (support/validation-trace-dispute-one-step-phase-
 * step.ts). */
import { it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { EVERY_TOKEN } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep(
  {
    role: "phase-A native scripts signature between",
    from: 31,
    phase: "phaseANativeScripts",
    semantic: 12,
    evidence: { auxiliary: 3 },
    at: "phaseANativeScriptsSignatureBetweenSemantic",
    ...EVERY_TOKEN,
  },
  (drive) => {
    it("refuses a transition other than the one the phase resolver committed, at the phase-A native scripts signature between semantic resolver", async () => {
      const refused = await drive.otherTransition();
      await expectOnchainRefusal(refused.build, {
        refusedBy:
          "fraud_proofs/validation_trace/phase_a_native_scripts_signature_between_payload_semantic_v1",
        check: /^Validator returned false$/u,
      });
      await refused.unmoved();
    }, 600_000);
  },
);
