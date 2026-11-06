/** The validation-trace dispute runner at the native scripts step after
 * state 87 of our trace, over the failing transaction's one script, a
 * Plutus script, whose execution descriptor has neither a first chunk nor
 * signer peaks: driven through the phase resolver and the native scripts
 * effectful semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { Constr } from "@lucid-evolution/lucid";
import { it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep(
  {
    role: "native scripts effectful",
    from: 87,
    phase: "nativeScripts",
    semantic: 2,
    evidence: { auxiliary: 33, fixed: { 15: new Constr(1, []), 16: [] } },
    at: "nativeScriptsEffectfulSemantic",
  },
  (drive) => {
    it("refuses a transition other than the one the phase resolver committed, at the native scripts effectful semantic resolver", async () => {
      const refused = await drive.otherTransition();
      await expectOnchainRefusal(refused.build, {
        refusedBy:
          "fraud_proofs/validation_trace/native_scripts_effectful_semantic_v1",
        check: /^Validator returned false$/u,
      });
      await refused.unmoved();
    }, 600_000);
  },
);
