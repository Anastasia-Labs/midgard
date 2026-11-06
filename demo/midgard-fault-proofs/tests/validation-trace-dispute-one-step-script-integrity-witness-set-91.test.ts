/** The validation-trace dispute runner at the script integrity witness set step
 * after state 91 of our trace: driven through the phase resolver and the
 * script integrity witness set semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep(
  {
    role: "script integrity witness set",
    from: 91,
    phase: "scriptIntegrity",
    semantic: 2,
    at: "scriptIntegrityWitnessSetSemantic",
  },
  (drive) => {
    it("refuses a transition other than the one the phase resolver committed, at the script integrity witness set semantic resolver", async () => {
      const refused = await drive.otherTransition();
      await expectOnchainRefusal(refused.build, {
        refusedBy:
          "fraud_proofs/validation_trace/script_integrity_witness_set_semantic_v1",
        check: /^Validator returned false$/u,
      });
      await refused.unmoved();
    }, 600_000);
  },
);
