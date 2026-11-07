/** The validation-trace dispute runner at the script integrity authentication step
 * after state 89 of our trace: driven through the phase resolver and the
 * script integrity authentication semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep(
  {
    role: "script integrity authentication",
    from: 89,
    phase: "scriptIntegrity",
    semantic: 0,
    at: "scriptIntegrityAuthenticationSemantic",
  },
  (drive) => {
    it("refuses a transition other than the one the phase resolver committed, at the script integrity authentication semantic resolver", async () => {
      const refused = await drive.otherTransition();
      await expectOnchainRefusal(refused.build, {
        refusedBy:
          "fraud_proofs/validation_trace/script_integrity_authentication_semantic_v1",
        check: /^Validator returned false$/u,
      });
      await refused.unmoved();
    }, 600_000);
  },
);
