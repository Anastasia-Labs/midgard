/** The validation-trace dispute runner at the native scripts step after
 * state 88 of our trace, past the failing transaction's last execution:
 * driven through the phase resolver and the native scripts terminal
 * semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep(
  {
    role: "native scripts terminal",
    from: 88,
    phase: "nativeScripts",
    semantic: 0,
    at: "nativeScriptsTerminalSemantic",
  },
  (drive) => {
    it("refuses a transition other than the one the phase resolver committed, at the native scripts terminal semantic resolver", async () => {
      const refused = await drive.otherTransition();
      await expectOnchainRefusal(refused.build, {
        refusedBy:
          "fraud_proofs/validation_trace/native_scripts_terminal_semantic_v1",
        check: /^Validator returned false$/u,
      });
      await refused.unmoved();
    }, 600_000);
  },
);
