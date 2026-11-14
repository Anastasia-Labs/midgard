/** The validation-trace dispute runner at the phase-A native scripts advance
 * step after state 21 of our trace, over the advance past a native script
 * the failing transaction's native spend witnesses, driven through the
 * phase resolver and the phase-A native scripts advance semantic resolver
 * to the award (support/validation-trace-dispute-one-step-phase-step.ts). */
import { it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep(
  {
    role: "phase-A native scripts advance",
    from: 21,
    phase: "phaseANativeScripts",
    semantic: 0,
    at: "phaseANativeScriptsAdvanceSemantic",
    failingNativeSpend: true,
  },
  (drive) => {
    it("refuses a transition other than the one the phase resolver committed, at the phase-A native scripts advance semantic resolver", async () => {
      const refused = await drive.otherTransition();
      await expectOnchainRefusal(refused.build, {
        refusedBy:
          "fraud_proofs/validation_trace/phase_a_native_scripts_advance_semantic_v1",
        check: /^Validator returned false$/u,
      });
      await refused.unmoved();
    }, 600_000);
  },
);
