/** The validation-trace dispute runner at the script-sources stage-nine
 * mismatch step after state 112 of our trace, over a witness script that is
 * not the source a scan looks for, driven through the phase resolver and
 * the script-sources stage-nine mismatch semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep(
  {
    role: "script-sources stage-nine mismatch",
    from: 112,
    phase: "scriptSources",
    semantic: 10,
    evidence: { auxiliary: 9 },
    at: "scriptSourcesStageNineMismatchSemantic",
    failingNativeSpend: true,
  },
  (drive) => {
    it("refuses a transition other than the one the phase resolver committed, at the script-sources stage-nine mismatch semantic resolver", async () => {
      const refused = await drive.otherTransition();
      await expectOnchainRefusal(refused.build, {
        refusedBy:
          "fraud_proofs/validation_trace/script_sources_stage_nine_mismatch_semantic_v1",
        check: /^Validator returned false$/u,
      });
      await refused.unmoved();
    }, 600_000);
  },
);
