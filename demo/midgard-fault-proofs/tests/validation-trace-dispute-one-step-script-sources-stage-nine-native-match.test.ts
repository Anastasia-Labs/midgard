/** The validation-trace dispute runner at the script-sources stage-nine
 * native match step after state 118 of our trace, over the native witness
 * script a scan finds as the source it looks for, driven through the phase
 * resolver and the script-sources stage-nine native match semantic resolver
 * to the award (support/validation-trace-dispute-one-step-phase-step.ts). */
import { it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep(
  {
    role: "script-sources stage-nine native match",
    from: 118,
    phase: "scriptSources",
    semantic: 11,
    evidence: { auxiliary: 9, fixed: { 3: 0n } },
    at: "scriptSourcesStageNineNativeMatchSemantic",
    failingNativeSpend: true,
  },
  (drive) => {
    it("refuses a transition other than the one the phase resolver committed, at the script-sources stage-nine native match semantic resolver", async () => {
      const refused = await drive.otherTransition();
      await expectOnchainRefusal(refused.build, {
        refusedBy:
          "fraud_proofs/validation_trace/script_sources_stage_nine_native_match_semantic_v1",
        check: /^Validator returned false$/u,
      });
      await refused.unmoved();
    }, 600_000);
  },
);
