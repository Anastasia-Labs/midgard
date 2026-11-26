/** The validation-trace dispute runner at the phase-A native scripts frame
 * step after state 23 of our trace, over the return to the `any`
 * container's frame after a branch, driven through the phase resolver and
 * the phase-A native scripts frame semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { EVERY_TOKEN } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep(
  {
    role: "phase-A native scripts frame",
    from: 23,
    phase: "phaseANativeScripts",
    semantic: 13,
    evidence: { auxiliary: 4 },
    at: "phaseANativeScriptsFrameSemantic",
    ...EVERY_TOKEN,
  },
  (drive) => {
    it("refuses a transition other than the one the phase resolver committed, at the phase-A native scripts frame semantic resolver", async () => {
      const refused = await drive.otherTransition();
      await expectOnchainRefusal(refused.build, {
        refusedBy:
          "fraud_proofs/validation_trace/phase_a_native_scripts_frame_semantic_v1",
        check: /^Validator returned false$/u,
      });
      await refused.unmoved();
    }, 600_000);
  },
);
