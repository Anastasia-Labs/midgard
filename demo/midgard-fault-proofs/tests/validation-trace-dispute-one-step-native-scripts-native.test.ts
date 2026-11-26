/** The validation-trace dispute runner at the native scripts step after
 * state 128 of our trace, over the native script whose output the failing
 * transaction also spends, whose execution descriptor carries its first
 * chunk's proof: driven through the phase resolver and the native scripts
 * native semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep(
  {
    role: "native scripts native",
    from: 128,
    phase: "nativeScripts",
    semantic: 1,
    evidence: { auxiliary: 33, fixed: { 1: 0n }, unwrapped: [15] },
    at: "nativeScriptsNativeSemantic",
    failingNativeSpend: true,
  },
  (drive) => {
    it("refuses a transition other than the one the phase resolver committed, at the native scripts native semantic resolver", async () => {
      const refused = await drive.otherTransition();
      await expectOnchainRefusal(refused.build, {
        refusedBy:
          "fraud_proofs/validation_trace/native_scripts_native_semantic_v1",
        check: /^Validator returned false$/u,
      });
      await refused.unmoved();
    }, 600_000);
  },
);
