/** The validation-trace dispute runner at the phase-A native scripts token
 * head step after state 19 of our trace, over the first token of a native
 * script the failing transaction's native spend witnesses, driven through
 * the phase resolver and the phase-A native scripts token head semantic
 * resolver to the award (support/validation-trace-dispute-one-step-phase-
 * step.ts). */
import { it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { NO_SIGNER_SET_PROOF } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep(
  {
    role: "phase-A native scripts token head",
    from: 19,
    phase: "phaseANativeScripts",
    semantic: 2,
    evidence: { auxiliary: 3, fixed: { 2: NO_SIGNER_SET_PROOF } },
    at: "phaseANativeScriptsTokenHeadSemantic",
    failingNativeSpend: true,
  },
  (drive) => {
    it("refuses a transition other than the one the phase resolver committed, at the phase-A native scripts token head semantic resolver", async () => {
      const refused = await drive.otherTransition();
      await expectOnchainRefusal(refused.build, {
        refusedBy:
          "fraud_proofs/validation_trace/phase_a_native_scripts_token_head_semantic_v1",
        check: /^Validator returned false$/u,
      });
      await refused.unmoved();
    }, 600_000);
  },
);
