/** The validation-trace dispute runner at the phase-A native scripts empty
 * at-least container step after state 40 of our trace, over an empty
 * `atLeast 0` container, driven through the phase resolver and the phase-A
 * native scripts empty at-least container semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  EVERY_TOKEN,
  NO_SIGNER_SET_PROOF,
} from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep(
  {
    role: "phase-A native scripts empty at-least container",
    from: 40,
    phase: "phaseANativeScripts",
    semantic: 6,
    evidence: { auxiliary: 3, fixed: { 2: NO_SIGNER_SET_PROOF } },
    at: "phaseANativeScriptsAtLeastEmptySemantic",
    ...EVERY_TOKEN,
  },
  (drive) => {
    it("refuses a transition other than the one the phase resolver committed, at the phase-A native scripts empty at-least container semantic resolver", async () => {
      const refused = await drive.otherTransition();
      await expectOnchainRefusal(refused.build, {
        refusedBy:
          "fraud_proofs/validation_trace/phase_a_native_scripts_at_least_empty_container_payload_semantic_v1",
        check: /^Validator returned false$/u,
      });
      await refused.unmoved();
    }, 600_000);
  },
);
