/** The validation-trace dispute runner at the phase-A native scripts empty
 * all-or-any container step after state 20 of our trace, over the empty
 * `all` of a native script the failing transaction's native spend
 * witnesses, driven through the phase resolver and the phase-A native
 * scripts empty all-or-any container semantic resolver to the award
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { NO_SIGNER_SET_PROOF } from "./support/validation-trace-dispute-native-failing-shapes.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

describePhaseStep(
  {
    role: "phase-A native scripts empty all-or-any container",
    from: 20,
    phase: "phaseANativeScripts",
    semantic: 4,
    evidence: { auxiliary: 3, fixed: { 2: NO_SIGNER_SET_PROOF } },
    at: "phaseANativeScriptsAllOrAnyEmptySemantic",
    failingNativeSpend: true,
  },
  (drive) => {
    it("refuses a transition other than the one the phase resolver committed, at the phase-A native scripts empty all-or-any container semantic resolver", async () => {
      const refused = await drive.otherTransition();
      await expectOnchainRefusal(refused.build, {
        refusedBy:
          "fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_empty_container_payload_semantic_v1",
        check: /^Validator returned false$/u,
      });
      await refused.unmoved();
    }, 600_000);
  },
);
