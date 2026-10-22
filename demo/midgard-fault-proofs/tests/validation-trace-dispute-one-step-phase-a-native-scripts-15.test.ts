/** The validation-trace dispute runner at the phase-A native scripts step
 * after state 15 of our trace, which reads the failing transaction's one
 * witness script, a Plutus script: driven through the phase resolver and
 * the phase-A native scripts item semantic resolver, beside the
 * witness-script field carried inline and the foreign item yield the
 * script's kind selects, to the award
 * (support/validation-trace-dispute-one-step-phase-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { phaseDrive } from "./support/validation-trace-dispute-one-step-phase-drive.js";

const drive = phaseDrive({
  from: 15,
  phase: "phaseANativeScripts",
  semantic: 1,
  evidence: {
    fieldIndex: 6,
    itemIndex: 0,
    itemYield: { kind: 1, name: "phaseANativeItemForeign" },
  },
  at: "phaseANativeScriptsItemSemantic",
});

describe("validation-trace dispute runner: driving a thread at the phase-A native scripts item step after state 15", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step through the phase resolver to the phase-A native scripts item semantic resolver, committing the transition beside the witness script and its field carried inline, with the foreign item yield",
    drive.routes,
    300_000,
  );

  it("refuses a transition other than the one the phase resolver committed, at the phase-A native scripts item semantic resolver", async () => {
    const refused = await drive.otherTransition();
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/phase_a_native_scripts_item_semantic_v1",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it("refuses the item step invoking the native item yield beside a Plutus script, at that yield", async () => {
    const refused = await drive.otherYield({
      kind: 0,
      name: "phaseANativeItemNative",
    });
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it(
    "moves the thread through the step's resolution to the award, and closes it",
    drive.awards,
    1_200_000,
  );

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
