/** The validation-trace dispute runner at the phase-A native scripts step
 * after state 18 of our trace, over a failing transaction that also spends
 * an output a native script holds, which it witnesses first: the step reads
 * that native script, driven through the phase resolver and the phase-A
 * native scripts item semantic resolver, beside the witness-script field
 * carried inline and the native item yield the script's kind selects, to
 * the award (support/validation-trace-dispute-one-step-phase-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { phaseDrive } from "./support/validation-trace-dispute-one-step-phase-drive.js";

const drive = phaseDrive({
  from: 18,
  phase: "phaseANativeScripts",
  semantic: 1,
  evidence: {
    fieldIndex: 6,
    itemIndex: 0,
    itemYield: { kind: 0, name: "phaseANativeItemNative" },
  },
  at: "phaseANativeScriptsItemSemantic",
  failingNativeSpend: true,
});

describe("validation-trace dispute runner: driving a thread at the phase-A native scripts item step after state 18, over a native script", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step through the phase resolver to the phase-A native scripts item semantic resolver, committing the transition beside the native script and its field carried inline, with the native item yield",
    drive.routes,
    300_000,
  );

  it("refuses the item step invoking the foreign item yield beside a native script, at that yield", async () => {
    const refused = await drive.otherYield({
      kind: 1,
      name: "phaseANativeItemForeign",
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
