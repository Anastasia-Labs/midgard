/** The validation-trace dispute runner at the canonical-decode step after
 * state 0 of our trace, the first item of the transaction's first field,
 * which the item semantic resolver and the item's stages verify, driven
 * through its resolution to the award
 * (support/validation-trace-dispute-one-step-canonical-decode-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { canonicalDecodeDrive } from "./support/validation-trace-dispute-one-step-canonical-decode-drive.js";

const drive = canonicalDecodeDrive({
  from: 0,
  kind: "item",
  at: "canonicalDecodeItemProof",
});

describe("validation-trace dispute runner: driving a thread at the canonical-decode step after state 0, an item step", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step to the item semantic resolver, committing the transition alone",
    drive.routes,
    300_000,
  );

  it("plans every canonical-decode step of our trace, the item steps through the item's stages, and none past them", async () =>
    await drive.plansEvery());

  it("refuses a successor whose encoded field length past the item is not the one the proof stage derives, at the stage", async () => {
    const refused = await drive.oneMore(2);
    await expectOnchainRefusal(refused.build, {
      refusedBy: "fraud_proofs/validation_trace/canonical_decode_item_proof_v1",
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
