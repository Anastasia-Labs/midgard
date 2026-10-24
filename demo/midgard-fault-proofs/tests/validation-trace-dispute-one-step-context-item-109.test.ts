/** The validation-trace dispute runner at a CEK context step of the item
 * bind stage: a data scan's step that heads a sequence of the item's value,
 * which continues the scan, driven through its redeemer item step and
 * resolution to the award
 * (support/validation-trace-dispute-one-step-context-item-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { contextItemDrive } from "./support/validation-trace-dispute-one-step-context-item-drive.js";

const drive = contextItemDrive(109);

describe("validation-trace dispute runner: driving a thread at the CEK context item step after state 109", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step to the item bind stage and its item step to its route",
    drive.routes,
    300_000,
  );

  it("refuses a pending item step other than the one the stage binds, at the stage", async () => {
    const refused = await drive.unboundControl();
    await expectOnchainRefusal(refused.build, {
      refusedBy: "fraud_proofs/validation_trace/cek_context_item_bind",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it(
    "moves the thread through the item step and the context step's resolution to the award, and closes it",
    drive.awards,
    1_800_000,
  );

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
