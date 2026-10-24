/** The validation-trace dispute runner at a CEK context step that the
 * mint init stage verifies, driven through its resolution to the award
 * (support/validation-trace-dispute-one-step-context-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { contextStageDrive } from "./support/validation-trace-dispute-one-step-context-drive.js";

const drive = contextStageDrive("mintInit");

describe("validation-trace dispute runner: driving a thread at a CEK context step of the mint init stage", () => {
  beforeAll(drive.setUp, 1_800_000);

  it("routes the step to the mint init stage", drive.routes, 300_000);

  it("refuses a successor other than the one the stage derives, at the stage", async () => {
    const refused = await drive.unchangedSuccessor();
    await expectOnchainRefusal(refused.build, {
      refusedBy: "fraud_proofs/validation_trace/cek_context_mint_init",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it(
    "moves the thread through the context step's resolution to the award, and closes it",
    drive.awards,
    1_200_000,
  );

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
