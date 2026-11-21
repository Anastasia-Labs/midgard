/** The validation-trace dispute runner at a CEK context step that selects a
 * redeemer, which the redeemer select authenticate, initialize, hash and
 * finish verify, driven through its resolution to the award
 * (support/validation-trace-dispute-one-step-context-redeemer-select-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { contextRedeemerSelectDrive } from "./support/validation-trace-dispute-one-step-context-redeemer-select-drive.js";

const drive = contextRedeemerSelectDrive();

describe("validation-trace dispute runner: driving a thread at a CEK context step that selects a redeemer", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step to the redeemer select authenticate with a select",
    drive.routes,
    300_000,
  );

  it("refuses a select whose purpose frontier the authenticate did not move, at the authenticate", async () => {
    const refused = await drive.unmovedFrontier();
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/cek_context_redeemer_select_authenticate",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it(
    "moves the thread through the select's initialize, hash and finish and the context step's resolution to the award, and closes it",
    drive.awards,
    1_200_000,
  );

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
