/** The validation-trace dispute runner at a CEK context step that finalizes
 * a Cardano spend, which the finalize authenticate and the finalize spend
 * verify, driven through its resolution to the award
 * (support/validation-trace-dispute-one-step-context-finalize-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { contextFinalizeDrive } from "./support/validation-trace-dispute-one-step-context-finalize-drive.js";

const drive = contextFinalizeDrive();

describe("validation-trace dispute runner: driving a thread at a CEK context step that finalizes a Cardano spend", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step to the finalize authenticate with the spend's finalization",
    drive.routes,
    300_000,
  );

  it("refuses a pending finalization whose redeemer is not the one the redeemers control holds, at the finalize authenticate", async () => {
    const refused = await drive.otherRedeemer();
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/cek_context_finalize_authenticate",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it(
    "moves the thread through the finalize spend and the context step's resolution to the award, and closes it",
    drive.awards,
    1_200_000,
  );

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
