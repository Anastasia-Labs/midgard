/** The validation-trace dispute runner at a CEK context step that the
 * observer authenticate and the observer fold verify, driven through its
 * resolution to the award
 * (support/validation-trace-dispute-one-step-context-observer-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { contextObserverDrive } from "./support/validation-trace-dispute-one-step-context-observer-drive.js";

const drive = contextObserverDrive();

describe("validation-trace dispute runner: driving a thread at a CEK context step of the observer stage", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step to the observer authenticate with its transaction's opening",
    drive.routes,
    300_000,
  );

  it("refuses an opening other than the one the step's transaction opens, at the observer authenticate", async () => {
    const refused = await drive.finishedOpening();
    await expectOnchainRefusal(refused.build, {
      refusedBy:
        "fraud_proofs/validation_trace/cek_context_observer_authenticate",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it(
    "moves the thread through the observer fold and the context step's resolution to the award, and closes it",
    drive.awards,
    1_200_000,
  );

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
