/** The validation-trace dispute runner at a CEK context step that opens an
 * observer a failing transaction requires, a native script it witnesses:
 * the observer authenticate and the observer fold verify it, driven through
 * its resolution to the award. Of one observer, and of each of two, which
 * the fold takes last first, each opened beside how many there are
 * (support/validation-trace-dispute-one-step-context-observer-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  contextObserverDrive,
  observedTrace,
  observerItemOpening,
  observerItemStepFrom,
} from "./support/validation-trace-dispute-one-step-context-observer-drive.js";

describe("validation-trace dispute runner: driving a thread at a CEK context step that opens a required observer", () => {
  const drive = contextObserverDrive({
    from: observerItemStepFrom(1),
    installs: observedTrace(1),
    opening: observerItemOpening(1),
  });

  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step to the observer authenticate with the observer its transaction requires",
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

describe.each([
  ["the later, which the fold takes first", 0],
  ["the earlier, which the fold takes last", 1],
])(
  "validation-trace dispute runner: driving a thread at a CEK context step that opens %s of two required observers",
  (_, taken) => {
    const drive = contextObserverDrive({
      from: observerItemStepFrom(2, taken),
      installs: observedTrace(2),
      opening: observerItemOpening(2, taken),
    });

    beforeAll(drive.setUp, 1_800_000);

    it(
      "routes the step to the observer authenticate with that observer, beside how many the transaction requires",
      drive.routes,
      300_000,
    );

    it(
      "moves the thread through the observer fold and the context step's resolution to the award, and closes it",
      drive.awards,
      1_200_000,
    );
  },
);
