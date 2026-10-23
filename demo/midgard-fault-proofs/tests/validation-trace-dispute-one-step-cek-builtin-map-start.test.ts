/** The validation-trace dispute runner at a CEK builtin step of core group 6,
 * a map conversion's start (unMapData), driven through the binder and the
 * group's arm chain to the award
 * (support/validation-trace-dispute-one-step-cek-builtin-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { cekBuiltinDrive } from "./support/validation-trace-dispute-one-step-cek-builtin-drive.js";

const drive = cekBuiltinDrive({
  group: 6,
  arm: 32,
  arms: ["mapStartRoots", "mapStartBudget", "mapStartNodes"],
});

describe("validation-trace dispute runner: driving a thread at a CEK builtin step of core group 6", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "plans the step through the binder to the group's arm chain, and leaves it unresolved where the chain is not installed",
    drive.plans,
    300_000,
  );

  it("refuses a hop whose successor keeps the hop's own progress, at the map start budget", async () => {
    const refused = await drive.unadvanced();
    await expectOnchainRefusal(refused.build, {
      refusedBy: "fraud_proofs/validation_trace/cek_core_map_start_budget",
      check: /^Validator returned false$/u,
    });
    await refused.unmoved();
  }, 600_000);

  it("resumes after a restart mid-chain, moving the thread through the rest of the chain to the award, and closes it", async () => {
    drive.restart();
    await drive.awards();
  }, 1_200_000);

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
