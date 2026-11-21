/** The validation-trace dispute runner at a CEK context step that finalizes
 * a PlutusV3 minting policy's context, over a failing transaction whose
 * spend passes and whose minting policy fails: the finalize authenticate
 * routes the step to the finalize mint, and the thread is driven through
 * its resolution to the award
 * (support/validation-trace-dispute-one-step-context-finalize-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { contextFinalizeDrive } from "./support/validation-trace-dispute-one-step-context-finalize-drive.js";

const drive = contextFinalizeDrive("mint");

describe("validation-trace dispute runner: driving a thread at a CEK context step that finalizes a PlutusV3 minting policy", () => {
  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step to the finalize authenticate with the minting policy's finalization, for the finalize mint",
    drive.routes,
    300_000,
  );

  it(
    "moves the thread through the finalize mint and the context step's resolution to the award, and closes it",
    drive.awards,
    1_200_000,
  );

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
