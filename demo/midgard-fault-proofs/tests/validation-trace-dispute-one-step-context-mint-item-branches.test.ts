/** The validation-trace dispute runner at the CEK context steps the mint
 * item stage verifies past a mint's first asset, each over a failing
 * transaction that mints under native scripts: the next asset under the
 * same policy, the first asset under the next policy, and the step past
 * the last asset, each driven through its resolution to the award
 * (support/validation-trace-dispute-one-step-context-drive.ts). The first
 * asset's step is driven by the mint item test. */
import { beforeAll, describe, it } from "vitest";

import {
  type ContextInstalls,
  contextStageDrive,
} from "./support/validation-trace-dispute-one-step-context-drive.js";

describe.each<
  readonly [branch: string, from: number, installs: ContextInstalls]
>([
  // A mint of two assets under one policy: its second asset's step.
  [
    "the next asset under the same policy",
    144,
    { failingNativeMint: { assets: 2 }, operatorFrom: 157 },
  ],
  // A mint of an asset under each of two policies: the second's step.
  [
    "the first asset under the next policy",
    183,
    { failingNativeMint: { policies: 2 }, operatorFrom: 197 },
  ],
  // A mint of one asset: the step past it.
  [
    "the step past the last asset",
    141,
    { failingNativeMint: true, operatorFrom: 153 },
  ],
])(
  "validation-trace dispute runner: driving a thread at a CEK context step of the mint item stage, at %s",
  (_, from, installs) => {
    const drive = contextStageDrive("mintItem", from, installs);

    beforeAll(drive.setUp, 1_800_000);

    it("routes the step to the mint item stage", drive.routes, 300_000);

    it(
      "moves the thread through the context step's resolution to the award, and closes it",
      drive.awards,
      1_200_000,
    );

    it(
      "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
      drive.pays,
    );
  },
);
