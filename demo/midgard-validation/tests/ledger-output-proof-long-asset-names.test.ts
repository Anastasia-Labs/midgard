import { describe, it } from "vitest";

import {
  LONG_ASSET_NAME_REFUSAL_ROWS,
  LONG_ASSET_NAME_ROWS,
} from "./fixtures/ledger-output-proof-replay.vectors.mjs";
import {
  expectRefusalVector,
  expectReplayVector,
} from "./helpers/ledger-output-proof-replay.js";

// Ledger output proofs of outputs whose Cardano value is spelled with long
// asset names up to the 5,000-byte value cap, across the 4,095-byte chunk
// boundary. The rows are `LONG_ASSET_NAME_ROWS` of the replay golden channel
// (see `tests/helpers/ledger-output-proof-replay.ts`); the on-chain steps
// replay the same witness lists to the same terminal controls. One byte over
// the cap, `LONG_ASSET_NAME_REFUSAL_ROWS`, the output scan refuses the output
// and the on-chain scan refuses it at the same step and control.

describe("ledger output proofs of values with long asset names", () => {
  it.each(LONG_ASSET_NAME_ROWS)("proves $label", (row) => {
    expectReplayVector(row, "longAssetNames");
  });

  it.each(LONG_ASSET_NAME_REFUSAL_ROWS)("refuses $label", (row) => {
    expectRefusalVector(row);
  });
});
