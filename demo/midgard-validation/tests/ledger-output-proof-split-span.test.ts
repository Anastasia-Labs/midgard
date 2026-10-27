import { describe, expect, it } from "vitest";

import * as vectors from "./fixtures/ledger-output-proof-replay.vectors.mjs";
import {
  expectReplayVector,
  replayGolden,
} from "./helpers/ledger-output-proof-replay.js";

// Ledger output proofs of outputs with a large chunked datum scalar, whose
// span attaches occur at the bytes and integer stages of the datum
// traversal, and of an empty byte string at each side of a chunk boundary.
// The rows are `DATUM_SCALAR_ROWS` of the replay golden channel (see
// `tests/helpers/ledger-output-proof-replay.ts`).

describe("ledger output proofs with span attaches inside a datum scalar", () => {
  it("covers exactly the rows of the generated fixture", () => {
    expect(Object.keys(vectors).sort()).toEqual([
      "DATUM_SCALAR_ROWS",
      "EMPTY_FIELD_ROWS",
      "LEDGER_OUTPUT_PROOF_REPLAY_ROWS",
      "LONG_ASSET_NAME_REFUSAL_ROWS",
      "LONG_ASSET_NAME_ROWS",
      "ledgerOutputProofRefusalClaimProblems",
      "ledgerOutputProofRefusalVector",
      "ledgerOutputProofReplayClaimProblems",
      "ledgerOutputProofReplayVector",
    ]);
    expect(replayGolden.rows.map(({ id, family }) => ({ id, family }))).toEqual(
      vectors.LEDGER_OUTPUT_PROOF_REPLAY_ROWS.map(({ row, family }) => ({
        id: row.id,
        family,
      })),
    );
    expect(replayGolden.refusals.map(({ id }) => id)).toEqual(
      vectors.LONG_ASSET_NAME_REFUSAL_ROWS.map(({ id }) => id),
    );
  });

  it.each(vectors.DATUM_SCALAR_ROWS)("proves $label", (row) => {
    expectReplayVector(row, "datumScalar");
  });
});
