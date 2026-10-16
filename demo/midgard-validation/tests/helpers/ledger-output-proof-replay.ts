import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  buildMidgardLedgerOutputProofTrace,
  encodeMidgardLedgerOutputProofControl,
  isExactMidgardLedgerOutputProofTerminal,
  MidgardLedgerOutputProofStages,
  nextMidgardCekDataTraverseSpan,
} from "@al-ft/midgard-core";
import {
  decodeMidgardDatum,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { expect } from "vitest";

import { plutusDataToCborHex } from "../../src/plutus-data.js";
import { ledgerOutputProofWitnessData } from "../../src/validation-machine-data.ledger-output-proof-witness-data.js";
import * as vectors from "../fixtures/ledger-output-proof-replay.vectors.mjs";

// The `src/` side of the ledger output proof replay golden channel. The rows
// are `tests/fixtures/ledger-output-proof-replay.vectors.mjs`; each is driven
// through `src/` and must reproduce its entry of the generated fixture
// `tests/fixtures/ledger-output-proof-replay.generated.json` exactly. The
// generator (`scripts/generate-ledger-output-proof-replay-goldens.mjs`) drives
// the same rows through `dist/` and writes the same values into
// `onchain/aiken/lib/midgard/ledger-output-proof-datum.test.ak`, where the
// typed and split on-chain steps replay each witness list to the same
// terminal control.

type ReplayGolden = {
  readonly rows: readonly (vectors.LedgerOutputProofReplayVectorV1 & {
    readonly family: vectors.LedgerOutputProofReplayFamilyV1;
  })[];
};

export const replayGolden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "../fixtures/ledger-output-proof-replay.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as ReplayGolden;

const twins: vectors.LedgerOutputProofReplayTwinsV1 = {
  buildMidgardLedgerOutputProofTrace,
  decodeMidgardDatum,
  encodeMidgardLedgerOutputProofControl,
  encodeMidgardTxOutput,
  isExactMidgardLedgerOutputProofTerminal,
  MidgardLedgerOutputProofStages,
  nextMidgardCekDataTraverseSpan,
  plutusDataToCborHex,
  witnessData: ledgerOutputProofWitnessData,
};

/**
 * Drives `row` of `family` through `src/`: the row's claims hold, and the
 * vector is exactly the row's entry of the generated fixture.
 */
export const expectReplayVector = (
  row: vectors.LedgerOutputProofReplayRowV1,
  family: vectors.LedgerOutputProofReplayFamilyV1,
): void => {
  const vector = vectors.ledgerOutputProofReplayVector(row, family, twins);
  expect(vectors.ledgerOutputProofReplayClaimProblems(row, vector)).toEqual([]);
  expect({ family, ...vector }).toEqual(
    replayGolden.rows.find((entry) => entry.id === row.id),
  );
};
