import {
  buildMidgardLedgerOutputProofTrace,
  summarizeMidgardLedgerOutputCardanoSpendDatum,
  summarizeMidgardLedgerOutputSpendDatumOf,
} from "@al-ft/midgard-core";
import {
  decodeMidgardDatum,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import { summarizeMidgardCekData } from "../src/cek-context.midgard-cek-context-control.js";
import { plutusDataFromCborIterative } from "../src/plutus-data-iterative.decode.js";
import { EMPTY_FIELD_ROWS } from "./fixtures/ledger-output-proof-replay.vectors.mjs";
import { expectReplayVector } from "./helpers/ledger-output-proof-replay.js";

// Ledger output proofs of outputs holding an empty field. The rows are
// `EMPTY_FIELD_ROWS` of the replay golden channel (see
// `tests/helpers/ledger-output-proof-replay.ts`). Every output fits one chunk,
// so its chunk and span-attach witnesses are written as bare constructors and
// rebuilt on-chain from the single chunk.

const address = Buffer.concat([Buffer.from([0x78]), Buffer.alloc(28, 0x11)]);

const output = (datumHex: string): Buffer =>
  encodeMidgardTxOutput({
    address,
    value: { lovelace: 2_000_000n, assets: new Map() },
    datum: decodeMidgardDatum(Buffer.from(datumHex, "hex")),
  });

describe("ledger output proofs of outputs holding an empty field", () => {
  it.each(EMPTY_FIELD_ROWS)("proves $label", (row) => {
    expectReplayVector(row, "emptyField");
  });

  it.each([
    ["40"],
    ["9f40ff"],
    ["9f4001ff"],
    ["d8799f40ff"],
    ["d8799f4001ff"],
    ["a14000"],
    ["a10040"],
    ["a24000410140"],
    ["9f9f40ff40ff"],
    ["d87a9f40d8799f40ffff"],
    ["4101"],
  ])("summarises datum %s as the datum itself", (datumHex) => {
    const trace = buildMidgardLedgerOutputProofTrace({
      outputIndex: 0,
      outputCbor: output(datumHex),
    });
    const expected = summarizeMidgardLedgerOutputSpendDatumOf(
      summarizeMidgardCekData(
        plutusDataFromCborIterative(Buffer.from(datumHex, "hex")),
      ),
    );
    expect(
      summarizeMidgardLedgerOutputCardanoSpendDatum(trace.terminal),
    ).toEqual(expected);
  });
});
