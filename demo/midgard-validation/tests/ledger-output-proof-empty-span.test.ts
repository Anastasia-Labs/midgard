import {
  buildMidgardLedgerOutputProofTrace,
  encodeMidgardLedgerOutputProofControl,
  isExactMidgardLedgerOutputProofTerminal,
  MidgardLedgerOutputProofStages,
  nextMidgardCekDataTraverseSpan,
  summarizeMidgardLedgerOutputCardanoSpendDatum,
  summarizeMidgardLedgerOutputSpendDatumOf,
} from "@al-ft/midgard-core";
import {
  decodeMidgardDatum,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { Constr } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { summarizeMidgardCekData } from "../src/cek-context.midgard-cek-context-control.js";
import { plutusDataToCborHex } from "../src/plutus-data.js";
import { plutusDataFromCborIterative } from "../src/plutus-data-iterative.decode.js";
import { ledgerOutputProofWitnessData } from "../src/validation-machine-data.ledger-output-proof-witness-data.js";

// Ledger output proofs of outputs holding an empty field. Each row's witness
// list hash and terminal control hash are pinned byte-for-byte in
// `onchain/aiken/lib/midgard/ledger-output-proof-datum.test.ak` as well, where
// the on-chain step replays the same witnesses to the same terminal control.
// Every output fits one chunk, so its chunk and span-attach witnesses are
// written as bare constructors there and rebuilt from the single chunk.

const address = Buffer.concat([Buffer.from([0x78]), Buffer.alloc(28, 0x11)]);

const output = ({
  datumHex,
  script,
  assets,
}: {
  readonly datumHex: string;
  readonly script?: Buffer;
  readonly assets?: Map<string, Map<string, bigint>>;
}): Buffer =>
  encodeMidgardTxOutput({
    address,
    value: { lovelace: 2_000_000n, assets: assets ?? new Map() },
    datum: decodeMidgardDatum(Buffer.from(datumHex, "hex")),
    ...(script === undefined
      ? {}
      : { script_ref: { language: "PlutusV3" as const, scriptBytes: script } }),
  });

const hash = (bytes: Uint8Array): string =>
  Buffer.from(blake2b(bytes, { dkLen: 32 })).toString("hex");

const witnessListCbor = (
  trace: ReturnType<typeof buildMidgardLedgerOutputProofTrace>,
): Buffer => {
  const witnesses = trace.steps.map(({ witness }) => {
    if (
      witness !== null &&
      (witness.kind === "chunks" || witness.kind === "spanAttach")
    ) {
      expect(witness.chunkProof.chunkIndex).toBe(0);
      expect(witness.nextChunkProof).toBeNull();
      return new Constr(witness.kind === "chunks" ? 1 : 5, []);
    }
    return ledgerOutputProofWitnessData(witness);
  });
  return Buffer.from(plutusDataToCborHex(witnesses), "hex");
};

const emptySpanSteps = (
  trace: ReturnType<typeof buildMidgardLedgerOutputProofTrace>,
): number =>
  trace.steps.filter(
    ({ control }) =>
      control.stage === MidgardLedgerOutputProofStages.DatumTraversal &&
      control.datum !== null &&
      nextMidgardCekDataTraverseSpan(control.datum)?.length === 0,
  ).length;

describe("ledger output proofs of outputs holding an empty field", () => {
  it.each([
    {
      label: "an empty byte string datum ending the output",
      cbor: output({ datumHex: "40" }),
      steps: 30,
      emptySteps: 1,
      witnesses:
        "8ec7e69b3ccc24ae8e1c0f7609cd07a0c6188f4cafcc9abbb2e7b4f34cec5586",
      terminal:
        "9b9e60d6da19d5dc8cca51cb2aa0512b5e019cb3789087c458b243faa157a37f",
    },
    {
      label: "a list holding an empty byte string",
      cbor: output({ datumHex: "9f40ff" }),
      steps: 34,
      emptySteps: 1,
      witnesses:
        "a3adbe1daaefc32482253e35f9a2f2cf1344525cc7af3cc37e9a2fdd0f40be3b",
      terminal:
        "9f22dd78891a1e99a7eb9d65632a90db25ff6fc0d47d15691de984cd8baf311f",
    },
    {
      label: "a list starting with an empty byte string",
      cbor: output({ datumHex: "9f4001ff" }),
      steps: 54,
      emptySteps: 1,
      witnesses:
        "1d7d0837c2643977fb81c14fe6236d2e320d256f473bf2f39d120d130f28950b",
      terminal:
        "ea04b9559d06e85d313d0d065117cc6aacbbbd98e070359df8e9fbb9f31c8f61",
    },
    {
      label: "a constructor holding an empty byte string",
      cbor: output({ datumHex: "d8799f40ff" }),
      steps: 34,
      emptySteps: 1,
      witnesses:
        "65c0e56cf73db6cc5c7c0f64aefc3588b29874c6a74558f4c4aff986cad76a4c",
      terminal:
        "77b28cc72668a185fa2277aebdae336a00f90b239891af697c6eec7b51c25f33",
    },
    {
      label: "a constructor starting with an empty byte string",
      cbor: output({ datumHex: "d8799f4001ff" }),
      steps: 54,
      emptySteps: 1,
      witnesses:
        "bea77d37825b28cd24c71cf13bfbbf164b61328b14ce7c6da6e584bbadfb6e5f",
      terminal:
        "9e865907b8a644d4520b6b6b66c15ba8c6cabce3dc8d5d1f6037aa521fe14fff",
    },
    {
      label: "a map with an empty byte string key",
      cbor: output({ datumHex: "a14000" }),
      steps: 52,
      emptySteps: 1,
      witnesses:
        "73a578ec81a5f213c7ae7eab2a93cf14ce44e99af083ab08730c806df5c853f3",
      terminal:
        "dd557e32210bd0ad747fc4bcea3559c41baac486c98a2fddb3de57cfcf4afa58",
    },
    {
      label: "a map with an empty byte string value",
      cbor: output({ datumHex: "a10040" }),
      steps: 52,
      emptySteps: 1,
      witnesses:
        "4215dc224cea39d9395b06479b3274ee6c0c272714cede23dbe0267099e63792",
      terminal:
        "4be44e4ab7c360d596fad6fda318ad08a40eac85d13011e034f9d8d81c43f373",
    },
    {
      label: "an empty byte string datum before a reference script",
      cbor: output({ datumHex: "40", script: Buffer.alloc(10, 0x6b) }),
      steps: 49,
      emptySteps: 1,
      witnesses:
        "02d9097578a569235cc9cf6d253eb558234a8e3308fc557fcc47d0365b9b93fb",
      terminal:
        "a233872d165bdefe6a4f5d0b32671ec758e4378900fa614101540c84c0867e07",
    },
    {
      label: "an empty asset name and an empty byte string datum",
      cbor: output({
        datumHex: "40",
        assets: new Map([["55".repeat(28), new Map([["", 1n]])]]),
      }),
      steps: 33,
      emptySteps: 1,
      witnesses:
        "38d26f3e0c278baf8ac930fc85dcc02aec5ab4022dd4f3fb385990cc9333d8a4",
      terminal:
        "b41d1323a9fc4c1775d9c3ac400fb4af1615e339bb415bcf0642ed458771588d",
    },
    {
      label: "an empty reference script",
      cbor: output({ datumHex: "00", script: Buffer.alloc(0) }),
      steps: 47,
      emptySteps: 0,
      witnesses:
        "f94cbc996375d18844219e49b747bcdf794147d536810931e121bda50f9c3c40",
      terminal:
        "bb248dd4fad390dc849958ddbd2ca8503274b3f4356f6857f17fc09d4376df36",
    },
    {
      label: "a one-byte byte string datum",
      cbor: output({ datumHex: "4101" }),
      steps: 30,
      emptySteps: 0,
      witnesses:
        "be1d3b4ae788dd5a68b99b4435b7ff461a5e729687b3a0dd6a10c809a96d5a9c",
      terminal:
        "3520b30ab186c1614adc9456d20e57251a3cc24f73f1d10665f37f56f0fa5a6a",
    },
  ])("proves $label", ({ cbor, steps, emptySteps, witnesses, terminal }) => {
    const trace = buildMidgardLedgerOutputProofTrace({
      outputIndex: 0,
      outputCbor: cbor,
    });
    expect(trace.steps).toHaveLength(steps);
    expect(emptySpanSteps(trace)).toBe(emptySteps);
    expect(isExactMidgardLedgerOutputProofTerminal(trace.terminal)).toBe(true);
    expect(hash(witnessListCbor(trace))).toBe(witnesses);
    expect(hash(encodeMidgardLedgerOutputProofControl(trace.terminal))).toBe(
      terminal,
    );
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
      outputCbor: output({ datumHex }),
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
