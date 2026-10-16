import {
  buildMidgardLedgerOutputProofTrace,
  encodeMidgardLedgerOutputProofControl,
  isExactMidgardLedgerOutputProofTerminal,
  type MidgardLedgerOutputProofControl,
  MidgardLedgerOutputProofStages,
  nextMidgardCekDataTraverseSpan,
} from "@al-ft/midgard-core";
import {
  decodeMidgardDatum,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { Constr } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { plutusDataToCborHex } from "../src/plutus-data.js";
import { ledgerOutputProofWitnessData } from "../src/validation-machine-data.ledger-output-proof-witness-data.js";

// Ledger output proofs of outputs with a large chunked datum scalar, whose
// span attaches occur at the bytes and integer stages of the datum
// traversal. Each row's witness list hash and terminal control hash are
// pinned byte-for-byte in
// `onchain/aiken/lib/midgard/ledger-output-proof-datum.test.ak` as well,
// where the on-chain typed and split steps replay the same witnesses to the
// same terminal control. A chunk or span-attach witness is written there as
// its constructor over its chunk indices `[first, next or -1]` and is rebuilt
// from the output's chunk proofs; a datum witness whose window is the
// recorded window is written as constructor 9 over its action alone.

const address = Buffer.concat([Buffer.from([0x78]), Buffer.alloc(28, 0x11)]);

/** An indefinite byte string of `length` bytes `fill` in 64-byte chunks. */
const chunkedBytes = (length: number, fill: string): string => {
  const parts = ["5f"];
  for (let at = 0; at < length; at += 64) {
    const size = Math.min(64, length - at);
    const head =
      size < 24 ? (0x40 + size).toString(16) : `58${size.toString(16)}`;
    parts.push(head, fill.repeat(size));
  }
  parts.push("ff");
  return parts.join("");
};

const output = (datumHex: string): Buffer =>
  encodeMidgardTxOutput({
    address,
    value: { lovelace: 2_000_000n, assets: new Map() },
    datum: decodeMidgardDatum(Buffer.from(datumHex, "hex")),
  });

const hash = (bytes: Uint8Array): string =>
  Buffer.from(blake2b(bytes, { dkLen: 32 })).toString("hex");

type Trace = ReturnType<typeof buildMidgardLedgerOutputProofTrace>;

const witnessListCbor = (cbor: Buffer, trace: Trace): Buffer => {
  const witnesses = trace.steps.map(({ control, witness }) => {
    if (
      witness !== null &&
      (witness.kind === "chunks" || witness.kind === "spanAttach")
    ) {
      return new Constr(witness.kind === "chunks" ? 1 : 5, [
        BigInt(witness.chunkProof.chunkIndex),
        witness.nextChunkProof === null
          ? -1n
          : BigInt(witness.nextChunkProof.chunkIndex),
      ]);
    }
    if (witness?.kind === "datum" && witness.window !== null) {
      const recorded = control.spanWindow!;
      expect(Buffer.from(witness.window)).toEqual(
        cbor.subarray(recorded.start, recorded.start + recorded.length),
      );
      const data = ledgerOutputProofWitnessData(witness) as Constr<unknown>;
      return new Constr(9, [data.fields[0]]);
    }
    return ledgerOutputProofWitnessData(witness);
  });
  return Buffer.from(plutusDataToCborHex(witnesses), "hex");
};

/**
 * Each datum-stage span attach as `100 * traverse stage + 10 * (blob
 * present) + (blob hash present)`.
 */
const attachShape = (control: MidgardLedgerOutputProofControl): number => {
  const datum = control.datum!;
  const blob = datum.integer?.blob ?? datum.bytes?.blob ?? null;
  return (
    100 * datum.stage + (blob === null ? 0 : blob.activeHash === null ? 10 : 11)
  );
};

const datumAttaches = (trace: Trace): number[] =>
  trace.steps
    .filter(
      ({ control, witness }) =>
        witness?.kind === "spanAttach" &&
        control.stage === MidgardLedgerOutputProofStages.DatumTraversal,
    )
    .map(({ control }) => attachShape(control));

const emptySpanSteps = (trace: Trace): number =>
  trace.steps.filter(
    ({ control }) =>
      control.stage === MidgardLedgerOutputProofStages.DatumTraversal &&
      control.datum !== null &&
      nextMidgardCekDataTraverseSpan(control.datum)?.length === 0,
  ).length;

describe("ledger output proofs with span attaches inside a datum scalar", () => {
  it.each([
    {
      label: "a 5000-byte byte string datum",
      datumHex: chunkedBytes(5_000, "ab"),
      total: 5_204,
      steps: 674,
      emptySteps: 0,
      attaches: [0, 200, 211, 211],
      witnesses:
        "cdd59a999045dc075b9258e70cdaca8e2ebba93d6f32f17b2f2da5382bb8a7bb",
      terminal:
        "7b393ab1038641890e29681cf557d56911c4bb02771923adc1981c2931a0a91c",
    },
    {
      label: "an integer datum over a 5000-byte byte string",
      datumHex: `c2${chunkedBytes(5_000, "01")}`,
      total: 5_205,
      steps: 688,
      emptySteps: 0,
      attaches: [0, 100, 111, 111],
      witnesses:
        "d6c0031e5a93784b0c112c6dd539471f0e258f3e66735baf9549af488e064ec3",
      terminal:
        "8ece54a17e93a232608c8648f24469709b3ed7bd5021704539e3df20c0e5e33d",
    },
    {
      label: "an empty byte string ending the first chunk",
      datumHex: `9f${chunkedBytes(3_922, "ab")}40ff`,
      total: 4_095,
      steps: 536,
      emptySteps: 1,
      attaches: [0],
      witnesses:
        "c1c0f43315ff1e270d83ba331215b429b575b6313e959148069f35cad2044c67",
      terminal:
        "c72dfbb16af6d51fe13aa613bf00b84f07d37b5bafc4c187f3d5baa71d93146c",
    },
    {
      label: "an empty byte string starting the second chunk",
      datumHex: `9f${chunkedBytes(3_923, "ab")}40ff`,
      total: 4_096,
      steps: 537,
      emptySteps: 1,
      attaches: [0],
      witnesses:
        "07640a40eff3f0bbc81f6d4885f7b534688bcf2e9b8fa19be958634c843b7b73",
      terminal:
        "1dd89d1832a803037d57e8f66acea9cfa48cba05f95895701c3a63b2e09cfd46",
    },
    {
      label: "an empty byte string inside the second chunk",
      datumHex: `9f${chunkedBytes(3_924, "ab")}40ff`,
      total: 4_097,
      steps: 537,
      emptySteps: 1,
      attaches: [0],
      witnesses:
        "47a4eee66ef6b45baf754bc998d7a489fca2d56a49bce3675df589dda67702fd",
      terminal:
        "9c402b99edf79ad299ccba340fdb6ba8cff66e7116e14509f40ff18ce240d43a",
    },
  ])(
    "proves $label",
    ({ datumHex, total, steps, emptySteps, attaches, witnesses, terminal }) => {
      const cbor = output(datumHex);
      expect(cbor.length).toBe(total);
      const trace = buildMidgardLedgerOutputProofTrace({
        outputIndex: 0,
        outputCbor: cbor,
      });
      expect(trace.steps).toHaveLength(steps);
      expect(emptySpanSteps(trace)).toBe(emptySteps);
      expect(datumAttaches(trace)).toEqual(attaches);
      expect(isExactMidgardLedgerOutputProofTerminal(trace.terminal)).toBe(
        true,
      );
      expect(hash(witnessListCbor(cbor, trace))).toBe(witnesses);
      expect(hash(encodeMidgardLedgerOutputProofControl(trace.terminal))).toBe(
        terminal,
      );
    },
  );
});
