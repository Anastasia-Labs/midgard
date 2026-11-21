import { expect, it } from "vitest";

import { commitMidgardCekBlob } from "../src/cek-proof.js";
import {
  hashMidgardCekDataNode,
  midgardCekDataBytesCborLength,
  midgardCekDataBytesMemory,
} from "../src/cek-semantic.js";
import { encodeCborBytes } from "../src/codec/cbor.js";
import { encodeMidgardTxOutput } from "../src/codec/output.js";
import { aikenSerialisedPlutusDataCbor } from "../src/plutus-data-cbor.js";

const LEDGER_OUTPUT_LIMIT_BYTES = 16_384;
// The admission bound the CEK source constant used to impose on a Data byte
// payload. It is retired: a ledger inline datum may carry every byte a maximal
// output can hold, so the commitment path must still admit payloads past it.
const RETIRED_SOURCE_CONSTANT_ADMISSION_BOUND = 9_215;

const serialisedInlineDatumCbor = (payload: Buffer): Buffer =>
  Buffer.from(
    aikenSerialisedPlutusDataCbor(encodeCborBytes(payload).toString("hex")),
    "hex",
  );

const bytesNodeRootHex = (payload: Buffer): string =>
  Buffer.from(
    hashMidgardCekDataNode({
      kind: "bytes",
      bytesRoot: commitMidgardCekBlob(payload).root,
      bytesLength: BigInt(payload.length),
      cborLength: midgardCekDataBytesCborLength(BigInt(payload.length)),
      memory: midgardCekDataBytesMemory(BigInt(payload.length)),
    }),
  ).toString("hex");

it("commits ledger Data bytes beyond the source-constant admission bound", () => {
  let maximum = 15000;
  const output = (length: number) =>
    encodeMidgardTxOutput({
      address: Buffer.from("60" + "aa".repeat(28), "hex"),
      value: { lovelace: 2_000_000n, assets: new Map() },
      datum: {
        kind: "inline",
        cbor: serialisedInlineDatumCbor(Buffer.alloc(length, 0xab)),
      },
    });
  while (output(maximum + 1).length <= LEDGER_OUTPUT_LIMIT_BYTES) maximum++;
  expect(output(maximum).length).toBe(LEDGER_OUTPUT_LIMIT_BYTES);
  expect(output(maximum + 1).length).toBeGreaterThan(LEDGER_OUTPUT_LIMIT_BYTES);
  expect(maximum).toBeGreaterThan(RETIRED_SOURCE_CONSTANT_ADMISSION_BOUND);

  for (const length of [
    RETIRED_SOURCE_CONSTANT_ADMISSION_BOUND,
    RETIRED_SOURCE_CONSTANT_ADMISSION_BOUND + 1,
    maximum,
  ]) {
    const payload = Buffer.alloc(length, 0xab);
    // The semantic CBOR length is checked against the length the real chunked
    // serialiser produces, not against a transcribed number, so a change to
    // either side that breaks their agreement fails here.
    expect(midgardCekDataBytesCborLength(BigInt(length))).toBe(
      BigInt(serialisedInlineDatumCbor(payload).length),
    );
    const flippedTailBit = Buffer.from(payload);
    flippedTailBit[length - 1] ^= 0x01;
    const root = bytesNodeRootHex(payload);
    expect(root).toHaveLength(64);
    // A committed payload past the retired bound must still bind its content
    // and its length: neither a one-bit edit at the tail nor a one-byte
    // truncation may reach the same node root.
    expect(bytesNodeRootHex(flippedTailBit)).not.toBe(root);
    expect(bytesNodeRootHex(payload.subarray(0, length - 1))).not.toBe(root);
  }
});
