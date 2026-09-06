import { expect, it } from "vitest";

import { commitMidgardCekBlob } from "../src/cek-proof.js";
import {
  hashMidgardCekDataNode,
  midgardCekDataBytesCborLength,
} from "../src/cek-semantic.js";
import { encodeCborBytes } from "../src/codec/cbor.js";
import { encodeMidgardTxOutput } from "../src/codec/output.js";
import { aikenSerialisedPlutusDataCbor } from "../src/plutus-data-cbor.js";
it("commits ledger Data bytes beyond the source-constant admission bound", () => {
  let maximum = 15000;
  const output = (length: number) =>
    encodeMidgardTxOutput({
      address: Buffer.from("60" + "aa".repeat(28), "hex"),
      value: { lovelace: 2_000_000n, assets: new Map() },
      datum: {
        kind: "inline",
        cbor: Buffer.from(
          aikenSerialisedPlutusDataCbor(
            encodeCborBytes(Buffer.alloc(length, 0xab)).toString("hex"),
          ),
          "hex",
        ),
      },
    });
  while (output(maximum + 1).length <= 16384) maximum++;
  expect(maximum).toBe(15841);
  expect(output(maximum).length).toBe(16384);
  expect(output(maximum + 1).length).toBeGreaterThan(16384);
  const rows = [9215, 9216, 12000, maximum, 16384].map((length) => {
    const bytes = Buffer.alloc(length, 0xab);
    const cborLength = midgardCekDataBytesCborLength(BigInt(length));
    const root = Buffer.from(
      hashMidgardCekDataNode({
        kind: "bytes",
        bytesRoot: commitMidgardCekBlob(bytes).root,
        bytesLength: BigInt(length),
        cborLength,
        memory: BigInt(length + 4),
      }),
    ).toString("hex");
    return { length, root, cborLength: Number(cborLength), memory: length + 4 };
  });
  expect(rows).toEqual([
    {
      length: 9215,
      root: "c2efac0c7d78bca123484c80799200772859a97f54965d59881f06c434892988",
      cborLength: 9505,
      memory: 9219,
    },
    {
      length: 9216,
      root: "61705d68831234591650ebd3281d2eaf55819dfc489938644dfeaade1b4658d2",
      cborLength: 9506,
      memory: 9220,
    },
    {
      length: 12000,
      root: "ff21a0015d198c6ca7497145d7091c3c1f82c6189d8ac36a659338b713ae5ffd",
      cborLength: 12378,
      memory: 12004,
    },
    {
      length: 15841,
      root: "d120efaa40f0772065121c25fb7039ce03943bb1ea0c59e4e22b1b3f6b7c7c8c",
      cborLength: 16339,
      memory: 15845,
    },
    {
      length: 16384,
      root: "31a6e4176316af58f404da1d2e282e07190cd552fb6ac7f309b8ddd8b48b1f40",
      cborLength: 16898,
      memory: 16388,
    },
  ]);
});
