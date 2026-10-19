/**
 * Which outputs at the staging address the script-decode lookup takes as a
 * script's staged bytes when other outputs at that address hold the same
 * datum.
 */
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  findStagedScriptDecodeChunks,
  scriptDecodeStagingDatums,
} from "../src/script-decode/index.js";

const ADDRESS = "addr_test1staging";
const FOREIGN_UNIT = `${"ab".repeat(28)}${"cd".repeat(4)}`;

/** Bytes that stage as two chunks. */
const scriptBytes = new Uint8Array(12_288 + 64).map((_, index) => index % 251);

const output = (
  txHashByte: string,
  outputIndex: number,
  datum: string,
  extra: Partial<UTxO> = {},
): UTxO => ({
  txHash: txHashByte.repeat(32),
  outputIndex,
  address: ADDRESS,
  assets: { lovelace: 50_000_000n },
  datum,
  datumHash: undefined,
  scriptRef: undefined,
  ...extra,
});

/** Outputs holding `datum` that are not plain staging carriage. */
const lookAlikes = (datum: string, txHashByte: string): UTxO[] => [
  output(txHashByte, 0, datum, {
    scriptRef: { type: "PlutusV3", script: "4e4d01000033222220051200120011" },
  }),
  output(txHashByte, 1, datum, {
    assets: { lovelace: 50_000_000n, [FOREIGN_UNIT]: 1n },
  }),
];

const lucidHolding = (utxos: readonly UTxO[]): LucidEvolution =>
  ({
    utxosAt: async (address: string) =>
      address === ADDRESS ? utxos.map((utxo) => ({ ...utxo })) : [],
  }) as unknown as LucidEvolution;

const outRef = (utxo: UTxO | undefined) =>
  utxo === undefined ? undefined : `${utxo.txHash}#${utxo.outputIndex}`;

describe("script-decode staged-chunk lookup", () => {
  const datums = scriptDecodeStagingDatums(scriptBytes);

  it("takes the plain staged outputs over look-alikes that sort before them", async () => {
    const real = datums.map((datum, index) => output("ff", index, datum));
    const held = [
      ...datums.flatMap((datum, index) =>
        lookAlikes(datum, index === 0 ? "00" : "01"),
      ),
      ...real,
    ];

    const staged = await findStagedScriptDecodeChunks({
      lucid: lucidHolding(held),
      scriptBytes,
      address: ADDRESS,
    });

    expect(datums).toHaveLength(2);
    expect(staged.map(outRef)).toEqual(real.map(outRef));
  });

  it("reports a chunk as missing when only look-alikes hold its datum", async () => {
    const held = [output("ff", 0, datums[0]!), ...lookAlikes(datums[1]!, "00")];

    const staged = await findStagedScriptDecodeChunks({
      lucid: lucidHolding(held),
      scriptBytes,
      address: ADDRESS,
    });

    expect(staged.map(outRef)).toEqual([`${"ff".repeat(32)}#0`, undefined]);
  });

  it("skips an output that also names a datum hash", async () => {
    const real = datums.map((datum, index) => output("ff", index, datum));
    const held = [
      output("00", 0, datums[0]!, { datumHash: "ee".repeat(32) }),
      ...real,
    ];

    const staged = await findStagedScriptDecodeChunks({
      lucid: lucidHolding(held),
      scriptBytes,
      address: ADDRESS,
    });

    expect(staged.map(outRef)).toEqual(real.map(outRef));
  });

  it("takes an output that lists another asset at quantity zero as plain staging", async () => {
    const held = [
      output("00", 0, datums[0]!, {
        assets: { lovelace: 50_000_000n, [FOREIGN_UNIT]: 0n },
      }),
      output("ff", 1, datums[1]!),
    ];

    const staged = await findStagedScriptDecodeChunks({
      lucid: lucidHolding(held),
      scriptBytes,
      address: ADDRESS,
    });

    expect(staged.map(outRef)).toEqual(held.map(outRef));
  });
});
