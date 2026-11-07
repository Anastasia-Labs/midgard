/**
 * Which outputs at the prover's address the transition-trace carriage lookup
 * takes as a chunk's carriage when other outputs there hold the same datum.
 */
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { resolveTransitionTraceDataCarriage } from "../src/transition-trace/proof-carriage.js";

const ADDRESS = "addr_test1prover";
const FOREIGN_UNIT = `${"ab".repeat(28)}${"cd".repeat(4)}`;
const DATUMS = [Data.to("aa".repeat(4096)), Data.to("bb".repeat(100))];

const output = (
  txHashByte: string,
  outputIndex: number,
  datum: string,
  extra: Partial<UTxO> = {},
): UTxO => ({
  txHash: txHashByte.repeat(32),
  outputIndex,
  address: ADDRESS,
  assets: { lovelace: 2_000_000n },
  datum,
  datumHash: undefined,
  scriptRef: undefined,
  ...extra,
});

/** Outputs holding `datum` that are not plain carriage. */
const lookAlikes = (datum: string, txHashByte: string): UTxO[] => [
  output(txHashByte, 0, datum, {
    scriptRef: { type: "PlutusV3", script: "4e4d01000033222220051200120011" },
  }),
  output(txHashByte, 1, datum, {
    assets: { lovelace: 2_000_000n, [FOREIGN_UNIT]: 1n },
  }),
];

/**
 * A wallet at `ADDRESS` holding `held`. A carriage publication it signs adds
 * a plain output with the paid datum, listed after everything already held.
 */
const walletHolding = (held: UTxO[]) => {
  const published: string[] = [];
  const lucid = {
    wallet: () => ({ address: async () => ADDRESS }),
    utxosAt: async (address: string) =>
      address === ADDRESS ? held.map((utxo) => ({ ...utxo })) : [],
    awaitTx: async () => true,
    newTx: () => ({
      pay: {
        ToAddressWithData: (
          address: string,
          datum: { readonly value: string },
        ) => ({
          complete: async () => ({
            sign: {
              withWallet: () => ({
                complete: async () => ({
                  submit: async () => {
                    expect(address).toBe(ADDRESS);
                    published.push(datum.value);
                    held.push(output("ee", published.length - 1, datum.value));
                    return "ee".repeat(32);
                  },
                }),
              }),
            },
          }),
        }),
      },
    }),
  } as unknown as LucidEvolution;
  return { lucid, published };
};

const outRef = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex}`;

describe("transition-trace carriage lookup", () => {
  it("takes the plain carriage over look-alikes listed before it", async () => {
    const real = DATUMS.map((datum, index) => output("ff", index, datum));
    const { lucid, published } = walletHolding([
      ...lookAlikes(DATUMS[0]!, "00"),
      ...lookAlikes(DATUMS[1]!, "01"),
      ...real,
    ]);

    const carriage = await resolveTransitionTraceDataCarriage({
      lucid,
      datums: DATUMS,
    });

    expect(carriage.map(outRef)).toEqual(real.map(outRef));
    expect(published).toEqual([]);
  });

  it("refuses to proceed when only look-alikes hold a chunk and publishing is off", async () => {
    const { lucid } = walletHolding([
      output("ff", 0, DATUMS[0]!),
      ...lookAlikes(DATUMS[1]!, "00"),
    ]);

    await expect(
      resolveTransitionTraceDataCarriage({ lucid, datums: DATUMS }),
    ).rejects.toThrow("requires a journaled chunk publication");
  });

  it("publishes a chunk only look-alikes hold and takes the published output", async () => {
    const { lucid, published } = walletHolding([
      output("ff", 0, DATUMS[0]!),
      ...lookAlikes(DATUMS[1]!, "00"),
    ]);

    const carriage = await resolveTransitionTraceDataCarriage({
      lucid,
      datums: DATUMS,
      publish: true,
    });

    expect(published).toEqual([DATUMS[1]]);
    expect(carriage.map(outRef)).toEqual([
      `${"ff".repeat(32)}#0`,
      `${"ee".repeat(32)}#0`,
    ]);
  });
});
