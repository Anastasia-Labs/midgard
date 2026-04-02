import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import {
  decodeStoredUtxo,
  formatUtxosResult,
  parseAddressArgument,
  sumAssets,
} from "@/commands/utxos.js";
import { Effect } from "effect";

const VALID_ADDRESS =
  "addr_test1qzyem8ex0v9v76q0u52x3t2xmj5rkhjd9rsd44kx3klsut4qga2669x30zsng46mhfrrk4ngylfnnlda7rkfvxq5fywqvurkrs";

describe("utxos command helpers", () => {
  it("normalizes a valid payment address", () => {
    expect(parseAddressArgument(` ${VALID_ADDRESS} `)).toEqual(VALID_ADDRESS);
  });

  it("rejects stake-only addresses", () => {
    expect(() =>
      parseAddressArgument(
        "stake_test1uqq7uu2uyy9drut7667w56x34n43kczafhvn8qe9nzcmc7q7unt22",
      ),
    ).toThrow("Address must include a payment credential.");
  });

  it("decodes stored UTxOs from ledger records", async () => {
    const txHash = CML.TransactionHash.from_hex("11".repeat(32));
    const outRef = CML.TransactionInput.new(txHash, 0n);
    const output = CML.TransactionOutput.new(
      CML.Address.from_bech32(VALID_ADDRESS),
      CML.Value.from_coin(1_500_000n),
    );

    const utxo = await Effect.runPromise(
      decodeStoredUtxo({
        outref: Buffer.from(outRef.to_cbor_bytes()),
        output: Buffer.from(output.to_cbor_bytes()),
      }),
    );

    expect(utxo.txHash).toEqual("11".repeat(32));
    expect(utxo.outputIndex).toEqual(0);
    expect(utxo.assets.lovelace).toEqual(1_500_000n);
    expect(utxo.address).toEqual(VALID_ADDRESS);
  });

  it("sums assets across UTxOs", () => {
    expect(
      sumAssets([
        {
          txHash: "11".repeat(32),
          outputIndex: 0,
          address: VALID_ADDRESS,
          assets: {
            lovelace: 1_500_000n,
            [`${"aa".repeat(28)}${"bb".repeat(2)}`]: 7n,
          },
        },
        {
          txHash: "22".repeat(32),
          outputIndex: 1,
          address: VALID_ADDRESS,
          assets: {
            lovelace: 2_500_000n,
            [`${"aa".repeat(28)}${"bb".repeat(2)}`]: 3n,
          },
        },
      ]),
    ).toEqual({
      lovelace: 4_000_000n,
      [`${"aa".repeat(28)}${"bb".repeat(2)}`]: 10n,
    });
  });

  it("formats output with bigint amounts preserved as decimal strings", () => {
    expect(
      formatUtxosResult({
        address: VALID_ADDRESS,
        utxoCount: 1,
        totals: { lovelace: 1_500_000n },
        utxos: [
          {
            txHash: "11".repeat(32),
            outputIndex: 0,
            address: VALID_ADDRESS,
            assets: { lovelace: 1_500_000n },
          },
        ],
      }),
    ).toContain('"lovelace": "1500000"');
  });
});
