import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import {
  decodeStoredUtxo,
  encodeStoredUtxo,
  formatUtxosResult,
  parseAddressArgument,
  requireByOutRefsSelector,
  parseTxOutRefCborHex,
  parseTxOutRefLabel,
  parseTxOutRefsRequest,
  orderStoredUtxosByOutRef,
  sumAssets,
} from "@/commands/utxos.js";
import { makeMidgardTxOutput } from "./midgard-output-helpers.js";
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
    const output = makeMidgardTxOutput(
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

  it("parses and canonicalizes txOutRef CBOR hex", () => {
    const txHash = CML.TransactionHash.from_hex("22".repeat(32));
    const outRef = CML.TransactionInput.new(txHash, 3n);
    expect(
      parseTxOutRefCborHex(
        ` ${Buffer.from(outRef.to_cbor_bytes()).toString("hex").toUpperCase()} `,
      ),
    ).toEqual(Buffer.from(outRef.to_cbor_bytes()));
  });

  it("rejects malformed txOutRef CBOR hex", () => {
    expect(() => parseTxOutRefCborHex("zz")).toThrow(
      "txOutRef must be an even-length hex string.",
    );
    expect(() => parseTxOutRefCborHex("80")).toThrow(
      "Invalid txOutRef: failed to decode TxOutRef CBOR",
    );
  });

  it("parses textual txOutRefs in txHash#outputIndex form", () => {
    const expected = Buffer.from(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex("33".repeat(32)),
        7n,
      ).to_cbor_bytes(),
    );
    expect(parseTxOutRefLabel(` ${"33".repeat(32)}#7 `)).toEqual(expected);
  });

  it("rejects malformed textual txOutRefs", () => {
    expect(() => parseTxOutRefLabel("not-an-outref")).toThrow(
      "txOutRef must use the format <txHash>#<outputIndex>.",
    );
    expect(() => parseTxOutRefLabel(`${"11".repeat(31)}#0`)).toThrow(
      "txOutRef.txHash must be a 32-byte hex string.",
    );
    expect(() => parseTxOutRefLabel(`${"11".repeat(32)}#-1`)).toThrow(
      "txOutRef.outputIndex must be a non-negative integer.",
    );
  });

  it("parses POST /utxos txOutRef lists and rejects duplicates", () => {
    const first = `${"33".repeat(32)}#0`;
    const second = `${"44".repeat(32)}#1`;

    const parsed = parseTxOutRefsRequest([first, second]);
    expect(parsed).toStrictEqual([
      Buffer.from(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex("33".repeat(32)),
          0n,
        ).to_cbor_bytes(),
      ),
      Buffer.from(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex("44".repeat(32)),
          1n,
        ).to_cbor_bytes(),
      ),
    ]);

    expect(() => parseTxOutRefsRequest([first, first])).toThrow(
      "Duplicate txOutRef provided at txOutRefs[1].",
    );
  });

  it("requires the by-outrefs query selector for batch lookup", () => {
    expect(() => requireByOutRefsSelector({})).toThrow(
      "POST /utxos requires the `?by-outrefs` query selector.",
    );
    expect(() => requireByOutRefsSelector({ "by-outrefs": "" })).not.toThrow();
  });

  it("orders stored UTxOs by the requested outref list", () => {
    const first = Buffer.from(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex("55".repeat(32)),
        0n,
      ).to_cbor_bytes(),
    );
    const second = Buffer.from(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex("66".repeat(32)),
        1n,
      ).to_cbor_bytes(),
    );

    const ordered = orderStoredUtxosByOutRef(
      [second, first],
      [
        { outref: first, output: Buffer.from("aa", "hex") },
        { outref: second, output: Buffer.from("bb", "hex") },
      ],
    );

    expect(ordered).toStrictEqual([
      { outref: second, output: Buffer.from("bb", "hex") },
      { outref: first, output: Buffer.from("aa", "hex") },
    ]);
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

  it("encodes stored UTxOs for the HTTP API", () => {
    expect(
      encodeStoredUtxo({
        outref: Buffer.from("abcd", "hex"),
        output: Buffer.from("0123", "hex"),
      }),
    ).toEqual({
      outref: "abcd",
      value: "0123",
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
