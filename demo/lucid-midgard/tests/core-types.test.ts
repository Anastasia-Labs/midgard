import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import {
  addAssets,
  assetsToCmlValue,
  authoredOutput,
  compareOutRefs,
  cmlValueToAssets,
  decodeMidgardTxOutput,
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  makeMidgardTxOutput,
  normalizeAssets,
  outputAddressProtected,
  outRefToCbor,
  outRefLabel,
  parseOutRefLabel,
  subtractAssets,
  utxoProtectedAddress,
  valueLikeToCmlValue,
  BuilderInvariantError,
  InsufficientFundsError,
} from "../src/index.js";

describe("core asset helpers", () => {
  it("normalizes away zero quantities", () => {
    expect(normalizeAssets({ lovelace: 1n, token: 0n })).toEqual({
      lovelace: 1n,
    });
  });

  it("adds and subtracts asset maps exactly", () => {
    const total = addAssets(
      { lovelace: 10n, token: 2n },
      { lovelace: 5n, token: -2n },
    );
    expect(total).toEqual({ lovelace: 15n });

    expect(subtractAssets(total, { lovelace: 4n })).toEqual({ lovelace: 11n });
  });

  it("throws structured insufficient funds errors", () => {
    try {
      subtractAssets({ lovelace: 1n }, { lovelace: 2n });
      throw new Error("expected insufficient funds error");
    } catch (error) {
      expect(error).toBeInstanceOf(InsufficientFundsError);
      expect((error as InsufficientFundsError).toJSON()).toEqual({
        name: "InsufficientFundsError",
        code: "INSUFFICIENT_FUNDS",
        message: "Insufficient lovelace: required 2, available 1",
        detail: null,
        unit: "lovelace",
        required: "2",
        available: "1",
        feeIncluded: false,
      });
    }
  });

  it("rejects negative debit quantities during subtraction", () => {
    expect(() => subtractAssets({ lovelace: 10n }, { lovelace: -5n })).toThrow(
      InsufficientFundsError,
    );
  });

  it("converts assets and CML values without losing multi-assets", () => {
    const assets = {
      lovelace: 1_500_000n,
      [`${"ab".repeat(28)}${Buffer.from("TOKEN").toString("hex")}`]: 3n,
    };

    expect(cmlValueToAssets(assetsToCmlValue(assets))).toEqual(assets);
    expect(cmlValueToAssets(valueLikeToCmlValue(1_500_000n))).toEqual({
      lovelace: 1_500_000n,
    });
  });
});

describe("outref helpers", () => {
  it("formats and parses outref labels", () => {
    const outRef = {
      txHash: "AA".repeat(32),
      outputIndex: 7,
    };

    expect(outRefLabel(outRef)).toBe(`${"aa".repeat(32)}#7`);
    expect(parseOutRefLabel(`${"aa".repeat(32)}#7`)).toEqual({
      txHash: "aa".repeat(32),
      outputIndex: 7,
    });
  });

  it("orders lexicographically by tx hash then output index", () => {
    const sorted = [
      { txHash: "22".repeat(32), outputIndex: 0 },
      { txHash: "11".repeat(32), outputIndex: 2 },
      { txHash: "11".repeat(32), outputIndex: 1 },
    ].sort(compareOutRefs);

    expect(sorted).toEqual([
      { txHash: "11".repeat(32), outputIndex: 1 },
      { txHash: "11".repeat(32), outputIndex: 2 },
      { txHash: "22".repeat(32), outputIndex: 0 },
    ]);
  });

  it("rejects non-canonical outref labels", () => {
    for (const label of [
      `${"aa".repeat(32)}#`,
      `${"aa".repeat(32)}#01`,
      `${"aa".repeat(32)}#+1`,
      `${"aa".repeat(32)}#1e2`,
      `${"aa".repeat(32)}#0x10`,
    ]) {
      expect(() => parseOutRefLabel(label)).toThrow(BuilderInvariantError);
    }
  });
});

describe("output helpers", () => {
  const address =
    "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

  it("builds ordinary and protected output encodings", () => {
    const ordinary = encodeMidgardTxOutput(address, { lovelace: 2_000_000n });
    const protectedOutput = encodeMidgardTxOutput(
      address,
      { lovelace: 2_000_000n },
      { kind: "protected" },
    );

    expect(decodeMidgardTxOutput(ordinary)).toMatchObject({
      address,
      assets: { lovelace: 2_000_000n },
    });
    expect(
      outputAddressProtected(decodeMidgardTxOutput(ordinary).address),
    ).toBe(false);
    expect(decodeMidgardTxOutput(protectedOutput)).toMatchObject({
      assets: { lovelace: 2_000_000n },
    });
    expect(
      outputAddressProtected(decodeMidgardTxOutput(protectedOutput).address),
    ).toBe(true);
  });

  it("rejects datum-hash outputs at construction time", () => {
    expect(() =>
      makeMidgardTxOutput(
        address,
        { lovelace: 2_000_000n },
        {
          datum: { kind: "hash", hash: "00".repeat(32) },
        },
      ),
    ).toThrow(BuilderInvariantError);
  });

  it("rejects invalid authored output state before encoding", () => {
    expect(() =>
      authoredOutput({
        address,
        value: { lovelace: 2_000_000n },
        datum: { kind: "hash", hash: "00".repeat(32) },
      }),
    ).toThrow(BuilderInvariantError);

    expect(() =>
      authoredOutput({
        address,
        value: { lovelace: -1n },
      }),
    ).toThrow(InsufficientFundsError);
  });

  it("rejects datum hashes when decoding output bytes", () => {
    const output = CML.ConwayFormatTxOut.new(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(2_000_000n),
    );
    output.set_datum_option(
      CML.DatumOption.new_hash(CML.DatumHash.from_hex("00".repeat(32))),
    );
    const encoded = Buffer.from(
      CML.TransactionOutput.new_conway_format_tx_out(output).to_cbor_bytes(),
    );

    expect(() => decodeMidgardTxOutput(encoded)).toThrow();
  });

  it("accepts inline datum output options", () => {
    const output = makeMidgardTxOutput(
      address,
      { lovelace: 2_000_000n },
      {
        datum: CML.PlutusData.new_integer(CML.BigInteger.from_str("7")),
      },
    );

    expect(output.datum).toBeDefined();
    expect(output.datum?.kind).toBe("inline");
  });

  it("rejects UTxO decodes whose outref CBOR disagrees with the explicit outref", () => {
    const outputCbor = encodeMidgardTxOutput(address, { lovelace: 2_000_000n });
    expect(() =>
      decodeMidgardUtxo({
        outRef: { txHash: "11".repeat(32), outputIndex: 0 },
        outRefCbor: outRefToCbor({ txHash: "22".repeat(32), outputIndex: 0 }),
        outputCbor,
      }),
    ).toThrow(BuilderInvariantError);
  });

  it("encodes MidgardV1 scriptRef metadata over PlutusV3 reference bytes", () => {
    const script = "4d".repeat(8);
    const outputCbor = encodeMidgardTxOutput({
      address,
      assets: { lovelace: 2_000_000n },
      scriptRef: { type: "MidgardV1", script },
    });

    expect(decodeMidgardTxOutput(outputCbor).txOutput.scriptRef).toEqual({
      type: "MidgardV1",
      script,
    });
  });

  it("derives protected status from CBOR when sparse UTxO metadata omits it", () => {
    const ref = { txHash: "11".repeat(32), outputIndex: 0 };
    const sparse = decodeMidgardUtxo({
      outRef: ref,
      outRefCbor: outRefToCbor(ref),
      outputCbor: encodeMidgardTxOutput(
        address,
        { lovelace: 2_000_000n },
        {
          kind: "protected",
        },
      ),
    });

    expect(utxoProtectedAddress(sparse)).toBe(true);
  });
});
