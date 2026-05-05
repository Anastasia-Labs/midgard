import { describe, expect, it } from "vitest";
import {
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
  decodeMidgardValue,
  decodeMidgardVersionedScript,
  encodeMidgardAddressText,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  midgardAddressFromText,
  protectMidgardAddress,
  type MidgardTxOutput,
} from "../src/codec/index.js";
import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborMapRaw,
  encodeCborUnsigned,
} from "../src/codec/cbor.js";

const unprotectedAddress =
  "addr1q9ynxme7c0tcmmvgk2tjuv63aw7zk9tk6yqkaqd48ulhkyl5f6v47dp5rc7286z5f57339d0c79khw4y3lwxzm8ywkzs02spk6";

const sampleOutput = (): MidgardTxOutput => ({
  address: midgardAddressFromText(unprotectedAddress),
  value: {
    lovelace: 2_000_000n,
    assets: new Map([
      [
        "00".repeat(28),
        new Map([
          ["", 1n],
          ["746f6b656e", 7n],
        ]),
      ],
    ]),
  },
  datum: {
    kind: "inline",
    cbor: Buffer.from("01", "hex"),
  },
  script_ref: {
    language: "MidgardV1",
    scriptBytes: Buffer.from("010203", "hex"),
  },
});

describe("Midgard output codec", () => {
  it("derives protected state from the address header bit and Bech32 text", () => {
    const protectedAddress = protectMidgardAddress(
      midgardAddressFromText(unprotectedAddress),
    );
    const protectedText = encodeMidgardAddressText(protectedAddress);

    expect(protectedText).toBe(
      "addr1s9ynxme7c0tcmmvgk2tjuv63aw7zk9tk6yqkaqd48ulhkyl5f6v47dp5rc7286z5f57339d0c79khw4y3lwxzm8ywkzsvf8xs7",
    );
    expect(decodeMidgardAddressBytes(midgardAddressFromText(protectedText))).toMatchObject({
      protected: true,
      networkId: 1,
    });
  });

  it("round trips output CBOR byte-exactly", () => {
    const encoded = encodeMidgardTxOutput(sampleOutput());
    const decoded = decodeMidgardTxOutput(encoded);

    expect(encodeMidgardTxOutput(decoded)).toEqual(encoded);
  });

  it("recovers MidgardV1 script-ref version from bytes", () => {
    const encoded = encodeMidgardVersionedScript({
      language: "MidgardV1",
      scriptBytes: Buffer.from("4d494447415244", "hex"),
    });

    expect(decodeMidgardVersionedScript(encoded)).toEqual({
      language: "MidgardV1",
      scriptBytes: Buffer.from("4d494447415244", "hex"),
    });
  });

  it("uses the Cardano PlutusV3 script-ref tag for PlutusV3 payloads", () => {
    const encoded = encodeMidgardVersionedScript({
      language: "PlutusV3",
      scriptBytes: Buffer.from("010203", "hex"),
    });

    expect(encoded.subarray(0, 2).toString("hex")).toBe("8203");
    expect(decodeMidgardVersionedScript(encoded).language).toBe("PlutusV3");
  });

  it("rejects non-canonical value policy ordering", () => {
    const assetMap = (quantity: bigint) =>
      encodeCborMapRaw([[encodeCborBytes(Buffer.alloc(0)), encodeCborUnsigned(quantity)]]);
    const nonCanonical = encodeCborArrayRaw([
      encodeCborUnsigned(0n),
      encodeCborMapRaw([
        [encodeCborBytes(Buffer.alloc(28, 2)), assetMap(1n)],
        [encodeCborBytes(Buffer.alloc(28, 1)), assetMap(1n)],
      ]),
    ]);

    expect(() => decodeMidgardValue(nonCanonical)).toThrow(
      /Value policies must be sorted/,
    );
  });

  it("rejects non-canonical value encoding on decode-to-encode mismatch", () => {
    const nonMinimalCoin = Buffer.from([0x82, 0x18, 0x01, 0xa0]);

    expect(() => decodeMidgardValue(nonMinimalCoin)).toThrow();
  });
});
