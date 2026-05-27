import { describe, expect, it } from "vitest";

import {
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
  decodeMidgardValue,
  decodeMidgardVersionedScript,
  encodeMidgardAddressText,
  encodeMidgardTxOutput,
  encodeMidgardValue,
  encodeMidgardVersionedScript,
  MIDGARD_PROTECTED_ADDRESS_HEADER_MASK,
  midgardAddressFromText,
  type MidgardTxOutput,
  type MidgardValue,
  protectMidgardAddress,
} from "../src/codec/index.js";

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

describe("Midgard binary output codec", () => {
  it("derives protected state from the address header bit and Bech32 text", () => {
    const unprotectedBytes = midgardAddressFromText(unprotectedAddress);
    const protectedAddress = protectMidgardAddress(unprotectedBytes);
    const protectedText = encodeMidgardAddressText(protectedAddress);

    expect(protectedText).toBe(
      "addr1p9ynxme7c0tcmmvgk2tjuv63aw7zk9tk6yqkaqd48ulhkyl5f6v47dp5rc7286z5f57339d0c79khw4y3lwxzm8ywkzsuegces",
    );
    expect(protectedAddress[0]).toBe(
      unprotectedBytes[0] | MIDGARD_PROTECTED_ADDRESS_HEADER_MASK,
    );
    expect(protectedAddress[0] & 0xf0).toBe(unprotectedBytes[0] & 0xf0);
    expect(
      decodeMidgardAddressBytes(midgardAddressFromText(protectedText)),
    ).toMatchObject({
      protected: true,
      networkId: 1,
    });
  });

  it("rejects old bit-7 protected address headers", () => {
    const oldProtectedBytes = Buffer.from(
      midgardAddressFromText(unprotectedAddress),
    );
    oldProtectedBytes[0] |= 0x80;

    expect(() => decodeMidgardAddressBytes(oldProtectedBytes)).toThrow(
      /Unsupported Midgard address family/,
    );
  });

  it("rejects reserved network-nibble bits other than the protected bit", () => {
    for (const reservedNetworkBit of [0x02, 0x04]) {
      const badAddress = Buffer.from(
        midgardAddressFromText(unprotectedAddress),
      );
      badAddress[0] |= reservedNetworkBit;

      expect(() => decodeMidgardAddressBytes(badAddress)).toThrow(
        /Unsupported Midgard address network id/,
      );
    }
  });

  it("round trips output binary byte-exactly", () => {
    const encoded = encodeMidgardTxOutput(sampleOutput());
    const decoded = decodeMidgardTxOutput(encoded);

    expect(encodeMidgardTxOutput(decoded)).toEqual(encoded);
  });

  it("round trips a coin-only value", () => {
    const value: MidgardValue = { lovelace: 12_345n, assets: new Map() };
    const encoded = encodeMidgardValue(value);
    expect(decodeMidgardValue(encoded)).toEqual(value);
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

  it("preserves the PlutusV3 script-ref tag for PlutusV3 payloads", () => {
    const encoded = encodeMidgardVersionedScript({
      language: "PlutusV3",
      scriptBytes: Buffer.from("010203", "hex"),
    });
    expect(decodeMidgardVersionedScript(encoded).language).toBe("PlutusV3");
  });
});
