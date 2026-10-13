/**
 * verifySchnorrSecp256k1Signature (builtin tag 53) against the BIP-340 test
 * vectors (bitcoin/bips bip-0340/test-vectors.csv, copied verbatim into
 * fixtures/bip340-test-vectors.csv). Cardano fails the builtin, rather than
 * returning False, when the key does not parse as an x-only point, so the
 * two vectors with an unparseable key expect a failure.
 */
import { readFileSync } from "node:fs";

import { schnorr } from "@noble/curves/secp256k1.js";
import { describe, expect, it } from "vitest";

import {
  type CardanoExactValue,
  evaluateCardanoExactBuiltin,
  MIDGARD_CEK_CARDANO_EXACT_BUILTIN_TAGS,
} from "../src/cek-builtin.cardano-exact.js";

const SCHNORR_TAG = 53;

type Vector = {
  readonly index: string;
  readonly key: Uint8Array;
  readonly message: Uint8Array;
  readonly signature: Uint8Array;
  readonly result: boolean;
  readonly comment: string;
};

const fromHex = (value: string): Uint8Array =>
  Uint8Array.from(Buffer.from(value, "hex"));

const vectors: readonly Vector[] = readFileSync(
  new URL("./fixtures/bip340-test-vectors.csv", import.meta.url),
  "utf8",
)
  .trim()
  .split("\n")
  .slice(1)
  .map((line) => {
    const [index, , key, , message, signature, result, comment] =
      line.split(",");
    return {
      index: index!,
      key: fromHex(key!),
      message: fromHex(message!),
      signature: fromHex(signature!),
      result: result === "TRUE",
      comment: comment ?? "",
    };
  });

// Vectors 5 and 14: the key is not an x-only point.
const UNPARSEABLE_KEY_VECTORS = new Set(["5", "14"]);

const verify = (
  key: Uint8Array,
  message: Uint8Array,
  signature: Uint8Array,
): CardanoExactValue | "failure" =>
  evaluateCardanoExactBuiltin(SCHNORR_TAG, [
    { kind: "bytes", value: key },
    { kind: "bytes", value: message },
    { kind: "bytes", value: signature },
  ]);

describe("verifySchnorrSecp256k1Signature", () => {
  it("is in the Cardano-exact set", () => {
    expect(vectors).toHaveLength(19);
    expect(MIDGARD_CEK_CARDANO_EXACT_BUILTIN_TAGS.has(SCHNORR_TAG)).toBe(true);
  });

  it.each(vectors.map((vector) => [vector.index, vector.comment, vector]))(
    "BIP-340 vector %s %s",
    (index, _comment, vector) => {
      expect(verify(vector.key, vector.message, vector.signature)).toEqual(
        UNPARSEABLE_KEY_VECTORS.has(index)
          ? "failure"
          : { kind: "bool", value: vector.result },
      );
    },
  );

  it("fails on a key or signature of the wrong length", () => {
    const valid = vectors[1]!;
    expect(verify(valid.key.subarray(1), valid.message, valid.signature)).toBe(
      "failure",
    );
    expect(
      verify(Uint8Array.of(2, ...valid.key), valid.message, valid.signature),
    ).toBe("failure");
    expect(verify(valid.key, valid.message, valid.signature.subarray(1))).toBe(
      "failure",
    );
    expect(
      verify(valid.key, valid.message, Uint8Array.of(...valid.signature, 0)),
    ).toBe("failure");
  });

  it("agrees with noble on fresh signatures and their corruptions", () => {
    for (let round = 0; round < 16; round++) {
      const { secretKey, publicKey } = schnorr.keygen();
      const message = schnorr.utils.randomSecretKey().subarray(0, round * 3);
      const signature = schnorr.sign(message, secretKey);
      expect(verify(publicKey, message, signature)).toEqual({
        kind: "bool",
        value: true,
      });
      const corrupted = Uint8Array.from(signature);
      corrupted[round % 64]! ^= 1;
      expect(verify(publicKey, message, corrupted)).toEqual({
        kind: "bool",
        value: schnorr.verify(corrupted, message, publicKey),
      });
    }
  });
});
