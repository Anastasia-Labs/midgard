import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { normalizeVKeyWitnessInput } from "../src/builder/witness-bundle.js";
import {
  assertVKeyWitness,
  makeVKeyWitness,
  parsePartialWitnessBundle,
  SigningError,
  verifyVKeyWitness,
  type VKeyWitness,
  walletFromExternalSigner,
} from "../src/index.js";

/**
 * The wallet and the witness import accept an address witness exactly when the
 * ledger's Ed25519 verifier does. Every vector below signs the same 32-byte
 * body hash and is refused by the ledger: small-order or non-canonical keys
 * with the identity R and S = 0, and an ordinary key whose R is the identity
 * encoding with S = h·a.
 */
const BODY_HASH = Buffer.from("42".repeat(32), "hex");
const IDENTITY_R = `01${"00".repeat(31)}`;
const ZERO_S = "00".repeat(32);

const LEDGER_REFUSED: readonly (readonly [string, string, string])[] = [
  [
    "own key, R identity, S h·a",
    "429ebfbf7efc75889380aabd40918cb4e9b6ad677432fab93030e03cd26231e8",
    `${IDENTITY_R}8d706bb6db79e1b174d3f16dabd92208742dc0889ffde5ff5b0b4fd1f70c050e`,
  ],
  ["key 01..00, R identity, S 0", IDENTITY_R, `${IDENTITY_R}${ZERO_S}`],
  [
    "key 01..80, R identity, S 0",
    `01${"00".repeat(30)}80`,
    `${IDENTITY_R}${ZERO_S}`,
  ],
  [
    "key 00..80, R identity, S 0",
    `${"00".repeat(31)}80`,
    `${IDENTITY_R}${ZERO_S}`,
  ],
  [
    "key ec..7f, R ec..7f, S 0",
    `ec${"ff".repeat(30)}7f`,
    `ec${"ff".repeat(30)}7f${ZERO_S}`,
  ],
  [
    "key ec..ff, R identity, S 0",
    `ec${"ff".repeat(31)}`,
    `${IDENTITY_R}${ZERO_S}`,
  ],
  [
    "key ee..7f, R identity, S 0",
    `ee${"ff".repeat(30)}7f`,
    `${IDENTITY_R}${ZERO_S}`,
  ],
  [
    "key ee..ff, R identity, S 0",
    `ee${"ff".repeat(31)}`,
    `${IDENTITY_R}${ZERO_S}`,
  ],
  [
    "key y = p+1, R identity, S 0",
    `ee${"ff".repeat(30)}7f`,
    `${IDENTITY_R}${ZERO_S}`,
  ],
  [
    "key y = p+1, sign set, R identity, S 0",
    `ee${"ff".repeat(31)}`,
    `${IDENTITY_R}${ZERO_S}`,
  ],
];

const NOT_VERIFIED = "VKey witness does not verify for body hash";

const witnessOf = (publicKey: string, signature: string): VKeyWitness =>
  CML.Vkeywitness.new(
    CML.PublicKey.from_bytes(Buffer.from(publicKey, "hex")),
    CML.Ed25519Signature.from_raw_bytes(Buffer.from(signature, "hex")),
  );

const bundleOf = (witness: VKeyWitness) => ({
  kind: "MidgardPartialWitnessBundleV1" as const,
  version: 1 as const,
  midgardNativeTxVersion: 1 as const,
  txId: BODY_HASH.toString("hex"),
  bodyHash: BODY_HASH.toString("hex"),
  witnesses: [Buffer.from(witness.to_cbor_bytes()).toString("hex")],
  signerKeyHashes: [witness.vkey().hash().to_hex()],
});

const externalWalletReturning = (witness: VKeyWitness) =>
  walletFromExternalSigner({
    keyHash: witness.vkey().hash().to_hex(),
    signBodyHash: () => witness,
  });

const honestWitness = (): VKeyWitness =>
  makeVKeyWitness(
    BODY_HASH,
    CML.PrivateKey.from_normal_bytes(
      Buffer.from(`${"00".repeat(31)}07`, "hex"),
    ),
  );

describe("ledger-exact address witness verification", () => {
  beforeAll(() => ledgerEd25519SignatureReady);

  it("accepts an honestly signed witness in the wallet and the import", async () => {
    const witness = honestWitness();
    expect(verifyVKeyWitness(BODY_HASH, witness)).toBe(true);
    expect(assertVKeyWitness(BODY_HASH, witness)).toBe(witness);
    expect(
      normalizeVKeyWitnessInput(
        Buffer.from(witness.to_cbor_bytes()).toString("hex"),
        BODY_HASH,
        "witness",
      ).to_cbor_hex(),
    ).toBe(witness.to_cbor_hex());
    expect(parsePartialWitnessBundle(bundleOf(witness))).toEqual(
      bundleOf(witness),
    );
    await expect(
      externalWalletReturning(witness).signBodyHash(BODY_HASH),
    ).resolves.toBe(witness);
  });

  it.each(LEDGER_REFUSED)(
    "the wallet refuses %s",
    (_label, publicKey, signature) => {
      const witness = witnessOf(publicKey, signature);
      expect(verifyVKeyWitness(BODY_HASH, witness)).toBe(false);
      expect(() => assertVKeyWitness(BODY_HASH, witness)).toThrow(
        new SigningError(NOT_VERIFIED),
      );
    },
  );

  it.each(LEDGER_REFUSED)(
    "the witness import refuses %s",
    (_label, publicKey, signature) => {
      const witness = witnessOf(publicKey, signature);
      expect(() =>
        normalizeVKeyWitnessInput(
          Buffer.from(witness.to_cbor_bytes()).toString("hex"),
          BODY_HASH,
          "witness",
        ),
      ).toThrow(new SigningError(NOT_VERIFIED));
      expect(() => parsePartialWitnessBundle(bundleOf(witness))).toThrow(
        new SigningError(NOT_VERIFIED),
      );
    },
  );

  it("an external signer's own-key witness with R the identity and S = h·a is rejected", async () => {
    const [, publicKey, signature] = LEDGER_REFUSED[0]!;
    const witness = witnessOf(publicKey, signature);
    await expect(
      externalWalletReturning(witness).signBodyHash(BODY_HASH),
    ).rejects.toThrow(new SigningError(NOT_VERIFIED));
  });
});
