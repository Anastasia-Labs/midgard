import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { daAvailabilityAttestationMessage } from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";
import { beforeAll, describe, expect, it } from "vitest";

import {
  ledgerEd25519Verdict,
  namedEd25519Vectors,
  ownKeyIdentityRSignature,
  signerPublicKey,
} from "../../midgard-validation/tests/ed25519-ledger-vectors.js";
import { validateDaSignatureRecord } from "../src/peer/signatures.js";
import { loadDaSigner, verifyDaSignatureWitness } from "../src/signer.js";
import {
  availabilityCommitment,
  DEPLOYMENT_FINGERPRINT,
  LOWER_HEADER_HASH,
  signatureRecord,
} from "./conflict-evidence.conflict-fixture.js";

/**
 * The committee accepts a DA attestation witness exactly when the on-chain
 * attestation check does: `verify_ed25519_signature` over the attestation
 * message, the committee key and the 64 bytes after the signer index
 * (onchain/aiken/validators/da-attestation.ak).
 */
const SEED = Buffer.from(`${"00".repeat(31)}01`, "hex");
const commitment = availabilityCommitment(LOWER_HEADER_HASH, "99".repeat(28));
const message = Buffer.from(
  daAvailabilityAttestationMessage(commitment.commitment),
);
const publicKeyHex = signerPublicKey(SEED).toString("hex");
const committeeSignersHash = Buffer.from(
  blake2b(Buffer.from(publicKeyHex, "hex"), { dkLen: 32 }),
).toString("hex");
const signerValidation = {
  committeeKeys: [publicKeyHex],
  committeeSignersHash,
  threshold: 1,
};

const witnessHex = (signature: Buffer): string =>
  `00${signature.toString("hex")}`;

const recordWith = async (witness?: string) => {
  const signer = await loadDaSigner(`hex:${SEED.toString("hex")}`);
  expect(signer.publicKeyHex).toBe(publicKeyHex);
  const record = signatureRecord({
    signer,
    commitment,
    payloadHash: "cc".repeat(32),
    committeeSignersHash,
  });
  return witness === undefined
    ? record
    : { ...record, signatureWitness: witness };
};

const recordVerdict = (body: Awaited<ReturnType<typeof recordWith>>) =>
  validateDaSignatureRecord({
    body,
    headerHash: commitment.commitment.header_hash,
    deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
    signerValidation,
  });

beforeAll(() => ledgerEd25519SignatureReady);

describe("DA attestation witness verification", () => {
  it("accepts the committee member's own witness", async () => {
    const record = await recordWith();
    expect(
      verifyDaSignatureWitness({
        publicKeyHex,
        availabilityCommitment: commitment.commitment,
        witnessHex: record.signatureWitness,
      }),
    ).toBe(true);
    expect(recordVerdict(record)).toBeUndefined();
  });

  it("refuses the member's own key with R the identity, as the ledger does", async () => {
    const signature = ownKeyIdentityRSignature(SEED, message);
    const vkey = Buffer.from(publicKeyHex, "hex");
    expect(
      ledgerEd25519Verdict({ label: "identity R", vkey, message, signature }),
    ).toBe(false);

    expect(
      verifyDaSignatureWitness({
        publicKeyHex,
        availabilityCommitment: commitment.commitment,
        witnessHex: witnessHex(signature),
      }),
    ).toBe(false);
    expect(
      recordVerdict(await recordWith(witnessHex(signature))),
    ).toBeDefined();
  });

  it("answers as the ledger does on every named vector over the attestation message", () => {
    const vectors = namedEd25519Vectors(message).filter(
      ({ vkey, signature }) => vkey.length === 32 && signature.length === 64,
    );
    expect(vectors.length).toBeGreaterThan(100);
    const disagreements = vectors.filter(
      (vector) =>
        verifyDaSignatureWitness({
          publicKeyHex: vector.vkey.toString("hex"),
          availabilityCommitment: commitment.commitment,
          witnessHex: witnessHex(vector.signature),
        }) !== ledgerEd25519Verdict(vector),
    );
    expect(disagreements.map(({ label }) => label)).toEqual([]);
    expect(
      vectors.some((vector) => ledgerEd25519Verdict(vector)) &&
        vectors.some((vector) => !ledgerEd25519Verdict(vector)),
    ).toBe(true);
  });
});
