import {
  availabilityResponseGeometryV1,
  buildDaAvailabilityCommitmentV1,
  daAvailabilityAttestationMessageV1,
} from "@al-ft/midgard-sdk";
import { CML, walletFromSeed } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  loadDaSigner,
  signDaAttestation,
  validateDaSignerMembership,
  verifyDaSignatureWitness,
} from "../src/signer.js";
import { bytesToHex } from "../src/utils/hex.js";

const availabilityCommitment = (headerHash: string) =>
  buildDaAvailabilityCommitmentV1({
    deploymentIdentity: "11".repeat(28),
    headerHash,
    payload: Uint8Array.from([1, 2, 3, 4]),
    bondOwner: "22".repeat(28),
    responseGeometry: availabilityResponseGeometryV1({
      chunkByteLength: 4095,
      trancheByteLength: 4 * 1024 * 1024,
      maxTrancheCount: 16,
    }),
  });

describe("DA signer", () => {
  it("validates committee membership and produces a stable witness vector", async () => {
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
    );
    const validation = validateDaSignerMembership({
      daParams: {
        committeeHex: signer.publicKeyHex,
        committeeSignersHash,
        threshold: 1,
      },
      signer,
      signerIndex: 0,
    });
    const headerHash = "ab".repeat(28);
    const commitment = availabilityCommitment(headerHash);
    const witness = signDaAttestation({
      signer,
      signerIndex: validation.signerIndex,
      availabilityCommitment: commitment,
    });
    expect(witness).toBe(
      "00c6d7424f81c0efa71e9b0cfa501c1f7e712b56d8bc264c6f1222a6376dc7d7b3d5a74f2cb20fbd2b47dcfb65c6bc61bcd908bd9db6ca055b1c5a4c021bc16500",
    );
    expect(
      verifyDaSignatureWitness({
        publicKeyHex: signer.publicKeyHex,
        availabilityCommitment: commitment,
        witnessHex: witness,
      }),
    ).toBe(true);
  });

  it("refuses a signer key at the wrong committee index", async () => {
    const signer = await loadDaSigner(`hex:${"00".repeat(31)}01`);
    expect(() =>
      validateDaSignerMembership({
        daParams: {
          committeeHex: "ff".repeat(32),
          committeeSignersHash: bytesToHex(
            blake2b(Buffer.from("ff".repeat(32), "hex"), { dkLen: 32 }),
          ),
          threshold: 1,
        },
        signer,
        signerIndex: 0,
      }),
    ).toThrow(/does not match/);
  });

  it("loads the Midgard demo Cardano payment key from a seed phrase", async () => {
    const seedPhrase =
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
    const wallet = walletFromSeed(seedPhrase, { network: "Preview" });
    const cardanoPrivateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
    const signer = await loadDaSigner(`cardano-seed:${seedPhrase}`);
    const headerHash = "cd".repeat(28);
    const commitment = availabilityCommitment(headerHash);
    const witness = signDaAttestation({
      signer,
      signerIndex: 0,
      availabilityCommitment: commitment,
    });

    expect(signer.publicKeyHex).toBe(
      Buffer.from(cardanoPrivateKey.to_public().to_raw_bytes()).toString("hex"),
    );
    expect(witness.slice(2)).toBe(
      cardanoPrivateKey
        .sign(Buffer.from(daAvailabilityAttestationMessageV1(commitment)))
        .to_hex(),
    );
  });
});
