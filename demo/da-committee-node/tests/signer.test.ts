import { CML, walletFromSeed } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  daAttestationMessage,
  loadDaSigner,
  signDaAttestation,
  validateDaSignerMembership,
  verifyDaSignatureWitness,
} from "../src/signer.js";
import { bytesToHex } from "../src/utils/hex.js";

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
    const witness = signDaAttestation({
      signer,
      signerIndex: validation.signerIndex,
      headerHash,
    });
    expect(witness).toBe(
      "00bc603b32eaff4cf125a120de253d11ff77f8d1c3c4166956afe8be3a1231b496a42a653d7c8b90f2585a809a9dfaff3aba0c03ae5a3f78ca72f8cb63c3623908",
    );
    expect(
      verifyDaSignatureWitness({
        publicKeyHex: signer.publicKeyHex,
        headerHash,
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
    const witness = signDaAttestation({
      signer,
      signerIndex: 0,
      headerHash,
    });

    expect(signer.publicKeyHex).toBe(
      Buffer.from(cardanoPrivateKey.to_public().to_raw_bytes()).toString("hex"),
    );
    expect(witness.slice(2)).toBe(
      cardanoPrivateKey.sign(daAttestationMessage(headerHash)).to_hex(),
    );
  });
});
