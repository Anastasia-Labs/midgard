import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { DaGossipTopic } from "@al-ft/midgard-core/da-transport";
import { blake2b } from "@noble/hashes/blake2.js";
import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";

import { ingestDaConflictEvidence } from "../src/committee-service.js";
import { validateDaSignatureRecord } from "../src/peer/signatures.js";
import { loadDaSigner, verifyDaSignatureWitness } from "../src/signer.js";
import {
  availabilityCommitment,
  conflictFixture,
  conflictTopic,
  DEPLOYMENT_FINGERPRINT,
  LOWER_HEADER_HASH,
  REPORTER_PEER_ID,
  signatureRecord,
} from "./conflict-evidence.conflict-fixture.js";

/**
 * A failure inside libsodium is a fault of this process, not a verdict on an
 * attestation witness: every committee check raises it, and none reports the
 * witness as invalid because of it.
 */
const injected = vi.hoisted(() => ({ verifyFault: false }));

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (await importOriginal<{ default: object }>()).default;
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "crypto_sign_verify_detached" && injected.verifyFault
            ? () => {
                throw new Error("injected libsodium fault");
              }
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

const commitment = availabilityCommitment(LOWER_HEADER_HASH, "99".repeat(28));

const honestRecord = async () => {
  const signer = await loadDaSigner(`hex:${"00".repeat(31)}01`);
  const committeeSignersHash = Buffer.from(
    blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
  ).toString("hex");
  return {
    publicKeyHex: signer.publicKeyHex,
    signerValidation: {
      committeeKeys: [signer.publicKeyHex],
      committeeSignersHash,
      threshold: 1,
    },
    record: signatureRecord({
      signer,
      commitment,
      payloadHash: "cc".repeat(32),
      committeeSignersHash,
    }),
  };
};

beforeAll(() => ledgerEd25519SignatureReady);

afterEach(() => {
  injected.verifyFault = false;
});

describe("DA attestation witness verification faults", () => {
  it("raises the fault from the witness check and the signature-record check", async () => {
    const { publicKeyHex, signerValidation, record } = await honestRecord();
    const witness = {
      publicKeyHex,
      availabilityCommitment: commitment.commitment,
      witnessHex: record.signatureWitness,
    };
    const recordCheck = () =>
      validateDaSignatureRecord({
        body: record,
        headerHash: commitment.commitment.header_hash,
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        signerValidation,
      });

    injected.verifyFault = true;
    expect(() => verifyDaSignatureWitness(witness)).toThrow(
      "injected libsodium fault",
    );
    expect(recordCheck).toThrow("injected libsodium fault");

    injected.verifyFault = false;
    expect(verifyDaSignatureWitness(witness)).toBe(true);
    expect(recordCheck()).toBeUndefined();
  });

  it("raises the verifier's local fault with its cause, and still refuses a wrong witness", async () => {
    const { publicKeyHex, record } = await honestRecord();
    const witness = {
      publicKeyHex,
      availabilityCommitment: commitment.commitment,
      witnessHex: record.signatureWitness,
    };
    injected.verifyFault = true;
    const raised = (() => {
      try {
        verifyDaSignatureWitness(witness);
      } catch (error) {
        return error as Error;
      }
      throw new Error("the witness check returned under a fault");
    })();
    expect(raised.name).toBe("LedgerSignatureVerifierUnavailableError");
    expect(raised.message).toContain("injected libsodium fault");
    expect((raised.cause as Error).message).toBe("injected libsodium fault");

    injected.verifyFault = false;
    const flipped = Buffer.from(witness.witnessHex, "hex");
    flipped[flipped.length - 1] ^= 0x01;
    expect(
      verifyDaSignatureWitness({
        ...witness,
        witnessHex: flipped.toString("hex"),
      }),
    ).toBe(false);
  });

  it("raises the fault from conflict-evidence intake and stores nothing", async () => {
    const fixture = await conflictFixture();
    const saveDaConflictEvidence = vi.fn(() => Promise.resolve(true));
    const ingest = () =>
      ingestDaConflictEvidence({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        registry: fixture.registry,
        store: { saveDaConflictEvidence },
        context: {
          topicId: conflictTopic(),
          topicName: DaGossipTopic.conflicts,
          data: fixture.encoded,
          remotePeerId: REPORTER_PEER_ID,
        },
        receivedAt: new Date("2026-07-27T00:00:00.000Z"),
      });

    injected.verifyFault = true;
    await expect(ingest()).rejects.toThrow("injected libsodium fault");
    expect(saveDaConflictEvidence).not.toHaveBeenCalled();

    injected.verifyFault = false;
    await expect(ingest()).resolves.toBe(true);
    expect(saveDaConflictEvidence).toHaveBeenCalledTimes(1);
  });
});
