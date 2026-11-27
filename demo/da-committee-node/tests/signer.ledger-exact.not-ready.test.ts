import { DaGossipTopic } from "@al-ft/midgard-core/da-transport";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it, vi } from "vitest";

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
 * Until libsodium has initialised there is no Ed25519 answer: the committee's
 * witness checks raise, and none reports a witness as invalid. libsodium here
 * never initialises, and nothing in this file waits for it.
 */
vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (await importOriginal<{ default: object }>()).default;
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "ready"
            ? new Promise<never>(() => undefined)
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

const commitment = availabilityCommitment(LOWER_HEADER_HASH, "99".repeat(28));

describe("DA attestation witness verification before libsodium initialises", () => {
  it("raises from the witness check and the signature-record check", async () => {
    const signer = await loadDaSigner(`hex:${"00".repeat(31)}01`);
    const committeeSignersHash = Buffer.from(
      blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
    ).toString("hex");
    const record = signatureRecord({
      signer,
      commitment,
      payloadHash: "cc".repeat(32),
      committeeSignersHash,
    });
    const brokenWitness = `00${"01".repeat(64)}`;

    for (const witnessHex of [record.signatureWitness, brokenWitness]) {
      expect(() =>
        verifyDaSignatureWitness({
          publicKeyHex: signer.publicKeyHex,
          availabilityCommitment: commitment.commitment,
          witnessHex,
        }),
      ).toThrow();
      expect(() =>
        validateDaSignatureRecord({
          body: { ...record, signatureWitness: witnessHex },
          headerHash: commitment.commitment.header_hash,
          deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
          signerValidation: {
            committeeKeys: [signer.publicKeyHex],
            committeeSignersHash,
            threshold: 1,
          },
        }),
      ).toThrow();
    }
  });

  it("holds conflict-evidence intake until libsodium is ready, storing nothing", async () => {
    const fixture = await conflictFixture();
    const saveDaConflictEvidence = vi.fn(() => Promise.resolve(true));
    const intake = ingestDaConflictEvidence({
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
    }).then(
      () => "settled",
      () => "settled",
    );
    const pending = new Promise<string>((resolve) => {
      setTimeout(() => resolve("pending"), 200);
    });
    await expect(Promise.race([intake, pending])).resolves.toBe("pending");
    expect(saveDaConflictEvidence).not.toHaveBeenCalled();
  });
});
