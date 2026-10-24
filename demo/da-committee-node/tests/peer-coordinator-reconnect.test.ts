import { blake2b } from "@noble/hashes/blake2.js";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { DaPayloadRecord } from "../src/domain.js";
import { PeerSignatureCoordinator } from "../src/peer/coordinator.js";
import {
  loadDaSigner,
  signDaAttestation,
  validateDaSignerMembership,
} from "../src/signer.js";
import { bytesToHex } from "../src/utils/hex.js";
import { openTestCommitteeStore } from "./helpers/committee-store.js";
import {
  availabilityCommitmentAuthority,
  commitmentFor,
  signatureRecord,
} from "./peer-coordinator.signature-record.js";

const headerHash = "07".repeat(28);

afterEach(() => {
  vi.useRealTimers();
});

/**
 * A coordinator broadcasting to one peer that is unreachable until `up` is
 * set, both for broadcasts and for signature polls. With `refuse` set, the
 * peer answers polls but refuses every broadcast.
 */
const harness = async (retryMaxAttempts = 2) => {
  const signer = await loadDaSigner(`hex:${"00".repeat(31)}51`);
  const committeeSignersHash = bytesToHex(
    blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
  );
  const signerValidation = validateDaSignerMembership({
    daParams: {
      committeeHex: signer.publicKeyHex,
      committeeSignersHash,
      threshold: 1,
    },
    signer,
    signerIndex: 0,
  });
  const store = await openTestCommitteeStore();
  // The poll records the peer reachable only for a header whose payload this
  // node verified; its bytes do not matter to a poll that returns nothing.
  Object.assign(store, {
    getDaPayload: async () =>
      ({
        payloadCborHex: "aabb",
        validationStatus: "verified",
        payloadSha256: "cc".repeat(32),
      }) as unknown as DaPayloadRecord,
  });
  const state = {
    up: false,
    refuse: false,
    broadcasts: 0,
    /** The broadcast's attempt count as each broadcast was sent. */
    attemptsSent: [] as number[],
  };
  const unreachable = () =>
    Promise.reject(new Error("dial failed: ECONNREFUSED"));
  const coordinator = new PeerSignatureCoordinator({
    deploymentFingerprint: "dep",
    peers: [{ peerId: "remote-peer", signerIndex: 1 }],
    attestationExchange: {
      publishAttestation: async () => {
        state.broadcasts += 1;
        const [row] = await store.listPeerBroadcasts(headerHash);
        state.attemptsSent.push(row?.attempts ?? 0);
        if (state.up && state.refuse) throw new Error("broadcast refused");
        return state.up
          ? Promise.resolve({ status: "accepted" as const })
          : unreachable();
      },
      attestationsByHeader: () =>
        state.up ? Promise.resolve([]) : unreachable(),
      publishConflictEvidence: async () => undefined,
    },
    signer,
    signerIndex: 0,
    signerValidation,
    availabilityCommitmentAuthority,
    store,
    retryInitialDelayMs: 0,
    retryMaxDelayMs: 0,
    retryMaxAttempts,
  });
  const commitment = commitmentFor(headerHash);
  const record = signatureRecord({
    deploymentFingerprint: "dep",
    headerHash,
    signerIndex: 0,
    committeeSignersHash,
    commitment,
    signatureWitness: signDaAttestation({
      signer,
      signerIndex: 0,
      availabilityCommitment: commitment.commitment,
    }),
  });
  return { coordinator, record, store, state };
};

describe("peer broadcast across a peer outage", () => {
  it("sends a broadcast that exhausted its retries again, exactly once, when the peer is back", async () => {
    const h = await harness();
    for (let tick = 0; tick < 4; tick += 1) {
      await expect(h.coordinator.publishSignature(h.record)).resolves.toBe(
        "post_failed",
      );
    }
    // Two attempts while down, then the budget held it back.
    expect(h.state.broadcasts).toBe(2);
    await expect(h.store.listPeerBroadcasts(headerHash)).resolves.toMatchObject(
      [{ status: "failed", lastError: "peer retry budget exhausted" }],
    );

    h.state.up = true;
    await expect(h.coordinator.publishSignature(h.record)).resolves.toBe(
      "posted",
    );
    expect(h.state.broadcasts).toBe(3);
    await expect(h.store.listPeerBroadcasts(headerHash)).resolves.toMatchObject(
      [{ status: "posted", attempts: 1 }],
    );
    // Posted is final: nothing more is sent.
    await h.coordinator.publishSignature(h.record);
    expect(h.state.broadcasts).toBe(3);
  });

  it("keeps an exhausted broadcast held back while the peer is still unreachable", async () => {
    const h = await harness();
    for (let tick = 0; tick < 6; tick += 1) {
      await h.coordinator.publishSignature(h.record);
    }
    expect(h.state.broadcasts).toBe(2);
  });

  it("walks a whole retry budget before each reset for a peer that answers polls but refuses the broadcast", async () => {
    vi.useFakeTimers({ toFake: ["Date"] });
    const base = Date.parse("2026-10-01T00:00:00.000Z");
    const h = await harness(3);
    h.state.up = true;
    h.state.refuse = true;
    for (let tick = 0; tick < 7; tick += 1) {
      vi.setSystemTime(base + tick * 1_000);
      await h.coordinator.publishSignature(h.record);
    }
    // The budget is spent in full, then reset once by a success that
    // postdates the last attempt, never at every poll.
    expect(h.state.attemptsSent).toEqual([1, 2, 3, 1, 2, 3, 1]);
  });
});
