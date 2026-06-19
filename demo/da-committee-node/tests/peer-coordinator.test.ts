import type { AddressInfo } from "node:net";

import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { createWatcherApiServer } from "../src/api/server.js";
import type { DaSignatureRecord } from "../src/domain.js";
import { PeerSignatureCoordinator } from "../src/peer/coordinator.js";
import {
  loadDaSigner,
  signDaAttestation,
  validateDaSignerMembership,
} from "../src/signer.js";
import { JsonFileWatcherStore } from "../src/store.js";
import { bytesToHex } from "../src/utils/hex.js";
import { tempDir } from "./helpers.js";

describe("PeerSignatureCoordinator", () => {
  it("broadcasts signed peer requests and persists broadcast/health state", async () => {
    const deploymentFingerprint = "dep";
    const headerHash = "01".repeat(28);
    const receiverSigner = await loadDaSigner(`hex:${"00".repeat(31)}01`);
    const senderSigner = await loadDaSigner(`hex:${"00".repeat(31)}02`);
    const committeeHex = receiverSigner.publicKeyHex + senderSigner.publicKeyHex;
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
    );
    const receiverValidation = validateDaSignerMembership({
      daParams: { committeeHex, committeeSignersHash, threshold: 2 },
      signer: receiverSigner,
      signerIndex: 0,
    });
    const senderValidation = validateDaSignerMembership({
      daParams: { committeeHex, committeeSignersHash, threshold: 2 },
      signer: senderSigner,
      signerIndex: 1,
    });
    const receiverStore = await JsonFileWatcherStore.open(await tempDir());
    const senderStore = await JsonFileWatcherStore.open(await tempDir());
    await receiverStore.saveDaPayload({
      deploymentFingerprint,
      headerHash,
      payloadCborHex: "aabb",
      payloadSha256: "22".repeat(32),
      sourceEndpoint: "fixture",
      fetchedAt: new Date().toISOString(),
      verifiedAt: new Date().toISOString(),
      validationStatus: "verified",
      conflictStatus: "none",
    });
    const api = createWatcherApiServer({
      deploymentFingerprint,
      signerIndex: 0,
      signerValidation: receiverValidation,
      store: receiverStore,
      ready: () => true,
    });
    await api.listen(0, "127.0.0.1");
    try {
      const peerBaseUrl = endpointFor(api);
      const coordinator = new PeerSignatureCoordinator({
        deploymentFingerprint,
        peers: [{ signerIndex: 0, baseUrl: peerBaseUrl }],
        signer: senderSigner,
        signerIndex: 1,
        signerValidation: senderValidation,
        store: senderStore,
        requestTimeoutMs: 1000,
        retryInitialDelayMs: 10,
        retryMaxDelayMs: 100,
        retryMaxAttempts: 3,
      });
      const result = await coordinator.publishSignature(
        signatureRecord({
          deploymentFingerprint,
          headerHash,
          signerIndex: 1,
          committeeSignersHash,
          signatureWitness: signDaAttestation({
            signer: senderSigner,
            signerIndex: 1,
            headerHash,
          }),
        }),
      );
      expect(result).toBe("posted");
      await expect(
        receiverStore.getDaSignature({ headerHash, signerIndex: 1 }),
      ).resolves.toMatchObject({ source: "peer", sourcePeer: expect.any(String) });
      await expect(senderStore.listPeerBroadcasts(headerHash)).resolves.toMatchObject([
        { peerBaseUrl, status: "posted", attempts: 1 },
      ]);
      await expect(senderStore.listPeerHealth()).resolves.toMatchObject([
        { peerBaseUrl, consecutiveFailures: 0 },
      ]);
    } finally {
      await api.close();
    }
  });

  it("keeps peer-not-ready broadcasts retryable without failing L1-backed publish", async () => {
    const deploymentFingerprint = "dep";
    const headerHash = "02".repeat(28);
    const receiverSigner = await loadDaSigner(`hex:${"00".repeat(31)}03`);
    const senderSigner = await loadDaSigner(`hex:${"00".repeat(31)}04`);
    const committeeHex = receiverSigner.publicKeyHex + senderSigner.publicKeyHex;
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
    );
    const receiverValidation = validateDaSignerMembership({
      daParams: { committeeHex, committeeSignersHash, threshold: 2 },
      signer: receiverSigner,
      signerIndex: 0,
    });
    const senderValidation = validateDaSignerMembership({
      daParams: { committeeHex, committeeSignersHash, threshold: 2 },
      signer: senderSigner,
      signerIndex: 1,
    });
    const receiverStore = await JsonFileWatcherStore.open(await tempDir());
    const senderStore = await JsonFileWatcherStore.open(await tempDir());
    const api = createWatcherApiServer({
      deploymentFingerprint,
      signerIndex: 0,
      signerValidation: receiverValidation,
      store: receiverStore,
      ready: () => true,
    });
    await api.listen(0, "127.0.0.1");
    try {
      const peerBaseUrl = endpointFor(api);
      const coordinator = new PeerSignatureCoordinator({
        deploymentFingerprint,
        peers: [{ signerIndex: 0, baseUrl: peerBaseUrl }],
        signer: senderSigner,
        signerIndex: 1,
        signerValidation: senderValidation,
        store: senderStore,
        requestTimeoutMs: 1000,
        retryInitialDelayMs: 10,
        retryMaxDelayMs: 100,
        retryMaxAttempts: 3,
        onChainCoordinator: {
          publishSignature: async () => "posted",
        },
      });
      const record = signatureRecord({
        deploymentFingerprint,
        headerHash,
        signerIndex: 1,
        committeeSignersHash,
        signatureWitness: signDaAttestation({
          signer: senderSigner,
          signerIndex: 1,
          headerHash,
        }),
      });

      await expect(coordinator.publishSignature(record)).resolves.toBe("posted");
      await expect(senderStore.listPeerBroadcasts(headerHash)).resolves.toMatchObject([
        { peerBaseUrl, status: "failed", attempts: 1 },
      ]);

      await receiverStore.saveDaPayload({
        deploymentFingerprint,
        headerHash,
        payloadCborHex: "aabb",
        payloadSha256: "22".repeat(32),
        sourceEndpoint: "fixture",
        fetchedAt: new Date().toISOString(),
        verifiedAt: new Date().toISOString(),
        validationStatus: "verified",
        conflictStatus: "none",
      });
      await new Promise((resolve) => setTimeout(resolve, 30));

      await expect(coordinator.publishSignature(record)).resolves.toBe("posted");
      await expect(senderStore.listPeerBroadcasts(headerHash)).resolves.toMatchObject([
        { peerBaseUrl, status: "posted", attempts: 2 },
      ]);
      await expect(
        receiverStore.getDaSignature({ headerHash, signerIndex: 1 }),
      ).resolves.toMatchObject({ source: "peer" });
    } finally {
      await api.close();
    }
  });
});

const endpointFor = (server: {
  readonly address: () => AddressInfo | string | null;
}): string => {
  const address = server.address();
  if (typeof address !== "object" || address === null) {
    throw new Error("server did not listen on a TCP address");
  }
  return `http://127.0.0.1:${address.port.toString()}`;
};

const signatureRecord = ({
  deploymentFingerprint,
  headerHash,
  signerIndex,
  committeeSignersHash,
  signatureWitness,
}: {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly signerIndex: number;
  readonly committeeSignersHash: string;
  readonly signatureWitness: string;
}): DaSignatureRecord => ({
  deploymentFingerprint,
  headerHash,
  signerIndex,
  signatureWitness,
  payloadHash: "22".repeat(32),
  committeeSignersHash,
  signedAt: new Date().toISOString(),
  broadcastStatus: "local",
  l1ChainPoint: {},
  validation: {
    payloadVersion: 1,
    rootsMatch: true,
    stateQueueOutRef: "tx#0",
    headerHash,
    rootSummary: {
      utxosRoot: "44".repeat(32),
      transactionsRoot: "55".repeat(32),
      depositsRoot: "66".repeat(32),
      withdrawalsRoot: "77".repeat(32),
    },
    l1Header: {
      startTime: "1",
      endTime: "2",
      operatorVkey: "88".repeat(28),
      prevHeaderHash: "99".repeat(28),
      protocolVersion: "1",
    },
  },
});
