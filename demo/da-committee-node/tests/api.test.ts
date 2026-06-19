import { AddressInfo } from "node:net";

import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { createWatcherApiServer } from "../src/api/server.js";
import type { DaSignatureRecord, StateQueueHeaderRecord } from "../src/domain.js";
import {
  loadDaSigner,
  signDaAttestation,
  validateDaSignerMembership,
} from "../src/signer.js";
import { signPeerRequest } from "../src/peer/auth.js";
import { JsonFileWatcherStore } from "../src/store.js";
import { bytesToHex } from "../src/utils/hex.js";
import { tempDir } from "./helpers.js";

describe("watcher API", () => {
  it("serves health, readiness, and stored signature records", async () => {
    const store = await JsonFileWatcherStore.open(await tempDir());
    const deploymentFingerprint = "dep";
    const headerHash = "01".repeat(28);
    await store.saveDaSignature({
      deploymentFingerprint,
      headerHash,
      signerIndex: 0,
      signatureWitness: "00" + "11".repeat(64),
      payloadHash: "22".repeat(32),
      committeeSignersHash: "33".repeat(32),
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
    const api = createWatcherApiServer({
      deploymentFingerprint,
      signerIndex: 0,
      store,
      ready: () => true,
    });
    await api.listen(0, "127.0.0.1");
    try {
      const address = api.address();
      const port =
        typeof address === "object" && address !== null
          ? (address as AddressInfo).port
          : 0;
      const health = await fetch(`http://127.0.0.1:${port.toString()}/healthz`);
      expect(health.status).toBe(200);
      const signature = await fetch(
        `http://127.0.0.1:${port.toString()}/v1/deployments/${deploymentFingerprint}/headers/${headerHash}/signature`,
      );
      expect(signature.status).toBe(200);
      await expect(signature.json()).resolves.toMatchObject({
        headerHash,
        signatureWitness: "00" + "11".repeat(64),
      });
    } finally {
      await api.close();
    }
  });

  it("serves status records with bigint header fields as JSON strings", async () => {
    const store = await JsonFileWatcherStore.open(await tempDir());
    const deploymentFingerprint = "dep";
    const headerHash = "02".repeat(28);
    const headerRecord: StateQueueHeaderRecord = {
      deploymentFingerprint,
      headerHash,
      stateQueueOutRef: "tx#0",
      blockAssetName: `MBLCT${headerHash}`,
      header: {
        prevUtxosRoot: "00".repeat(32),
        utxosRoot: "11".repeat(32),
        transactionsRoot: "22".repeat(32),
        depositsRoot: "33".repeat(32),
        withdrawalsRoot: "44".repeat(32),
        startTime: 1n,
        endTime: 2n,
        prevHeaderHash: "55".repeat(28),
        operatorVkey: "66".repeat(28),
        protocolVersion: 0n,
      },
      computedHeaderHash: headerHash,
      daAttestation: "77".repeat(28),
      observedChainPoint: { depth: 10 },
      finalized: true,
      status: "attested",
      validationErrors: [],
      updatedAt: new Date().toISOString(),
    };
    await store.upsertStateQueueHeader(headerRecord);
    const api = createWatcherApiServer({
      deploymentFingerprint,
      signerIndex: 0,
      store,
      ready: () => true,
    });
    await api.listen(0, "127.0.0.1");
    try {
      const address = api.address();
      const port =
        typeof address === "object" && address !== null
          ? (address as AddressInfo).port
          : 0;
      const status = await fetch(
        `http://127.0.0.1:${port.toString()}/v1/deployments/${deploymentFingerprint}/headers/${headerHash}/status`,
      );
      expect(status.status).toBe(200);
      await expect(status.json()).resolves.toMatchObject({
        headerHash,
        header: {
          header: {
            startTime: "1",
            endTime: "2",
            protocolVersion: "0",
          },
        },
      });
    } finally {
      await api.close();
    }
  });

  it("accepts valid peer signatures and rejects invalid witnesses", async () => {
    const store = await JsonFileWatcherStore.open(await tempDir());
    const deploymentFingerprint = "dep";
    const headerHash = "01".repeat(28);
    const localSigner = await loadDaSigner(`hex:${"00".repeat(31)}01`);
    const peerSigner = await loadDaSigner(`hex:${"00".repeat(31)}02`);
    const committeeHex = localSigner.publicKeyHex + peerSigner.publicKeyHex;
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
    );
    const signerValidation = validateDaSignerMembership({
      daParams: {
        committeeHex,
        committeeSignersHash,
        threshold: 2,
      },
      signer: localSigner,
      signerIndex: 0,
    });
    const api = createWatcherApiServer({
      deploymentFingerprint,
      signerIndex: 0,
      signerValidation,
      store,
      ready: () => true,
    });
    await api.listen(0, "127.0.0.1");
    try {
      const address = api.address();
      const port =
        typeof address === "object" && address !== null
          ? (address as AddressInfo).port
          : 0;
      const url = `http://127.0.0.1:${port.toString()}/v1/deployments/${deploymentFingerprint}/headers/${headerHash}/signature`;
      const validPeerRecord = signatureRecord({
        deploymentFingerprint,
        headerHash,
        signerIndex: 1,
        committeeSignersHash,
        signatureWitness: signDaAttestation({
          signer: peerSigner,
          signerIndex: 1,
          headerHash,
        }),
      });
      const accepted = await fetch(url, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify(validPeerRecord),
      });
      expect(accepted.status).toBe(202);

      const storedPeer = await fetch(`${url}?signer_index=1`);
      expect(storedPeer.status).toBe(200);
      await expect(storedPeer.json()).resolves.toMatchObject({
        headerHash,
        signerIndex: 1,
        broadcastStatus: "posted",
      });

      const invalidPeerRecord = {
        ...validPeerRecord,
        signatureWitness: signDaAttestation({
          signer: localSigner,
          signerIndex: 1,
          headerHash,
        }),
      };
      const rejected = await fetch(url, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify(invalidPeerRecord),
      });
      expect(rejected.status).toBe(400);

      const badSignerQuery = await fetch(`${url}?signer_index=not-a-number`);
      expect(badSignerQuery.status).toBe(400);

      const badJson = await fetch(url, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: "{",
      });
      expect(badJson.status).toBe(400);
    } finally {
      await api.close();
    }
  });

  it("requires signed peer auth, verified local payload, and fresh nonces on plural signature POSTs", async () => {
    const store = await JsonFileWatcherStore.open(await tempDir());
    const deploymentFingerprint = "dep";
    const headerHash = "01".repeat(28);
    const localSigner = await loadDaSigner(`hex:${"00".repeat(31)}01`);
    const peerSigner = await loadDaSigner(`hex:${"00".repeat(31)}02`);
    const committeeHex = localSigner.publicKeyHex + peerSigner.publicKeyHex;
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
    );
    const signerValidation = validateDaSignerMembership({
      daParams: {
        committeeHex,
        committeeSignersHash,
        threshold: 2,
      },
      signer: localSigner,
      signerIndex: 0,
    });
    await store.saveDaPayload({
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
      signerValidation,
      store,
      ready: () => true,
    });
    await api.listen(0, "127.0.0.1");
    try {
      const address = api.address();
      const port =
        typeof address === "object" && address !== null
          ? (address as AddressInfo).port
          : 0;
      const pathAndSearch = `/v1/deployments/${deploymentFingerprint}/headers/${headerHash}/signatures`;
      const url = `http://127.0.0.1:${port.toString()}${pathAndSearch}`;
      const validPeerRecord = signatureRecord({
        deploymentFingerprint,
        headerHash,
        signerIndex: 1,
        committeeSignersHash,
        signatureWitness: signDaAttestation({
          signer: peerSigner,
          signerIndex: 1,
          headerHash,
        }),
      });
      const body = Buffer.from(JSON.stringify(validPeerRecord), "utf8");
      const authHeaders = signPeerRequest({
        signer: peerSigner,
        signerIndex: 1,
        deploymentFingerprint,
        method: "POST",
        pathAndSearch,
        body,
        timestampMs: Date.now(),
        nonce: "aa".repeat(16),
      });
      const unsigned = await fetch(url, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body,
      });
      expect(unsigned.status).toBe(401);

      const accepted = await fetch(url, {
        method: "POST",
        headers: { "content-type": "application/json", ...authHeaders },
        body,
      });
      expect(accepted.status).toBe(202);
      await expect(
        store.getDaSignature({ headerHash, signerIndex: 1 }),
      ).resolves.toMatchObject({
        source: "peer",
        signerIndex: 1,
      });

      const replayed = await fetch(url, {
        method: "POST",
        headers: { "content-type": "application/json", ...authHeaders },
        body,
      });
      expect(replayed.status).toBe(409);
    } finally {
      await api.close();
    }
  });
});

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
