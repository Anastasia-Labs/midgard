import { createServer } from "node:http";
import type { AddressInfo } from "node:net";

import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { DaPayloadClient } from "../src/da/client.js";
import { daPayloadSha256 } from "../src/da/payload.js";
import { OnChainLifecycleCoordinator } from "../src/coordinator/on-chain.js";
import { SubmitterReconciler } from "../src/coordinator/submitter-reconciler.js";
import { PeerSignaturePoller } from "../src/peer/poller.js";
import { JsonFileWatcherStore } from "../src/store.js";
import { WatcherService } from "../src/watcher.js";
import { bytesToHex } from "../src/utils/hex.js";
import {
  loadDaSigner,
  signDaAttestation,
  validateDaCommittee,
  validateDaSignerMembership,
} from "../src/signer.js";
import {
  IDENTITY_TX_PROJECTOR,
  makeObservedNode,
  makePayloadFixture,
  minimalConfig,
  tempDir,
} from "./helpers.js";
import type {
  DaAttestationCandidateRecord,
  DaSignatureRecord,
} from "../src/domain.js";

describe("WatcherService", () => {
  it("fetches, verifies, signs, and persists one finalized unattested header", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configWithDaHash = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configWithDaHash.daParams,
      signer,
      signerIndex: 0,
    });
    const store = await JsonFileWatcherStore.open(dir);
    const payloadClient = new DaPayloadClient({
      endpoints: ["http://da.example"],
      fetchFn: (async (url: string | URL | Request) => {
        const value = url.toString();
        if (value.includes("/metadata")) {
          return Response.json({ headerHash });
        }
        return new Response(payloadCbor, {
          status: 200,
          headers: { "content-type": "application/cbor" },
        });
      }) as typeof fetch,
    });
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadClient,
      signer,
      signerValidation,
      transactionProjector: IDENTITY_TX_PROJECTOR,
    });
    await service.initialize();
    const result = await service.tick();
    expect(result).toMatchObject({ scannedHeaders: 1, signedHeaders: 1 });
    await expect(
      store.getDaSignature({ headerHash, signerIndex: 0 }),
    ).resolves.toMatchObject({ headerHash, signerIndex: 0 });
  });

  it("fetches payload bytes from a Midgard-node-compatible HTTP endpoint", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const payloadServer = createServer((request, response) => {
      const url = new URL(request.url ?? "/", "http://midgard-node.local");
      if (
        request.method === "GET" &&
        url.pathname === "/da/payload" &&
        url.searchParams.get("header_hash") === headerHash
      ) {
        response.writeHead(200, { "content-type": "application/cbor" });
        response.end(payloadCbor);
        return;
      }
      if (
        request.method === "GET" &&
        url.pathname === "/da/payload/metadata" &&
        url.searchParams.get("header_hash") === headerHash
      ) {
        response.writeHead(200, { "content-type": "application/json" });
        response.end(
          `${JSON.stringify({
            headerHash,
            payloadBytes: payloadCbor.length,
            status: "available",
          })}\n`,
        );
        return;
      }
      response.writeHead(404, { "content-type": "application/json" });
      response.end('{"error":"not found"}\n');
    });
    await new Promise<void>((resolve) => {
      payloadServer.listen(0, "127.0.0.1", resolve);
    });
    try {
      const address = payloadServer.address();
      const port =
        typeof address === "object" && address !== null
          ? (address as AddressInfo).port
          : 0;
      const endpoint = `http://127.0.0.1:${port.toString()}`;
      const seed = "00".repeat(31) + "01";
      const signer = await loadDaSigner(`hex:${seed}`);
      const config = minimalConfig({
        dir,
        manifestPath: `${dir}/manifest.json`,
        deploymentInfoPath: `${dir}/deployment.json`,
        signerSeed: seed,
        signerPublicKey: signer.publicKeyHex,
      });
      const configWithDaHash = {
        ...config,
        daParams: {
          ...config.daParams,
          committeeSignersHash: bytesToHex(
            blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
          ),
        },
        daPayloadEndpoints: [endpoint],
      };
      const signerValidation = validateDaSignerMembership({
        daParams: configWithDaHash.daParams,
        signer,
        signerIndex: 0,
      });
      const store = await JsonFileWatcherStore.open(dir);
      const service = new WatcherService({
        config: configWithDaHash,
        store,
        stateQueueProvider: {
          fetchStateQueueNodes: async () => [
            makeObservedNode({ header, headerHash, depth: 10 }),
          ],
        },
        payloadClient: new DaPayloadClient({
          endpoints: configWithDaHash.daPayloadEndpoints,
        }),
        signer,
        signerValidation,
        transactionProjector: IDENTITY_TX_PROJECTOR,
      });

      await service.initialize();
      await expect(service.tick()).resolves.toMatchObject({
        scannedHeaders: 1,
        signedHeaders: 1,
        skippedHeaders: 0,
        errors: [],
      });
      await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
        headerHash,
        sourceEndpoint: endpoint,
        validationStatus: "verified",
      });
      await expect(
        store.getDaSignature({ headerHash, signerIndex: 0 }),
      ).resolves.toMatchObject({ headerHash, broadcastStatus: "local" });
    } finally {
      await new Promise<void>((resolve, reject) => {
        payloadServer.close((error) =>
          error === undefined ? resolve() : reject(error),
        );
      });
    }
  });

  it("detects conflicting payload bytes across DA endpoints and refuses to sign", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const conflictingPayload = Buffer.from(payloadCbor);
    conflictingPayload[conflictingPayload.length - 1] =
      (conflictingPayload[conflictingPayload.length - 1] ?? 0) ^ 0xff;
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configWithDaHash = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
      daPayloadEndpoints: ["http://da-a.example", "http://da-b.example"],
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configWithDaHash.daParams,
      signer,
      signerIndex: 0,
    });
    const store = await JsonFileWatcherStore.open(dir);
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadClient: new DaPayloadClient({
        endpoints: configWithDaHash.daPayloadEndpoints,
        retries: 0,
        fetchFn: (async (url: string | URL | Request) => {
          const value = url.toString();
          if (value.includes("/metadata")) {
            return Response.json({ headerHash });
          }
          return new Response(
            value.startsWith("http://da-a.example")
              ? payloadCbor
              : conflictingPayload,
            {
              status: 200,
              headers: { "content-type": "application/cbor" },
            },
          );
        }) as typeof fetch,
      }),
      signer,
      signerValidation,
      transactionProjector: IDENTITY_TX_PROJECTOR,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 0,
      skippedHeaders: 1,
      errors: [`conflicting DA payload bytes for ${headerHash}`],
    });
    await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
      headerHash,
      validationStatus: "conflicted",
      conflictStatus: "conflicting_bytes",
      sourceEndpoint: "http://da-a.example,http://da-b.example",
    });
    await expect(
      store.getDaSignature({ headerHash, signerIndex: 0 }),
    ).resolves.toBeUndefined();
  });

  it("signs after a transient missing DA payload becomes available", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configWithDaHash = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configWithDaHash.daParams,
      signer,
      signerIndex: 0,
    });
    let payloadAvailable = false;
    const store = await JsonFileWatcherStore.open(dir);
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadClient: new DaPayloadClient({
        endpoints: ["http://da.example"],
        retries: 0,
        fetchFn: (async (url: string | URL | Request) => {
          if (url.toString().includes("/metadata")) {
            return Response.json({ headerHash });
          }
          return payloadAvailable
            ? new Response(payloadCbor, {
                status: 200,
                headers: { "content-type": "application/cbor" },
              })
            : new Response("missing", { status: 404 });
        }) as typeof fetch,
      }),
      signer,
      signerValidation,
      transactionProjector: IDENTITY_TX_PROJECTOR,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 0,
      skippedHeaders: 1,
      errors: [`missing DA payload for ${headerHash}`],
    });
    await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
      validationStatus: "missing_da",
      payloadSha256: "",
    });

    payloadAvailable = true;
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      skippedHeaders: 0,
      errors: [],
    });
    await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
      validationStatus: "verified",
      payloadSha256: daPayloadSha256(payloadCbor),
      conflictStatus: "none",
    });
  });

  it("coalesces overlapping ticks to avoid duplicate local signing work", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configWithDaHash = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configWithDaHash.daParams,
      signer,
      signerIndex: 0,
    });
    let releasePayload!: () => void;
    const payloadGate = new Promise<void>((resolve) => {
      releasePayload = resolve;
    });
    let payloadFetchStarted!: () => void;
    const payloadFetchStartedSignal = new Promise<void>((resolve) => {
      payloadFetchStarted = resolve;
    });
    let scans = 0;
    let payloadFetches = 0;
    const store = await JsonFileWatcherStore.open(dir);
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => {
          scans += 1;
          return [makeObservedNode({ header, headerHash, depth: 10 })];
        },
      },
      payloadClient: new DaPayloadClient({
        endpoints: ["http://da.example"],
        fetchFn: (async (url: string | URL | Request) => {
          if (url.toString().includes("/metadata")) {
            return Response.json({ headerHash });
          }
          payloadFetches += 1;
          payloadFetchStarted();
          await payloadGate;
          return new Response(payloadCbor, {
            status: 200,
            headers: { "content-type": "application/cbor" },
          });
        }) as typeof fetch,
      }),
      signer,
      signerValidation,
      transactionProjector: IDENTITY_TX_PROJECTOR,
    });

    await service.initialize();
    const first = service.tick();
    const second = service.tick();
    await payloadFetchStartedSignal;
    expect(scans).toBe(1);
    expect(payloadFetches).toBe(1);
    releasePayload();
    const [firstResult, secondResult] = await Promise.all([first, second]);
    expect(firstResult).toEqual(secondResult);
    expect(firstResult).toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      skippedHeaders: 0,
      errors: [],
    });
    expect(scans).toBe(1);
    expect(payloadFetches).toBe(1);
  });

  it("retries published signatures when the coordinator owns an on-chain lifecycle", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configWithDaHash = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configWithDaHash.daParams,
      signer,
      signerIndex: 0,
    });
    const store = await JsonFileWatcherStore.open(dir);
    const payloadClient = new DaPayloadClient({
      endpoints: ["http://da.example"],
      fetchFn: (async () =>
        new Response(payloadCbor, {
          status: 200,
          headers: { "content-type": "application/cbor" },
        })) as typeof fetch,
    });
    const published: string[] = [];
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadClient,
      signer,
      signerValidation,
      coordinator: {
        retryPublishedSignatures: true,
        publishSignature: async (record) => {
          published.push(record.headerHash);
          return "posted";
        },
      },
      transactionProjector: IDENTITY_TX_PROJECTOR,
    });
    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      signedHeaders: 1,
      errors: [],
    });
    await expect(service.tick()).resolves.toMatchObject({
      signedHeaders: 0,
      skippedHeaders: 1,
      errors: [],
    });
    expect(published).toEqual([headerHash, headerHash]);
  });

  it("surfaces coordinator publish failures and keeps signatures retryable", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configWithDaHash = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configWithDaHash.daParams,
      signer,
      signerIndex: 0,
    });
    const store = await JsonFileWatcherStore.open(dir);
    const payloadClient = new DaPayloadClient({
      endpoints: ["http://da.example"],
      fetchFn: (async () =>
        new Response(payloadCbor, {
          status: 200,
          headers: { "content-type": "application/cbor" },
        })) as typeof fetch,
    });
    const published: string[] = [];
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadClient,
      signer,
      signerValidation,
      coordinator: {
        publishSignature: async (record) => {
          published.push(record.headerHash);
          return "post_failed";
        },
      },
      transactionProjector: IDENTITY_TX_PROJECTOR,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      skippedHeaders: 0,
      errors: [
        `failed to publish DA signature for ${headerHash} signer 0`,
      ],
    });
    await expect(
      store.getDaSignature({ headerHash, signerIndex: 0 }),
    ).resolves.toMatchObject({ broadcastStatus: "post_failed" });

    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 0,
      skippedHeaders: 1,
      errors: [
        `failed to publish DA signature for ${headerHash} signer 0`,
      ],
    });
    expect(published).toEqual([headerHash, headerHash]);
  });

  it("republishes existing signatures for attested headers when the coordinator opts in", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configWithDaHash = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configWithDaHash.daParams,
      signer,
      signerIndex: 0,
    });
    const store = await JsonFileWatcherStore.open(dir);
    const signature: DaSignatureRecord = {
      deploymentFingerprint: configWithDaHash.deploymentFingerprint,
      headerHash,
      signerIndex: 0,
      signatureWitness: signDaAttestation({
        signer,
        signerIndex: 0,
        headerHash,
      }),
      payloadHash: daPayloadSha256(payloadCbor),
      committeeSignersHash: configWithDaHash.daParams.committeeSignersHash,
      signedAt: new Date().toISOString(),
      broadcastStatus: "post_failed",
      source: "local",
      verifiedAt: new Date().toISOString(),
      l1ChainPoint: {},
      validation: {
        payloadVersion: 1,
        rootsMatch: true,
        stateQueueOutRef: "ab".repeat(32) + "#0",
        headerHash,
        rootSummary: {
          utxosRoot: header.utxosRoot,
          transactionsRoot: header.transactionsRoot,
          depositsRoot: header.depositsRoot,
          withdrawalsRoot: header.withdrawalsRoot,
        },
        l1Header: {
          startTime: header.startTime.toString(),
          endTime: header.endTime.toString(),
          operatorVkey: header.operatorVkey,
          prevHeaderHash: header.prevHeaderHash,
          protocolVersion: header.protocolVersion.toString(),
        },
      },
    };
    await store.saveDaSignature(signature);
    const published: string[] = [];
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({
            header,
            headerHash,
            daAttestation: configWithDaHash.daAttestationPolicyId,
            depth: 10,
          }),
        ],
      },
      payloadClient: new DaPayloadClient({
        endpoints: ["http://da.example"],
        fetchFn: (async () => {
          throw new Error("payload should not be fetched for attested header");
        }) as typeof fetch,
      }),
      signer,
      signerValidation,
      coordinator: {
        retryPublishedSignatures: true,
        retryPublishedSignaturesForAttestedHeaders: true,
        publishSignature: async (record) => {
          published.push(record.headerHash);
          return "posted";
        },
      },
      transactionProjector: IDENTITY_TX_PROJECTOR,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 0,
      skippedHeaders: 1,
      errors: [],
    });
    expect(published).toEqual([headerHash]);
    await expect(
      store.getDaSignature({ headerHash, signerIndex: 0 }),
    ).resolves.toMatchObject({ broadcastStatus: "posted" });
  });

  it("fetches, signs, runs on-chain lifecycle, and journals L1 submissions", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configWithDaHash = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configWithDaHash.daParams,
      signer,
      signerIndex: 0,
    });
    const store = await JsonFileWatcherStore.open(dir);
    const payloadClient = new DaPayloadClient({
      endpoints: ["http://da.example"],
      fetchFn: (async () =>
        new Response(payloadCbor, {
          status: 200,
          headers: { "content-type": "application/cbor" },
        })) as typeof fetch,
    });
    const initialized = candidateRecord({
      headerHash,
      committeeSignersHash: signerValidation.committeeSignersHash,
      attestationCount: 0,
    });
    const threshold = candidateRecord({
      headerHash,
      committeeSignersHash: signerValidation.committeeSignersHash,
      attestationCount: 1,
      status: "threshold",
      bitmap: "80" + "00".repeat(31),
    });
    const candidateResponses = [[], [initialized], [threshold]];
    const coordinator = new OnChainLifecycleCoordinator({
      threshold: 1,
      visibilityRetryCount: 0,
      chainReader: {
        fetchDaAttestationCandidates: async () => candidateResponses.shift() ?? [],
      },
      recordSubmission: (record) => store.saveL1Submission(record),
      submitter: {
        initAttestation: async () => "initTx",
        addSignatures: async () => "addTx",
        applyAttestation: async () => "applyTx",
      },
    });
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadClient,
      signer,
      signerValidation,
      coordinator,
      transactionProjector: IDENTITY_TX_PROJECTOR,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      errors: [],
    });
    await expect(store.listL1Submissions()).resolves.toMatchObject([
      { headerHash, txKind: "add_signatures", txHash: "addTx" },
      { headerHash, txKind: "apply", txHash: "applyTx" },
      { headerHash, txKind: "init", txHash: "initTx" },
    ]);
  });

  it("joins an existing attestation candidate, waits for threshold, and applies on a later tick", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const peerPublicKey = "ff".repeat(32);
    const committeeHex = signer.publicKeyHex + peerPublicKey;
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configWithDaHash = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeHex,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
        ),
        threshold: 2,
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configWithDaHash.daParams,
      signer,
      signerIndex: 0,
    });
    const store = await JsonFileWatcherStore.open(dir);
    const payloadClient = new DaPayloadClient({
      endpoints: ["http://da.example"],
      fetchFn: (async () =>
        new Response(payloadCbor, {
          status: 200,
          headers: { "content-type": "application/cbor" },
        })) as typeof fetch,
    });
    const initialized = candidateRecord({
      headerHash,
      committeeSignersHash: signerValidation.committeeSignersHash,
      attestationCount: 0,
      threshold: 2,
    });
    const signedByThisNode = candidateRecord({
      headerHash,
      committeeSignersHash: signerValidation.committeeSignersHash,
      attestationCount: 1,
      threshold: 2,
      status: "signed",
      bitmap: "80" + "00".repeat(31),
    });
    const signedByPeerToo = candidateRecord({
      headerHash,
      committeeSignersHash: signerValidation.committeeSignersHash,
      attestationCount: 2,
      threshold: 2,
      status: "threshold",
      bitmap: "c0" + "00".repeat(31),
    });
    const candidateResponses = [
      [initialized],
      [signedByThisNode],
      [signedByPeerToo],
    ];
    const calls: string[] = [];
    const coordinator = new OnChainLifecycleCoordinator({
      threshold: 2,
      visibilityRetryCount: 0,
      chainReader: {
        fetchDaAttestationCandidates: async () => candidateResponses.shift() ?? [],
      },
      recordSubmission: (record) => store.saveL1Submission(record),
      submitter: {
        initAttestation: async () => {
          calls.push("init");
          return "initTx";
        },
        addSignatures: async ({ packedWitnessesHex, signerIndexes }) => {
          calls.push(
            `add:${signerIndexes.join(",")}:${packedWitnessesHex.slice(0, 2)}`,
          );
          return "addTx";
        },
        applyAttestation: async ({ candidate }) => {
          calls.push(`apply:${candidate.outRef}`);
          return "applyTx";
        },
      },
    });
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadClient,
      signer,
      signerValidation,
      coordinator,
      transactionProjector: IDENTITY_TX_PROJECTOR,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      skippedHeaders: 0,
      errors: [],
    });
    await expect(store.listL1Submissions()).resolves.toMatchObject([
      { headerHash, txKind: "add_signatures", txHash: "addTx" },
    ]);

    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 0,
      skippedHeaders: 1,
      errors: [],
    });
    expect(calls).toEqual(["add:0:00", `apply:${initialized.outRef}`]);
    await expect(store.listL1Submissions()).resolves.toMatchObject([
      { headerHash, txKind: "add_signatures", txHash: "addTx" },
      { headerHash, txKind: "apply", txHash: "applyTx" },
    ]);
  });

  it("uses a stored peer signature to reach threshold in one add-signatures transaction", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const peerSigner = await loadDaSigner(`hex:${"00".repeat(31)}02`);
    const committeeHex = signer.publicKeyHex + peerSigner.publicKeyHex;
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configWithDaHash = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeHex,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
        ),
        threshold: 2,
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configWithDaHash.daParams,
      signer,
      signerIndex: 0,
    });
    const store = await JsonFileWatcherStore.open(dir);
    await store.saveDaSignature({
      deploymentFingerprint: configWithDaHash.deploymentFingerprint,
      headerHash,
      signerIndex: 1,
      signatureWitness: signDaAttestation({
        signer: peerSigner,
        signerIndex: 1,
        headerHash,
      }),
      payloadHash: daPayloadSha256(payloadCbor),
      committeeSignersHash: signerValidation.committeeSignersHash,
      signedAt: new Date().toISOString(),
      broadcastStatus: "posted",
      l1ChainPoint: {},
      validation: {
        payloadVersion: 1,
        rootsMatch: true,
        stateQueueOutRef: "peer#0",
        headerHash,
        rootSummary: {
          utxosRoot: header.utxosRoot,
          transactionsRoot: header.transactionsRoot,
          depositsRoot: header.depositsRoot,
          withdrawalsRoot: header.withdrawalsRoot,
        },
        l1Header: {
          startTime: header.startTime.toString(),
          endTime: header.endTime.toString(),
          operatorVkey: header.operatorVkey,
          prevHeaderHash: header.prevHeaderHash,
          protocolVersion: header.protocolVersion.toString(),
        },
      },
    });
    await store.saveDaSignature({
      deploymentFingerprint: configWithDaHash.deploymentFingerprint,
      headerHash,
      signerIndex: 2,
      signatureWitness: signDaAttestation({
        signer: peerSigner,
        signerIndex: 2,
        headerHash,
      }),
      payloadHash: "99".repeat(32),
      committeeSignersHash: signerValidation.committeeSignersHash,
      signedAt: new Date().toISOString(),
      broadcastStatus: "posted",
      l1ChainPoint: {},
      validation: {
        payloadVersion: 1,
        rootsMatch: true,
        stateQueueOutRef: "stale#0",
        headerHash,
        rootSummary: {
          utxosRoot: header.utxosRoot,
          transactionsRoot: header.transactionsRoot,
          depositsRoot: header.depositsRoot,
          withdrawalsRoot: header.withdrawalsRoot,
        },
        l1Header: {
          startTime: header.startTime.toString(),
          endTime: header.endTime.toString(),
          operatorVkey: header.operatorVkey,
          prevHeaderHash: header.prevHeaderHash,
          protocolVersion: header.protocolVersion.toString(),
        },
      },
    });
    const initialized = candidateRecord({
      headerHash,
      committeeSignersHash: signerValidation.committeeSignersHash,
      attestationCount: 0,
      threshold: 2,
    });
    const threshold = candidateRecord({
      headerHash,
      committeeSignersHash: signerValidation.committeeSignersHash,
      attestationCount: 2,
      threshold: 2,
      status: "threshold",
      bitmap: "c0" + "00".repeat(31),
    });
    const candidateResponses = [[initialized], [threshold]];
    const calls: string[] = [];
    const coordinator = new OnChainLifecycleCoordinator({
      threshold: 2,
      visibilityRetryCount: 0,
      peerSignaturesFor: (candidateHeaderHash) =>
        store.listDaSignatures(candidateHeaderHash),
      chainReader: {
        fetchDaAttestationCandidates: async () =>
          candidateResponses.shift() ?? [threshold],
      },
      recordSubmission: (record) => store.saveL1Submission(record),
      submitter: {
        initAttestation: async () => {
          throw new Error("unexpected init");
        },
        addSignatures: async ({ packedWitnessesHex, signerIndexes }) => {
          calls.push(
            `add:${signerIndexes.join(",")}:${packedWitnessesHex.slice(0, 2)}`,
          );
          return "addTx";
        },
        applyAttestation: async ({ candidate }) => {
          calls.push(`apply:${candidate.outRef}`);
          return "applyTx";
        },
      },
    });
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadClient: new DaPayloadClient({
        endpoints: ["http://da.example"],
        fetchFn: (async () =>
          new Response(payloadCbor, {
            status: 200,
            headers: { "content-type": "application/cbor" },
          })) as typeof fetch,
      }),
      signer,
      signerValidation,
      coordinator,
      transactionProjector: IDENTITY_TX_PROJECTOR,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      skippedHeaders: 0,
      errors: [],
    });
    expect(calls).toEqual(["add:0,1:00", `apply:${initialized.outRef}`]);
    await expect(store.listL1Submissions()).resolves.toMatchObject([
      { headerHash, txKind: "add_signatures", txHash: "addTx" },
      { headerHash, txKind: "apply", txHash: "applyTx" },
    ]);
  });

  it("submitter-only mode polls peer signatures and submits the L1 lifecycle", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const signer0 = await loadDaSigner(`hex:${"00".repeat(31)}01`);
    const signer1 = await loadDaSigner(`hex:${"00".repeat(31)}02`);
    const committeeHex = signer0.publicKeyHex + signer1.publicKeyHex;
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
    );
    const config = {
      ...minimalConfig({
        dir,
        manifestPath: `${dir}/manifest.json`,
        deploymentInfoPath: `${dir}/deployment.json`,
        signerSeed: "00".repeat(31) + "01",
        signerPublicKey: signer0.publicKeyHex,
      }),
      signerIndex: undefined,
      signerKeySource: undefined,
      l1SubmissionEnabled: true,
      daParams: {
        committeeHex,
        committeeSignersHash,
        threshold: 2,
      },
    };
    const committeeValidation = validateDaCommittee({
      daParams: config.daParams,
    });
    const rootSummary = {
      utxosRoot: header.utxosRoot,
      transactionsRoot: header.transactionsRoot,
      depositsRoot: header.depositsRoot,
      withdrawalsRoot: header.withdrawalsRoot,
    };
    const peerSignatures: readonly DaSignatureRecord[] = [
      {
        deploymentFingerprint: config.deploymentFingerprint,
        headerHash,
        signerIndex: 0,
        signatureWitness: signDaAttestation({
          signer: signer0,
          signerIndex: 0,
          headerHash,
        }),
        payloadHash: daPayloadSha256(payloadCbor),
        committeeSignersHash,
        signedAt: new Date().toISOString(),
        broadcastStatus: "posted",
        l1ChainPoint: {},
        validation: {
          payloadVersion: 1,
          rootsMatch: true,
          stateQueueOutRef: "peer#0",
          headerHash,
          rootSummary,
          l1Header: {
            startTime: header.startTime.toString(),
            endTime: header.endTime.toString(),
            operatorVkey: header.operatorVkey,
            prevHeaderHash: header.prevHeaderHash,
            protocolVersion: header.protocolVersion.toString(),
          },
        },
      },
      {
        deploymentFingerprint: config.deploymentFingerprint,
        headerHash,
        signerIndex: 1,
        signatureWitness: signDaAttestation({
          signer: signer1,
          signerIndex: 1,
          headerHash,
        }),
        payloadHash: daPayloadSha256(payloadCbor),
        committeeSignersHash,
        signedAt: new Date().toISOString(),
        broadcastStatus: "posted",
        l1ChainPoint: {},
        validation: {
          payloadVersion: 1,
          rootsMatch: true,
          stateQueueOutRef: "peer#1",
          headerHash,
          rootSummary,
          l1Header: {
            startTime: header.startTime.toString(),
            endTime: header.endTime.toString(),
            operatorVkey: header.operatorVkey,
            prevHeaderHash: header.prevHeaderHash,
            protocolVersion: header.protocolVersion.toString(),
          },
        },
      },
    ];
    const peerServer = createServer((request, response) => {
      if (request.method === "GET") {
        response.writeHead(200, { "content-type": "application/json" });
        response.end(`${JSON.stringify({ signatures: peerSignatures })}\n`);
        return;
      }
      response.writeHead(405);
      response.end();
    });
    await new Promise<void>((resolve) => {
      peerServer.listen(0, "127.0.0.1", resolve);
    });
    try {
      const address = peerServer.address();
      const port =
        typeof address === "object" && address !== null
          ? (address as AddressInfo).port
          : 0;
      const peerBaseUrl = `http://127.0.0.1:${port.toString()}`;
      const store = await JsonFileWatcherStore.open(dir);
      const initialized = candidateRecord({
        headerHash,
        committeeSignersHash,
        attestationCount: 0,
        threshold: 2,
      });
      const threshold = candidateRecord({
        headerHash,
        committeeSignersHash,
        attestationCount: 2,
        threshold: 2,
        status: "threshold",
        bitmap: "c0" + "00".repeat(31),
      });
      const candidateResponses = [[], [initialized], [threshold]];
      const calls: string[] = [];
      const onChainCoordinator = new OnChainLifecycleCoordinator({
        threshold: 2,
        visibilityRetryCount: 0,
        chainReader: {
          fetchDaAttestationCandidates: async () =>
            candidateResponses.shift() ?? [threshold],
        },
        recordSubmission: (record) => store.saveL1Submission(record),
        submitter: {
          initAttestation: async () => {
            calls.push("init");
            return "initTx";
          },
          addSignatures: async ({ signerIndexes }) => {
            calls.push(`add:${signerIndexes.join(",")}`);
            return "addTx";
          },
          applyAttestation: async ({ candidate }) => {
            calls.push(`apply:${candidate.outRef}`);
            return "applyTx";
          },
        },
      });
      const peerPoller = new PeerSignaturePoller({
        deploymentFingerprint: config.deploymentFingerprint,
        peers: [{ baseUrl: peerBaseUrl }],
        signerValidation: committeeValidation,
        store,
        requestTimeoutMs: 1000,
      });
      const submitterReconciler = new SubmitterReconciler({
        deploymentFingerprint: config.deploymentFingerprint,
        committeeValidation,
        store,
        coordinator: onChainCoordinator,
        peerPoller,
      });
      const service = new WatcherService({
        config,
        store,
        stateQueueProvider: {
          fetchStateQueueNodes: async () => [
            makeObservedNode({ header, headerHash, depth: 10 }),
          ],
        },
        payloadClient: new DaPayloadClient({
          endpoints: ["http://da.example"],
          fetchFn: (async () =>
            new Response(payloadCbor, {
              status: 200,
              headers: { "content-type": "application/cbor" },
            })) as typeof fetch,
        }),
        submitterReconciler,
        transactionProjector: IDENTITY_TX_PROJECTOR,
      });

      await service.initialize();
      await expect(service.tick()).resolves.toMatchObject({
        scannedHeaders: 1,
        signedHeaders: 0,
        reconciledHeaders: 1,
        skippedHeaders: 1,
        errors: [],
      });
      expect(calls).toEqual(["init", "add:0,1", `apply:${initialized.outRef}`]);
      await expect(store.listDaSignatures(headerHash)).resolves.toMatchObject([
        { headerHash, signerIndex: 0, source: "peer" },
        { headerHash, signerIndex: 1, source: "peer" },
      ]);
      await expect(store.listL1Submissions()).resolves.toMatchObject([
        { headerHash, txKind: "add_signatures", txHash: "addTx" },
        { headerHash, txKind: "apply", txHash: "applyTx" },
        { headerHash, txKind: "init", txHash: "initTx" },
      ]);
    } finally {
      await new Promise<void>((resolve, reject) => {
        peerServer.close((error) =>
          error === undefined ? resolve() : reject(error),
        );
      });
    }
  });

  it("recovers after restart between add-signatures and threshold apply", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const peerPublicKey = "ee".repeat(32);
    const committeeHex = signer.publicKeyHex + peerPublicKey;
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configWithDaHash = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeHex,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
        ),
        threshold: 2,
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configWithDaHash.daParams,
      signer,
      signerIndex: 0,
    });
    const initialized = candidateRecord({
      headerHash,
      committeeSignersHash: signerValidation.committeeSignersHash,
      attestationCount: 0,
      threshold: 2,
    });
    const signedByThisNode = candidateRecord({
      headerHash,
      committeeSignersHash: signerValidation.committeeSignersHash,
      attestationCount: 1,
      threshold: 2,
      status: "signed",
      bitmap: "80" + "00".repeat(31),
    });
    const thresholdCandidate = candidateRecord({
      headerHash,
      committeeSignersHash: signerValidation.committeeSignersHash,
      attestationCount: 2,
      threshold: 2,
      status: "threshold",
      bitmap: "c0" + "00".repeat(31),
    });
    const calls: string[] = [];
    const firstStore = await JsonFileWatcherStore.open(dir);
    const firstCoordinator = new OnChainLifecycleCoordinator({
      threshold: 2,
      visibilityRetryCount: 0,
      chainReader: {
        fetchDaAttestationCandidates: async () =>
          calls.length === 0 ? [initialized] : [signedByThisNode],
      },
      recordCandidate: (record) => firstStore.saveDaAttestationCandidate(record),
      recordSubmission: (record) => firstStore.saveL1Submission(record),
      submitter: {
        initAttestation: async () => {
          throw new Error("unexpected init");
        },
        addSignatures: async () => {
          calls.push("add");
          return "addTx";
        },
        applyAttestation: async () => {
          throw new Error("unexpected apply before threshold");
        },
      },
    });
    const firstService = new WatcherService({
      config: configWithDaHash,
      store: firstStore,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadClient: new DaPayloadClient({
        endpoints: ["http://da.example"],
        fetchFn: (async () =>
          new Response(payloadCbor, {
            status: 200,
            headers: { "content-type": "application/cbor" },
          })) as typeof fetch,
      }),
      signer,
      signerValidation,
      coordinator: firstCoordinator,
      transactionProjector: IDENTITY_TX_PROJECTOR,
    });

    await firstService.initialize();
    await expect(firstService.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      skippedHeaders: 0,
      errors: [],
    });
    await expect(
      firstStore.getDaSignature({ headerHash, signerIndex: 0 }),
    ).resolves.toMatchObject({ broadcastStatus: "posted" });

    const restartedStore = await JsonFileWatcherStore.open(dir);
    const restartedCoordinator = new OnChainLifecycleCoordinator({
      threshold: 2,
      visibilityRetryCount: 0,
      chainReader: {
        fetchDaAttestationCandidates: async () => [thresholdCandidate],
      },
      recordCandidate: (record) =>
        restartedStore.saveDaAttestationCandidate(record),
      recordSubmission: (record) => restartedStore.saveL1Submission(record),
      submitter: {
        initAttestation: async () => {
          throw new Error("unexpected init after restart");
        },
        addSignatures: async () => {
          throw new Error("unexpected add after restart");
        },
        applyAttestation: async () => {
          calls.push("apply");
          return "applyTx";
        },
      },
    });
    const restartedService = new WatcherService({
      config: configWithDaHash,
      store: restartedStore,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadClient: new DaPayloadClient({
        endpoints: ["http://da.example"],
        fetchFn: (async () => {
          throw new Error("payload should not be refetched after restart");
        }) as typeof fetch,
      }),
      signer,
      signerValidation,
      coordinator: restartedCoordinator,
      transactionProjector: IDENTITY_TX_PROJECTOR,
    });

    await restartedService.initialize();
    await expect(restartedService.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 0,
      skippedHeaders: 1,
      errors: [],
    });
    expect(calls).toEqual(["add", "apply"]);
    await expect(
      restartedStore.listDaAttestationCandidates(headerHash),
    ).resolves.toMatchObject([
      {
        headerHash,
        outRef: thresholdCandidate.outRef,
        attestationCount: 2,
        status: "threshold",
      },
    ]);
    await expect(restartedStore.listL1Submissions()).resolves.toMatchObject([
      { headerHash, txKind: "add_signatures", txHash: "addTx" },
      { headerHash, txKind: "apply", txHash: "applyTx" },
    ]);
  });
});

const candidateRecord = ({
  headerHash,
  committeeSignersHash,
  attestationCount,
  threshold = 1,
  status = "initialized",
  bitmap = "00".repeat(32),
}: {
  readonly headerHash: string;
  readonly committeeSignersHash: string;
  readonly attestationCount: number;
  readonly threshold?: number;
  readonly status?: DaAttestationCandidateRecord["status"];
  readonly bitmap?: string;
}): DaAttestationCandidateRecord => ({
  deploymentFingerprint: "dep",
  headerHash,
  outRef: "ab".repeat(32) + "#1",
  datumCbor: "d87980",
  attestationCount,
  threshold,
  committeeSignersHash,
  bitmap,
  observedChainPoint: {},
  status,
});
