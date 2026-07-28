import * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { OnChainLifecycleCoordinator } from "../src/coordinator/on-chain.js";
import { SubmitterReconciler } from "../src/coordinator/submitter-reconciler.js";
import { daPayloadSha256 } from "../src/da/payload.js";
import type { DaPayloadCandidate, DaPayloadSource } from "../src/da/source.js";
import type {
  DaAttestationCandidateRecord,
  DaSignatureRecord,
  DaStoredPayloadCountSetV1,
  DaStoredPayloadRootSetV1,
  HeaderV1,
} from "../src/domain.js";
import { PeerSignaturePoller } from "../src/peer/poller.js";
import {
  loadDaSigner,
  signDaAttestation,
  validateDaCommittee,
  validateDaSignerMembership,
} from "../src/signer.js";
import { JsonFileWatcherStore } from "../src/store.js";
import { bytesToHex } from "../src/utils/hex.js";
import { WatcherService } from "../src/watcher.js";
import {
  makeObservedNode,
  makePayloadFixture,
  minimalConfig,
  payloadSourceFromBytes,
  tempDir,
} from "./helpers.js";

const rootSummaryFromHeader = (header: HeaderV1): DaStoredPayloadRootSetV1 => ({
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
});

const countSummaryFromHeader = (
  header: HeaderV1,
): DaStoredPayloadCountSetV1 => ({
  withdrawalCount: header.withdrawalCount,
  forcedTransactionCount: header.forcedTransactionCount,
  l2TransactionCount: header.l2TransactionCount,
  depositCount: header.depositCount,
  totalEventCount: header.totalEventCount,
  transitionStepCount: header.transitionStepCount,
  validationTraceCount: header.validationTraceCount,
});

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
    const payloadSource = payloadSourceFromBytes(payloadCbor);
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadSource,
      signer,
      signerValidation,
    });
    await service.initialize();
    await expect(service.readinessSnapshot()).resolves.toMatchObject({
      ready: false,
      deployment: {
        configuredFingerprint: configWithDaHash.deploymentFingerprint,
        storeFingerprint: configWithDaHash.deploymentFingerprint,
        storeMatchesConfigured: true,
      },
      scanner: { status: "not_started" },
      reasons: ["state queue scanner has not completed a tick"],
    });
    const result = await service.tick();
    expect(result).toMatchObject({ scannedHeaders: 1, signedHeaders: 1 });
    await expect(
      store.getDaSignature({ headerHash, signerIndex: 0 }),
    ).resolves.toMatchObject({ headerHash, signerIndex: 0 });
    await expect(
      service.readinessSnapshot({ localPeerId: "watcher-peer" }),
    ).resolves.toMatchObject({
      ready: true,
      deployment: {
        configuredFingerprint: configWithDaHash.deploymentFingerprint,
        storeFingerprint: configWithDaHash.deploymentFingerprint,
        storeMatchesConfigured: true,
      },
      peer: {
        localPeerId: "watcher-peer",
        l1SubmitterPreflight: { status: "not_required" },
      },
      scanner: { status: "ok", scannedHeaders: 1, signedHeaders: 1 },
      counts: {
        discoveredHeaders: 1,
        missingPayloads: 0,
        verifiedPayloads: 1,
        signatures: 1,
      },
      reasons: [],
    });
  });

  it("fails readiness after a scanner tick failure", async () => {
    const dir = await tempDir();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const store = await JsonFileWatcherStore.open(dir);
    const service = new WatcherService({
      config,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => {
          throw new Error("scanner unavailable");
        },
      },
      payloadSource: failPayloadSource("payload should not be fetched"),
    });
    await service.initialize();

    await expect(service.tick()).rejects.toThrow("scanner unavailable");
    await expect(service.readinessSnapshot()).resolves.toMatchObject({
      ready: false,
      scanner: {
        status: "failed",
        errors: ["scanner unavailable"],
      },
      reasons: ["last state queue scanner tick failed"],
    });
  });

  it("fetches payload bytes from the configured DA payload source", async () => {
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
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadSource: payloadSourceFromBytes(payloadCbor, "producer-peer"),
      signer,
      signerValidation,
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
      sourcePeerId: "producer-peer",
      validationStatus: "verified",
    });
    await expect(
      store.getDaSignature({ headerHash, signerIndex: 0 }),
    ).resolves.toMatchObject({ headerHash, broadcastStatus: "local" });
  });

  it("verifies and signs a locally accepted libp2p payload without refetching", async () => {
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
    await store.saveDaPayload({
      deploymentFingerprint: configWithDaHash.deploymentFingerprint,
      headerHash,
      payloadSchemaVersion: 1,
      payloadCborHex: payloadCbor.toString("hex"),
      payloadSha256: daPayloadSha256(payloadCbor),
      sourcePeerId: "libp2p:payload-submit",
      fetchedAt: new Date().toISOString(),
      validationStatus: "fetched",
    });
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadSource: failPayloadSource("payload source must not be used"),
      signer,
      signerValidation,
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
      sourcePeerId: "libp2p:payload-submit",
      validationStatus: "verified",
      payloadSha256: daPayloadSha256(payloadCbor),
      conflictStatus: "none",
    });
    await expect(
      store.getDaSignature({ headerHash, signerIndex: 0 }),
    ).resolves.toMatchObject({ headerHash, broadcastStatus: "local" });
  });

  it("fails closed for malformed locally accepted payload bytes", async () => {
    const dir = await tempDir();
    const { header, headerHash } = await makePayloadFixture();
    const invalidPayload = Buffer.from("deadbeef", "hex");
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
    await store.saveDaPayload({
      deploymentFingerprint: configWithDaHash.deploymentFingerprint,
      headerHash,
      payloadSchemaVersion: 1,
      payloadCborHex: invalidPayload.toString("hex"),
      payloadSha256: daPayloadSha256(invalidPayload),
      sourcePeerId: "libp2p:payload-submit",
      fetchedAt: new Date().toISOString(),
      validationStatus: "fetched",
    });
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadSource: failPayloadSource("payload source must not be used"),
      signer,
      signerValidation,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 0,
      skippedHeaders: 1,
    });
    await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
      headerHash,
      sourcePeerId: "libp2p:payload-submit",
      validationStatus: "malformed_da",
    });
    await expect(
      store.getDaSignature({ headerHash, signerIndex: 0 }),
    ).resolves.toBeUndefined();
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
      payloadSource: payloadSourceFromCandidates([
        { sourcePeerId: "da-peer-a", payloadCbor },
        { sourcePeerId: "da-peer-b", payloadCbor: conflictingPayload },
      ]),
      signer,
      signerValidation,
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
      sourcePeerId: "da-peer-a,da-peer-b",
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
      payloadSource: {
        fetchPayloadCandidates: async () =>
          payloadAvailable
            ? payloadCandidates([{ sourcePeerId: "da-peer", payloadCbor }])
            : missingPayload("da-peer"),
      },
      signer,
      signerValidation,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 0,
      skippedHeaders: 1,
      payloadFetches: [
        {
          headerHash,
          status: "missing_da",
          sourcePeerIds: ["da-peer"],
          detail: "da-peer:not_found",
        },
      ],
      errors: [],
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
      payloadSource: {
        fetchPayloadCandidates: async () => {
          payloadFetches += 1;
          payloadFetchStarted();
          await payloadGate;
          return payloadCandidates([{ sourcePeerId: "da-peer", payloadCbor }]);
        },
      },
      signer,
      signerValidation,
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
    const payloadSource = payloadSourceFromBytes(payloadCbor);
    const published: string[] = [];
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadSource,
      signer,
      signerValidation,
      coordinator: {
        retryPublishedSignatures: true,
        publishSignature: async (record) => {
          published.push(record.headerHash);
          return "posted";
        },
      },
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
    const payloadSource = payloadSourceFromBytes(payloadCbor);
    const published: string[] = [];
    const service = new WatcherService({
      config: configWithDaHash,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadSource,
      signer,
      signerValidation,
      coordinator: {
        publishSignature: async (record) => {
          published.push(record.headerHash);
          return "post_failed";
        },
      },
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      skippedHeaders: 0,
      errors: [`failed to publish DA signature for ${headerHash} signer 0`],
    });
    await expect(
      store.getDaSignature({ headerHash, signerIndex: 0 }),
    ).resolves.toMatchObject({ broadcastStatus: "post_failed" });

    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 0,
      skippedHeaders: 1,
      errors: [`failed to publish DA signature for ${headerHash} signer 0`],
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
        payloadVersion: Number(SDK.DA_PAYLOAD_V1_VERSION),
        rootsMatch: true,
        stateQueueOutRef: "ab".repeat(32) + "#0",
        headerHash,
        rootSummary: rootSummaryFromHeader(header),
        countSummary: countSummaryFromHeader(header),
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
      payloadSource: failPayloadSource(
        "payload should not be fetched for attested header",
      ),
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
    const payloadSource = payloadSourceFromBytes(payloadCbor);
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
        fetchDaAttestationCandidates: async () =>
          candidateResponses.shift() ?? [],
      },
      recordSubmission: (record) => store.saveL1Submission(record),
      submitter: {
        initAttestation: async () => submitted("initTx"),
        addSignatures: async () => submitted("addTx"),
        applyAttestation: async () => submitted("applyTx"),
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
      payloadSource,
      signer,
      signerValidation,
      coordinator,
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
    const payloadSource = payloadSourceFromBytes(payloadCbor);
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
        fetchDaAttestationCandidates: async () =>
          candidateResponses.shift() ?? [],
      },
      recordSubmission: (record) => store.saveL1Submission(record),
      submitter: {
        initAttestation: async () => {
          calls.push("init");
          return submitted("initTx");
        },
        addSignatures: async ({ signerIndexes }) => {
          calls.push(`add:${signerIndexes.join(",")}`);
          return submitted("addTx");
        },
        applyAttestation: async ({ candidate }) => {
          calls.push(`apply:${candidate.outRef}`);
          return submitted("applyTx");
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
      payloadSource,
      signer,
      signerValidation,
      coordinator,
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
    expect(calls).toEqual(["add:0", `apply:${initialized.outRef}`]);
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
      source: "peer",
      l1ChainPoint: {},
      validation: {
        payloadVersion: Number(SDK.DA_PAYLOAD_V1_VERSION),
        rootsMatch: true,
        stateQueueOutRef: "peer#0",
        headerHash,
        rootSummary: rootSummaryFromHeader(header),
        countSummary: countSummaryFromHeader(header),
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
      source: "peer",
      l1ChainPoint: {},
      validation: {
        payloadVersion: Number(SDK.DA_PAYLOAD_V1_VERSION),
        rootsMatch: true,
        stateQueueOutRef: "stale#0",
        headerHash,
        rootSummary: rootSummaryFromHeader(header),
        countSummary: countSummaryFromHeader(header),
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
        addSignatures: async ({ signerIndexes }) => {
          calls.push(`add:${signerIndexes.join(",")}`);
          return submitted("addTx");
        },
        applyAttestation: async ({ candidate }) => {
          calls.push(`apply:${candidate.outRef}`);
          return submitted("applyTx");
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
      payloadSource: payloadSourceFromBytes(payloadCbor),
      signer,
      signerValidation,
      coordinator,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      skippedHeaders: 0,
      errors: [],
    });
    expect(calls).toEqual(["add:0,1", `apply:${initialized.outRef}`]);
    await expect(store.listL1Submissions()).resolves.toMatchObject([
      { headerHash, txKind: "add_signatures", txHash: "addTx" },
      { headerHash, txKind: "apply", txHash: "applyTx" },
    ]);
  });

  it("submitter-only mode polls peer signatures and initializes the L1 lifecycle", async () => {
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
    const rootSummary = rootSummaryFromHeader(header);
    const countSummary = countSummaryFromHeader(header);
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
        source: "peer",
        l1ChainPoint: {},
        validation: {
          payloadVersion: Number(SDK.DA_PAYLOAD_V1_VERSION),
          rootsMatch: true,
          stateQueueOutRef: "peer#0",
          headerHash,
          rootSummary,
          countSummary,
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
        source: "peer",
        l1ChainPoint: {},
        validation: {
          payloadVersion: Number(SDK.DA_PAYLOAD_V1_VERSION),
          rootsMatch: true,
          stateQueueOutRef: "peer#1",
          headerHash,
          rootSummary,
          countSummary,
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
    const peerId = "peer-libp2p-1";
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
          return submitted("initTx");
        },
        addSignatures: async ({ signerIndexes }) => {
          calls.push(`add:${signerIndexes.join(",")}`);
          return submitted("addTx");
        },
        applyAttestation: async ({ candidate }) => {
          calls.push(`apply:${candidate.outRef}`);
          return submitted("applyTx");
        },
      },
    });
    const peerPoller = new PeerSignaturePoller({
      deploymentFingerprint: config.deploymentFingerprint,
      peers: [{ peerId }],
      attestationExchange: {
        publishAttestation: async () => ({ status: "accepted" }),
        attestationsByHeader: async ({
          deploymentFingerprint,
          headerHash: requestedHeaderHash,
          peer,
        }) =>
          peer.peerId === peerId &&
          deploymentFingerprint === config.deploymentFingerprint &&
          requestedHeaderHash === headerHash
            ? peerSignatures
            : [],
      },
      signerValidation: committeeValidation,
      store,
      requestTimeoutMs: 1000,
    });
    const submitterReconciler = new SubmitterReconciler({
      deploymentFingerprint: config.deploymentFingerprint,
      committeeValidation,
      daAttestationPolicyId: config.daAttestationPolicyId,
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
      payloadSource: payloadSourceFromBytes(payloadCbor),
      submitterReconciler,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 0,
      reconciledHeaders: 1,
      skippedHeaders: 1,
      errors: [],
    });
    expect(calls).toEqual(["init", "add:0,1", `apply:${threshold.outRef}`]);
    await expect(store.listDaSignatures(headerHash)).resolves.toMatchObject([
      { headerHash, signerIndex: 0, source: "peer" },
      { headerHash, signerIndex: 1, source: "peer" },
    ]);
    await expect(store.listL1Submissions()).resolves.toMatchObject([
      { headerHash, txKind: "add_signatures", txHash: "addTx" },
      { headerHash, txKind: "apply", txHash: "applyTx" },
      { headerHash, txKind: "init", txHash: "initTx" },
    ]);
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
      recordCandidate: (record) =>
        firstStore.saveDaAttestationCandidate(record),
      recordSubmission: (record) => firstStore.saveL1Submission(record),
      submitter: {
        initAttestation: async () => {
          throw new Error("unexpected init");
        },
        addSignatures: async () => {
          calls.push("add");
          return submitted("addTx");
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
      payloadSource: payloadSourceFromBytes(payloadCbor),
      signer,
      signerValidation,
      coordinator: firstCoordinator,
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
          return submitted("applyTx");
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
      payloadSource: failPayloadSource(
        "payload should not be refetched after restart",
      ),
      signer,
      signerValidation,
      coordinator: restartedCoordinator,
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

const submitted = (txHash: string) => ({
  status: "submitted" as const,
  txHash,
});

type V1CandidateFixture = Omit<DaPayloadCandidate, "payloadSchemaVersion"> & {
  readonly payloadSchemaVersion?: 1;
};

const payloadSourceFromCandidates = (
  candidates: readonly V1CandidateFixture[],
): DaPayloadSource => ({
  fetchPayloadCandidates: async () => payloadCandidates(candidates),
});

const payloadCandidates = (
  candidates: readonly V1CandidateFixture[],
): Awaited<ReturnType<DaPayloadSource["fetchPayloadCandidates"]>> => ({
  ok: true,
  candidates: candidates.map((candidate) => ({
    ...candidate,
    payloadSchemaVersion: 1,
  })),
  attempts: [],
});

const missingPayload = (
  sourcePeerId: string,
): Awaited<ReturnType<DaPayloadSource["fetchPayloadCandidates"]>> => ({
  ok: false,
  attempts: [
    { sourcePeerId, status: "not_found", detail: "payload not found" },
  ],
});

const failPayloadSource = (message: string): DaPayloadSource => ({
  fetchPayloadCandidates: async () => {
    throw new Error(message);
  },
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
