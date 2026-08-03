import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import { DaGossipTopic } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";
import { afterEach, describe, expect, it, vi } from "vitest";

import { OnChainLifecycleCoordinator } from "../src/coordinator/on-chain.js";
import { SubmitterReconciler } from "../src/coordinator/submitter-reconciler.js";
import { DaPeerRegistry } from "../src/da/libp2p/DaPeerRegistry.js";
import { daPayloadSha256 } from "../src/da/payload.js";
import type { DaPayloadCandidate, DaPayloadSource } from "../src/da/source.js";
import type {
  DaAttestationCandidateRecord,
  DaSignatureRecord,
  DaStoredPayloadCountSetV1,
  DaStoredPayloadRootSetV1,
  HeaderV1,
} from "../src/domain.js";
import type { ChainSyncCursor, ChainSyncEvent } from "../src/l1/provider.js";
import { PeerSignaturePoller } from "../src/peer/poller.js";
import {
  loadDaSigner,
  signDaAttestation,
  validateDaCommittee,
  validateDaSignerMembership,
} from "../src/signer.js";
import {
  DECISION_EFFECT_PENDING_LEASE_MS,
  JsonFileWatcherStore,
} from "../src/store.js";
import { bytesToHex } from "../src/utils/hex.js";
import { WatcherService } from "../src/watcher.js";
import {
  makeObservedNode,
  makePayloadFixture,
  minimalConfig,
  payloadSourceFromBytes,
  tempDir,
} from "./helpers.js";

const openStores = new Set<JsonFileWatcherStore>();

const openJsonWatcherStore = async (
  path: string,
): Promise<JsonFileWatcherStore> => {
  const store = await JsonFileWatcherStore.open(path);
  openStores.add(store);
  return store;
};

afterEach(async () => {
  await Promise.all([...openStores].map(async (store) => store.close()));
  openStores.clear();
});

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
  it("registers the store-backed conflict handler before libp2p startup", async () => {
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
    const registry = DaPeerRegistry.fromConfig(config.daTransport);
    const setGossipHandler = vi.fn();

    new WatcherService({
      config,
      store: await JsonFileWatcherStore.open(dir),
      stateQueueProvider: { fetchStateQueueNodes: async () => [] },
      payloadSource: {
        fetchPayloadCandidates: async () => ({
          ok: false,
          attempts: [],
        }),
      },
      daLibp2pNode: { setGossipHandler },
      daPeerRegistry: registry,
    });

    expect(setGossipHandler).toHaveBeenCalledWith(
      DaGossipTopic.conflicts,
      expect.any(Function),
    );
  });

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
    const store = await openJsonWatcherStore(dir);
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

  it("durably begins signature effects before publish and replays one deterministic effect after an acknowledgement crash", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "61";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configured = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configured.daParams,
      signer,
      signerIndex: 0,
    });
    const firstStore = await openJsonWatcherStore(dir);
    const completeDecisionEffect =
      firstStore.completeDecisionEffect.bind(firstStore);
    let failAcknowledgement = true;
    firstStore.completeDecisionEffect = async (args) => {
      if (failAcknowledgement) {
        failAcknowledgement = false;
        throw new Error("simulated crash after publish before durable ack");
      }
      await completeDecisionEffect(args);
    };
    const publishedWitnesses: string[] = [];
    const first = new WatcherService({
      config: configured,
      store: firstStore,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadSource: payloadSourceFromBytes(payloadCbor),
      signer,
      signerValidation,
      coordinator: {
        publishSignature: async (signature) => {
          publishedWitnesses.push(signature.signatureWitness);
          await expect(
            firstStore.listDecisionOutbox(headerHash),
          ).resolves.toMatchObject([
            {
              effectKind: "signature_publish",
              status: "pending",
              attemptCount: 1,
            },
          ]);
          await expect(firstStore.getL1SourceState()).resolves.toMatchObject({
            status: "healthy",
            observations: [{ headerHash, hasPersistedDecision: true }],
          });
          await expect(
            firstStore.getDaSignature({ headerHash, signerIndex: 0 }),
          ).resolves.toMatchObject({
            headerHash,
            broadcastStatus: "local",
          });
          return "posted";
        },
      },
    });
    await first.initialize();
    await expect(first.tick()).resolves.toMatchObject({
      errors: [
        expect.stringContaining(
          "simulated crash after publish before durable ack",
        ),
      ],
    });
    await expect(
      firstStore.listDecisionOutbox(headerHash),
    ).resolves.toMatchObject([
      {
        effectKind: "signature_publish",
        status: "pending",
        attemptCount: 1,
      },
    ]);

    await firstStore.close();
    const restartedStore = await openJsonWatcherStore(dir);
    const restarted = new WatcherService({
      config: configured,
      store: restartedStore,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadSource: failPayloadSource(
        "durable local signature must suppress payload refetch",
      ),
      signer,
      signerValidation,
      coordinator: {
        publishSignature: async (signature) => {
          publishedWitnesses.push(signature.signatureWitness);
          return "posted";
        },
      },
      now: () =>
        new Date(Date.now() + DECISION_EFFECT_PENDING_LEASE_MS + 1_000),
    });
    await restarted.initialize();
    await expect(restarted.tick()).resolves.toMatchObject({
      signedHeaders: 0,
      skippedHeaders: 1,
      errors: [],
    });
    expect(publishedWitnesses).toHaveLength(2);
    expect(new Set(publishedWitnesses)).toHaveLength(1);
    await expect(
      restartedStore.listDecisionOutbox(headerHash),
    ).resolves.toMatchObject([
      {
        effectKind: "signature_publish",
        status: "published",
        attemptCount: 2,
      },
    ]);
  });

  it("allows only one watcher worker to own a pending external effect", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "62";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configured = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configured.daParams,
      signer,
      signerIndex: 0,
    });
    const store = await openJsonWatcherStore(dir);
    let publishCalls = 0;
    let enteredPublish!: () => void;
    let releasePublish!: () => void;
    const entered = new Promise<void>((resolve) => {
      enteredPublish = resolve;
    });
    const released = new Promise<void>((resolve) => {
      releasePublish = resolve;
    });
    const coordinator = {
      publishSignature: async () => {
        publishCalls += 1;
        enteredPublish();
        await released;
        return "posted" as const;
      },
    };
    const service = () =>
      new WatcherService({
        config: configured,
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
    const first = service();
    const second = service();
    await first.initialize();
    await second.initialize();
    const firstTick = first.tick();
    await entered;
    await expect(second.tick()).rejects.toThrow(
      /pending attempt lease has not expired/u,
    );
    expect(publishCalls).toBe(1);
    releasePublish();
    await expect(firstTick).resolves.toMatchObject({
      signedHeaders: 1,
      errors: [],
    });
    expect(publishCalls).toBe(1);
  });

  it("accepts the canonical unattested-to-attested state queue replacement", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "31";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configured = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configured.daParams,
      signer,
      signerIndex: 0,
    });
    const firstOutRef = `${"ab".repeat(32)}#0`;
    const attestedOutRef = `${"ac".repeat(32)}#1`;
    let sourceView: "unattested" | "attested" | "missing" = "unattested";
    const store = await openJsonWatcherStore(dir);
    const service = new WatcherService({
      config: configured,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () =>
          sourceView === "missing"
            ? []
            : [
                sourceView === "attested"
                  ? makeObservedNode({
                      header,
                      headerHash,
                      daAttestation: configured.daAttestationPolicyId,
                      outRef: attestedOutRef,
                      slot: 2,
                      blockHash: "de".repeat(32),
                      depth: 10,
                    })
                  : makeObservedNode({
                      header,
                      headerHash,
                      outRef: firstOutRef,
                      slot: 1,
                      depth: 10,
                    }),
              ],
      },
      payloadSource: payloadSourceFromBytes(payloadCbor),
      signer,
      signerValidation,
      coordinator: {
        publishSignature: async () => "posted",
      },
    });
    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      errors: [],
    });

    sourceView = "attested";
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 0,
      skippedHeaders: 1,
      errors: [],
    });
    await expect(store.getL1SourceState()).resolves.toMatchObject({
      status: "healthy",
      observations: [
        {
          headerHash,
          stateQueueOutRef: attestedOutRef,
          stateQueueStatus: "attested",
          slot: 2,
          blockHash: "de".repeat(32),
          hasPersistedDecision: true,
        },
      ],
    });

    sourceView = "missing";
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 0,
      signedHeaders: 0,
      errors: [expect.stringContaining("decision_disappeared")],
    });
    await expect(store.getL1SourceState()).resolves.toMatchObject({
      status: "quarantined",
      quarantineReason: expect.stringContaining("decision_disappeared"),
      observations: [
        {
          headerHash,
          stateQueueStatus: "attested",
          stateQueueOutRef: attestedOutRef,
        },
      ],
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
    const store = await openJsonWatcherStore(dir);
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

  it("persists L1 disappearance quarantine across restart and prevents processing or rebroadcast", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "21";
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
    const firstStore = await openJsonWatcherStore(dir);
    const first = new WatcherService({
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
      coordinator: {
        publishSignature: async () => "posted",
      },
    });
    await first.initialize();
    await expect(first.tick()).resolves.toMatchObject({ signedHeaders: 1 });
    await expect(firstStore.getL1SourceState()).resolves.toMatchObject({
      status: "healthy",
      observations: [{ headerHash, hasPersistedDecision: true }],
    });
    await firstStore.saveL1Submission({
      deploymentFingerprint: configWithDaHash.deploymentFingerprint,
      headerHash,
      txKind: "init",
      txHash: "71".repeat(32),
      inputsUsed: [],
      submittedAt: "2026-07-28T00:00:00.000Z",
      resultStatus: "submitted",
    });
    await firstStore.savePeerBroadcast({
      deploymentFingerprint: configWithDaHash.deploymentFingerprint,
      peerId: "peer-a",
      headerHash,
      signerIndex: 0,
      status: "pending",
      attempts: 1,
      nextAttemptAt: "2026-07-28T00:01:00.000Z",
      updatedAt: "2026-07-28T00:00:00.000Z",
    });

    await firstStore.close();
    const restartedStore = await openJsonWatcherStore(dir);
    const disappeared = new WatcherService({
      config: configWithDaHash,
      store: restartedStore,
      stateQueueProvider: { fetchStateQueueNodes: async () => [] },
      payloadSource: failPayloadSource("quarantine must suppress DA fetch"),
      signer,
      signerValidation,
    });
    await disappeared.initialize();
    await expect(disappeared.tick()).resolves.toMatchObject({
      scannedHeaders: 0,
      signedHeaders: 0,
      errors: [expect.stringContaining("decision_disappeared")],
    });
    await expect(restartedStore.getL1SourceState()).resolves.toMatchObject({
      status: "quarantined",
      quarantineReason: expect.stringContaining("decision_disappeared"),
    });
    await expect(
      restartedStore.getStateQueueHeader(headerHash),
    ).resolves.toMatchObject({
      status: "conflicted",
      validationErrors: [expect.stringContaining("l1_source_quarantined")],
    });
    await expect(
      restartedStore.getDaPayload(headerHash),
    ).resolves.toMatchObject({
      validationStatus: "conflicted",
      validationError: expect.stringContaining("l1_source_quarantined"),
    });
    await expect(
      restartedStore.getDaSignature({ headerHash, signerIndex: 0 }),
    ).resolves.toMatchObject({ broadcastStatus: "post_failed" });
    await expect(restartedStore.listL1Submissions()).resolves.toMatchObject([
      {
        headerHash,
        resultStatus: "failed",
        failureCause: expect.stringContaining("l1_source_quarantined"),
      },
    ]);
    const [quarantinedBroadcast] =
      await restartedStore.listPeerBroadcasts(headerHash);
    expect(quarantinedBroadcast).toMatchObject({
      status: "failed",
      lastError: expect.stringContaining("l1_source_quarantined"),
    });
    expect(quarantinedBroadcast).not.toHaveProperty("nextAttemptAt");
    await expect(
      restartedStore.listDecisionOutbox(headerHash),
    ).resolves.toMatchObject([
      {
        status: "failed",
        lastError: expect.stringContaining("l1_source_quarantined"),
        quarantineReason: expect.stringContaining("decision_disappeared"),
        quarantinedAt: expect.any(String),
      },
    ]);

    let publishCalls = 0;
    await restartedStore.close();
    const afterQuarantine = new WatcherService({
      config: configWithDaHash,
      store: await openJsonWatcherStore(dir),
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadSource: failPayloadSource("quarantine must survive restart"),
      signer,
      signerValidation,
      coordinator: {
        retryPublishedSignatures: true,
        publishSignature: async () => {
          publishCalls += 1;
          return "posted";
        },
      },
    });
    await afterQuarantine.initialize();
    await expect(afterQuarantine.tick()).resolves.toMatchObject({
      scannedHeaders: 0,
      errors: [expect.stringContaining("L1 source quarantined")],
    });
    expect(publishCalls).toBe(0);
  });

  it("consumes a restarted local-node rollback feed and quarantines a persisted decision", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "23";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configured = {
      ...config,
      l1Source: {
        sourceMode: "local_node" as const,
        authorityNodeId: "node-a",
        chainSyncProviderUrl: "chain-sync:ogmios:ws://ogmios.local",
        chainSyncCursorPath: `${dir}/chain-sync.json`,
        queryProviderUrls: ["kupmios:http://kupo.local|ws://ogmios.local"],
      },
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configured.daParams,
      signer,
      signerIndex: 0,
    });
    const initialPoint = {
      network: "Preview",
      slot: 10,
      blockHash: "11".repeat(32),
      providerSource: "chain-sync:node-a",
      observedAt: "2026-07-28T00:00:00.000Z",
    };
    const initialCursor: ChainSyncCursor = {
      sequence: 0,
      point: initialPoint,
      rollbackGeneration: 0,
    };
    let consumedCursor: ChainSyncCursor | undefined;
    const firstProvider = {
      fetchStateQueueNodes: async () => [
        makeObservedNode({
          header,
          headerHash,
          depth: 10,
          slot: 10,
          blockHash: "11".repeat(32),
        }),
      ],
      currentChainSyncCursor: async () => initialCursor,
      replayChainSyncEvents: async () => [],
      loadConsumedChainSyncCursor: async () => consumedCursor,
      acknowledgeChainSyncCursor: async (cursor: ChainSyncCursor) => {
        consumedCursor = cursor;
      },
    };
    const firstStore = await openJsonWatcherStore(dir);
    const first = new WatcherService({
      config: configured,
      store: firstStore,
      stateQueueProvider: firstProvider,
      payloadSource: payloadSourceFromBytes(payloadCbor),
      signer,
      signerValidation,
    });
    await first.initialize();
    await expect(first.tick()).resolves.toMatchObject({ signedHeaders: 1 });
    await expect(firstStore.getL1SourceState()).resolves.toMatchObject({
      status: "healthy",
      observations: [
        {
          headerHash,
          slot: 10,
          blockHash: "11".repeat(32),
          hasPersistedDecision: true,
        },
      ],
    });
    expect(consumedCursor).toEqual(initialCursor);

    await firstStore.close();
    const rollbackPoint = {
      network: "Preview",
      slot: 9,
      blockHash: "22".repeat(32),
      providerSource: "chain-sync:node-a",
      observedAt: "1970-01-01T00:00:00.000Z",
    };
    const currentPoint = {
      ...rollbackPoint,
      slot: 12,
      blockHash: "33".repeat(32),
    };
    const currentCursor: ChainSyncCursor = {
      sequence: 2,
      point: currentPoint,
      rollbackGeneration: 1,
    };
    const rollbackEvents: readonly ChainSyncEvent[] = [
      { direction: "roll_backward", point: rollbackPoint },
      { direction: "roll_forward", point: currentPoint },
    ];
    const replayChainSyncEvents = async (
      afterSequence: number,
    ): Promise<readonly ChainSyncEvent[]> => {
      expect(afterSequence).toBe(initialCursor.sequence);
      return rollbackEvents;
    };
    const restartedProvider = {
      fetchStateQueueNodes: async () => [
        makeObservedNode({
          header,
          headerHash,
          depth: 10,
          slot: 10,
          blockHash: "11".repeat(32),
        }),
      ],
      currentChainSyncCursor: async () => currentCursor,
      replayChainSyncEvents,
      loadConsumedChainSyncCursor: async () => consumedCursor,
      acknowledgeChainSyncCursor: async () => {
        throw new Error("quarantined rollback must not be acknowledged");
      },
    };
    const restartedStore = await openJsonWatcherStore(dir);
    const restarted = new WatcherService({
      config: configured,
      store: restartedStore,
      stateQueueProvider: restartedProvider,
      payloadSource: failPayloadSource(
        "rollback quarantine must suppress DA fetch",
      ),
      signer,
      signerValidation,
    });
    await restarted.initialize();

    await expect(restarted.tick()).resolves.toMatchObject({
      scannedHeaders: 0,
      signedHeaders: 0,
      errors: [expect.stringContaining("chain_sync_rollback")],
    });
    await expect(restartedStore.getL1SourceState()).resolves.toMatchObject({
      status: "quarantined",
      quarantineReason: expect.stringContaining(
        `l1_source_chain_sync_rollback:${headerHash}:9:${"22".repeat(32)}`,
      ),
    });
  });

  it("quarantines restart state when the exact L1 authority changes", async () => {
    const dir = await tempDir();
    const seed = "00".repeat(31) + "25";
    const signer = await loadDaSigner(`hex:${seed}`);
    const baseConfig = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const config = {
      ...baseConfig,
      l1Source: {
        sourceMode: "local_node" as const,
        authorityNodeId: "fixture-node",
        chainSyncProviderUrl: "chain-sync:fixture:/tmp/state-queue.json",
        chainSyncCursorPath: `${dir}/chain-sync.json`,
        queryProviderUrls: ["fixture:/tmp/state-queue.json"],
      },
    };
    const firstStore = await openJsonWatcherStore(dir);
    const first = new WatcherService({
      config,
      store: firstStore,
      stateQueueProvider: { fetchStateQueueNodes: async () => [] },
      payloadSource: failPayloadSource("no payload should be fetched"),
    });
    await first.initialize();
    await expect(first.tick()).resolves.toMatchObject({ scannedHeaders: 0 });
    const persisted = await firstStore.getL1SourceState();
    expect(persisted).toMatchObject({
      status: "healthy",
      authoritySha256: expect.stringMatching(/^[0-9a-f]{64}$/u),
    });

    await firstStore.close();
    const restartedStore = await openJsonWatcherStore(dir);
    const restarted = new WatcherService({
      config: {
        ...config,
        l1Source: {
          ...config.l1Source,
          authorityNodeId: "replacement-node",
        },
      },
      store: restartedStore,
      stateQueueProvider: { fetchStateQueueNodes: async () => [] },
      payloadSource: failPayloadSource("authority drift must fail at startup"),
    });
    await expect(restarted.initialize()).rejects.toThrow(
      /does not match configured source mode\/network/u,
    );
    const quarantined = await restartedStore.getL1SourceState();
    expect(quarantined).toMatchObject({
      status: "quarantined",
      quarantineReason: expect.stringContaining(
        "l1_source_configuration_changed",
      ),
      authoritySha256: expect.stringMatching(/^[0-9a-f]{64}$/u),
    });
    expect(quarantined?.authoritySha256).not.toBe(persisted?.authoritySha256);
  });

  it("quarantines a persisted decision when a stale query view loses finality", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "22";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configured = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configured.daParams,
      signer,
      signerIndex: 0,
    });
    const firstStore = await openJsonWatcherStore(dir);
    const first = new WatcherService({
      config: configured,
      store: firstStore,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      },
      payloadSource: payloadSourceFromBytes(payloadCbor),
      signer,
      signerValidation,
    });
    await first.initialize();
    await expect(first.tick()).resolves.toMatchObject({ signedHeaders: 1 });

    let publishCalls = 0;
    await firstStore.close();
    const stale = new WatcherService({
      config: configured,
      store: await openJsonWatcherStore(dir),
      stateQueueProvider: {
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 0 }),
        ],
      },
      payloadSource: failPayloadSource("stale L1 view must not fetch DA"),
      signer,
      signerValidation,
      coordinator: {
        retryPublishedSignatures: true,
        publishSignature: async () => {
          publishCalls += 1;
          return "posted";
        },
      },
    });
    await stale.initialize();
    await expect(stale.tick()).resolves.toMatchObject({
      scannedHeaders: 0,
      errors: [expect.stringContaining("lost_finality")],
    });
    expect(publishCalls).toBe(0);
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
    const store = await openJsonWatcherStore(dir);
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
    const store = await openJsonWatcherStore(dir);
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
    // A payload-submit ACK can retain a canonical outer envelope whose inner
    // body is malformed.  The watcher remains the sole semantic gate.
    const invalidPayload = await wrapDaPayloadV1(
      Buffer.from("deadbeef", "hex"),
      { mode: "identity" },
    );
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
    const store = await openJsonWatcherStore(dir);
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
    const store = await openJsonWatcherStore(dir);
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
    const store = await openJsonWatcherStore(dir);
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
    const store = await openJsonWatcherStore(dir);
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
    const store = await openJsonWatcherStore(dir);
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
    const store = await openJsonWatcherStore(dir);
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
    const store = await openJsonWatcherStore(dir);
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
      l1ChainPoint: {
        slot: 1,
        blockHash: "cd".repeat(32),
        depth: 10,
        providerSource: "fixture",
      },
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
    const store = await openJsonWatcherStore(dir);
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
    const store = await openJsonWatcherStore(dir);
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
    const store = await openJsonWatcherStore(dir);
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
    const store = await openJsonWatcherStore(dir);
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
    const firstStore = await openJsonWatcherStore(dir);
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

    await firstStore.close();
    const restartedStore = await openJsonWatcherStore(dir);
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
