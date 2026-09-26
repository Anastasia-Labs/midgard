import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DaGossipTopic } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";
import { afterEach, describe, expect, it, vi } from "vitest";

import { CommitteeService } from "../src/committee-service.js";
import type { CommitteeConfig } from "../src/config.js";
import { OnChainLifecycleCoordinator } from "../src/coordinator/on-chain.js";
import { SubmitterReconciler } from "../src/coordinator/submitter-reconciler.js";
import { DaPeerRegistry } from "../src/da/libp2p/DaPeerRegistry.js";
import { daPayloadSha256 } from "../src/da/payload.js";
import type { DaPayloadCandidate, DaPayloadSource } from "../src/da/source.js";
import type {
  DaAttestationCandidateRecord,
  DaSignatureRecord,
  DaStoredPayloadCountSet,
  DaStoredPayloadRootSet,
  Header,
} from "../src/domain.js";
import type { ChainSyncCursor, ChainSyncEvent } from "../src/l1/provider.js";
import { L1SourceIntegrityError } from "../src/l1/source-integrity.js";
import {
  hashBlockHeader,
  type StateQueueProvider,
  stateQueueReplayWalkLimit,
} from "../src/l1/state-queue-scanner.js";
import { PeerSignaturePoller } from "../src/peer/poller.js";
import {
  type DaAvailabilityCommitmentAuthority,
  deriveExpectedDaAvailabilityCommitment,
} from "../src/peer/signatures.js";
import {
  loadDaSigner,
  signDaAttestation,
  validateDaCommittee,
  validateDaSignerMembership,
} from "../src/signer.js";
import {
  DECISION_EFFECT_PENDING_LEASE_MS,
  JsonFileCommitteeStore,
  UNKNOWN_STATE_QUEUE_STATUS,
} from "../src/store.js";
import {
  createCommitteeTickRunner,
  L1_VIEW_UNAVAILABLE_EXIT_CODE,
} from "../src/tick-runner.js";
import { bytesToHex } from "../src/utils/hex.js";
import {
  makeObservedNode,
  makePayloadFixture,
  minimalConfig,
  payloadSourceFromBytes,
  tempDir,
} from "./helpers.js";
import { withFinalSnapshot } from "./helpers/final-snapshot.js";
import {
  type ChainHeader,
  createStateQueueChain,
} from "./helpers/state-queue-chain.js";

const openStores = new Set<JsonFileCommitteeStore>();

const openJsonCommitteeStore = async (
  path: string,
): Promise<JsonFileCommitteeStore> => {
  const store = await JsonFileCommitteeStore.open(path);
  openStores.add(store);
  return store;
};

afterEach(async () => {
  await Promise.all([...openStores].map(async (store) => store.close()));
  openStores.clear();
});

type CommitmentConfig = Pick<
  CommitteeConfig,
  "hubOraclePolicyId" | "availabilityChallenge"
>;

const commitmentAuthority = (
  config: CommitmentConfig,
): DaAvailabilityCommitmentAuthority => ({
  deploymentIdentity: config.hubOraclePolicyId,
  bondOwnerCredential: config.availabilityChallenge.bondOwnerCredential,
  responseGeometry: config.availabilityChallenge.responseGeometry,
});

const expectedCommitment = (
  config: CommitmentConfig,
  headerHash: string,
  payloadCbor: Buffer,
) =>
  deriveExpectedDaAvailabilityCommitment({
    authority: commitmentAuthority(config),
    headerHash,
    payloadCborHex: payloadCbor.toString("hex"),
  });

/**
 * Moves the persisted replay anchor to `queue`, as if it had run ahead of the
 * persisted decision observations: authenticated replay from it then carries
 * no step for any output change before it. No honest tick does this; it
 * reaches the decision-transition check behind a successful scan.
 */
const runAnchorAheadOfObservations = async (
  store: JsonFileCommitteeStore,
  queue: SDK.StateQueueTransitionNode[],
): Promise<void> => {
  const state = (await store.getL1SourceState())!;
  await store.saveL1SourceState({
    ...state,
    stateQueueReplayAnchor: { ...state.stateQueueReplayAnchor!, queue },
  });
};

const attestedDaStatus = (): SDK.DaAvailabilityStateQueueStatus => ({
  Attested: { da_bond_asset_name: "aa".repeat(32) },
});

const rootSummaryFromHeader = (header: Header): DaStoredPayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
});

const countSummaryFromHeader = (header: Header): DaStoredPayloadCountSet => ({
  withdrawalCount: header.withdrawalCount,
  forcedTransactionCount: header.forcedTransactionCount,
  l2TransactionCount: header.l2TransactionCount,
  depositCount: header.depositCount,
  totalEventCount: header.totalEventCount,
  transitionStepCount: header.transitionStepCount,
  validationTraceCount: header.validationTraceCount,
});

describe("CommitteeService", () => {
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

    new CommitteeService({
      config,
      store: await JsonFileCommitteeStore.open(dir),
      stateQueueProvider: { fetchStateQueueNodes: async () => [] },
      payloadSource: {
        fetchPayloadCandidates: async () => ({
          ok: false,
          attempts: [],
        }),
      },
      daLibp2pNode: { setGossipHandler, publishGossip: vi.fn() },
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
    const store = await openJsonCommitteeStore(dir);
    const payloadSource = payloadSourceFromBytes(payloadCbor);
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
      store.getDaSignature({
        headerHash,
        availabilityCommitmentDigest: expectedCommitment(
          configWithDaHash,
          headerHash,
          payloadCbor,
        ).commitmentDigest,
        signerIndex: 0,
      }),
    ).resolves.toMatchObject({ headerHash, signerIndex: 0 });
    await expect(
      service.readinessSnapshot({ localPeerId: "committee-peer" }),
    ).resolves.toMatchObject({
      ready: true,
      deployment: {
        configuredFingerprint: configWithDaHash.deploymentFingerprint,
        storeFingerprint: configWithDaHash.deploymentFingerprint,
        storeMatchesConfigured: true,
      },
      peer: {
        localPeerId: "committee-peer",
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
    await expect(
      service.readinessSnapshot({
        localPeerId: "committee-peer",
        retention: {
          status: "alerting",
          checkedAt: "2026-08-29T00:00:00.000Z",
          scanned: 1,
          retained: 1,
          prunable: 0,
          alerting: 1,
        },
      }),
    ).resolves.toMatchObject({
      ready: false,
      retention: { status: "alerting", alerting: 1 },
      reasons: expect.arrayContaining(["retention_deadline_alert:1"]),
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
    const firstStore = await openJsonCommitteeStore(dir);
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
    const first = new CommitteeService({
      config: configured,
      store: firstStore,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
            firstStore.getDaSignature({
              headerHash,
              availabilityCommitmentDigest: expectedCommitment(
                configured,
                headerHash,
                payloadCbor,
              ).commitmentDigest,
              signerIndex: 0,
            }),
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
    const restartedStore = await openJsonCommitteeStore(dir);
    const restarted = new CommitteeService({
      config: configured,
      store: restartedStore,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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

  it("allows only one committee node worker to own a pending external effect", async () => {
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
    const store = await openJsonCommitteeStore(dir);
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
      new CommitteeService({
        config: configured,
        store,
        stateQueueProvider: withFinalSnapshot({
          fetchStateQueueNodes: async () => [
            makeObservedNode({ header, headerHash, depth: 10 }),
          ],
        }),
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

  it("quarantines an attested replacement of a signed header that authenticated replay from the durable anchor does not explain", async () => {
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
    let sourceView: "unattested" | "attested" = "unattested";
    const store = await openJsonCommitteeStore(dir);
    const service = new CommitteeService({
      config: configured,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          sourceView === "attested"
            ? makeObservedNode({
                header,
                headerHash,
                daAttestation: attestedDaStatus(),
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
      }),
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

    // The scan of the replaced output succeeds from an anchor already past
    // it, but no authenticated step leads from the signed output to it.
    await expect(store.getL1SourceState()).resolves.toMatchObject({
      stateQueueReplayAnchor: {
        queue: [
          { headerHash: null, outRef: `${"00".repeat(32)}#0` },
          { headerHash, outRef: firstOutRef },
        ],
      },
    });
    await runAnchorAheadOfObservations(store, [
      { headerHash: null, outRef: `${"00".repeat(32)}#0` },
      { headerHash, outRef: attestedOutRef },
    ]);
    sourceView = "attested";
    await expect(service.tick()).resolves.toMatchObject({
      signedHeaders: 0,
      errors: [expect.stringContaining("decision_forked")],
    });
    await expect(store.getL1SourceState()).resolves.toMatchObject({
      status: "quarantined",
      quarantineReason: expect.stringContaining("decision_forked"),
      observations: [
        {
          headerHash,
          stateQueueOutRef: firstOutRef,
          stateQueueStatus: "unattested",
          hasPersistedDecision: true,
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
    const store = await openJsonCommitteeStore(dir);
    const service = new CommitteeService({
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
      reasons: [
        "last state queue scanner tick failed",
        "L1 state queue has no durable replay anchor yet: no decision is made until its history is final",
      ],
    });
  });

  describe("L1 observation versus integrity failures", () => {
    /**
     * A local-node committee that has signed nothing yet. `source.failure`,
     * when set, is thrown by the state-queue snapshot read; `consumedCursor`
     * is the durable rollback-feed consumer cursor.
     */
    const observedCommittee = async (seedByte: string) => {
      const dir = await tempDir();
      const { header, headerHash, payloadCbor } = await makePayloadFixture();
      const seed = "00".repeat(31) + seedByte;
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
      const cursor: ChainSyncCursor = {
        sequence: 0,
        point: {
          network: "Preprod",
          slot: 10,
          blockHash: "11".repeat(32),
          providerSource: "chain-sync:node-a",
          observedAt: "2026-07-28T00:00:00.000Z",
        },
        rollbackGeneration: 0,
      };
      const node = makeObservedNode({
        header,
        headerHash,
        depth: 10,
        slot: 10,
        blockHash: "11".repeat(32),
      });
      const source: {
        failure?: Error;
        consumedCursor?: ChainSyncCursor;
        nowMs: number;
      } = { nowMs: Date.parse("2026-07-28T00:00:00.000Z") };
      const stateQueueProvider = {
        fetchStateQueueNodes: async () => [node],
        fetchStateQueueSnapshot: async () => {
          if (source.failure !== undefined) throw source.failure;
          return {
            nodes: [node],
            confirmedHeaderHash: "00".repeat(28),
            confirmedStateOutRef: `${"00".repeat(32)}#0`,
            observedChainPoint: node.chainPoint,
          };
        },
        currentChainSyncCursor: async () => cursor,
        replayChainSyncEvents: async () => [],
        loadConsumedChainSyncCursor: async () => source.consumedCursor,
        acknowledgeChainSyncCursor: async (next: ChainSyncCursor) => {
          source.consumedCursor = next;
        },
      };
      const store = await openJsonCommitteeStore(dir);
      const service = new CommitteeService({
        config: configured,
        store,
        stateQueueProvider,
        payloadSource: payloadSourceFromBytes(payloadCbor),
        signer,
        signerValidation,
        coordinator: { publishSignature: async () => "posted" },
        now: () => new Date(source.nowMs),
      });
      await service.initialize();
      /** Everything a tick could persist about the L1 source and the header. */
      const durable = async () => ({
        l1Source: await store.getL1SourceState(),
        header: await store.getStateQueueHeader(headerHash),
        payload: await store.getDaPayload(headerHash),
      });
      return { service, store, source, headerHash, durable };
    };

    // The five transients observed live, each as its provider raises it.
    const transients = [
      "Kupmios query surfaces are not aligned after 8 reads: Kupo=100:" +
        "33".repeat(32) +
        ", Ogmios=101:" +
        "44".repeat(32),
      "Ogmios replay tip height is invalid",
      "local_node query surface query:node-a:0 changed chain point while its snapshot was read",
      "Ogmios chain-sync request timed out",
      "Kupmios chain point changed while deriving block confirmations",
    ];

    it.each(transients)(
      "fails only the tick on a transient (%s) and resumes on the next healthy tick",
      async (message) => {
        const { service, source, durable } = await observedCommittee("41");

        // Before any decision exists.
        source.failure = new Error(message);
        const beforeFirst = await durable();
        await expect(service.tick()).rejects.toThrow(message);
        expect(await durable()).toEqual(beforeFirst);
        expect(service.latestL1View()).toBeUndefined();

        source.failure = undefined;
        await expect(service.tick()).resolves.toMatchObject({
          scannedHeaders: 1,
          signedHeaders: 1,
          errors: [],
        });
        const view = service.latestL1View();
        expect(view).toBeDefined();

        // With a persisted decision the transient still quarantines nothing.
        source.failure = new Error(message);
        source.nowMs += 60_000;
        const beforeSecond = await durable();
        expect(beforeSecond.l1Source).toMatchObject({
          status: "healthy",
          observations: [{ hasPersistedDecision: true }],
        });
        await expect(service.tick()).rejects.toThrow(message);
        const afterSecond = await durable();
        expect(afterSecond).toEqual(beforeSecond);
        expect(afterSecond.l1Source?.status).toBe("healthy");
        expect(afterSecond.header?.status).not.toBe("conflicted");
        expect(afterSecond.payload?.validationStatus).not.toBe("conflicted");
        // No fresh L1 view was accepted, so its age keeps growing.
        expect(service.latestL1View()).toBe(view);
        await expect(service.readinessSnapshot()).resolves.toMatchObject({
          scanner: { status: "failed", errors: [message] },
        });

        source.failure = undefined;
        await expect(service.tick()).resolves.toMatchObject({
          scannedHeaders: 1,
          signedHeaders: 0,
          errors: [],
        });
        await expect(service.readinessSnapshot()).resolves.toMatchObject({
          scanner: { status: "ok", errors: [] },
        });
        expect(service.latestL1View()?.observedAtMs).toBe(source.nowMs);
        expect((await durable()).l1Source?.status).toBe("healthy");
      },
    );

    it("quarantines a persisted decision on an L1 source integrity failure", async () => {
      const { service, source, headerHash, durable } =
        await observedCommittee("42");
      await expect(service.tick()).resolves.toMatchObject({
        signedHeaders: 1,
      });

      source.failure = new L1SourceIntegrityError(
        "state queue root provider disagreement in local_node mode",
      );
      await expect(service.tick()).rejects.toThrow(
        /state queue root provider disagreement/u,
      );
      const after = await durable();
      expect(after.l1Source).toMatchObject({
        status: "quarantined",
        quarantineReason:
          "l1_source_integrity_failed: state queue root provider disagreement in local_node mode",
      });
      expect(after.header).toMatchObject({
        headerHash,
        status: "conflicted",
        validationErrors: [expect.stringContaining("l1_source_quarantined")],
      });

      // The quarantine is persisted: a healthy source no longer clears it.
      source.failure = undefined;
      await expect(service.tick()).resolves.toMatchObject({
        scannedHeaders: 0,
        signedHeaders: 0,
        errors: [expect.stringContaining("l1_source_integrity_failed")],
      });
    });

    it("quarantines a persisted decision whose rollback feed lost its durable consumer cursor", async () => {
      const { service, source, durable } = await observedCommittee("43");
      await expect(service.tick()).resolves.toMatchObject({
        signedHeaders: 1,
      });
      expect(source.consumedCursor).toBeDefined();

      source.consumedCursor = undefined;
      await expect(service.tick()).rejects.toThrow(
        /lack a durable chain-sync consumer cursor/u,
      );
      await expect(durable()).resolves.toMatchObject({
        l1Source: {
          status: "quarantined",
          quarantineReason: expect.stringContaining(
            "l1_source_rollback_feed_failed: persisted L1 decisions lack a durable chain-sync consumer cursor",
          ),
        },
        header: { status: "conflicted" },
      });
    });

    it("lets a sustained observation failure reach the L1-view fatal exit without quarantining", async () => {
      const { service, source, durable } = await observedCommittee("44");
      const fatalMs = 240_000;
      const exits: number[] = [];
      const runner = createCommitteeTickRunner({
        tick: async () => service.tick(),
        runAvailabilityResponse: async () => undefined,
        runRetention: async () => undefined,
        latestL1View: () => service.latestL1View(),
        latestL1ProgressAtMs: () => service.latestL1ProgressAtMs(),
        setRetentionReadiness: () => undefined,
        l1ViewFatalMs: fatalMs,
        startedAtMs: source.nowMs,
        nowMs: () => source.nowMs,
        write: () => undefined,
        shutdown: async () => undefined,
        exit: (code) => exits.push(code),
        shutdownGraceMs: 10,
      });
      await runner.runTick();
      expect(service.latestL1View()?.observedAtMs).toBe(source.nowMs);

      source.failure = new Error("Ogmios chain-sync request timed out");
      for (let elapsed = 0; elapsed < fatalMs; elapsed += 60_000) {
        source.nowMs += 60_000;
        await runner.runTick();
        expect(exits).toEqual([]);
      }
      source.nowMs += 1;
      await runner.runTick();
      expect(exits).toEqual([L1_VIEW_UNAVAILABLE_EXIT_CODE]);
      expect((await durable()).l1Source?.status).toBe("healthy");
    });
  });

  describe("L1 history younger than the finality depth", () => {
    const finalityDepth = 3;
    const blockMs = 20_000;

    /**
     * A committee over a live state queue whose genesis (block 1) holds only
     * `signed`, an unattested header this committee signs, so it is the tail
     * the first append continues; with `signedIsTail` false an attested
     * header follows it, so appends leave it alone. `spare` are attested
     * headers to append. `service(false)` builds a committee that signs
     * nothing; `events` holds the structured events the committee logged.
     */
    const chainCommittee = async (
      seedByte: string,
      tip: number,
      spareCount = 8,
      signedIsTail = true,
    ) => {
      const dir = await tempDir();
      const { header, headerHash, payloadCbor } = await makePayloadFixture();
      const seed = "00".repeat(31) + seedByte;
      const signer = await loadDaSigner(`hex:${seed}`);
      const base = minimalConfig({
        dir,
        manifestPath: `${dir}/manifest.json`,
        deploymentInfoPath: `${dir}/deployment.json`,
        signerSeed: seed,
        signerPublicKey: signer.publicKeyHex,
      });
      const config = {
        ...base,
        finalityDepth,
        daParams: {
          ...base.daParams,
          committeeSignersHash: bytesToHex(
            blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
          ),
        },
      };
      const signerValidation = validateDaSignerMembership({
        daParams: config.daParams,
        signer,
        signerIndex: 0,
      });
      const attested = Array.from(
        { length: spareCount + 1 },
        (_, index): ChainHeader => {
          const variant = {
            ...header,
            endTime: header.endTime + 1n + BigInt(index),
          };
          return {
            header: variant,
            headerHash: hashBlockHeader(variant),
            daAttestation: {
              Attested: { da_bond_asset_name: "33".repeat(32) },
            },
          };
        },
      );
      const chain = createStateQueueChain({
        deploymentIdentityDigest: config.deploymentFingerprint,
        stateQueuePolicyId: config.stateQueuePolicyId,
        headers: [
          { header, headerHash },
          ...(signedIsTail ? [] : [attested[spareCount]!]),
        ],
        tip,
      });
      const clock = { nowMs: Date.parse("2026-07-28T00:00:00.000Z") };
      const store = await openJsonCommitteeStore(dir);
      const events: Record<string, unknown>[] = [];
      const chainProvider: StateQueueProvider = {
        fetchStateQueueNodes: async () => chain.snapshot().nodes,
        fetchStateQueueSnapshot: async () => chain.snapshot(),
        fetchStateQueueReplayCheckpoints:
          chain.fetchStateQueueReplayCheckpoints,
      };
      const service = (withSigner = true, provider = chainProvider) =>
        new CommitteeService({
          config,
          store,
          stateQueueProvider: provider,
          payloadSource: payloadSourceFromBytes(payloadCbor),
          ...(withSigner
            ? {
                signer,
                signerValidation,
                coordinator: {
                  // Republishing each tick makes every tick bind a decision
                  // to the signed header's current output.
                  retryPublishedSignatures: true,
                  publishSignature: async () => "posted" as const,
                },
              }
            : {}),
          now: () => new Date(clock.nowMs),
          writeEvent: (line) => {
            events.push(JSON.parse(line) as Record<string, unknown>);
          },
        });
      return {
        chain,
        clock,
        store,
        service,
        config,
        events,
        chainProvider,
        signed: headerHash,
        spare: attested.slice(0, spareCount),
      };
    };

    it("keeps ticking with a fresh L1 view while appends land every block, and records a merge once it is final", async () => {
      const { chain, clock, store, service, signed, spare } =
        await chainCommittee("51", 5);
      const committee = service();
      await committee.initialize();
      const results: Awaited<ReturnType<CommitteeService["tick"]>>[] = [];
      const exits: number[] = [];
      const runner = createCommitteeTickRunner({
        tick: async () => {
          const result = await committee.tick();
          results.push(result);
          return result;
        },
        runAvailabilityResponse: async () => undefined,
        runRetention: async () => undefined,
        latestL1View: () => committee.latestL1View(),
        latestL1ProgressAtMs: () => committee.latestL1ProgressAtMs(),
        setRetentionReadiness: () => undefined,
        l1ViewFatalMs: 3 * blockMs,
        startedAtMs: clock.nowMs,
        nowMs: () => clock.nowMs,
        write: () => undefined,
        shutdown: async () => undefined,
        exit: (code) => exits.push(code),
        shutdownGraceMs: 10,
      });

      // The signed header is final and the tail when it is signed.
      await runner.runTick();
      expect(results[0]!.signedHeaders).toBe(1);
      results.length = 0;
      // Every block appends a header, the first continuing the signed tail;
      // one of them also merges the signed header. The tail is never final.
      const blocks = spare
        .slice(0, 7)
        .map((header, index) =>
          index === 1
            ? ["merge" as const, { append: header }]
            : [{ append: header }],
        );
      const anchors: string[] = [];
      for (const [index, block] of blocks.entries()) {
        chain.mine(...block);
        clock.nowMs += blockMs;
        await runner.runTick();
        expect(exits).toEqual([]);
        expect(results).toHaveLength(index + 1);
        expect(results.at(-1)!.errors).toEqual([]);
        expect(committee.latestL1View()?.observedAtMs).toBe(clock.nowMs);
        const state = await store.getL1SourceState();
        expect(state?.status).toBe("healthy");
        if (state?.stateQueueReplayAnchor !== undefined) {
          expect(
            chain.isFinal(state.stateQueueReplayAnchor.queue, finalityDepth),
          ).toBe(true);
          anchors.push(JSON.stringify(state.stateQueueReplayAnchor.queue));
        }
      }
      expect(new Set(anchors).size).toBeGreaterThan(1);
      // The merge (block 2 of 7) is final by now; its outcome is recorded.
      await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
        status: "merged",
        finalized: true,
      });
      expect(
        (await store.getL1SourceState())?.observations.find(
          ({ headerHash }) => headerHash === signed,
        ),
      ).toMatchObject({
        stateQueueStatus: "merged",
        hasPersistedDecision: true,
      });
    });

    it("keeps ticking through a shallow rollback of the latest append", async () => {
      const { chain, clock, store, service, spare } = await chainCommittee(
        "52",
        5,
      );
      const committee = service();
      await committee.initialize();
      const tick = async () => {
        clock.nowMs += blockMs;
        await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
        const state = await store.getL1SourceState();
        expect(state?.status).toBe("healthy");
        // The rollback never undoes an output the anchor names.
        if (state?.stateQueueReplayAnchor !== undefined) {
          expect(chain.isFinal(state.stateQueueReplayAnchor.queue, 1)).toBe(
            true,
          );
        }
      };

      // Rolled back while the committee holds only a bootstrap candidate.
      chain.mine({ append: spare[0]! });
      await tick();
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeUndefined();
      chain.rollback(1);
      chain.mine({ append: spare[1]! });
      await tick();

      // Rolled back after a durable anchor is recorded: the first append on
      // top of the candidate is final once the finality depth of blocks is
      // on top of it.
      for (const header of spare.slice(2, 6)) {
        chain.mine({ append: header });
        await tick();
      }
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeDefined();
      chain.rollback(1);
      await tick();
      chain.mine({ append: spare[6]! });
      await tick();
      chain.mine({ append: spare[7]! });
      await tick();
    });

    it("keeps the replay anchor when a tick fails between a signature and the healthy-state write", async () => {
      const { chain, clock, store, service, signed } = await chainCommittee(
        "53",
        5,
      );
      // A committee that signs nothing records the durable anchor first.
      const observer = service(false);
      await observer.initialize();
      await expect(observer.tick()).resolves.toMatchObject({
        signedHeaders: 0,
      });
      const anchor = (await store.getL1SourceState())?.stateQueueReplayAnchor;
      expect(anchor).toBeDefined();

      const committee = service();
      await committee.initialize();
      clock.nowMs += blockMs;
      const listL1Submissions = vi
        .spyOn(store, "listL1Submissions")
        .mockRejectedValueOnce(new Error("store read failed"));
      await expect(committee.tick()).rejects.toThrow("store read failed");
      expect(listL1Submissions).toHaveBeenCalled();
      const afterFailure = await store.getL1SourceState();
      expect(afterFailure).toMatchObject({
        status: "healthy",
        stateQueueReplayAnchor: anchor,
      });
      expect(
        afterFailure?.observations.find(
          ({ headerHash }) => headerHash === signed,
        ),
      ).toMatchObject({ hasPersistedDecision: true });

      // The signed header is merged; once that is final the committee
      // records it rather than seeing a persisted decision disappear.
      chain.mine("merge");
      chain.mine();
      chain.mine();
      chain.mine();
      clock.nowMs += blockMs;
      await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      const state = await store.getL1SourceState();
      expect(state?.status).toBe("healthy");
      await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
        status: "merged",
      });
    });

    /** A committee over `chainCommittee`, ticking one block's time apart. */
    const ticking = async (seedByte: string) => {
      const harness = await chainCommittee(seedByte, 5);
      const committee = harness.service();
      await committee.initialize();
      const tick = async () => {
        harness.clock.nowMs += blockMs;
        const result = await committee.tick();
        expect(result.errors).toEqual([]);
        expect((await harness.store.getL1SourceState())?.status).toBe(
          "healthy",
        );
        return result;
      };
      const observed = async () =>
        (await harness.store.getL1SourceState())?.observations.find(
          ({ headerHash }) => headerHash === harness.signed,
        );
      const output = () =>
        harness.chain
          .queue()
          .find(({ headerHash }) => headerHash === harness.signed)!.outRef;
      const decidedOutputs = async () =>
        new Set(
          (await harness.store.listDecisionOutbox(harness.signed)).map(
            ({ stateQueueOutRef }) => stateQueueOutRef,
          ),
        );
      return { ...harness, tick, observed, output, decidedOutputs };
    };

    it("follows a signed tail header to the output the next append continues it at, once that append is final", async () => {
      const {
        chain,
        signed,
        spare,
        store,
        tick,
        observed,
        output,
        decidedOutputs,
      } = await ticking("54");
      await expect(tick()).resolves.toMatchObject({ signedHeaders: 1 });
      const signedAt = output();
      expect(await observed()).toMatchObject({
        stateQueueOutRef: signedAt,
        hasPersistedDecision: true,
      });

      // The next append respends the signed tail. While that is younger than
      // the finality depth the decision keeps its output and nothing binds to
      // the new one.
      chain.mine({ append: spare[0]! });
      const continued = output();
      expect(continued).not.toBe(signedAt);
      await tick();
      chain.mine();
      await tick();
      chain.mine();
      await tick();
      expect(await observed()).toMatchObject({ stateQueueOutRef: signedAt });
      expect(await decidedOutputs()).not.toContain(continued);

      // Once it is final, the decision follows the continued output, and the
      // committee keeps deciding on it.
      chain.mine();
      await tick();
      expect(await observed()).toMatchObject({
        stateQueueOutRef: continued,
        stateQueueStatus: "unattested",
        finalized: true,
        hasPersistedDecision: true,
      });
      expect(await decidedOutputs()).toContain(continued);
      // The republished signature is bound where the header now is.
      expect(await store.listDaSignatures(signed)).toMatchObject([
        { validation: { stateQueueOutRef: continued } },
      ]);

      // Its attestation lands on that output and is followed the same way.
      chain.mine({ attest: signed });
      chain.mine({ append: spare[1]! });
      await tick();
      expect(await observed()).toMatchObject({ stateQueueOutRef: continued });
      chain.mine();
      chain.mine();
      chain.mine();
      await tick();
      expect(await observed()).toMatchObject({
        stateQueueOutRef: output(),
        stateQueueStatus: "attested",
        finalized: true,
        hasPersistedDecision: true,
      });
    });

    it("follows a datum update attesting a signed header, once it is final", async () => {
      const { chain, signed, tick, observed, output } = await ticking("55");
      await expect(tick()).resolves.toMatchObject({ signedHeaders: 1 });
      const signedAt = output();
      chain.mine({ attest: signed });
      await tick();
      chain.mine();
      await tick();
      chain.mine();
      await tick();
      expect(await observed()).toMatchObject({
        stateQueueOutRef: signedAt,
        stateQueueStatus: "unattested",
      });
      chain.mine();
      await tick();
      expect(output()).not.toBe(signedAt);
      expect(await observed()).toMatchObject({
        stateQueueOutRef: output(),
        stateQueueStatus: "attested",
        finalized: true,
        hasPersistedDecision: true,
      });
    });

    it("quarantines at the scan a snapshot showing a signed header at an output authenticated replay does not reproduce", async () => {
      const { chain, clock, store, service, signed } = await chainCommittee(
        "56",
        5,
      );
      let forged = false;
      const committee = service(true, {
        fetchStateQueueNodes: async () => chain.snapshot().nodes,
        fetchStateQueueSnapshot: async () => {
          const snapshot = chain.snapshot();
          return forged
            ? {
                ...snapshot,
                nodes: snapshot.nodes.map((node) =>
                  node.linkedListKey === signed
                    ? {
                        ...node,
                        outRef: `${"5f".repeat(32)}#0`,
                        chainPoint: { ...node.chainPoint, slot: 200 },
                      }
                    : node,
                ),
              }
            : snapshot;
        },
        fetchStateQueueReplayCheckpoints:
          chain.fetchStateQueueReplayCheckpoints,
      });
      await committee.initialize();
      await expect(committee.tick()).resolves.toMatchObject({
        signedHeaders: 1,
      });
      forged = true;
      chain.mine();
      clock.nowMs += blockMs;
      await expect(committee.tick()).rejects.toBeInstanceOf(
        L1SourceIntegrityError,
      );
      await expect(store.getL1SourceState()).resolves.toMatchObject({
        status: "quarantined",
        quarantineReason: expect.stringContaining("l1_source_integrity_failed"),
      });
    });

    it("quarantines a final move of a signed header that no authenticated step from the durable anchor explains", async () => {
      const { chain, store, signed, tick, observed, output } =
        await ticking("59");
      await expect(tick()).resolves.toMatchObject({ signedHeaders: 1 });
      const signedAt = output();
      chain.mine({ attest: signed });
      for (let block = 0; block <= finalityDepth; block += 1) chain.mine();
      // The record is final, the snapshot agrees with the replay, and the
      // replay from an anchor already past the move carries no step for it.
      await runAnchorAheadOfObservations(store, chain.queue());
      await expect(tick()).rejects.toThrow();
      expect(await observed()).toMatchObject({ stateQueueOutRef: signedAt });
      await expect(store.getL1SourceState()).resolves.toMatchObject({
        status: "quarantined",
        quarantineReason: `l1_source_decision_forked:${signed}`,
      });
    });

    it("judges replayed history at the snapshot's tip when a block lands before the replay reads it", async () => {
      const { chain, clock, store, service, signed } = await chainCommittee(
        "5a",
        5,
      );
      let mineDuringReplay = false;
      const tips: { readonly snapshot: number; readonly replay: number }[] = [];
      let snapshotTip = 0;
      const committee = service(true, {
        fetchStateQueueNodes: async () => chain.snapshot().nodes,
        fetchStateQueueSnapshot: async () => {
          const snapshot = chain.snapshot();
          snapshotTip = snapshot.tipBlockNo!;
          return snapshot;
        },
        fetchStateQueueReplayCheckpoints: async (
          anchor,
          current,
          tip,
          limit,
        ) => {
          tips.push({ snapshot: snapshotTip, replay: tip });
          if (mineDuringReplay) {
            mineDuringReplay = false;
            chain.mine();
          }
          return chain.fetchStateQueueReplayCheckpoints(
            anchor,
            current,
            tip,
            limit,
          );
        },
      });
      await committee.initialize();
      const tick = async () => {
        clock.nowMs += blockMs;
        await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      };
      await tick();
      const signedAt = chain.queue()[1]!.outRef;
      // The attestation's output has one block fewer than the finality depth
      // on top when the snapshot is read, and the finality depth once the
      // block mined during the replay lands.
      chain.mine({ attest: signed });
      await tick();
      chain.mine();
      chain.mine();
      mineDuringReplay = true;
      await tick();
      expect(tips.every(({ snapshot, replay }) => snapshot === replay)).toBe(
        true,
      );
      const observedSigned = async () =>
        (await store.getL1SourceState())?.observations.find(
          ({ headerHash }) => headerHash === signed,
        );
      expect(await observedSigned()).toMatchObject({
        stateQueueOutRef: signedAt,
      });
      await tick();
      expect(await observedSigned()).toMatchObject({
        stateQueueOutRef: chain.queue()[1]!.outRef,
        stateQueueStatus: "attested",
        finalized: true,
      });
    });

    it("defers, rather than forks, a move replay judges final at a later tip than the snapshot", async () => {
      const { chain, clock, store, service, signed } = await chainCommittee(
        "5b",
        5,
      );
      let mineDuringReplay = false;
      // A replay source that reads its own, later tip.
      const committee = service(true, {
        fetchStateQueueNodes: async () => chain.snapshot().nodes,
        fetchStateQueueSnapshot: async () => chain.snapshot(),
        fetchStateQueueReplayCheckpoints: async (anchor, current, _, limit) => {
          if (mineDuringReplay) {
            mineDuringReplay = false;
            chain.mine();
          }
          return chain.fetchStateQueueReplayCheckpoints(
            anchor,
            current,
            chain.tip,
            limit,
          );
        },
      });
      await committee.initialize();
      const tick = async () => {
        clock.nowMs += blockMs;
        await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      };
      await tick();
      const signedAt = chain.queue()[1]!.outRef;
      chain.mine({ attest: signed });
      await tick();
      chain.mine();
      chain.mine();
      mineDuringReplay = true;
      await tick();
      const state = await store.getL1SourceState();
      expect(state?.status).toBe("healthy");
      expect(
        state?.observations.find(({ headerHash }) => headerHash === signed),
      ).toMatchObject({ stateQueueOutRef: signedAt });
    });

    it("makes no decision before a durable anchor, reaches one within the finality depth while appends land every block, then signs", async () => {
      const { chain, clock, store, service, signed, spare } =
        await chainCommittee("5c", 5);
      // Appends have landed every block: the header to sign is final, but
      // the queue's tail is always younger than the finality depth.
      for (const header of spare.slice(0, finalityDepth + 1)) {
        chain.mine({ append: header });
      }
      const committee = service();
      await committee.initialize();
      const noAnchor =
        "L1 state queue has no durable replay anchor yet: no decision is made until its history is final";
      const tick = async () => {
        clock.nowMs += blockMs;
        const result = await committee.tick();
        expect(result.errors).toEqual([]);
        expect((await store.getL1SourceState())?.status).toBe("healthy");
        return result;
      };
      await expect(tick()).resolves.toMatchObject({ signedHeaders: 0 });
      await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
        status: "unattested",
        finalized: true,
      });
      expect((await committee.readinessSnapshot()).reasons).toContain(noAnchor);
      let blocks = 0;
      for (const header of spare.slice(finalityDepth + 1)) {
        chain.mine({ append: header });
        blocks += 1;
        const result = await tick();
        const state = await store.getL1SourceState();
        if (state?.stateQueueReplayAnchor !== undefined) {
          expect(result.signedHeaders).toBe(1);
          break;
        }
        expect(result.signedHeaders).toBe(0);
        expect(await store.listDecisionOutbox(signed)).toEqual([]);
        expect(await store.listDaSignatures(signed)).toEqual([]);
        expect(
          state?.observations.every(
            ({ hasPersistedDecision }) => !hasPersistedDecision,
          ),
        ).toBe(true);
        expect((await committee.readinessSnapshot()).reasons).toContain(
          noAnchor,
        );
      }
      // The first append is final once the finality depth of blocks is on
      // top of it, and the anchor is the queue after it.
      expect(blocks).toBe(finalityDepth + 1);
      expect(
        chain.isFinal(
          (await store.getL1SourceState())!.stateQueueReplayAnchor!.queue,
          finalityDepth,
        ),
      ).toBe(true);
      expect((await committee.readinessSnapshot()).reasons).not.toContain(
        noAnchor,
      );
    });

    it("makes no decision and quarantines nothing across a restart before any durable anchor", async () => {
      const { chain, clock, store, service, signed, spare } =
        await chainCommittee("5d", 5);
      const first = service();
      await first.initialize();
      // Appends every block: the header to sign is final, the tail young,
      // and the first committee holds only a candidate.
      for (const header of spare.slice(0, finalityDepth + 1)) {
        chain.mine({ append: header });
      }
      await expect(first.tick()).resolves.toMatchObject({
        signedHeaders: 0,
        errors: [],
      });
      await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
        status: "unattested",
        finalized: true,
      });
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeUndefined();

      // Restarted, it sees that header moved by an attestation it has no
      // anchor to authenticate.
      const restarted = service();
      await restarted.initialize();
      chain.mine({ attest: signed });
      clock.nowMs += blockMs;
      await expect(restarted.tick()).resolves.toMatchObject({
        signedHeaders: 0,
        errors: [],
      });
      const state = await store.getL1SourceState();
      expect(state?.status).toBe("healthy");
      expect(state?.stateQueueReplayAnchor).toBeUndefined();
      expect(await store.listDecisionOutbox(signed)).toEqual([]);
      expect(await store.listDaSignatures(signed)).toEqual([]);
      expect(
        state?.observations.every(
          ({ hasPersistedDecision }) => !hasPersistedDecision,
        ),
      ).toBe(true);
      // Its own bootstrap candidate, read after the attestation, becomes the
      // durable anchor once it is final; still healthy.
      for (let block = 0; block < finalityDepth; block += 1) chain.mine();
      clock.nowMs += blockMs;
      await expect(restarted.tick()).resolves.toMatchObject({ errors: [] });
      const anchored = await store.getL1SourceState();
      expect(anchored?.status).toBe("healthy");
      expect(anchored?.stateQueueReplayAnchor).toBeDefined();
    });

    it("keeps a durable anchor established by a tick that fails before its healthy write", async () => {
      const { chain, clock, store, service, spare } = await chainCommittee(
        "5e",
        1,
      );
      const observer = service(false);
      await observer.initialize();
      const tick = async () => {
        clock.nowMs += blockMs;
        return observer.tick();
      };
      await tick();
      for (let block = 0; block < finalityDepth; block += 1) {
        chain.mine({ append: spare[block]! });
        await tick();
      }
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeUndefined();
      // This block makes the first append final: the tick establishes the
      // durable anchor, then fails before writing it.
      chain.mine({ append: spare[finalityDepth]! });
      vi.spyOn(store, "listL1Submissions").mockRejectedValueOnce(
        new Error("store read failed"),
      );
      await expect(tick()).rejects.toThrow("store read failed");
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeUndefined();
      expect((await observer.readinessSnapshot()).reasons).not.toContain(
        "L1 state queue has no durable replay anchor yet: no decision is made until its history is final",
      );
      // The next tick replays from it, rather than bootstrapping again and
      // waiting out the finality depth once more.
      chain.mine({ append: spare[finalityDepth + 1]! });
      await expect(tick()).resolves.toMatchObject({ errors: [] });
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeDefined();
    });

    it("writes the durable anchor with the first decision, so a restart after that decision can still explain its moves", async () => {
      const { chain, clock, store, service, signed, spare } =
        await chainCommittee("5f", 1);
      const committee = service();
      await committee.initialize();
      const tick = async (on: CommitteeService) => {
        clock.nowMs += blockMs;
        return on.tick();
      };
      await tick(committee);
      for (let block = 0; block < finalityDepth; block += 1) {
        chain.mine({ append: spare[block]! });
        await tick(committee);
      }
      // The tick that establishes the anchor signs, then fails before its
      // healthy write.
      chain.mine({ append: spare[finalityDepth]! });
      vi.spyOn(store, "listL1Submissions").mockRejectedValueOnce(
        new Error("store read failed"),
      );
      await expect(tick(committee)).rejects.toThrow("store read failed");
      const afterFailure = await store.getL1SourceState();
      expect(afterFailure?.stateQueueReplayAnchor).toBeDefined();
      const signedAt = chain.queue()[1]!.outRef;
      expect(
        afterFailure?.observations.find(
          ({ headerHash }) => headerHash === signed,
        ),
      ).toMatchObject({
        stateQueueOutRef: signedAt,
        hasPersistedDecision: true,
      });

      // Restarted, it follows the signed header's attestation.
      const restarted = service();
      await restarted.initialize();
      chain.mine({ attest: signed });
      for (let block = 0; block <= finalityDepth; block += 1) chain.mine();
      await expect(tick(restarted)).resolves.toMatchObject({ errors: [] });
      const state = await store.getL1SourceState();
      expect(state?.status).toBe("healthy");
      expect(
        state?.observations.find(({ headerHash }) => headerHash === signed),
      ).toMatchObject({
        stateQueueOutRef: chain.queue()[1]!.outRef,
        stateQueueStatus: "attested",
        hasPersistedDecision: true,
      });
    });

    it("keeps a deferred header's stored record at its last final output, and records its terminal outcome on the output it was last moved to", async () => {
      const { chain, store, signed, tick, output } = await ticking("60");
      await expect(tick()).resolves.toMatchObject({ signedHeaders: 1 });
      const signedAt = output();
      chain.mine({ attest: signed });
      const attestedAt = output();
      await tick();
      // The attestation is young: the stored record stays where it was
      // final, rather than taking the output a rollback could still undo.
      await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
        stateQueueOutRef: signedAt,
        status: "unattested",
      });
      chain.mine("merge");
      await tick();
      await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
        stateQueueOutRef: signedAt,
        status: "unattested",
      });
      for (let block = 0; block < finalityDepth; block += 1) {
        chain.mine();
        await tick();
      }
      await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
        stateQueueOutRef: attestedAt,
        status: "merged",
        finalized: true,
      });
      expect(
        (await store.getL1SourceState())?.observations.find(
          ({ headerHash }) => headerHash === signed,
        ),
      ).toMatchObject({
        stateQueueOutRef: attestedAt,
        stateQueueStatus: "merged",
        hasPersistedDecision: true,
      });
    });

    it("logs one structured event each time it discards a rolled-back bootstrap candidate", async () => {
      const { chain, clock, service, events, spare } = await chainCommittee(
        "57",
        5,
      );
      const committee = service(false);
      await committee.initialize();
      const tick = async () => {
        clock.nowMs += blockMs;
        await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      };
      // The latest append is young: the first tick holds only a candidate.
      chain.mine({ append: spare[0]! });
      await tick();
      expect(events).toEqual([]);
      // The candidate names outputs of the rolled-back append, which Kupo
      // no longer knows.
      const discarded = (transactionHash: string) => ({
        event: "l1_replay_anchor_candidate_discarded",
        reason: expect.stringMatching(
          new RegExp(
            `^Kupo does not know state-queue output ${transactionHash}#[0-9]+ exactly once \\(0 matches\\)$`,
            "u",
          ),
        ),
        candidateBlockNo: "6",
        candidateTransactionIndex: "0",
      });
      for (const header of spare.slice(1, 3)) {
        const rolledBack = chain.queue().at(-1)!.outRef.slice(0, 64);
        chain.rollback(1);
        chain.mine({ append: header });
        await tick();
        expect(events.at(-1)).toEqual(discarded(rolledBack));
      }
      expect(events).toHaveLength(2);
    });

    it("quarantines, rather than discards, a bootstrap candidate whose replay fails on anything but not extending it", async () => {
      const { chain, clock, store, service, events, spare } =
        await chainCommittee("58", 5);
      let corrupt = false;
      const committee = service(false, {
        fetchStateQueueNodes: async () => chain.snapshot().nodes,
        fetchStateQueueSnapshot: async () => chain.snapshot(),
        fetchStateQueueReplayCheckpoints: async (
          anchor,
          current,
          tip,
          limit,
        ) => {
          if (corrupt) {
            throw new L1SourceIntegrityError(
              "Kupo replay assigned one transaction to competing points",
            );
          }
          return chain.fetchStateQueueReplayCheckpoints(
            anchor,
            current,
            tip,
            limit,
          );
        },
      });
      await committee.initialize();
      chain.mine({ append: spare[0]! });
      await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeUndefined();
      corrupt = true;
      chain.mine({ append: spare[1]! });
      clock.nowMs += blockMs;
      await expect(committee.tick()).rejects.toThrow(/competing points/u);
      expect(events).toEqual([]);
      await expect(store.getL1SourceState()).resolves.toMatchObject({
        status: "quarantined",
        quarantineReason: expect.stringContaining(
          "l1_source_integrity_failed: Kupo replay assigned one transaction to competing points",
        ),
      });
    });

    it(
      "catches up after downtime longer than one replay walk, moving its durable anchor, then follows its headers and decides again",
      { timeout: 120_000 },
      async () => {
        const walkLimit = stateQueueReplayWalkLimit(finalityDepth);
        const { chain, clock, store, service, events, signed, spare } =
          await chainCommittee("6a", 5, walkLimit + 1);
        const committee = service();
        await committee.initialize();
        await expect(committee.tick()).resolves.toMatchObject({
          signedHeaders: 1,
          errors: [],
        });
        const anchorOf = async () =>
          (await store.getL1SourceState())!.stateQueueReplayAnchor!;
        expect((await anchorOf()).blockNo).toBe("1");
        // Offline while one more append lands than a single walk takes.
        for (const header of spare) chain.mine({ append: header });
        const view = committee.latestL1View();
        clock.nowMs += 3_600_000;
        const catchUps: string[] = [];
        for (;;) {
          clock.nowMs += blockMs;
          const outcome = await committee.tick().then(
            (result) => result,
            (error: unknown) => error as Error,
          );
          if (!(outcome instanceof Error)) {
            expect(outcome.errors).toEqual([]);
            break;
          }
          expect(outcome.message).toMatch(
            /^state-queue replay is catching up/u,
          );
          // Not a quarantine, and no view to decide on: only the anchor moved.
          const state = await store.getL1SourceState();
          expect(state?.status).toBe("healthy");
          expect(committee.latestL1View()).toEqual(view);
          catchUps.push((await anchorOf()).blockNo);
          expect(catchUps.length).toBeLessThan(4);
        }
        // One walk covered all but the last append, and the anchor moved
        // through its final checkpoints: every block not younger than F.
        expect(catchUps).toEqual([(chain.tip - finalityDepth).toString()]);
        expect(
          events.filter(
            ({ event }) => event === "l1_state_queue_replay_catching_up",
          ),
        ).toEqual([
          {
            event: "l1_state_queue_replay_catching_up",
            walkedCheckpoints: walkLimit,
            anchorBlockNo: catchUps[0],
            anchorTransactionIndex: "0",
          },
        ]);
        const state = await store.getL1SourceState();
        expect(state?.status).toBe("healthy");
        expect(
          chain.isFinal(state!.stateQueueReplayAnchor!.queue, finalityDepth),
        ).toBe(true);
        expect(committee.latestL1View()?.observedAtMs).toBe(clock.nowMs);
        // The first offline append moved the signed header; its observation
        // followed it there.
        expect(
          state?.observations.find(({ headerHash }) => headerHash === signed),
        ).toMatchObject({
          stateQueueOutRef: chain.queue()[1]!.outRef,
          stateQueueStatus: "unattested",
          hasPersistedDecision: true,
        });
        chain.mine();
        clock.nowMs += blockMs;
        await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      },
    );

    it(
      "does not exit on the L1 view deadline while a long catch-up keeps moving its anchor, and records the outcome it caught up past",
      { timeout: 120_000 },
      async () => {
        const walkLimit = stateQueueReplayWalkLimit(finalityDepth);
        const offline = 3 * walkLimit;
        const { chain, clock, store, service, signed, spare } =
          await chainCommittee("6b", 5, offline + 16);
        const committee = service();
        await committee.initialize();
        const results: Awaited<ReturnType<CommitteeService["tick"]>>[] = [];
        const exits: number[] = [];
        const l1ViewFatalMs = 3 * blockMs;
        const runner = createCommitteeTickRunner({
          tick: async () => {
            const result = await committee.tick();
            results.push(result);
            return result;
          },
          runAvailabilityResponse: async () => undefined,
          runRetention: async () => undefined,
          latestL1View: () => committee.latestL1View(),
          latestL1ProgressAtMs: () => committee.latestL1ProgressAtMs(),
          setRetentionReadiness: () => undefined,
          l1ViewFatalMs,
          startedAtMs: clock.nowMs,
          nowMs: () => clock.nowMs,
          write: () => undefined,
          shutdown: async () => undefined,
          exit: (code) => exits.push(code),
          shutdownGraceMs: 10,
        });
        await runner.runTick();
        expect(results).toMatchObject([{ signedHeaders: 1, errors: [] }]);
        results.length = 0;
        // Offline: the signed header is attested and merged, then three
        // walks' worth of appends land.
        chain.mine({ attest: signed });
        chain.mine({ append: spare[0]! });
        chain.mine("merge");
        for (const header of spare.slice(1, offline))
          chain.mine({ append: header });
        // Every tick is further apart than the deadline, and one more append
        // lands between ticks.
        const anchors: bigint[] = [];
        let next = offline;
        while (results.length === 0) {
          chain.mine({ append: spare[next]! });
          next += 1;
          clock.nowMs += l1ViewFatalMs + blockMs;
          await runner.runTick();
          expect(exits).toEqual([]);
          const state = await store.getL1SourceState();
          expect(state?.status).toBe("healthy");
          anchors.push(BigInt(state!.stateQueueReplayAnchor!.blockNo));
          expect(anchors.length).toBeLessThan(8);
        }
        expect(anchors.length).toBeGreaterThanOrEqual(3);
        expect(anchors).toEqual([...anchors].sort((a, b) => (a < b ? -1 : 1)));
        expect(new Set(anchors).size).toBe(anchors.length);
        expect(results).toMatchObject([{ errors: [] }]);
        expect(committee.latestL1View()?.observedAtMs).toBe(clock.nowMs);
        await expect(store.getStateQueueHeader(signed)).resolves.toMatchObject({
          status: "merged",
          finalized: true,
        });
        expect(
          (await store.getL1SourceState())?.observations.find(
            ({ headerHash }) => headerHash === signed,
          ),
        ).toMatchObject({
          stateQueueStatus: "merged",
          hasPersistedDecision: true,
        });
        // Once caught up, the deadline applies to the views again.
        clock.nowMs += l1ViewFatalMs + blockMs;
        vi.spyOn(committee, "tick").mockRejectedValueOnce(
          new Error("provider unreachable"),
        );
        await runner.runTick();
        expect(exits).toEqual([70]);
      },
    );

    it("counts no decision made before any durable anchor, so a header with a decision record from then follows its attestation healthily once anchored", async () => {
      const { chain, clock, store, service, config, signed, spare } =
        await chainCommittee("6c", 5);
      // A record of a decision about the signed header exists before any
      // anchor does.
      await store.saveL1Submission({
        deploymentFingerprint: config.deploymentFingerprint,
        headerHash: signed,
        txKind: "init",
        txHash: "71".repeat(32),
        inputsUsed: [],
        submittedAt: "2026-07-28T00:00:00.000Z",
        resultStatus: "submitted",
      });
      const first = service(false);
      await first.initialize();
      chain.mine({ append: spare[0]! });
      await expect(first.tick()).resolves.toMatchObject({ errors: [] });
      const before = await store.getL1SourceState();
      expect(before?.stateQueueReplayAnchor).toBeUndefined();
      expect(
        before?.observations.every(
          ({ hasPersistedDecision }) => !hasPersistedDecision,
        ),
      ).toBe(true);

      // Restarted, it sees the header attested, and anchors past it.
      const restarted = service(false);
      await restarted.initialize();
      chain.mine({ attest: signed });
      for (let block = 0; block < finalityDepth; block += 1) chain.mine();
      clock.nowMs += blockMs;
      await expect(restarted.tick()).resolves.toMatchObject({ errors: [] });
      const state = await store.getL1SourceState();
      expect(state?.status).toBe("healthy");
      expect(state?.stateQueueReplayAnchor).toBeDefined();
      expect(
        state?.observations.find(({ headerHash }) => headerHash === signed),
      ).toMatchObject({
        stateQueueOutRef: chain.queue()[1]!.outRef,
        stateQueueStatus: "attested",
        hasPersistedDecision: true,
      });
    });

    it("discards, rather than quarantines, a bootstrap candidate whose history the SDK finds no path through", async () => {
      const { chain, clock, store, service, events, spare } =
        await chainCommittee("6d", 5);
      let tamper = false;
      const committee = service(false, {
        fetchStateQueueNodes: async () => chain.snapshot().nodes,
        fetchStateQueueSnapshot: async () => chain.snapshot(),
        fetchStateQueueReplayCheckpoints: async (
          anchor,
          current,
          tip,
          limit,
        ) => {
          const checkpoints = await chain.fetchStateQueueReplayCheckpoints(
            anchor,
            current,
            tip,
            limit,
          );
          return tamper
            ? checkpoints.map((checkpoint) => ({
                ...checkpoint,
                checkpointDigest: "00".repeat(32),
              }))
            : checkpoints;
        },
      });
      await committee.initialize();
      chain.mine({ append: spare[0]! });
      await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      expect(
        (await store.getL1SourceState())?.stateQueueReplayAnchor,
      ).toBeUndefined();
      tamper = true;
      chain.mine({ append: spare[1]! });
      clock.nowMs += blockMs;
      await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      expect(events).toEqual([
        {
          event: "l1_replay_anchor_candidate_discarded",
          reason:
            "state-queue checkpoint history is non-canonical or does not extend the durable cursor",
          candidateBlockNo: "6",
          candidateTransactionIndex: "0",
        },
      ]);
      await expect(store.getL1SourceState()).resolves.toMatchObject({
        status: "healthy",
      });
    });

    it("fails the tick, without quarantine, when a provider with no replay source sees the queue change", async () => {
      const { chain, clock, store, service, spare } = await chainCommittee(
        "6e",
        10,
      );
      const committee = service(false, {
        fetchStateQueueNodes: async () => chain.snapshot().nodes,
        fetchStateQueueSnapshot: async () => chain.snapshot(),
      });
      await committee.initialize();
      for (let tick = 0; tick < 2; tick += 1) {
        clock.nowMs += blockMs;
        await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
      }
      chain.mine({ append: spare[0]! });
      for (let tick = 0; tick < 2; tick += 1) {
        clock.nowMs += blockMs;
        await expect(committee.tick()).rejects.toThrow(
          "state-queue provider has no authenticated ordered history source",
        );
        await expect(store.getL1SourceState()).resolves.toMatchObject({
          status: "healthy",
        });
      }
    });

    describe("catching up on history longer than one walk", () => {
      const walkLimit = stateQueueReplayWalkLimit(finalityDepth);
      const catchingUp = /^state-queue replay is catching up/u;
      const observationOf = async (
        store: Awaited<ReturnType<typeof chainCommittee>>["store"],
        headerHash: string,
      ) =>
        (await store.getL1SourceState())?.observations.find(
          (observation) => observation.headerHash === headerHash,
        );

      it(
        "takes a decided header's status from the snapshot when the walk leaves it at the output the snapshot shows",
        { timeout: 120_000 },
        async () => {
          const { chain, clock, store, service, signed, spare } =
            await chainCommittee("70", 5, walkLimit, false);
          const committee = service();
          await committee.initialize();
          await expect(committee.tick()).resolves.toMatchObject({
            signedHeaders: 1,
          });
          // Offline: a datum update attests the signed header, then a walk's
          // worth of appends lands after it.
          chain.mine({ attest: signed });
          const attestedAt = chain.queue()[1]!.outRef;
          for (const header of spare) chain.mine({ append: header });
          clock.nowMs += blockMs;
          await expect(committee.tick()).rejects.toThrow(catchingUp);
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueOutRef: attestedAt,
            stateQueueStatus: "attested",
            hasPersistedDecision: true,
          });
          for (let tick = 0; tick < 3; tick += 1) {
            clock.nowMs += blockMs;
            await expect(committee.tick()).resolves.toMatchObject({
              errors: [],
            });
            expect((await store.getL1SourceState())?.status).toBe("healthy");
            chain.mine();
          }
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueOutRef: attestedAt,
            stateQueueStatus: "attested",
            hasPersistedDecision: true,
          });
        },
      );

      /**
       * A signed header attested in the final part of a walk and attested
       * again in its young part: the catch-up leaves it at the first
       * attestation's output, which the snapshot no longer shows.
       */
      const movedAgainWhileYoung = async (
        seedByte: string,
        spareCount = walkLimit,
      ) => {
        const setup = await chainCommittee(seedByte, 5, spareCount, false);
        const { chain, clock, store, service, signed, spare } = setup;
        const committee = service();
        await committee.initialize();
        await expect(committee.tick()).resolves.toMatchObject({
          signedHeaders: 1,
        });
        chain.mine({ attest: signed });
        const attestedAt = chain.queue()[1]!.outRef;
        for (const header of spare.slice(0, walkLimit - finalityDepth)) {
          chain.mine({ append: header });
        }
        chain.mine({ attest: signed });
        const reattestedAt = chain.queue()[1]!.outRef;
        for (const header of spare.slice(
          walkLimit - finalityDepth,
          walkLimit - 1,
        )) {
          chain.mine({ append: header });
        }
        const outbox = (await store.listDecisionOutbox(signed)).length;
        clock.nowMs += blockMs;
        await expect(committee.tick()).rejects.toThrow(catchingUp);
        await expect(observationOf(store, signed)).resolves.toMatchObject({
          stateQueueOutRef: attestedAt,
          stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
          finalized: true,
          hasPersistedDecision: true,
        });
        expect(await store.listDecisionOutbox(signed)).toHaveLength(outbox);
        return { ...setup, committee, attestedAt, reattestedAt, outbox };
      };

      it(
        "records an unknown status when the walk leaves a decided header where the snapshot no longer shows it, and fills it from the next final move",
        { timeout: 120_000 },
        async () => {
          const {
            chain,
            clock,
            store,
            committee,
            signed,
            reattestedAt,
            outbox,
          } = await movedAgainWhileYoung("71");
          // The second attestation is still young: the header is deferred, and
          // nothing is decided on it while its status is unknown.
          clock.nowMs += blockMs;
          await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
          });
          expect(await store.listDecisionOutbox(signed)).toHaveLength(outbox);
          // Once it is final, replay explains the move from the unknown one.
          chain.mine();
          clock.nowMs += blockMs;
          await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
          expect((await store.getL1SourceState())?.status).toBe("healthy");
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueOutRef: reattestedAt,
            stateQueueStatus: "attested",
            hasPersistedDecision: true,
          });
        },
      );

      it(
        "fills an unknown status from the next observation of the same output",
        { timeout: 120_000 },
        async () => {
          const { chain, clock, store, committee, signed, attestedAt } =
            await movedAgainWhileYoung("72");
          // The young second attestation is rolled back: the header is back
          // at the output the catch-up left it at.
          chain.rollback(finalityDepth);
          for (let block = 0; block < finalityDepth; block += 1) chain.mine();
          clock.nowMs += blockMs;
          await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
          expect((await store.getL1SourceState())?.status).toBe("healthy");
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueOutRef: attestedAt,
            stateQueueStatus: "attested",
            hasPersistedDecision: true,
          });
        },
      );

      it(
        "fills an unknown status on a later catch-up that finds the header at the same output",
        { timeout: 120_000 },
        async () => {
          const { chain, clock, store, committee, signed, attestedAt, spare } =
            await movedAgainWhileYoung("79", 2 * walkLimit + 1);
          // The young second attestation is rolled back, and another walk's
          // worth of appends lands after the anchor before the next tick.
          chain.rollback(finalityDepth);
          for (const header of spare.slice(walkLimit)) {
            chain.mine({ append: header });
          }
          clock.nowMs += blockMs;
          await expect(committee.tick()).rejects.toThrow(catchingUp);
          expect((await store.getL1SourceState())?.status).toBe("healthy");
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueOutRef: attestedAt,
            stateQueueStatus: "attested",
            hasPersistedDecision: true,
          });
        },
      );

      it("still quarantines a decided header whose known status is contradicted at an unchanged output", async () => {
        const { chainProvider, clock, store, service, signed } =
          await chainCommittee("73", 5);
        let contradict = false;
        const lying = async () => {
          const snapshot = await chainProvider.fetchStateQueueSnapshot!();
          return {
            ...snapshot,
            nodes: snapshot.nodes.map((node) =>
              contradict && node.linkedListKey === signed
                ? {
                    ...node,
                    daAttestation: {
                      Attested: { da_bond_asset_name: "44".repeat(32) },
                    },
                  }
                : node,
            ),
          };
        };
        const committee = service(true, {
          ...chainProvider,
          fetchStateQueueNodes: async () => (await lying()).nodes,
          fetchStateQueueSnapshot: lying,
        });
        await committee.initialize();
        await expect(committee.tick()).resolves.toMatchObject({
          signedHeaders: 1,
        });
        contradict = true;
        clock.nowMs += blockMs;
        await committee.tick();
        await expect(store.getL1SourceState()).resolves.toMatchObject({
          status: "quarantined",
          quarantineReason: `l1_source_decision_forked:${signed}`,
        });
      });

      it(
        "records no merge its walk saw only in young history, and records it once final",
        { timeout: 120_000 },
        async () => {
          const { chain, clock, store, service, signed, spare } =
            await chainCommittee("74", 5, walkLimit, false);
          const committee = service();
          await committee.initialize();
          await expect(committee.tick()).resolves.toMatchObject({
            signedHeaders: 1,
          });
          // Offline: the signed header is attested (final by the catch-up
          // tick), appends fill the walk, and its last block merges it (young).
          chain.mine({ attest: signed });
          const attestedAt = chain.queue()[1]!.outRef;
          for (const header of spare.slice(0, walkLimit - 2)) {
            chain.mine({ append: header });
          }
          chain.mine("merge");
          chain.mine({ append: spare[walkLimit - 2]! });
          clock.nowMs += blockMs;
          await expect(committee.tick()).rejects.toThrow(catchingUp);
          // Neither the stored record nor the observation is merged yet.
          await expect(
            store.getStateQueueHeader(signed),
          ).resolves.toMatchObject({
            status: "unattested",
          });
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueOutRef: attestedAt,
            stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
          });
          let healthyTicks = 0;
          for (let tick = 0; tick < 4; tick += 1) {
            clock.nowMs += blockMs;
            const outcome = await committee.tick().then(
              (result) => result,
              (error: unknown) => error as Error,
            );
            expect(outcome).not.toBeInstanceOf(Error);
            expect((await store.getL1SourceState())?.status).toBe("healthy");
            healthyTicks += 1;
            chain.mine();
          }
          expect(healthyTicks).toBe(4);
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueStatus: "merged",
            hasPersistedDecision: true,
          });
          await expect(
            store.getStateQueueHeader(signed),
          ).resolves.toMatchObject({
            status: "merged",
            finalized: true,
          });
        },
      );

      it(
        "replays a history of exactly one walk that reaches the snapshot as a normal tick",
        { timeout: 120_000 },
        async () => {
          const { chain, clock, service, events, spare } = await chainCommittee(
            "75",
            5,
            walkLimit,
          );
          const committee = service();
          await committee.initialize();
          await committee.tick();
          for (const header of spare) chain.mine({ append: header });
          clock.nowMs += blockMs;
          await expect(committee.tick()).resolves.toMatchObject({ errors: [] });
          expect(
            events.filter(
              ({ event }) => event === "l1_state_queue_replay_catching_up",
            ),
          ).toEqual([]);
        },
      );

      it(
        "makes no progress, and still meets the L1 view deadline, on a walk none of whose checkpoints is final",
        { timeout: 120_000 },
        async () => {
          const perBlock = Math.ceil((walkLimit + 1) / finalityDepth);
          const { chain, clock, store, service, spare } = await chainCommittee(
            "76",
            5,
            perBlock * finalityDepth,
          );
          const committee = service();
          await committee.initialize();
          await committee.tick();
          const anchor = (await store.getL1SourceState())!
            .stateQueueReplayAnchor;
          // More than a walk of checkpoints, all inside the young window.
          for (let block = 0; block < finalityDepth; block += 1) {
            chain.mine(
              ...spare
                .slice(block * perBlock, (block + 1) * perBlock)
                .map((header) => ({ append: header })),
            );
          }
          const exits: number[] = [];
          const l1ViewFatalMs = 3 * blockMs;
          const runner = createCommitteeTickRunner({
            tick: () => committee.tick(),
            runAvailabilityResponse: async () => undefined,
            runRetention: async () => undefined,
            latestL1View: () => committee.latestL1View(),
            latestL1ProgressAtMs: () => committee.latestL1ProgressAtMs(),
            setRetentionReadiness: () => undefined,
            l1ViewFatalMs,
            startedAtMs: clock.nowMs,
            nowMs: () => clock.nowMs,
            write: () => undefined,
            shutdown: async () => undefined,
            exit: (code) => exits.push(code),
            shutdownGraceMs: 10,
          });
          clock.nowMs += blockMs;
          const outcome = await committee.tick().then(
            () => undefined,
            (error: unknown) => error,
          );
          // An observation failure, not an integrity one.
          expect(outcome).toBeInstanceOf(Error);
          expect(outcome).not.toBeInstanceOf(L1SourceIntegrityError);
          expect((outcome as Error).message).toMatch(
            /^state-queue replay is catching up, but none of the next \d+ checkpoints is final yet$/u,
          );
          expect(committee.latestL1ProgressAtMs()).toBeUndefined();
          const state = await store.getL1SourceState();
          expect(state?.status).toBe("healthy");
          expect(state?.stateQueueReplayAnchor).toEqual(anchor);
          clock.nowMs += l1ViewFatalMs;
          await runner.runTick();
          expect(exits).toEqual([70]);
          expect((await store.getL1SourceState())?.status).toBe("healthy");
        },
      );

      it(
        "quarantines a decided observation its walk's final steps do not lead from",
        { timeout: 120_000 },
        async () => {
          // Replay from the anchor always starts where a decided observation
          // is, so only durable state that disagrees with the anchor reaches
          // this check: here a decided observation of the queue's tail at an
          // output it never had.
          const { chain, clock, store, service, spare } = await chainCommittee(
            "77",
            5,
            walkLimit + 1,
            false,
          );
          const committee = service();
          await committee.initialize();
          await committee.tick();
          const tail = chain.queue()[2]!.headerHash!;
          for (const header of spare) chain.mine({ append: header });
          const read = store.getL1SourceState.bind(store);
          vi.spyOn(store, "getL1SourceState").mockImplementationOnce(
            async () => {
              const state = (await read())!;
              return {
                ...state,
                observations: state.observations.map((observation) =>
                  observation.headerHash === tail
                    ? {
                        ...observation,
                        stateQueueOutRef: `${"ee".repeat(32)}#0`,
                        hasPersistedDecision: true,
                      }
                    : observation,
                ),
              };
            },
          );
          clock.nowMs += blockMs;
          await committee.tick();
          await expect(store.getL1SourceState()).resolves.toMatchObject({
            status: "quarantined",
            quarantineReason: `l1_source_decision_forked:${tail}`,
          });
        },
      );

      it(
        "quarantines a decided unknown observation its walk's final steps do not lead from",
        { timeout: 120_000 },
        async () => {
          // As above, but the tampered observation's status is unknown: it is
          // checked like a known one, not left to the store to refuse.
          const { chain, clock, store, service, spare } = await chainCommittee(
            "7a",
            5,
            walkLimit + 1,
            false,
          );
          const committee = service();
          await committee.initialize();
          await committee.tick();
          const tail = chain.queue()[2]!.headerHash!;
          for (const header of spare) chain.mine({ append: header });
          const read = store.getL1SourceState.bind(store);
          vi.spyOn(store, "getL1SourceState").mockImplementationOnce(
            async () => {
              const state = (await read())!;
              return {
                ...state,
                observations: state.observations.map((observation) =>
                  observation.headerHash === tail
                    ? {
                        ...observation,
                        stateQueueOutRef: `${"ee".repeat(32)}#0`,
                        stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
                        lastKnownStatus: "attested" as const,
                        hasPersistedDecision: true,
                      }
                    : observation,
                ),
              };
            },
          );
          clock.nowMs += blockMs;
          await committee.tick();
          await expect(store.getL1SourceState()).resolves.toMatchObject({
            status: "quarantined",
            quarantineReason: `l1_source_decision_forked:${tail}`,
          });
        },
      );

      it(
        "quarantines a decided header whose known status a snapshot contradicts across an unknown one",
        { timeout: 120_000 },
        async () => {
          const { chain, chainProvider, clock, store, service, signed, spare } =
            await chainCommittee("7b", 5, walkLimit, false);
          let contradict = false;
          const lying = async () => {
            const snapshot = await chainProvider.fetchStateQueueSnapshot!();
            return {
              ...snapshot,
              nodes: snapshot.nodes.map((node) =>
                contradict && node.linkedListKey === signed
                  ? { ...node, daAttestation: SDK.NO_DA_ATTESTATION }
                  : node,
              ),
            };
          };
          const committee = service(true, {
            ...chainProvider,
            fetchStateQueueNodes: async () => (await lying()).nodes,
            fetchStateQueueSnapshot: lying,
          });
          await committee.initialize();
          await expect(committee.tick()).resolves.toMatchObject({
            signedHeaders: 1,
          });
          // The node sees the signed header attested, and that final.
          chain.mine({ attest: signed });
          for (let block = 0; block <= finalityDepth; block += 1) {
            chain.mine();
            clock.nowMs += blockMs;
            await committee.tick();
          }
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueStatus: "attested",
            hasPersistedDecision: true,
          });
          // Offline: it is attested again in the final part of a walk and
          // again in its young part; the snapshot then reports it unattested.
          chain.mine({ attest: signed });
          for (const header of spare.slice(0, walkLimit - finalityDepth)) {
            chain.mine({ append: header });
          }
          chain.mine({ attest: signed });
          for (const header of spare.slice(
            walkLimit - finalityDepth,
            walkLimit - 1,
          )) {
            chain.mine({ append: header });
          }
          contradict = true;
          clock.nowMs += blockMs;
          await expect(committee.tick()).rejects.toThrow(catchingUp);
          await expect(observationOf(store, signed)).resolves.toMatchObject({
            stateQueueStatus: UNKNOWN_STATE_QUEUE_STATUS,
            lastKnownStatus: "attested",
          });
          expect((await store.getL1SourceState())?.status).toBe("healthy");
          // Once the young move is final, the unattested status it would fill
          // the unknown one in with contradicts the attested one known before.
          for (let tick = 0; tick <= finalityDepth; tick += 1) {
            clock.nowMs += blockMs;
            await committee.tick().catch(() => undefined);
            chain.mine();
          }
          await expect(store.getL1SourceState()).resolves.toMatchObject({
            status: "quarantined",
            quarantineReason: `l1_source_decision_forked:${signed}`,
          });
        },
      );

      it(
        "acknowledges the chain-sync rollback feed it checked when it catches up",
        { timeout: 120_000 },
        async () => {
          const { chain, chainProvider, clock, config, store, service, spare } =
            await chainCommittee("78", 5, walkLimit + 1);
          const cursorAt = (sequence: number): ChainSyncCursor => ({
            sequence,
            point: {
              network: config.network,
              slot: sequence,
              blockHash: sequence.toString(16).padStart(64, "0"),
              providerSource: "chain-sync:node-a",
              observedAt: "2026-07-28T00:00:00.000Z",
            },
            rollbackGeneration: 0,
          });
          const feed: { current: ChainSyncCursor; consumed?: ChainSyncCursor } =
            { current: cursorAt(0) };
          const committee = service(true, {
            ...chainProvider,
            currentChainSyncCursor: async () => feed.current,
            replayChainSyncEvents: async (fromSequence: number) =>
              Array.from(
                { length: feed.current.sequence - fromSequence },
                (_, index): ChainSyncEvent => ({
                  direction: "roll_forward",
                  point: cursorAt(fromSequence + index + 1).point,
                }),
              ),
            loadConsumedChainSyncCursor: async () => feed.consumed,
            acknowledgeChainSyncCursor: async (cursor: ChainSyncCursor) => {
              feed.consumed = cursor;
            },
          } as StateQueueProvider);
          await committee.initialize();
          await expect(committee.tick()).resolves.toMatchObject({
            signedHeaders: 1,
          });
          expect(feed.consumed).toEqual(cursorAt(0));
          for (const header of spare) chain.mine({ append: header });
          feed.current = cursorAt(1);
          clock.nowMs += blockMs;
          await expect(committee.tick()).rejects.toThrow(catchingUp);
          expect(feed.consumed).toEqual(cursorAt(1));
          expect((await store.getL1SourceState())?.status).toBe("healthy");
        },
      );
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
    const firstStore = await openJsonCommitteeStore(dir);
    const first = new CommitteeService({
      config: configWithDaHash,
      store: firstStore,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
    // The anchor already past the header's disappearance, so the scan of
    // the empty queue succeeds and only the persisted decision tells.
    await runAnchorAheadOfObservations(firstStore, [
      { headerHash: null, outRef: `${"00".repeat(32)}#0` },
    ]);
    await firstStore.savePeerBroadcast({
      deploymentFingerprint: configWithDaHash.deploymentFingerprint,
      peerId: "peer-a",
      headerHash,
      availabilityCommitmentDigest: expectedCommitment(
        configWithDaHash,
        headerHash,
        payloadCbor,
      ).commitmentDigest,
      signerIndex: 0,
      status: "pending",
      attempts: 1,
      nextAttemptAt: "2026-07-28T00:01:00.000Z",
      updatedAt: "2026-07-28T00:00:00.000Z",
    });

    await firstStore.close();
    const restartedStore = await openJsonCommitteeStore(dir);
    const disappeared = new CommitteeService({
      config: configWithDaHash,
      store: restartedStore,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [],
      }),
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
      restartedStore.getDaSignature({
        headerHash,
        availabilityCommitmentDigest: expectedCommitment(
          configWithDaHash,
          headerHash,
          payloadCbor,
        ).commitmentDigest,
        signerIndex: 0,
      }),
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
    const afterQuarantine = new CommitteeService({
      config: configWithDaHash,
      store: await openJsonCommitteeStore(dir),
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
    const firstProvider = withFinalSnapshot({
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
    });
    const firstStore = await openJsonCommitteeStore(dir);
    const first = new CommitteeService({
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
    const restartedProvider = withFinalSnapshot({
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
    });
    const restartedStore = await openJsonCommitteeStore(dir);
    const restarted = new CommitteeService({
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
    const firstStore = await openJsonCommitteeStore(dir);
    const first = new CommitteeService({
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
    const restartedStore = await openJsonCommitteeStore(dir);
    const restarted = new CommitteeService({
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
    const firstStore = await openJsonCommitteeStore(dir);
    const first = new CommitteeService({
      config: configured,
      store: firstStore,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
      payloadSource: payloadSourceFromBytes(payloadCbor),
      signer,
      signerValidation,
    });
    await first.initialize();
    await expect(first.tick()).resolves.toMatchObject({ signedHeaders: 1 });

    let publishCalls = 0;
    await firstStore.close();
    const stale = new CommitteeService({
      config: configured,
      store: await openJsonCommitteeStore(dir),
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 0 }),
        ],
      }),
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
    const store = await openJsonCommitteeStore(dir);
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
      store.getDaSignature({
        headerHash,
        availabilityCommitmentDigest: expectedCommitment(
          configWithDaHash,
          headerHash,
          payloadCbor,
        ).commitmentDigest,
        signerIndex: 0,
      }),
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
    const store = await openJsonCommitteeStore(dir);
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
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
      store.getDaSignature({
        headerHash,
        availabilityCommitmentDigest: expectedCommitment(
          configWithDaHash,
          headerHash,
          payloadCbor,
        ).commitmentDigest,
        signerIndex: 0,
      }),
    ).resolves.toMatchObject({ headerHash, broadcastStatus: "local" });
  });

  it("fails closed for malformed locally accepted payload bytes", async () => {
    const dir = await tempDir();
    const { header, headerHash } = await makePayloadFixture();
    // A payload-submit ACK can retain a canonical outer envelope whose inner
    // body is malformed.  The watcher remains the sole semantic gate.
    const invalidPayload = await wrapDaPayload(Buffer.from("deadbeef", "hex"), {
      mode: "identity",
    });
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
    const store = await openJsonCommitteeStore(dir);
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
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
      store.getDaSignature({
        headerHash,
        availabilityCommitmentDigest: expectedCommitment(
          configWithDaHash,
          headerHash,
          invalidPayload,
        ).commitmentDigest,
        signerIndex: 0,
      }),
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
    const store = await openJsonCommitteeStore(dir);
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
      store.getDaSignature({
        headerHash,
        availabilityCommitmentDigest: expectedCommitment(
          configWithDaHash,
          headerHash,
          payloadCbor,
        ).commitmentDigest,
        signerIndex: 0,
      }),
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
    const store = await openJsonCommitteeStore(dir);
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
    const store = await openJsonCommitteeStore(dir);
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => {
          scans += 1;
          return [makeObservedNode({ header, headerHash, depth: 10 })];
        },
      }),
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
    const store = await openJsonCommitteeStore(dir);
    const payloadSource = payloadSourceFromBytes(payloadCbor);
    const published: string[] = [];
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
    const store = await openJsonCommitteeStore(dir);
    const payloadSource = payloadSourceFromBytes(payloadCbor);
    const published: string[] = [];
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
      store.getDaSignature({
        headerHash,
        availabilityCommitmentDigest: expectedCommitment(
          configWithDaHash,
          headerHash,
          payloadCbor,
        ).commitmentDigest,
        signerIndex: 0,
      }),
    ).resolves.toMatchObject({ broadcastStatus: "post_failed" });

    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 0,
      skippedHeaders: 1,
      errors: [`failed to publish DA signature for ${headerHash} signer 0`],
    });
    expect(published).toEqual([headerHash, headerHash]);
  });

  // Skipped: retention pruning now removes the local payload before the
  // republish path runs, and whether republish-after-retention should
  // rehydrate or refuse is an open design question. Re-enable once
  // https://github.com/Anastasia-Labs/midgard/issues/646 is decided —
  // neither polarity should be pinned before then.
  it.skip("republishes existing signatures for attested headers when the coordinator opts in", async () => {
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
    const store = await openJsonCommitteeStore(dir);
    const commitment = expectedCommitment(
      configWithDaHash,
      headerHash,
      payloadCbor,
    );
    const signature: DaSignatureRecord = {
      deploymentFingerprint: configWithDaHash.deploymentFingerprint,
      headerHash,
      signerIndex: 0,
      signatureWitness: signDaAttestation({
        signer,
        signerIndex: 0,
        availabilityCommitment: commitment.commitment,
      }),
      availabilityCommitmentCbor: commitment.commitmentCbor,
      availabilityCommitmentDigest: commitment.commitmentDigest,
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
        payloadVersion: Number(SDK.DA_PAYLOAD_VERSION),
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
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({
            header,
            headerHash,
            daAttestation: attestedDaStatus(),
            depth: 10,
          }),
        ],
      }),
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
      store.getDaSignature({
        headerHash,
        availabilityCommitmentDigest: expectedCommitment(
          configWithDaHash,
          headerHash,
          payloadCbor,
        ).commitmentDigest,
        signerIndex: 0,
      }),
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
    const store = await openJsonCommitteeStore(dir);
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
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
    const store = await openJsonCommitteeStore(dir);
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
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
    const store = await openJsonCommitteeStore(dir);
    const commitment = expectedCommitment(
      configWithDaHash,
      headerHash,
      payloadCbor,
    );
    await store.saveDaSignature({
      deploymentFingerprint: configWithDaHash.deploymentFingerprint,
      headerHash,
      signerIndex: 1,
      signatureWitness: signDaAttestation({
        signer: peerSigner,
        signerIndex: 1,
        availabilityCommitment: commitment.commitment,
      }),
      availabilityCommitmentCbor: commitment.commitmentCbor,
      availabilityCommitmentDigest: commitment.commitmentDigest,
      payloadHash: daPayloadSha256(payloadCbor),
      committeeSignersHash: signerValidation.committeeSignersHash,
      signedAt: new Date().toISOString(),
      broadcastStatus: "posted",
      source: "peer",
      l1ChainPoint: {},
      validation: {
        payloadVersion: Number(SDK.DA_PAYLOAD_VERSION),
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
        availabilityCommitment: commitment.commitment,
      }),
      availabilityCommitmentCbor: commitment.commitmentCbor,
      availabilityCommitmentDigest: commitment.commitmentDigest,
      payloadHash: "99".repeat(32),
      committeeSignersHash: signerValidation.committeeSignersHash,
      signedAt: new Date().toISOString(),
      broadcastStatus: "posted",
      source: "peer",
      l1ChainPoint: {},
      validation: {
        payloadVersion: Number(SDK.DA_PAYLOAD_VERSION),
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
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
    const commitment = expectedCommitment(config, headerHash, payloadCbor);
    const peerSignatures: readonly DaSignatureRecord[] = [
      {
        deploymentFingerprint: config.deploymentFingerprint,
        headerHash,
        signerIndex: 0,
        signatureWitness: signDaAttestation({
          signer: signer0,
          signerIndex: 0,
          availabilityCommitment: commitment.commitment,
        }),
        availabilityCommitmentCbor: commitment.commitmentCbor,
        availabilityCommitmentDigest: commitment.commitmentDigest,
        payloadHash: daPayloadSha256(payloadCbor),
        committeeSignersHash,
        signedAt: new Date().toISOString(),
        broadcastStatus: "posted",
        source: "peer",
        l1ChainPoint: {},
        validation: {
          payloadVersion: Number(SDK.DA_PAYLOAD_VERSION),
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
          availabilityCommitment: commitment.commitment,
        }),
        availabilityCommitmentCbor: commitment.commitmentCbor,
        availabilityCommitmentDigest: commitment.commitmentDigest,
        payloadHash: daPayloadSha256(payloadCbor),
        committeeSignersHash,
        signedAt: new Date().toISOString(),
        broadcastStatus: "posted",
        source: "peer",
        l1ChainPoint: {},
        validation: {
          payloadVersion: Number(SDK.DA_PAYLOAD_VERSION),
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
    const store = await openJsonCommitteeStore(dir);
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
        publishConflictEvidence: async () => undefined,
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
      availabilityCommitmentAuthority: commitmentAuthority(config),
      store,
      requestTimeoutMs: 1000,
    });
    const submitterReconciler = new SubmitterReconciler({
      deploymentFingerprint: config.deploymentFingerprint,
      committeeValidation,
      availabilityCommitmentAuthority: commitmentAuthority(config),
      store,
      coordinator: onChainCoordinator,
      peerPoller,
    });
    const service = new CommitteeService({
      config,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
    const firstStore = await openJsonCommitteeStore(dir);
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
    const firstService = new CommitteeService({
      config: configWithDaHash,
      store: firstStore,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
      firstStore.getDaSignature({
        headerHash,
        availabilityCommitmentDigest: expectedCommitment(
          configWithDaHash,
          headerHash,
          payloadCbor,
        ).commitmentDigest,
        signerIndex: 0,
      }),
    ).resolves.toMatchObject({ broadcastStatus: "posted" });

    await firstStore.close();
    const restartedStore = await openJsonCommitteeStore(dir);
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
    const restartedService = new CommitteeService({
      config: configWithDaHash,
      store: restartedStore,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
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
