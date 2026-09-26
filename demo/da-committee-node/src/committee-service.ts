import {
  computeDaSha256Hash,
  DaGossipTopic,
  decodeDaConflictEvidenceCbor,
  decodeDaConflictingSignatureHeaderEvidenceCbor,
  encodeDaConflictEvidenceCbor,
  encodeDaConflictingSignatureHeaderEvidenceCbor,
} from "@al-ft/midgard-core/da-transport";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";

import { type CommitteeConfig, l1SourceAuthorityDigest } from "./config.js";
import type { AttestationCoordinator } from "./coordinator/coordinator.js";
import type { SubmitterReconciler } from "./coordinator/submitter-reconciler.js";
import type {
  DaGossipMessageHandler,
  DaGossipMessageHandlerContext,
} from "./da/libp2p/DaGossip.js";
import type { DaLibp2pNode } from "./da/libp2p/DaLibp2pNode.js";
import type { DaPeerRegistry } from "./da/libp2p/DaPeerRegistry.js";
import {
  daPayloadSha256,
  DaPayloadValidationError,
  type VerifiedDaPayload,
  verifyDaPayloadAgainstHeader,
} from "./da/payload.js";
import type {
  DaPayloadCandidate,
  DaPayloadFetchFailure,
  DaPayloadSource,
} from "./da/source.js";
import type {
  DaPayloadRecord,
  DaSignatureRecord,
  DaStoredConflictEvidenceRecord,
  StateQueueHeaderRecord,
} from "./domain.js";
import type { DaAttestationChainReader } from "./l1/da-attestation-reader.js";
import type {
  ChainSyncCursor,
  ChainSyncReplayProvider,
} from "./l1/provider.js";
import { L1SourceIntegrityError } from "./l1/source-integrity.js";
import {
  scanStateQueue,
  type StateQueueCatchUp,
  type StateQueueL1View,
  type StateQueueProvider,
  type StateQueueReplayAnchor,
} from "./l1/state-queue-scanner.js";
import type { StateQueueOutputStep } from "./l1/terminal-retention-observation.js";
import {
  buildDaSignatureConflictEvidence,
  classifyDaLocalSigningCommitment,
  deriveExpectedDaAvailabilityCommitment,
  validateDaSignatureRecord,
} from "./peer/signatures.js";
import {
  type DaSigner,
  type DaSignerValidation,
  signDaAttestation,
  verifyDaSignatureWitness,
} from "./signer.js";
import {
  type CommitteeStore,
  decisionEffectId,
  DecisionEffectInFlightError,
  type DecisionOutboxRecord,
  hasPayloadBytes,
  type L1ObservedDecision,
  type L1ObservedStatus,
  type L1SourceState,
  persistedDecisionTransition,
  UNKNOWN_STATE_QUEUE_STATUS,
  withObservedStatus,
} from "./store.js";
import { hexToBytes } from "./utils/hex.js";

export type CommitteeServiceDeps = {
  readonly config: CommitteeConfig;
  readonly store: CommitteeStore;
  readonly stateQueueProvider: StateQueueProvider;
  readonly payloadSource: DaPayloadSource;
  readonly signer?: DaSigner;
  readonly signerValidation?: DaSignerValidation;
  readonly coordinator?: AttestationCoordinator;
  readonly submitterReconciler?: Pick<SubmitterReconciler, "reconcileHeader">;
  readonly daChainReader?: DaAttestationChainReader;
  readonly daLibp2pNode?: Pick<
    DaLibp2pNode,
    "setGossipHandler" | "publishGossip"
  >;
  readonly daPeerRegistry?: DaPeerRegistry;
  readonly now?: () => Date;
  /** Writes one structured JSON log line; defaults to stderr. */
  readonly writeEvent?: (line: string) => void;
};

export type CommitteeTickResult = {
  readonly scannedHeaders: number;
  readonly signedHeaders: number;
  readonly reconciledHeaders: number;
  readonly skippedHeaders: number;
  readonly payloadFetches: readonly CommitteePayloadFetchObservation[];
  readonly errors: readonly string[];
};

export type CommitteePayloadFetchObservation = {
  readonly headerHash: string;
  readonly status: "missing_da" | "available" | "fetch_failed";
  readonly sourcePeerIds: readonly string[];
  readonly detail?: string;
};

export type CommitteeL1SubmitterPreflightSnapshot = {
  readonly status: "ready" | "funded" | "failed" | "not_required" | "not_run";
  readonly detail?: unknown;
  readonly error?: string;
};

export type CommitteeRetentionReadinessSnapshot = {
  readonly status:
    | "not_checked"
    | "ok"
    | "alerting"
    | "failed"
    | "l1_view_stale";
  readonly checkedAt?: string;
  /** Age of the last fresh authenticated L1 view, when it is stale. */
  readonly l1ViewAgeMs?: number;
  readonly scanned: number;
  readonly retained: number;
  readonly prunable: number;
  readonly alerting: number;
  readonly error?: string;
};

export type CommitteeReadinessPeerSnapshot = {
  readonly localPeerId?: string;
  readonly signerIndex?: number;
  readonly producerPeerIds: readonly string[];
  readonly configuredPeerCount: number;
  readonly producerTargetCount: number;
  readonly localPeerIsProducer: boolean;
  readonly l1SubmissionEnabled: boolean;
  readonly l1SubmitterId?: string;
  readonly l1SubmitterIds: readonly string[];
  readonly l1SubmitterSignerIndexes: readonly number[];
  readonly l1SubmitterPreflight: CommitteeL1SubmitterPreflightSnapshot;
};

export type CommitteeReadinessSnapshot = {
  readonly ready: boolean;
  readonly l1Source?: {
    readonly sourceMode: "local_node" | "external_providers";
    readonly status: "uninitialized" | "healthy" | "quarantined";
    readonly observedAt?: string;
    readonly quarantineReason?: string;
  };
  readonly deployment: {
    readonly configuredFingerprint: string;
    readonly storeFingerprint?: string;
    readonly storeMatchesConfigured: boolean;
    readonly manifestSha256: string;
    readonly storeManifestSha256?: string;
    readonly contractDeploymentInfoSha256: string;
    readonly storeContractDeploymentInfoSha256?: string;
  };
  readonly contracts: {
    readonly stateQueuePolicyId: string;
    readonly stateQueueAddress: string;
    readonly daAttestationPolicyId: string;
    readonly daAttestationAddress: string;
    readonly daParamsGovernorPolicyId: string;
    readonly daParamsGovernorAddress: string;
    readonly committeeSignersHash: string;
    readonly threshold: number;
  };
  readonly peer: CommitteeReadinessPeerSnapshot;
  readonly scanner: {
    readonly status: "not_started" | "ok" | "degraded" | "failed";
    readonly lastStartedAt?: string;
    readonly lastFinishedAt?: string;
    readonly scannedHeaders: number;
    readonly signedHeaders: number;
    readonly reconciledHeaders: number;
    readonly skippedHeaders: number;
    readonly errors: readonly string[];
  };
  readonly retention?: CommitteeRetentionReadinessSnapshot;
  readonly counts: {
    readonly discoveredHeaders: number;
    readonly missingPayloads: number;
    readonly verifiedPayloads: number;
    readonly verifiedPayloadsMissingL1Attestation: number;
    readonly signatures: number;
    readonly l1AttestationSubmissions: number;
    readonly submittedOrConfirmedL1Attestations: number;
  };
  readonly reasons: readonly string[];
};

type SignedHeaderResult = {
  readonly signature: DaSignatureRecord;
};

type CoordinatorPublishResult = {
  readonly broadcastStatus: DaSignatureRecord["broadcastStatus"];
  readonly error?: string;
};

/**
 * Retention exemption sets of the last tick whose L1 observation passed every
 * source check, stamped with the time it was accepted.
 */
export type CommitteeL1View = {
  readonly observedAtMs: number;
  readonly confirmedHeadHash: string;
  readonly liveQueueHeaderHashes: ReadonlySet<string>;
};

export class CommitteeService {
  private readonly deps: CommitteeServiceDeps;
  private tickInFlight?: Promise<CommitteeTickResult>;
  private l1View: CommitteeL1View | undefined;
  /**
   * Not-yet-final bootstrap replay anchor, held in memory only until a final
   * queue can be recorded as the durable anchor.
   */
  private replayAnchorCandidate: StateQueueReplayAnchor | undefined;
  /**
   * A durable (final) replay anchor a scan established that no healthy
   * state write has persisted yet. Held in memory so that a tick failing
   * between its scan and that write does not lose it; it is what decisions
   * made before the write bind to.
   */
  private unpersistedReplayAnchor: StateQueueReplayAnchor | undefined;
  /**
   * The final authenticated replay steps of the tick in flight (ticks are
   * single-flight), attached to every decision observation the tick writes
   * so the store can check an output change against them.
   */
  private authenticatedSteps: ReadonlyMap<
    string,
    readonly StateQueueOutputStep[]
  > = new Map();
  /**
   * When a tick last moved the durable replay anchor forward while catching
   * up on history too long for one tick. Such a tick accepts no L1 view, but
   * it is progress on one.
   */
  private l1ProgressAtMs: number | undefined;
  private lastTick:
    | {
        readonly status: "ok" | "degraded" | "failed";
        readonly startedAt: string;
        readonly finishedAt: string;
        readonly result?: CommitteeTickResult;
        readonly error?: string;
      }
    | undefined;

  constructor(deps: CommitteeServiceDeps) {
    this.deps = deps;
    if (
      (deps.daLibp2pNode === undefined) !==
      (deps.daPeerRegistry === undefined)
    ) {
      throw new Error(
        "DA conflict evidence gossip requires both node and peer registry",
      );
    }
    if (deps.daLibp2pNode !== undefined && deps.daPeerRegistry !== undefined) {
      deps.daLibp2pNode.setGossipHandler(
        DaGossipTopic.conflicts,
        createDaConflictEvidenceGossipHandler({
          deploymentFingerprint: deps.config.deploymentFingerprint,
          registry: deps.daPeerRegistry,
          store: deps.store,
        }),
      );
    }
  }

  /** Latest accepted L1 view, or undefined before the first healthy scan. */
  latestL1View(): CommitteeL1View | undefined {
    return this.l1View;
  }

  /**
   * When a tick last made authenticated progress toward an L1 view without
   * reaching one: it moved the durable replay anchor while catching up.
   */
  latestL1ProgressAtMs(): number | undefined {
    return this.l1ProgressAtMs;
  }

  private nowIso(): string {
    return (this.deps.now?.() ?? new Date()).toISOString();
  }

  async initialize(): Promise<void> {
    await this.deps.store.initDeployment({
      marker: makeDeploymentMarker(this.deps.config.deploymentFingerprint),
      manifestSha256: this.deps.config.deploymentManifestSha256,
      contractDeploymentInfoSha256:
        this.deps.config.contractDeploymentInfoSha256,
      manifestRaw: this.deps.config.deploymentManifestRaw,
    });
    const l1State = await this.deps.store.getL1SourceState();
    if (
      l1State !== undefined &&
      (l1State.network !== this.deps.config.network ||
        l1State.sourceMode !== this.l1SourceMode() ||
        l1State.authoritySha256 !== this.l1SourceAuthoritySha256())
    ) {
      await this.quarantineL1Source(
        `l1_source_configuration_changed: stored=${l1State.sourceMode}/${l1State.network}/${l1State.authoritySha256}, configured=${this.l1SourceMode()}/${this.deps.config.network}/${this.l1SourceAuthoritySha256()}`,
        l1State,
      );
      throw new Error(
        "persisted L1 source state does not match configured source mode/network",
      );
    }
    if (this.deps.daChainReader !== undefined) {
      try {
        const daParams = await this.deps.daChainReader.fetchDaParams();
        if (daParams.committeeHex !== this.deps.config.daParams.committeeHex) {
          throw new L1SourceIntegrityError(
            "on-chain DA committee does not match committee node config",
          );
        }
        if (
          daParams.committeeSignersHash !==
          this.deps.config.daParams.committeeSignersHash
        ) {
          throw new L1SourceIntegrityError(
            "on-chain DA committee_signers_hash does not match committee node config",
          );
        }
        if (daParams.threshold !== this.deps.config.daParams.threshold) {
          throw new L1SourceIntegrityError(
            "on-chain DA threshold does not match committee node config",
          );
        }
      } catch (error) {
        return this.quarantineOnIntegrityFailure(
          "l1_da_params_mismatch",
          error,
          l1State,
        );
      }
    }
    if (l1State === undefined) {
      await this.deps.store.saveL1SourceState({
        schemaVersion: 1,
        sourceMode: this.l1SourceMode(),
        network: this.deps.config.network,
        authoritySha256: this.l1SourceAuthoritySha256(),
        status: "healthy",
        observations: [],
        observedAt: this.nowIso(),
      });
    }
  }

  async tick(): Promise<CommitteeTickResult> {
    if (this.tickInFlight !== undefined) {
      return this.tickInFlight;
    }
    const tickPromise = this.tickOnceWithStatus().finally(() => {
      if (this.tickInFlight === tickPromise) {
        this.tickInFlight = undefined;
      }
    });
    this.tickInFlight = tickPromise;
    return tickPromise;
  }

  async readinessSnapshot(
    args: {
      readonly localPeerId?: string;
      readonly l1SubmitterPreflight?: CommitteeL1SubmitterPreflightSnapshot;
      readonly retention?: CommitteeRetentionReadinessSnapshot;
    } = {},
  ): Promise<CommitteeReadinessSnapshot> {
    const deployment = await this.deps.store.getDeployment();
    const l1SourceState = await this.deps.store.getL1SourceState();
    const headers = await this.deps.store.listStateQueueHeaders();
    const payloads = await Promise.all(
      headers.map((header) => this.deps.store.getDaPayload(header.headerHash)),
    );
    const signatures = await this.deps.store.listDaSignatures();
    const l1Submissions = await this.deps.store.listL1Submissions();
    const l1SubmittedOrConfirmedHeaders = new Set(
      l1Submissions
        .filter(
          (record) =>
            record.resultStatus === "submitted" ||
            record.resultStatus === "confirmed",
        )
        .map((record) => record.headerHash),
    );
    const verifiedPayloads = payloads.filter(
      (payload) => payload?.validationStatus === "verified",
    );
    const missingPayloads = headers.filter((header, index) => {
      const payload = payloads[index];
      return (
        (header.status === "unattested" || header.status === "attesting") &&
        payload?.validationStatus !== "verified"
      );
    }).length;
    const verifiedPayloadsMissingL1Attestation = headers.filter(
      (header, index) =>
        payloads[index]?.validationStatus === "verified" &&
        (header.status === "unattested" || header.status === "attesting") &&
        !l1SubmittedOrConfirmedHeaders.has(header.headerHash),
    ).length;
    const producerPeerIds = this.deps.config.daTransport.peers
      .filter((peer) => peer.roles.includes("producer"))
      .map((peer) => peer.peerId);
    const localPeerIsProducer =
      args.localPeerId !== undefined &&
      producerPeerIds.includes(args.localPeerId);
    const l1SubmitterPreflight =
      args.l1SubmitterPreflight ??
      (this.deps.config.l1SubmissionEnabled
        ? { status: "not_run" as const }
        : { status: "not_required" as const });
    const tick = this.lastTick;
    const scanner: CommitteeReadinessSnapshot["scanner"] = {
      status: tick?.status ?? "not_started",
      ...(tick?.startedAt === undefined
        ? {}
        : { lastStartedAt: tick.startedAt }),
      ...(tick?.finishedAt === undefined
        ? {}
        : { lastFinishedAt: tick.finishedAt }),
      scannedHeaders: tick?.result?.scannedHeaders ?? 0,
      signedHeaders: tick?.result?.signedHeaders ?? 0,
      reconciledHeaders: tick?.result?.reconciledHeaders ?? 0,
      skippedHeaders: tick?.result?.skippedHeaders ?? 0,
      errors:
        tick?.result?.errors ?? (tick?.error === undefined ? [] : [tick.error]),
    };
    const reasons: string[] = [];
    if (deployment === undefined) {
      reasons.push("store deployment is not initialized");
    } else if (
      deployment.marker.manifestId !== this.deps.config.deploymentFingerprint
    ) {
      reasons.push(
        `store deployment manifest ID ${deployment.marker.manifestId} does not match configured ${this.deps.config.deploymentFingerprint}`,
      );
    }
    if (
      deployment?.manifestSha256 !== undefined &&
      deployment.manifestSha256 !== this.deps.config.deploymentManifestSha256
    ) {
      reasons.push(
        "store deployment manifest hash does not match configured manifest",
      );
    }
    if (
      deployment?.contractDeploymentInfoSha256 !== undefined &&
      deployment.contractDeploymentInfoSha256 !==
        this.deps.config.contractDeploymentInfoSha256
    ) {
      reasons.push(
        "store contract deployment info hash does not match configured deployment info",
      );
    }
    if (scanner.status === "not_started") {
      reasons.push("state queue scanner has not completed a tick");
    }
    if (scanner.status === "failed") {
      reasons.push("last state queue scanner tick failed");
    }
    if (scanner.status === "degraded") {
      reasons.push("last committee node tick completed with errors");
    }
    // Until chain-sync reaches the tip nothing is decided, whatever the last
    // tick reported.
    const chainSyncCatchUp = (
      this.deps.stateQueueProvider as Partial<ChainSyncReplayProvider>
    ).chainSyncCatchUpProgress?.();
    if (chainSyncCatchUp !== undefined) {
      reasons.push(
        `l1_chain_sync_catching_up: events=${chainSyncCatchUp.events.toString()}, cursorSlot=${chainSyncCatchUp.cursorSlot.toString()}, tipSlot=${chainSyncCatchUp.tipSlot.toString()}`,
      );
    }
    if (
      scanner.status !== "not_started" &&
      l1SourceState?.status !== "quarantined" &&
      (l1SourceState?.stateQueueReplayAnchor ??
        this.unpersistedReplayAnchor) === undefined
    ) {
      reasons.push(
        "L1 state queue has no durable replay anchor yet: no decision is made until its history is final",
      );
    }
    if (
      l1SourceState?.status === "quarantined" &&
      scanner.status !== "failed"
    ) {
      reasons.push(
        `L1 source is quarantined: ${l1SourceState.quarantineReason ?? "unknown reason"}`,
      );
    }
    if (
      this.deps.config.l1SubmissionEnabled &&
      l1SubmitterPreflight.status === "not_run"
    ) {
      reasons.push("L1 submitter preflight has not completed");
    }
    if (
      this.deps.config.l1SubmissionEnabled &&
      l1SubmitterPreflight.status === "failed"
    ) {
      reasons.push("L1 submitter preflight failed");
    }
    if (args.retention?.status === "not_checked") {
      reasons.push("retention check has not completed");
    }
    if (args.retention?.status === "failed") {
      reasons.push(
        `retention check failed: ${args.retention.error ?? "unknown error"}`,
      );
    }
    if (args.retention?.status === "l1_view_stale") {
      reasons.push(
        `l1_view_stale:${(args.retention.l1ViewAgeMs ?? 0).toString()}`,
      );
    }
    if (args.retention?.status === "alerting") {
      reasons.push(
        `retention_deadline_alert:${args.retention.alerting.toString()}`,
      );
    }

    return {
      ready: reasons.length === 0,
      l1Source: {
        sourceMode: this.l1SourceMode(),
        status: l1SourceState?.status ?? "uninitialized",
        ...(l1SourceState?.observedAt === undefined
          ? {}
          : { observedAt: l1SourceState.observedAt }),
        ...(l1SourceState?.quarantineReason === undefined
          ? {}
          : { quarantineReason: l1SourceState.quarantineReason }),
      },
      deployment: {
        configuredFingerprint: this.deps.config.deploymentFingerprint,
        ...(deployment?.marker.manifestId === undefined
          ? {}
          : { storeFingerprint: deployment.marker.manifestId }),
        storeMatchesConfigured:
          deployment?.marker.manifestId ===
          this.deps.config.deploymentFingerprint,
        manifestSha256: this.deps.config.deploymentManifestSha256,
        ...(deployment?.manifestSha256 === undefined
          ? {}
          : { storeManifestSha256: deployment.manifestSha256 }),
        contractDeploymentInfoSha256:
          this.deps.config.contractDeploymentInfoSha256,
        ...(deployment?.contractDeploymentInfoSha256 === undefined
          ? {}
          : {
              storeContractDeploymentInfoSha256:
                deployment.contractDeploymentInfoSha256,
            }),
      },
      contracts: {
        stateQueuePolicyId: this.deps.config.stateQueuePolicyId,
        stateQueueAddress: this.deps.config.stateQueueAddress,
        daAttestationPolicyId: this.deps.config.daAttestationPolicyId,
        daAttestationAddress: this.deps.config.daAttestationAddress,
        daParamsGovernorPolicyId: this.deps.config.daParamsGovernorPolicyId,
        daParamsGovernorAddress: this.deps.config.daParamsGovernorAddress,
        committeeSignersHash: this.deps.config.daParams.committeeSignersHash,
        threshold: this.deps.config.daParams.threshold,
      },
      peer: {
        ...(args.localPeerId === undefined
          ? {}
          : { localPeerId: args.localPeerId }),
        ...(this.deps.config.signerIndex === undefined
          ? {}
          : { signerIndex: this.deps.config.signerIndex }),
        producerPeerIds,
        configuredPeerCount: this.deps.config.daTransport.peers.length,
        producerTargetCount: producerPeerIds.length,
        localPeerIsProducer,
        l1SubmissionEnabled: this.deps.config.l1SubmissionEnabled,
        ...(this.deps.config.l1SubmitterId === undefined
          ? {}
          : { l1SubmitterId: this.deps.config.l1SubmitterId }),
        l1SubmitterIds: this.deps.config.l1SubmitterIds,
        l1SubmitterSignerIndexes: this.deps.config.l1SubmitterSignerIndexes,
        l1SubmitterPreflight,
      },
      scanner,
      ...(args.retention === undefined ? {} : { retention: args.retention }),
      counts: {
        discoveredHeaders: headers.length,
        missingPayloads,
        verifiedPayloads: verifiedPayloads.length,
        verifiedPayloadsMissingL1Attestation,
        signatures: signatures.length,
        l1AttestationSubmissions: l1Submissions.length,
        submittedOrConfirmedL1Attestations: l1SubmittedOrConfirmedHeaders.size,
      },
      reasons,
    };
  }

  private async tickOnceWithStatus(): Promise<CommitteeTickResult> {
    const startedAt = new Date().toISOString();
    try {
      const result = await this.tickOnce();
      this.lastTick = {
        status: result.errors.length === 0 ? "ok" : "degraded",
        startedAt,
        finishedAt: new Date().toISOString(),
        result,
      };
      return result;
    } catch (error) {
      this.lastTick = {
        status: "failed",
        startedAt,
        finishedAt: new Date().toISOString(),
        error: error instanceof Error ? error.message : String(error),
      };
      throw error;
    }
  }

  private async tickOnce(): Promise<CommitteeTickResult> {
    const priorL1State = await this.deps.store.getL1SourceState();
    if (priorL1State?.status === "quarantined") {
      return quarantinedTickResult(priorL1State);
    }
    let records: Awaited<ReturnType<typeof scanStateQueue>>;
    let replayAnchor: StateQueueReplayAnchor | undefined;
    let replayAnchorCandidate: StateQueueReplayAnchor | undefined;
    let deferredHeaders: ReadonlySet<string> = new Set();
    let authenticatedSteps: ReadonlyMap<
      string,
      readonly StateQueueOutputStep[]
    > = new Map();
    this.authenticatedSteps = authenticatedSteps;
    let scannedL1View: StateQueueL1View | undefined;
    let catchUp: StateQueueCatchUp | undefined;
    let snapshotChainSyncCursor: ChainSyncCursor | undefined;
    const durableReplayAnchor =
      priorL1State?.stateQueueReplayAnchor ?? this.unpersistedReplayAnchor;
    try {
      const previousHeaders = await this.deps.store.listStateQueueHeaders();
      records = await scanStateQueue(this.deps.stateQueueProvider, {
        deploymentFingerprint: this.deps.config.deploymentFingerprint,
        deploymentIdentityDigest: this.deps.config.deploymentFingerprint,
        stateQueuePolicyId: this.deps.config.stateQueuePolicyId,
        daAttestationPolicyId: this.deps.config.daAttestationPolicyId,
        finalityDepth: this.deps.config.finalityDepth,
        consensusProfile: this.deps.config.consensusProfile,
        previousHeaders,
        ...(durableReplayAnchor === undefined
          ? {}
          : { terminalReplayAnchor: durableReplayAnchor }),
        ...(this.replayAnchorCandidate === undefined
          ? {}
          : { provisionalReplayAnchor: this.replayAnchorCandidate }),
        recordReplayAnchor: (anchor) => {
          replayAnchor = anchor;
        },
        recordProvisionalReplayAnchor: (anchor) => {
          replayAnchorCandidate = anchor;
        },
        recordDiscardedReplayAnchorCandidate: (candidate, reason) => {
          this.writeEvent({
            event: "l1_replay_anchor_candidate_discarded",
            reason,
            candidateBlockNo: candidate.blockNo,
            candidateTransactionIndex: candidate.transactionIndex,
          });
        },
        recordReplayedHeaderSteps: ({ deferredHeaderHashes, finalSteps }) => {
          deferredHeaders = new Set(deferredHeaderHashes);
          authenticatedSteps = finalSteps;
        },
        recordL1View: (view) => {
          scannedL1View = view;
        },
        recordCatchUp: (progress) => {
          catchUp = progress;
        },
        recordChainSyncCursor: (cursor) => {
          snapshotChainSyncCursor = cursor;
        },
      });
      this.replayAnchorCandidate = replayAnchorCandidate;
      this.unpersistedReplayAnchor = replayAnchor;
      this.authenticatedSteps = authenticatedSteps;
    } catch (error) {
      return this.quarantineOnIntegrityFailure(
        "l1_source_integrity_failed",
        error,
        priorL1State,
      );
    }
    let rollbackCheck: L1RollbackFeedCheck;
    try {
      rollbackCheck = await checkL1RollbackFeed(
        priorL1State,
        this.deps.stateQueueProvider,
        snapshotChainSyncCursor,
      );
    } catch (error) {
      return this.quarantineOnIntegrityFailure(
        "l1_source_rollback_feed_failed",
        error,
        priorL1State,
      );
    }
    if (rollbackCheck.failure !== undefined) {
      const quarantined = await this.quarantineL1Source(
        rollbackCheck.failure,
        priorL1State,
      );
      return quarantinedTickResult(quarantined);
    }
    if (catchUp !== undefined) {
      return this.catchUpL1Source(catchUp, priorL1State, rollbackCheck);
    }
    const transitionFailure = l1ObservationTransitionFailure(
      priorL1State,
      new Map(
        records.map((record) => [
          record.headerHash,
          this.observedDecision(record, true),
        ]),
      ),
      deferredHeaders,
    );
    if (transitionFailure !== undefined) {
      const quarantined = await this.quarantineL1Source(
        transitionFailure,
        priorL1State,
      );
      return quarantinedTickResult(quarantined);
    }
    if (
      records.some(
        (record) =>
          record.finalized &&
          (record.observedChainPoint.slot === undefined ||
            record.observedChainPoint.blockHash === undefined),
      )
    ) {
      const quarantined = await this.quarantineL1Source(
        "l1_source_finalized_decision_missing_canonical_chain_point",
        priorL1State,
      );
      return quarantinedTickResult(quarantined);
    }
    if (scannedL1View !== undefined) {
      this.l1View = {
        observedAtMs: (this.deps.now?.() ?? new Date()).getTime(),
        confirmedHeadHash: scannedL1View.confirmedHeaderHash,
        liveQueueHeaderHashes: new Set(scannedL1View.liveQueueHeaderHashes),
      };
    }
    const errors: string[] = [];
    const payloadFetches: CommitteePayloadFetchObservation[] = [];
    let signedHeaders = 0;
    let reconciledHeaders = 0;
    let skippedHeaders = 0;
    // Decisions bind to outputs whose later moves only authenticated replay
    // from a durable anchor can explain, so none is made before one exists.
    const decisionsAllowed = replayAnchor !== undefined;
    for (const record of records) {
      // A header a young checkpoint moved has no canonical output until that
      // checkpoint is final: its stored record keeps its last final output
      // until then, and no decision may bind to it yet.
      if (deferredHeaders.has(record.headerHash)) {
        skippedHeaders += 1;
        continue;
      }
      await this.deps.store.upsertStateQueueHeader(record);
      if (
        !decisionsAllowed ||
        record.status === "merged" ||
        record.status === "removed"
      ) {
        skippedHeaders += 1;
        continue;
      }
      if (
        this.deps.signer === undefined ||
        this.deps.signerValidation === undefined ||
        this.deps.config.signerIndex === undefined
      ) {
        await this.ensurePayloadForSubmitter(record, errors, payloadFetches);
        const reconciled = await this.reconcileHeader(record, errors);
        if (reconciled) {
          reconciledHeaders += 1;
        }
        skippedHeaders += 1;
        continue;
      }
      const retainedPayload = await this.deps.store.getDaPayload(
        record.headerHash,
      );
      const expectedCommitment =
        retainedPayload?.validationStatus === "verified"
          ? deriveExpectedDaAvailabilityCommitment({
              authority: {
                deploymentIdentity: this.deps.config.hubOraclePolicyId,
                bondOwnerCredential:
                  this.deps.config.availabilityChallenge.bondOwnerCredential,
                responseGeometry:
                  this.deps.config.availabilityChallenge.responseGeometry,
              },
              headerHash: record.headerHash,
              payloadCborHex: retainedPayload.payloadCborHex,
            })
          : undefined;
      const signerVariants = (
        await this.deps.store.listDaSignatures(record.headerHash)
      ).filter(
        (candidate) =>
          candidate.signerIndex === this.deps.config.signerIndex &&
          validateDaSignatureRecord({
            body: candidate,
            headerHash: record.headerHash,
            deploymentFingerprint: this.deps.config.deploymentFingerprint,
            signerValidation: this.deps.signerValidation,
          }) === undefined,
      );
      const localCommitment =
        expectedCommitment === undefined
          ? undefined
          : classifyDaLocalSigningCommitment({
              records: signerVariants,
              signerIndex: this.deps.config.signerIndex,
              expectedCommitmentDigest: expectedCommitment.commitmentDigest,
            });
      if (signerVariants.length > 1) {
        const conflict = buildDaSignatureConflictEvidence({
          first: signerVariants[0]!,
          second: signerVariants[1]!,
          daVkey: this.deps.signerValidation.signerPublicKeyHex,
          reporterPeerId: "local-da-committee",
          receivedAt: this.nowIso(),
        });
        if (
          conflict !== undefined &&
          (await this.deps.store.saveDaConflictEvidence(conflict.record))
        ) {
          await this.deps.daLibp2pNode?.publishGossip(
            DaGossipTopic.conflicts,
            conflict.gossipCbor,
          );
        }
      }
      if (localCommitment !== undefined && !localCommitment.maySign) {
        skippedHeaders += 1;
        errors.push(
          `refusing to sign ${record.headerHash}: this signer already signed a different availability commitment`,
        );
        continue;
      }
      const existingSignature =
        expectedCommitment === undefined
          ? undefined
          : await this.deps.store.getDaSignature({
              headerHash: record.headerHash,
              availabilityCommitmentDigest: expectedCommitment.commitmentDigest,
              signerIndex: this.deps.config.signerIndex,
            });
      if (
        existingSignature !== undefined &&
        retainedPayload !== undefined &&
        validateDaSignatureRecord({
          body: existingSignature,
          headerHash: record.headerHash,
          deploymentFingerprint: this.deps.config.deploymentFingerprint,
          signerValidation: this.deps.signerValidation,
          verifiedPayload: retainedPayload,
          expectedAvailabilityCommitmentCbor:
            expectedCommitment!.commitmentCbor,
          expectedAvailabilityCommitmentDigest:
            expectedCommitment!.commitmentDigest,
        }) !== undefined
      ) {
        throw new Error(
          "persisted local DA signature does not match the authenticated availability commitment",
        );
      }
      if (existingSignature !== undefined) {
        if (
          this.shouldRepublishExistingSignatureForHeader(
            existingSignature,
            record.status,
          )
        ) {
          const published = await this.publishSignatureWithOutbox(
            record,
            existingSignature,
          );
          if (
            published !== "deferred" &&
            published.broadcastStatus === "post_failed"
          ) {
            errors.push(
              published.error ??
                this.coordinatorPostFailedMessage(existingSignature),
            );
          }
        }
        const reconciled = await this.reconcileHeader(record, errors);
        if (reconciled) {
          reconciledHeaders += 1;
        }
        skippedHeaders += 1;
        continue;
      }
      if (record.status !== "unattested" || !record.finalized) {
        const reconciled = await this.reconcileHeader(record, errors);
        if (reconciled) {
          reconciledHeaders += 1;
        }
        skippedHeaders += 1;
        continue;
      }
      try {
        const signed = await this.fetchVerifyAndSign(record, payloadFetches);
        if (signed === undefined) {
          skippedHeaders += 1;
          continue;
        }
        const { signature: localSignature } = signed;
        const published =
          this.deps.coordinator === undefined
            ? undefined
            : await this.publishSignatureWithOutbox(record, localSignature);
        if (published === "deferred") {
          skippedHeaders += 1;
          continue;
        }
        const signature =
          published === undefined
            ? localSignature
            : {
                ...localSignature,
                broadcastStatus: published.broadcastStatus,
              };
        if (published === undefined) {
          await this.deps.store.saveDaSignature(signature);
        }
        signedHeaders += 1;
        if (signature.broadcastStatus === "post_failed") {
          errors.push(
            published?.error ?? this.coordinatorPostFailedMessage(signature),
          );
        }
        const reconciled = await this.reconcileHeader(record, errors);
        if (reconciled) {
          reconciledHeaders += 1;
        }
      } catch (error) {
        skippedHeaders += 1;
        errors.push(error instanceof Error ? error.message : String(error));
      }
    }
    const result = {
      scannedHeaders: records.length,
      signedHeaders,
      reconciledHeaders,
      skippedHeaders,
      payloadFetches,
      errors,
    };
    await this.persistHealthyL1SourceState(
      records.filter(({ headerHash }) => !deferredHeaders.has(headerHash)),
      replayAnchor,
    );
    this.unpersistedReplayAnchor = undefined;
    if (rollbackCheck.cursor !== undefined) {
      await acknowledgeL1RollbackFeed(
        this.deps.stateQueueProvider,
        rollbackCheck.cursor,
        (event) => this.writeEvent(event),
      );
    }
    return result;
  }

  private l1SourceMode(): L1SourceState["sourceMode"] {
    return this.deps.config.l1Source.sourceMode;
  }

  private l1SourceAuthoritySha256(): string {
    return l1SourceAuthorityDigest(
      this.deps.config.network,
      this.deps.config.l1Source,
    );
  }

  /**
   * Quarantines the L1 source when `error` is an integrity failure, then
   * rethrows it. Any other error is an observation failure: it fails this
   * tick without touching durable state, the L1 view keeps ageing, and the
   * next tick retries.
   */
  private async quarantineOnIntegrityFailure(
    reasonPrefix: string,
    error: unknown,
    previous: L1SourceState | undefined,
  ): Promise<never> {
    if (error instanceof L1SourceIntegrityError) {
      await this.quarantineL1Source(
        `${reasonPrefix}: ${error.message}`,
        previous,
      );
    }
    throw error;
  }

  private async quarantineL1Source(
    reason: string,
    previous?: L1SourceState,
  ): Promise<L1SourceState> {
    const now = new Date().toISOString();
    const state: L1SourceState = {
      schemaVersion: 1,
      sourceMode: this.l1SourceMode(),
      network: this.deps.config.network,
      authoritySha256: this.l1SourceAuthoritySha256(),
      status: "quarantined",
      observations: previous?.observations ?? [],
      observedAt: previous?.observedAt ?? now,
      ...(previous?.stateQueueReplayAnchor === undefined
        ? {}
        : { stateQueueReplayAnchor: previous.stateQueueReplayAnchor }),
      quarantineReason: reason,
      quarantinedAt: now,
    };
    await this.deps.store.quarantineL1Decisions(state);
    return state;
  }

  /**
   * Records a scan that walked only the first part of a history too long for
   * one tick: the durable anchor moves past the final checkpoints it walked,
   * and every persisted observation follows its header's final steps there,
   * so the next tick resumes from that anchor. An observation's status comes
   * from the snapshot when the snapshot shows its header at the output it
   * landed on, and is otherwise recorded as unknown; nothing is decided on an
   * unknown status, and a later tick fills it in. The snapshot was not reached,
   * so no decision is made and no L1 view is accepted; the tick fails as an
   * observation failure, and a later tick finishes the walk.
   */
  private async catchUpL1Source(
    catchUp: StateQueueCatchUp,
    prior: L1SourceState | undefined,
    rollbackCheck: L1RollbackFeedCheck,
  ): Promise<CommitteeTickResult> {
    // Replay names each header's outputs but not their datums, so a status
    // is known only at an output the snapshot shows the header at: an
    // output's datum never changes. Anywhere else it is unknown, never
    // guessed; a later tick fills it in (see `persistedDecisionTransition`).
    const snapshotRecords = new Map(
      catchUp.snapshotRecords.map((record) => [record.headerHash, record]),
    );
    const statusAt = (headerHash: string, outRef: string): L1ObservedStatus => {
      const record = snapshotRecords.get(headerHash);
      return record?.stateQueueOutRef === outRef
        ? record.status
        : UNKNOWN_STATE_QUEUE_STATUS;
    };
    const observations = (prior?.observations ?? []).map(
      (observation): L1ObservedDecision => {
        const steps = catchUp.finalSteps.get(observation.headerHash);
        if (steps === undefined) {
          return observation.stateQueueStatus === UNKNOWN_STATE_QUEUE_STATUS
            ? withObservedStatus(
                observation,
                statusAt(observation.headerHash, observation.stateQueueOutRef),
              )
            : observation;
        }
        const last = steps.at(-1)!;
        return {
          ...withObservedStatus(
            observation,
            last.toOutRef === undefined
              ? (catchUp.terminalStatuses.get(observation.headerHash) ??
                  UNKNOWN_STATE_QUEUE_STATUS)
              : statusAt(observation.headerHash, last.toOutRef),
          ),
          stateQueueOutRef: last.toOutRef ?? last.fromOutRef,
          slot: last.slot,
          blockHash: last.blockHash,
          finalized: true,
          authenticatedSteps: steps,
        };
      },
    );
    const transitionFailure = l1ObservationTransitionFailure(
      prior,
      new Map(
        observations.map((observation) => [
          observation.headerHash,
          observation,
        ]),
      ),
      new Set(),
    );
    if (transitionFailure !== undefined) {
      return quarantinedTickResult(
        await this.quarantineL1Source(transitionFailure, prior),
      );
    }
    for (const record of catchUp.terminalRecords) {
      await this.deps.store.upsertStateQueueHeader(record);
    }
    await this.deps.store.saveL1SourceState({
      schemaVersion: 1,
      sourceMode: this.l1SourceMode(),
      network: this.deps.config.network,
      authoritySha256: this.l1SourceAuthoritySha256(),
      status: "healthy",
      observations,
      observedAt: new Date().toISOString(),
      stateQueueReplayAnchor: catchUp.anchor,
    });
    this.unpersistedReplayAnchor = undefined;
    if (rollbackCheck.cursor !== undefined) {
      await acknowledgeL1RollbackFeed(
        this.deps.stateQueueProvider,
        rollbackCheck.cursor,
        (event) => this.writeEvent(event),
      );
    }
    this.l1ProgressAtMs = (this.deps.now?.() ?? new Date()).getTime();
    this.writeEvent({
      event: "l1_state_queue_replay_catching_up",
      walkedCheckpoints: catchUp.walkedCheckpoints,
      anchorBlockNo: catchUp.anchor.blockNo,
      anchorTransactionIndex: catchUp.anchor.transactionIndex,
    });
    throw new Error(
      `state-queue replay is catching up: walked ${catchUp.walkedCheckpoints.toString()} checkpoints and moved the durable anchor to block ${catchUp.anchor.blockNo}; no decision is made until the replay reaches the L1 snapshot`,
    );
  }

  private async persistHealthyL1SourceState(
    records: Awaited<ReturnType<typeof scanStateQueue>>,
    stateQueueReplayAnchor: StateQueueReplayAnchor | undefined,
  ): Promise<void> {
    const submissions = await this.deps.store.listL1Submissions();
    const submittedHeaders = new Set(
      submissions.map(({ headerHash }) => headerHash),
    );
    // Without a durable anchor no decision was made, and none can be
    // checked against authenticated replay: a peer's signature alone does
    // not make one.
    const observations = await Promise.all(
      records.map(
        async (record): Promise<L1ObservedDecision> =>
          this.observedDecision(
            record,
            stateQueueReplayAnchor !== undefined &&
              (submittedHeaders.has(record.headerHash) ||
                (await this.deps.store.listDaSignatures(record.headerHash))
                  .length > 0 ||
                (await this.deps.store.listDecisionOutbox(record.headerHash))
                  .length > 0),
          ),
      ),
    );
    await this.deps.store.saveL1SourceState({
      schemaVersion: 1,
      sourceMode: this.l1SourceMode(),
      network: this.deps.config.network,
      authoritySha256: this.l1SourceAuthoritySha256(),
      status: "healthy",
      observations: observations.sort((left, right) =>
        left.headerHash.localeCompare(right.headerHash),
      ),
      observedAt: new Date().toISOString(),
      ...(stateQueueReplayAnchor === undefined
        ? {}
        : { stateQueueReplayAnchor }),
    });
  }

  private writeEvent(event: Readonly<Record<string, unknown>>): void {
    const line = `${JSON.stringify(event)}\n`;
    if (this.deps.writeEvent === undefined) {
      process.stderr.write(line);
    } else {
      this.deps.writeEvent(line);
    }
  }

  /** The durable L1 observation of `record` made by this tick. */
  private observedDecision(
    record: StateQueueHeaderRecord,
    hasPersistedDecision: boolean,
  ): L1ObservedDecision {
    const steps = this.authenticatedSteps.get(record.headerHash);
    return {
      headerHash: record.headerHash,
      stateQueueOutRef: record.stateQueueOutRef,
      stateQueueStatus: record.status,
      ...(record.observedChainPoint.slot === undefined
        ? {}
        : { slot: record.observedChainPoint.slot }),
      ...(record.observedChainPoint.blockHash === undefined
        ? {}
        : { blockHash: record.observedChainPoint.blockHash }),
      finalized: record.finalized,
      hasPersistedDecision,
      ...(steps === undefined ? {} : { authenticatedSteps: steps }),
    };
  }

  private async fetchVerifyAndSign(
    record: Awaited<ReturnType<typeof scanStateQueue>>[number],
    payloadFetches: CommitteePayloadFetchObservation[],
  ): Promise<SignedHeaderResult | undefined> {
    const verified = await this.fetchVerifyPayload(record, payloadFetches);
    if (verified === undefined) {
      return undefined;
    }
    if (
      this.deps.signer === undefined ||
      this.deps.signerValidation === undefined ||
      this.deps.config.signerIndex === undefined
    ) {
      throw new Error("DA signer is not configured");
    }
    const expectedCommitment = deriveExpectedDaAvailabilityCommitment({
      authority: {
        deploymentIdentity: this.deps.config.hubOraclePolicyId,
        bondOwnerCredential:
          this.deps.config.availabilityChallenge.bondOwnerCredential,
        responseGeometry:
          this.deps.config.availabilityChallenge.responseGeometry,
      },
      headerHash: record.headerHash,
      payloadCborHex: verified.storedPayloadCbor.toString("hex"),
    });
    const signatureWitness = signDaAttestation({
      signer: this.deps.signer,
      signerIndex: this.deps.config.signerIndex,
      availabilityCommitment: expectedCommitment.commitment,
    });
    const signature: DaSignatureRecord = {
      deploymentFingerprint: this.deps.config.deploymentFingerprint,
      headerHash: record.headerHash,
      signerIndex: this.deps.config.signerIndex,
      signatureWitness,
      availabilityCommitmentCbor: expectedCommitment.commitmentCbor,
      availabilityCommitmentDigest: expectedCommitment.commitmentDigest,
      payloadHash: verified.payloadSha256,
      committeeSignersHash: this.deps.signerValidation.committeeSignersHash,
      signedAt: new Date().toISOString(),
      broadcastStatus: "local",
      source: "local",
      verifiedAt: new Date().toISOString(),
      l1ChainPoint: record.observedChainPoint,
      validation: verified.validation,
    };
    return { signature };
  }

  private async fetchVerifyPayload(
    record: Awaited<ReturnType<typeof scanStateQueue>>[number],
    payloadFetches: CommitteePayloadFetchObservation[],
  ): Promise<VerifiedDaPayload | undefined> {
    const existing = await this.deps.store.getDaPayload(record.headerHash);
    if (existing !== undefined && hasPayloadBytes(existing)) {
      return this.verifyStoredPayload(record, existing);
    }
    if (existing?.validationStatus === "conflicted") {
      throw new Error(existing.validationError ?? "payload conflict");
    }
    const fetched = await this.deps.payloadSource.fetchPayloadCandidates(
      record.headerHash,
    );
    if (!fetched.ok) {
      const observation = payloadFetchObservation(
        record.headerHash,
        fetched.attempts,
      );
      const saved = await this.deps.store.saveDaPayload({
        deploymentFingerprint: this.deps.config.deploymentFingerprint,
        headerHash: record.headerHash,
        payloadSchemaVersion: 1,
        payloadCborHex: "",
        payloadSha256: "",
        sourcePeerId: "",
        fetchedAt: new Date().toISOString(),
        payloadFetchStatus: observation.status,
        validationStatus: "missing_da",
        validationError: observation.detail,
      });
      if (hasPayloadBytes(saved)) {
        return this.verifyStoredPayload(record, saved);
      }
      payloadFetches.push(observation);
      return undefined;
    }
    const selectedPayload = await this.selectUnambiguousPayload(
      record.headerHash,
      fetched.candidates,
    );
    payloadFetches.push({
      headerHash: record.headerHash,
      status: "available",
      sourcePeerIds: [selectedPayload.sourcePeerId],
      detail:
        fetched.attempts.length === 0
          ? undefined
          : fetched.attempts
              .map((attempt) => `${attempt.sourcePeerId}:${attempt.status}`)
              .join(","),
    });
    const payloadRecord = await this.deps.store.saveDaPayload({
      deploymentFingerprint: this.deps.config.deploymentFingerprint,
      headerHash: record.headerHash,
      payloadSchemaVersion: selectedPayload.payloadSchemaVersion,
      payloadCborHex: selectedPayload.payloadCbor.toString("hex"),
      payloadSha256: daPayloadSha256(selectedPayload.payloadCbor),
      sourcePeerId: selectedPayload.sourcePeerId,
      fetchedAt: new Date().toISOString(),
      payloadFetchStatus: "available",
      validationStatus: "fetched",
      conflictStatus: "none",
    });
    if (payloadRecord.validationStatus === "conflicted") {
      throw new Error(payloadRecord.validationError ?? "payload conflict");
    }
    const verified = await this.verifyAndStorePayload(
      selectedPayload.payloadCbor,
      record,
      payloadRecord,
    );
    return verified;
  }

  private async verifyStoredPayload(
    record: Awaited<ReturnType<typeof scanStateQueue>>[number],
    payloadRecord: DaPayloadRecord,
  ): Promise<VerifiedDaPayload> {
    if (payloadRecord.validationStatus === "conflicted") {
      throw new Error(payloadRecord.validationError ?? "payload conflict");
    }
    if (
      payloadRecord.validationStatus === "malformed_da" ||
      payloadRecord.validationStatus === "root_mismatch"
    ) {
      throw new Error(
        payloadRecord.validationError ??
          `stored DA payload is ${payloadRecord.validationStatus}`,
      );
    }
    let payloadCbor: Buffer;
    try {
      payloadCbor = hexToBytes(
        payloadRecord.payloadCborHex,
        `stored DA payload ${payloadRecord.headerHash}`,
      );
    } catch (error) {
      await this.deps.store.saveDaPayload({
        ...payloadRecord,
        validationStatus: "malformed_da",
        validationError: error instanceof Error ? error.message : String(error),
      });
      throw error;
    }
    const actualPayloadSha256 = daPayloadSha256(payloadCbor);
    if (actualPayloadSha256 !== payloadRecord.payloadSha256) {
      const error = new Error(
        `stored DA payload hash mismatch: expected ${payloadRecord.payloadSha256}, computed ${actualPayloadSha256}`,
      );
      await this.deps.store.saveDaPayload({
        ...payloadRecord,
        validationStatus: "malformed_da",
        validationError: error.message,
      });
      throw error;
    }
    return this.verifyAndStorePayload(payloadCbor, record, {
      ...payloadRecord,
      conflictStatus: payloadRecord.conflictStatus ?? "none",
    });
  }

  private async selectUnambiguousPayload(
    headerHash: string,
    candidates: readonly DaPayloadCandidate[],
  ): Promise<DaPayloadCandidate> {
    const byHash = new Map<
      string,
      {
        readonly payloadCbor: Buffer;
        readonly sourcePeerIds: string[];
      }
    >();
    for (const candidate of candidates) {
      if (candidate.payloadSchemaVersion !== 1) {
        throw new DaPayloadValidationError(
          "wrong_version",
          "DA payload candidate is not bound to canonical schema V1",
        );
      }
      const hash = daPayloadSha256(candidate.payloadCbor);
      const existing = byHash.get(hash);
      if (existing === undefined) {
        byHash.set(hash, {
          payloadCbor: candidate.payloadCbor,
          sourcePeerIds: [candidate.sourcePeerId],
        });
      } else {
        existing.sourcePeerIds.push(candidate.sourcePeerId);
      }
    }
    if (byHash.size === 1) {
      const [, entry] = [...byHash.entries()][0]!;
      return {
        sourcePeerId: entry.sourcePeerIds[0]!,
        payloadCbor: entry.payloadCbor,
        payloadSchemaVersion: 1,
      };
    }
    const conflictDetail = [...byHash.entries()]
      .map(([hash, entry]) => `${hash}@${entry.sourcePeerIds.join("+")}`)
      .join(",");
    await this.deps.store.saveDaPayload({
      deploymentFingerprint: this.deps.config.deploymentFingerprint,
      headerHash,
      payloadSchemaVersion: 1,
      payloadCborHex: candidates[0]?.payloadCbor.toString("hex") ?? "",
      payloadSha256:
        candidates[0] === undefined
          ? ""
          : daPayloadSha256(candidates[0].payloadCbor),
      sourcePeerId: candidates
        .map((candidate) => candidate.sourcePeerId)
        .join(","),
      fetchedAt: new Date().toISOString(),
      payloadFetchStatus:
        candidates.length === 0 ? "fetch_failed" : "available",
      validationStatus: "conflicted",
      conflictStatus: "conflicting_bytes",
      validationError: `conflicting DA payload bytes from peers: ${conflictDetail}`,
    });
    throw new Error(`conflicting DA payload bytes for ${headerHash}`);
  }

  private async verifyAndStorePayload(
    payloadCbor: Buffer,
    record: Awaited<ReturnType<typeof scanStateQueue>>[number],
    payloadRecord: DaPayloadRecord,
  ): Promise<VerifiedDaPayload> {
    try {
      const verificationOptions = {
        payloadSchemaVersion: 1,
        stateQueueOutRef: record.stateQueueOutRef,
      } as const;
      const verified = await verifyDaPayloadAgainstHeader(
        payloadCbor,
        record.headerHash,
        record.header,
        verificationOptions,
      );
      const verifiedPayloadRecord: DaPayloadRecord = {
        ...payloadRecord,
        payloadFetchStatus: "available",
        verifiedAt: new Date().toISOString(),
        rootSummary: verified.roots,
        validationStatus: "verified",
      };
      await this.deps.store.saveDaPayload(verifiedPayloadRecord);
      return verified;
    } catch (error) {
      const status =
        error instanceof DaPayloadValidationError &&
        error.code === "root_mismatch"
          ? "root_mismatch"
          : "malformed_da";
      await this.deps.store.saveDaPayload({
        ...payloadRecord,
        payloadFetchStatus: "available",
        validationStatus: status,
        validationError: error instanceof Error ? error.message : String(error),
      });
      throw error;
    }
  }

  private async ensurePayloadForSubmitter(
    record: StateQueueHeaderRecord,
    errors: string[],
    payloadFetches: CommitteePayloadFetchObservation[],
  ): Promise<void> {
    if (
      this.deps.submitterReconciler === undefined ||
      !this.isSubmitterHeaderScope(record)
    ) {
      return;
    }
    const existing = await this.deps.store.getDaPayload(record.headerHash);
    if (existing?.validationStatus === "verified") {
      return;
    }
    try {
      await this.fetchVerifyPayload(record, payloadFetches);
    } catch (error) {
      errors.push(error instanceof Error ? error.message : String(error));
    }
  }

  private async reconcileHeader(
    record: StateQueueHeaderRecord,
    errors: string[],
  ): Promise<boolean> {
    const reconciler = this.deps.submitterReconciler;
    if (reconciler === undefined || !record.finalized) {
      return false;
    }
    const existing = await this.decisionOutboxFor(record, "l1_reconcile");
    if (existing?.status === "reconciled") {
      return true;
    }
    const effect = await this.beginDecisionEffectUnlessInFlight(
      record,
      "l1_reconcile",
    );
    if (effect === undefined) {
      return false;
    }
    let result: Awaited<ReturnType<typeof reconciler.reconcileHeader>>;
    try {
      result = await reconciler.reconcileHeader(record);
    } catch (error) {
      await this.completeFailedAfterThrow(effect, error);
      throw error;
    }
    await this.deps.store.completeDecisionEffect({
      effectId: effect.effectId,
      expectedAttemptCount: effect.attemptCount,
      status: result.status === "reconciled" ? "reconciled" : "failed",
      updatedAt: this.nowIso(),
      ...(result.status === "reconciled"
        ? {}
        : {
            lastError:
              result.reason ??
              (result.status === "skipped"
                ? "L1 reconciliation was skipped"
                : "L1 reconciliation failed"),
          }),
    });
    if (result.status === "post_failed") {
      errors.push(
        `failed to reconcile DA attestation for ${record.headerHash}${
          result.reason === undefined ? "" : `: ${result.reason}`
        }`,
      );
    }
    return result.status === "reconciled";
  }

  private isSubmitterHeaderScope(record: StateQueueHeaderRecord): boolean {
    return (
      record.finalized &&
      (record.status === "unattested" || record.status === "attesting")
    );
  }

  private async publishSignature(
    record: DaSignatureRecord,
  ): Promise<CoordinatorPublishResult> {
    const coordinator = this.deps.coordinator;
    if (coordinator === undefined) {
      return { broadcastStatus: "local" };
    }
    try {
      const broadcastStatus = await coordinator.publishSignature(record);
      return broadcastStatus === "post_failed"
        ? {
            broadcastStatus,
            error: this.coordinatorPostFailedMessage(record),
          }
        : { broadcastStatus };
    } catch (error) {
      return {
        broadcastStatus: "post_failed",
        error: `${this.coordinatorPostFailedMessage(record)}: ${
          error instanceof Error ? error.message : String(error)
        }`,
      };
    }
  }

  private async publishSignatureWithOutbox(
    record: StateQueueHeaderRecord,
    validatedSignature: DaSignatureRecord,
  ): Promise<CoordinatorPublishResult | "deferred"> {
    // The witness signs the availability commitment only; the L1 output and
    // point are where the header was observed. A signature validated on an
    // output that final authenticated replay has since moved the header from
    // follows the header, as its decision observation did this tick.
    const signature: DaSignatureRecord =
      validatedSignature.validation.stateQueueOutRef === record.stateQueueOutRef
        ? validatedSignature
        : {
            ...validatedSignature,
            l1ChainPoint: record.observedChainPoint,
            validation: {
              ...validatedSignature.validation,
              stateQueueOutRef: record.stateQueueOutRef,
            },
          };
    const effect = await this.beginDecisionEffectUnlessInFlight(
      record,
      "signature_publish",
      signature,
    );
    if (effect === undefined) {
      return "deferred";
    }
    const published = await this.publishSignature(signature);
    await this.deps.store.completeDecisionEffect({
      effectId: effect.effectId,
      expectedAttemptCount: effect.attemptCount,
      status: published.broadcastStatus === "posted" ? "published" : "failed",
      updatedAt: this.nowIso(),
      ...(published.error === undefined ? {} : { lastError: published.error }),
      signature: {
        ...signature,
        broadcastStatus: published.broadcastStatus,
      },
    });
    return published;
  }

  private async decisionOutboxFor(
    record: StateQueueHeaderRecord,
    effectKind: DecisionOutboxRecord["effectKind"],
    signerIndex?: number,
  ): Promise<DecisionOutboxRecord | undefined> {
    return this.deps.store.getDecisionOutbox(
      decisionEffectId({
        deploymentFingerprint: this.deps.config.deploymentFingerprint,
        headerHash: record.headerHash,
        stateQueueOutRef: record.stateQueueOutRef,
        effectKind,
        ...(signerIndex === undefined ? {} : { signerIndex }),
      }),
    );
  }

  /**
   * Begins a decision effect, or defers it for this tick when an attempt for
   * the same effect is still running in this process. Deferral is not a tick
   * error: the running attempt completes the effect, and the next tick
   * observes its outcome.
   */
  private async beginDecisionEffectUnlessInFlight(
    record: StateQueueHeaderRecord,
    effectKind: DecisionOutboxRecord["effectKind"],
    signature?: DaSignatureRecord,
  ): Promise<DecisionOutboxRecord | undefined> {
    try {
      return await this.beginDecisionEffect(record, effectKind, signature);
    } catch (error) {
      if (!(error instanceof DecisionEffectInFlightError)) {
        throw error;
      }
      this.writeEvent({
        event: "decision_effect_deferred",
        effectKind,
        headerHash: record.headerHash,
        effectId: error.effectId,
        reason: error.message,
      });
      return undefined;
    }
  }

  /**
   * Records a failed outcome for an attempt whose external effect threw, so
   * the attempt does not stay in flight for the life of the process. The
   * original error is what the caller reports; a failure to record is
   * secondary and must not mask it.
   */
  private async completeFailedAfterThrow(
    effect: DecisionOutboxRecord,
    error: unknown,
  ): Promise<void> {
    try {
      await this.deps.store.completeDecisionEffect({
        effectId: effect.effectId,
        expectedAttemptCount: effect.attemptCount,
        status: "failed",
        updatedAt: this.nowIso(),
        lastError: error instanceof Error ? error.message : String(error),
      });
    } catch {
      // The store releases the in-flight claim even when completion fails.
    }
  }

  private async beginDecisionEffect(
    record: StateQueueHeaderRecord,
    effectKind: DecisionOutboxRecord["effectKind"],
    signature?: DaSignatureRecord,
  ): Promise<DecisionOutboxRecord> {
    const signerIndex =
      effectKind === "signature_publish" ? signature?.signerIndex : undefined;
    const effectId = decisionEffectId({
      deploymentFingerprint: this.deps.config.deploymentFingerprint,
      headerHash: record.headerHash,
      stateQueueOutRef: record.stateQueueOutRef,
      effectKind,
      ...(signerIndex === undefined ? {} : { signerIndex }),
    });
    const existing = await this.deps.store.getDecisionOutbox(effectId);
    const now = this.nowIso();
    const effect: DecisionOutboxRecord = {
      schemaVersion: 1,
      effectId,
      deploymentFingerprint: this.deps.config.deploymentFingerprint,
      sourceMode: this.l1SourceMode(),
      network: this.deps.config.network,
      effectKind,
      headerHash: record.headerHash,
      stateQueueOutRef: record.stateQueueOutRef,
      ...(signerIndex === undefined ? {} : { signerIndex }),
      ...(record.observedChainPoint.slot === undefined
        ? {}
        : { slot: record.observedChainPoint.slot }),
      ...(record.observedChainPoint.blockHash === undefined
        ? {}
        : { blockHash: record.observedChainPoint.blockHash }),
      finalized: true,
      status: "pending",
      attemptCount: (existing?.attemptCount ?? 0) + 1,
      createdAt: existing?.createdAt ?? now,
      updatedAt: now,
    };
    const prior = await this.deps.store.getL1SourceState();
    if (prior?.status === "quarantined") {
      throw new Error(
        `cannot begin decision effect while L1 source is quarantined: ${
          prior.quarantineReason ?? "unknown reason"
        }`,
      );
    }
    const replayAnchor =
      prior?.stateQueueReplayAnchor ?? this.unpersistedReplayAnchor;
    if (replayAnchor === undefined) {
      throw new Error(
        "cannot begin decision effect without a durable state-queue replay anchor",
      );
    }
    const observation = this.observedDecision(record, true);
    const observations = [
      ...(prior?.observations.filter(
        ({ headerHash }) => headerHash !== record.headerHash,
      ) ?? []),
      observation,
    ].sort((left, right) => left.headerHash.localeCompare(right.headerHash));
    await this.deps.store.beginDecisionEffect({
      effect,
      sourceState: {
        schemaVersion: 1,
        sourceMode: this.l1SourceMode(),
        network: this.deps.config.network,
        authoritySha256: this.l1SourceAuthoritySha256(),
        status: "healthy",
        observations,
        observedAt: now,
        // The anchor this decision's later moves are replayed from, written
        // with the decision when no healthy write has persisted one yet.
        stateQueueReplayAnchor: replayAnchor,
      },
      ...(signature === undefined ? {} : { signature }),
    });
    return effect;
  }

  private coordinatorPostFailedMessage(
    record: Pick<DaSignatureRecord, "headerHash" | "signerIndex">,
  ): string {
    const coordinatorError = this.deps.coordinator?.lastPublishError?.(record);
    return `failed to publish DA signature for ${record.headerHash} signer ${record.signerIndex.toString()}${
      coordinatorError === undefined ? "" : `: ${coordinatorError}`
    }`;
  }

  private shouldRepublishExistingSignature(record: DaSignatureRecord): boolean {
    const coordinator = this.deps.coordinator;
    if (coordinator === undefined) {
      return false;
    }
    return (
      record.broadcastStatus !== "posted" ||
      coordinator.retryPublishedSignatures === true
    );
  }

  private shouldRepublishExistingSignatureForHeader(
    record: DaSignatureRecord,
    status: Awaited<ReturnType<typeof scanStateQueue>>[number]["status"],
  ): boolean {
    if (!this.shouldRepublishExistingSignature(record)) {
      return false;
    }
    if (status === "unattested" || status === "attesting") {
      return true;
    }
    return (
      status === "attested" &&
      this.deps.coordinator?.retryPublishedSignaturesForAttestedHeaders === true
    );
  }
}

export const createDaConflictEvidenceGossipHandler = (args: {
  readonly deploymentFingerprint: string;
  readonly registry: DaPeerRegistry;
  readonly store: Pick<CommitteeStore, "saveDaConflictEvidence">;
  readonly now?: () => Date;
}): DaGossipMessageHandler => {
  const now = args.now ?? (() => new Date());
  return async (context) => {
    await ingestDaConflictEvidence({
      ...args,
      context,
      receivedAt: now(),
    });
  };
};

export const ingestDaConflictEvidence = async (args: {
  readonly deploymentFingerprint: string;
  readonly registry: DaPeerRegistry;
  readonly store: Pick<CommitteeStore, "saveDaConflictEvidence">;
  readonly context: DaGossipMessageHandlerContext;
  readonly receivedAt: Date;
}): Promise<boolean> => {
  if (args.context.topicName !== DaGossipTopic.conflicts) {
    throw new Error("DA conflict evidence arrived on the wrong gossip topic");
  }
  args.registry.requireKnownPeer(args.context.remotePeerId);
  const conflict = decodeDaConflictEvidenceCbor(args.context.data);
  if (!encodeDaConflictEvidenceCbor(conflict).equals(args.context.data)) {
    throw new Error("DA conflict evidence must use canonical CBOR");
  }
  const deploymentFingerprint = conflict.deploymentFingerprint.toString("hex");
  if (deploymentFingerprint !== args.deploymentFingerprint) {
    throw new Error(
      "DA conflict evidence deployment does not match configured deployment",
    );
  }
  if (
    conflict.evidenceKind !== "equivocation" ||
    conflict.compactEvidence === null
  ) {
    throw new Error(
      "DA conflict evidence must contain canonical signature/header equivocation evidence",
    );
  }
  if (
    !computeDaSha256Hash(conflict.compactEvidence).equals(conflict.evidenceHash)
  ) {
    throw new Error(
      "DA conflict evidence hash does not match compact evidence",
    );
  }
  const equivocation = decodeDaConflictingSignatureHeaderEvidenceCbor(
    conflict.compactEvidence,
  );
  if (
    !encodeDaConflictingSignatureHeaderEvidenceCbor(equivocation).equals(
      conflict.compactEvidence,
    )
  ) {
    throw new Error(
      "DA conflicting signature/header evidence must use canonical CBOR",
    );
  }
  if (!conflict.headerHash.equals(equivocation.lowerHeaderHash)) {
    throw new Error(
      "DA conflict evidence header does not match the lower conflicting header",
    );
  }
  const signerPeer = args.registry.getBySignerIndex(equivocation.signerIndex);
  if (
    signerPeer?.daVkey === undefined ||
    signerPeer.daVkey !== equivocation.daVkey.toString("hex")
  ) {
    throw new Error(
      "DA conflict evidence signer identity does not match the configured committee",
    );
  }
  const lowerHeaderHash = equivocation.lowerHeaderHash.toString("hex");
  const upperHeaderHash = equivocation.upperHeaderHash.toString("hex");
  const lowerCommitment = SDK.parseDaAvailabilityCommitmentCbor(
    equivocation.lowerCommitmentCbor.toString("hex"),
  );
  const upperCommitment = SDK.parseDaAvailabilityCommitmentCbor(
    equivocation.upperCommitmentCbor.toString("hex"),
  );
  if (
    lowerCommitment.header_hash !== lowerHeaderHash ||
    upperCommitment.header_hash !== upperHeaderHash
  ) {
    throw new Error(
      "DA conflict evidence commitment identity does not match its header",
    );
  }
  if (
    !verifyDaSignatureWitness({
      publicKeyHex: signerPeer.daVkey,
      availabilityCommitment: lowerCommitment,
      witnessHex: equivocation.lowerHeaderWitness.toString("hex"),
    }) ||
    !verifyDaSignatureWitness({
      publicKeyHex: signerPeer.daVkey,
      availabilityCommitment: upperCommitment,
      witnessHex: equivocation.upperHeaderWitness.toString("hex"),
    })
  ) {
    throw new Error(
      "DA conflict evidence contains an invalid attestation signature",
    );
  }
  const record: DaStoredConflictEvidenceRecord = {
    conflictSchemaVersion: 1,
    deploymentFingerprint,
    headerHash: lowerHeaderHash,
    commitmentDigest: computeDaSha256Hash(
      equivocation.lowerCommitmentCbor,
    ).toString("hex"),
    conflictingHeaderHash: upperHeaderHash,
    conflictingCommitmentDigest: computeDaSha256Hash(
      equivocation.upperCommitmentCbor,
    ).toString("hex"),
    signerIndex: equivocation.signerIndex,
    evidenceKind: "equivocation",
    evidenceHash: conflict.evidenceHash.toString("hex"),
    compactEvidenceCborHex: conflict.compactEvidence.toString("hex"),
    reporterPeerId: args.context.remotePeerId,
    receivedAt: args.receivedAt.toISOString(),
  };
  return args.store.saveDaConflictEvidence(record);
};

/**
 * Checks every persisted decision against this tick's observation of its
 * header. `deferred` holds headers an authenticated but not-yet-final
 * checkpoint moved or took out of the queue: they are neither forked nor
 * disappeared yet, and are checked again once that checkpoint is final. Any
 * other change must be explained by final authenticated replay.
 */
const l1ObservationTransitionFailure = (
  previous: L1SourceState | undefined,
  current: ReadonlyMap<string, L1ObservedDecision>,
  deferred: ReadonlySet<string>,
): string | undefined => {
  if (previous === undefined) {
    return undefined;
  }
  for (const prior of previous.observations) {
    if (!prior.hasPersistedDecision || deferred.has(prior.headerHash)) {
      continue;
    }
    const observed = current.get(prior.headerHash);
    if (observed === undefined) {
      return `l1_source_decision_disappeared:${prior.headerHash}`;
    }
    const transition = persistedDecisionTransition(prior, observed);
    if (transition === "unexplained") {
      return `l1_source_decision_forked:${prior.headerHash}`;
    }
    if (transition === "same" && !observed.finalized) {
      return `l1_source_decision_lost_finality:${prior.headerHash}`;
    }
  }
  return undefined;
};

type L1RollbackFeedCheck = {
  /** Where the next tick's rollback replay starts: the snapshot's cursor. */
  readonly cursor?: ChainSyncCursor;
  readonly failure?: string;
};

type DurableChainSyncReplayProvider = StateQueueProvider &
  ChainSyncReplayProvider;

/**
 * Replays the rollback feed since the durable consumer cursor against the
 * persisted decisions. This tick decides on a snapshot read at
 * `snapshotCursor`, and a rollback after it may undo what that snapshot
 * showed, so the tick acknowledges `snapshotCursor`, not the authority's
 * current cursor: the next tick replays every later event against the
 * decisions this tick persists.
 */
const checkL1RollbackFeed = async (
  previous: L1SourceState | undefined,
  provider: StateQueueProvider,
  snapshotCursor: ChainSyncCursor | undefined,
): Promise<L1RollbackFeedCheck> => {
  const replayProvider = durableChainSyncReplayProvider(provider);
  if (replayProvider === undefined) {
    return {};
  }
  if (snapshotCursor === undefined) {
    throw new Error(
      "local-node state-queue snapshot carries no chain-sync cursor to acknowledge",
    );
  }
  const current = await replayProvider.currentChainSyncCursor();
  const consumed = await replayProvider.loadConsumedChainSyncCursor();
  const decisions =
    previous?.observations.filter(
      ({ hasPersistedDecision }) => hasPersistedDecision,
    ) ?? [];
  if (previous === undefined || decisions.length === 0) {
    return { cursor: snapshotCursor };
  }
  if (consumed === undefined) {
    throw new L1SourceIntegrityError(
      "persisted L1 decisions lack a durable chain-sync consumer cursor",
    );
  }
  if (
    consumed.sequence > current.sequence ||
    consumed.rollbackGeneration > current.rollbackGeneration ||
    (consumed.sequence === current.sequence &&
      !sameChainSyncCursor(consumed, current))
  ) {
    throw new L1SourceIntegrityError(
      "durable chain-sync consumer cursor is ahead of or conflicts with the authority cursor",
    );
  }
  const events = await replayProvider.replayChainSyncEvents(consumed.sequence);
  if (events.length !== current.sequence - consumed.sequence) {
    throw new L1SourceIntegrityError(
      "chain-sync rollback replay is not contiguous with its durable consumer cursor",
    );
  }
  const replayedRollbacks = events.filter(
    ({ direction }) => direction === "roll_backward",
  );
  if (
    consumed.rollbackGeneration + replayedRollbacks.length !==
    current.rollbackGeneration
  ) {
    throw new L1SourceIntegrityError(
      "chain-sync rollback replay does not match the durable rollback generation",
    );
  }
  for (const rollback of replayedRollbacks) {
    for (const decision of decisions) {
      if (
        decision.slot === undefined ||
        decision.blockHash === undefined ||
        rollback.point.slot < decision.slot ||
        (rollback.point.slot === decision.slot &&
          rollback.point.blockHash !== decision.blockHash)
      ) {
        return {
          cursor: snapshotCursor,
          failure: `l1_source_chain_sync_rollback:${decision.headerHash}:${rollback.point.slot.toString()}:${rollback.point.blockHash}`,
        };
      }
    }
  }
  return { cursor: snapshotCursor };
};

/**
 * Acknowledges the cursor the tick's decisions were made at. The authority
 * may have synchronized past it during the tick, rollbacks included; the next
 * tick replays those events from this cursor.
 */
const acknowledgeL1RollbackFeed = async (
  provider: StateQueueProvider,
  cursor: ChainSyncCursor,
  writeEvent: (event: Readonly<Record<string, unknown>>) => void,
): Promise<void> => {
  const replayProvider = durableChainSyncReplayProvider(provider);
  if (replayProvider === undefined) {
    throw new Error(
      "chain-sync rollback replay capabilities disappeared before acknowledgement",
    );
  }
  const acknowledgement =
    await replayProvider.acknowledgeChainSyncCursor(cursor);
  if (acknowledgement.rollbackSinceCapture) {
    writeEvent({
      event: "l1_chain_sync_rollback_after_acknowledged_cursor",
      sequence: cursor.sequence,
      rollbackGeneration: cursor.rollbackGeneration,
    });
  }
};

const durableChainSyncReplayProvider = (
  provider: StateQueueProvider,
): DurableChainSyncReplayProvider | undefined => {
  const candidate = provider as Partial<ChainSyncReplayProvider>;
  const capabilities = [
    candidate.currentChainSyncCursor,
    candidate.replayChainSyncEvents,
    candidate.loadConsumedChainSyncCursor,
    candidate.acknowledgeChainSyncCursor,
  ];
  if (capabilities.every((capability) => capability === undefined)) {
    return undefined;
  }
  if (capabilities.some((capability) => typeof capability !== "function")) {
    throw new Error(
      "local-node provider exposes incomplete durable rollback replay capabilities",
    );
  }
  return provider as DurableChainSyncReplayProvider;
};

const sameChainSyncCursor = (
  left: ChainSyncCursor,
  right: ChainSyncCursor,
): boolean =>
  left.sequence === right.sequence &&
  left.rollbackGeneration === right.rollbackGeneration &&
  left.point.network === right.point.network &&
  left.point.slot === right.point.slot &&
  left.point.blockHash === right.point.blockHash &&
  left.point.providerSource === right.point.providerSource &&
  left.point.observedAt === right.point.observedAt;

const quarantinedTickResult = (state: L1SourceState): CommitteeTickResult => ({
  scannedHeaders: 0,
  signedHeaders: 0,
  reconciledHeaders: 0,
  skippedHeaders: 0,
  payloadFetches: [],
  errors: [
    `L1 source quarantined: ${state.quarantineReason ?? "unknown reason"}`,
  ],
});

const payloadFetchObservation = (
  headerHash: string,
  attempts: DaPayloadFetchFailure["attempts"],
): CommitteePayloadFetchObservation => {
  const status = attempts.every((attempt) => attempt.status === "not_found")
    ? "missing_da"
    : "fetch_failed";
  const detail = attempts
    .map((attempt) => `${attempt.sourcePeerId}:${attempt.status}`)
    .join(",");
  return {
    headerHash,
    status,
    sourcePeerIds: attempts.map((attempt) => attempt.sourcePeerId),
    ...(detail.length === 0 ? {} : { detail }),
  };
};
