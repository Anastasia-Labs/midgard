import type { AvailabilityResponseAdmissionDecision } from "@al-ft/midgard-core";
import { DaGossipTopic } from "@al-ft/midgard-core/da-transport";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { WALLET_SEED_PENDING } from "@al-ft/midgard-l1-follower";

import { committeePromisePolicyStatus } from "./availability/promise-profile-selection.js";
import {
  coordinatorPostFailedMessage,
  shouldRepublishSignatureForHeader,
} from "./committee-service.coordinator-post-failed-message.js";
import {
  type CommitteeL1SubmitterPreflightSnapshot,
  type CommitteeL1View,
  type CommitteePayloadFetchObservation,
  type CommitteeReadinessSnapshot,
  type CommitteeRetentionReadinessSnapshot,
  type CommitteeServiceDeps,
  type CommitteeTickResult,
  type CoordinatorPublishResult,
  createDaConflictEvidenceGossipHandler,
  payloadFetchObservation,
  type SignedHeaderResult,
} from "./committee-service.ingest-da-conflict-evidence.js";
import {
  daParamsMismatch,
  headerRecordOf,
  heldTickResult,
  L1_DA_PARAMS_MISMATCH,
  L1_DA_PARAMS_UNAVAILABLE,
  L1_FOLLOWER_NOT_INITIALIZED,
  L1_STATE_QUEUE_UNHEALTHY,
  retirementFloorHold,
  statusIdentityMismatch,
  STORE_INTEGRITY,
  terminalRecordOf,
} from "./committee-service.l1-tick.js";
import { signVerifiedCommitteePayload } from "./committee-service.sign-verified-payload.js";
import { l1SourceAuthorityDigest } from "./config.js";
import type { DaSubmitterFundingCheck } from "./coordinator/lucid-submitter.js";
import {
  type DaBondPoolCheck,
  daBondPoolReadinessReasons,
} from "./coordinator/pool-monitor.js";
import {
  daPayloadSha256,
  DaPayloadValidationError,
  resolvePreBlockUtxos,
  type VerifiedDaPayload,
  verifyDaPayloadAgainstHeader,
} from "./da/payload.js";
import type { DaPayloadCandidate } from "./da/source.js";
import type {
  DaPayloadRecord,
  DaSignatureRecord,
  StateQueueHeaderRecord,
} from "./domain.js";
import { committeeL1InterventionReason } from "./l1/follower/l1-follower.js";
import {
  buildDaSignatureConflictEvidence,
  classifyDaLocalSigningCommitment,
  deriveExpectedDaAvailabilityCommitment,
  validateDaSignatureRecord,
} from "./peer/signatures.js";
import {
  decisionEffectId,
  DecisionEffectInFlightError,
  type DecisionOutboxRecord,
  hasPayloadBytes,
  type L1ObservedDecision,
} from "./store.js";
import { hexToBytes } from "./utils/hex.js";

/** The fields of a header record a tick re-writes when they change. */
const recordIdentity = (record: StateQueueHeaderRecord): string =>
  JSON.stringify([
    record.stateQueueOutRef,
    record.status,
    record.finalized,
    record.observedChainPoint.blockHash,
    record.observedChainPoint.providerSource,
    record.validationErrors,
  ]);

export class CommitteeService {
  private readonly deps: CommitteeServiceDeps;
  private promiseAdmission?: AvailabilityResponseAdmissionDecision;
  private tickInFlight?: Promise<CommitteeTickResult>;
  private l1View: CommitteeL1View | undefined;
  /**
   * Headers whose payloads retirement must keep: the landed queue, the queue
   * at the latest final block, and every signed header whose commit is not
   * yet final or provably unable to land. Set by the last tick that read a
   * view.
   */
  private retirementPins: ReadonlySet<string> = new Set();
  readRetirementOperationalPins(): readonly string[] {
    return [...this.retirementPins];
  }
  /**
   * Why the last tick that read a view made no decision (the follower's own
   * reasons are read live). Empty when it decided.
   */
  private holdReasons: readonly string[] = [];
  /**
   * A stored record the facts contradict. Sticky for the life of the
   * process: nothing is written or decided until an operator looks.
   */
  private storeIntegrity: string | undefined;
  /** A stored L1 source state for another network; sticky like the above. */
  private l1SourceMismatch: string | undefined;
  /** When the follower's cursor last moved, seen by `latestL1ProgressAtMs`. */
  private l1ProgressAtMs: number | undefined;
  private l1ProgressCursorSlot: number | null = null;
  /**
   * Stored payloads (`headerHash:payloadSha256`) whose cached malformed or
   * root-mismatch verdict this process has already re-checked. A cached
   * rejection may come from an earlier verifier build; verification is a pure
   * function of the hash-checked bytes and the header, so re-checking once per
   * process lets a fixed verifier admit what an old one wrongly refused
   * without re-verifying a genuinely bad payload on every tick.
   */
  private readonly reverifiedRejectedPayloads = new Set<string>();
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
   * When the follower's cursor was last seen moving. A follower catching up
   * accepts no view, but it is progress toward one.
   */
  latestL1ProgressAtMs(): number | undefined {
    const cursorSlot = this.deps.l1.cursorSlot();
    if (cursorSlot !== null && cursorSlot !== this.l1ProgressCursorSlot) {
      this.l1ProgressAtMs = (this.deps.now?.() ?? new Date()).getTime();
    }
    this.l1ProgressCursorSlot = cursorSlot;
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
    // The on-chain DA params are checked on every tick against the
    // follower's facts, never here: a startup read could only fail closed.
    const l1State = await this.deps.store.getL1SourceState();
    if (l1State !== undefined && l1State.network !== this.deps.config.network) {
      this.l1SourceMismatch = `l1_source_configuration_changed: stored network ${l1State.network}, configured ${this.deps.config.network}`;
      this.writeEvent({
        event: "l1_source_configuration_changed",
        detail: this.l1SourceMismatch,
      });
      return;
    }
    if (
      l1State === undefined ||
      l1State.authoritySha256 !== this.l1SourceAuthoritySha256()
    ) {
      if (l1State !== undefined) {
        this.writeEvent({
          event: "l1_source_configuration_changed",
          stored: l1State.authoritySha256,
          configured: this.l1SourceAuthoritySha256(),
        });
      }
      await this.deps.store.saveL1SourceState({
        schemaVersion: 1,
        sourceMode: "local_node",
        network: this.deps.config.network,
        authoritySha256: this.l1SourceAuthoritySha256(),
        status: "healthy",
        observations: l1State?.observations ?? [],
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
      readonly l1SubmitterFunding?: DaSubmitterFundingCheck;
      /**
       * The last successful pooled DA bond read. A short or Withdrawing pool
       * cannot back an attestation, so this node is not ready.
       */
      readonly daBondPool?: DaBondPoolCheck;
      readonly retention?: CommitteeRetentionReadinessSnapshot;
    } = {},
  ): Promise<CommitteeReadinessSnapshot> {
    const deployment = await this.deps.store.getDeployment();
    const l1SourceState = await this.deps.store.getL1SourceState();
    // Indexed counters and the open-header slice; never a full table scan.
    const counts = await this.deps.store.readinessCounts();
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
    // The follower's named reasons, read live, then why the last tick that
    // read a view decided nothing.
    const l1Held = this.deps.l1.readiness();
    const l1Intervention = committeeL1InterventionReason(l1Held);
    reasons.push(
      ...l1Held.map(({ reason, detail }) => `${reason}: ${detail}`),
      ...(this.l1SourceMismatch === undefined ? [] : [this.l1SourceMismatch]),
      ...this.holdReasons,
    );
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
    if (args.l1SubmitterFunding?.sufficient === false) {
      reasons.push(
        `l1_submitter_fee_funding_short: plainAdaLovelace=${args.l1SubmitterFunding.plainAdaLovelace.toString()}, requiredLovelace=${args.l1SubmitterFunding.requiredLovelace.toString()}, checkedAt=${args.l1SubmitterFunding.checkedAt}`,
      );
    }
    if (args.daBondPool !== undefined) {
      reasons.push(...daBondPoolReadinessReasons(args.daBondPool));
    }
    if (args.retention?.status === "not_checked")
      reasons.push("retention check has not completed");
    if (args.retention?.status === "failed") {
      reasons.push(
        `retention check failed: ${args.retention.error ?? "unknown error"}`,
      );
    }
    reasons.push(...(args.retention?.pinFailures ?? []));
    if (args.retention?.status === "l1_view_stale") {
      reasons.push(
        `l1_view_stale:${(args.retention.l1ViewAgeMs ?? 0).toString()}`,
      );
    }

    const promiseAdmissionPolicy =
      this.deps.signer === undefined
        ? undefined
        : await committeePromisePolicyStatus(this.deps);
    if (promiseAdmissionPolicy?.status === "unavailable")
      reasons.push(
        `da_new_promise_policy_unavailable: ${promiseAdmissionPolicy.reason}`,
      );
    if (
      this.promiseAdmission !== undefined &&
      this.promiseAdmission.status !== "admitted"
    ) {
      reasons.push(
        `da_new_promise_${this.promiseAdmission.status}: ${this.promiseAdmission.reason}`,
      );
    }
    return {
      ready: reasons.length === 0,
      ...(promiseAdmissionPolicy === undefined
        ? {}
        : { promiseAdmissionPolicy }),
      ...(this.promiseAdmission === undefined
        ? {}
        : { promiseAdmission: this.promiseAdmission }),
      l1Source: {
        sourceMode: "local_node",
        ...(l1Intervention === undefined
          ? { status: l1SourceState?.status ?? "uninitialized" }
          : {
              status: "intervention" as const,
              intervention: `${l1Intervention.reason}: ${l1Intervention.detail}`,
            }),
        ...(l1SourceState?.observedAt === undefined
          ? {}
          : { observedAt: l1SourceState.observedAt }),
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
      counts,
      reasons,
    };
  }
  private async tickOnceWithStatus(): Promise<CommitteeTickResult> {
    const startedAt = new Date().toISOString();
    try {
      const result = await this.deps.store.withRetirementDiscovery(() =>
        this.tickOnce(),
      );
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
    const l1 = this.deps.l1;
    // The follower's own reasons are reported live; nothing is read or
    // decided until it is ready. An owed own-wallet seed holds submission
    // only, not decisions.
    const following = l1
      .readiness()
      .filter(({ reason }) => reason !== WALLET_SEED_PENDING);
    if (this.l1SourceMismatch !== undefined || following.length > 0) {
      this.holdReasons = [];
      return heldTickResult([
        ...(this.l1SourceMismatch === undefined ? [] : [this.l1SourceMismatch]),
        ...following.map(({ reason, detail }) => `${reason}: ${detail}`),
      ]);
    }
    const floorHold = await retirementFloorHold(this.deps.store, l1);
    const stored = await this.deps.store.listUnsettledStateQueueHeaders();
    const signed = await this.deps.store.listSignedDecisions();
    const view = await l1.readView({
      signed,
      exitsOf: stored.map(({ headerHash }) => headerHash),
    });
    if (view === null) {
      this.holdReasons = [
        `${L1_FOLLOWER_NOT_INITIALIZED}: the follower store holds no cursor yet`,
      ];
      return heldTickResult(this.holdReasons);
    }
    const now = this.nowIso();
    const fingerprint = this.deps.config.deploymentFingerprint;
    const records = view.queue.nodes.flatMap((node) => {
      const record = headerRecordOf(
        node,
        view,
        l1.parameters,
        fingerprint,
        now,
      );
      return record === null ? [] : [record];
    });
    const live = new Set(records.map(({ headerHash }) => headerHash));
    const exits = new Map(view.exits.map((exit) => [exit.headerHash, exit]));
    const terminal = stored.flatMap((record) => {
      const exit = live.has(record.headerHash)
        ? undefined
        : exits.get(record.headerHash);
      const settled =
        exit === undefined
          ? null
          : terminalRecordOf(record, exit, l1.parameters, now);
      return settled === null ? [] : [settled];
    });
    const mismatch = statusIdentityMismatch(stored, records);
    if (mismatch !== undefined && this.storeIntegrity === undefined) {
      this.storeIntegrity = `${STORE_INTEGRITY}: ${mismatch}`;
      this.writeEvent({ event: "committee_store_integrity", detail: mismatch });
    }
    this.holdReasons = [
      ...(this.storeIntegrity === undefined ? [] : [this.storeIntegrity]),
      ...(floorHold === undefined ? [] : [floorHold]),
      ...(view.queue.healthy
        ? []
        : [
            `${L1_STATE_QUEUE_UNHEALTHY}: ${view.queue.reason ?? ""} ${view.queue.detail ?? ""}`,
          ]),
      ...(await this.daParamsHold()),
    ];
    if (this.storeIntegrity !== undefined) {
      return heldTickResult(this.holdReasons);
    }
    const storedIdentity = new Map(
      stored.map((record) => [record.headerHash, recordIdentity(record)]),
    );
    for (const record of [...records, ...terminal]) {
      if (storedIdentity.get(record.headerHash) !== recordIdentity(record)) {
        await this.deps.store.upsertStateQueueHeader(record);
      }
    }
    this.retirementPins = new Set([
      ...live,
      ...view.finalQueueHeaderHashes,
      ...view.obligations
        .filter(({ retainCommitRecord }) => retainCommitRecord)
        .map(({ headerHash }) => headerHash),
    ]);
    const decisionsAllowed = this.holdReasons.length === 0;
    if (decisionsAllowed && view.queue.root !== null) {
      this.l1View = {
        observedAtMs: (this.deps.now?.() ?? new Date()).getTime(),
        confirmedHeadHash: view.queue.root.headerHash,
        liveQueueHeaderHashes: new Set([
          ...live,
          ...view.finalQueueHeaderHashes,
        ]),
        finalBlockTimeMs: view.finalBlockTimeMs,
      };
    }
    const errors: string[] = [];
    const payloadFetches: CommitteePayloadFetchObservation[] = [];
    let signedHeaders = 0;
    let reconciledHeaders = 0;
    let skippedHeaders = 0;
    for (const record of records) {
      if (!decisionsAllowed) {
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
    await this.persistL1SourceState(
      [...records, ...terminal],
      new Set(signed.map(({ headerHash }) => headerHash)),
    );
    return {
      scannedHeaders: records.length,
      signedHeaders,
      reconciledHeaders,
      skippedHeaders,
      payloadFetches,
      errors,
      ...(decisionsAllowed ? {} : { held: this.holdReasons }),
    };
  }

  /**
   * Why the on-chain DA params hold decisions this tick: they differ from
   * the configured ones, or the follower's facts could not be read.
   */
  private async daParamsHold(): Promise<readonly string[]> {
    const reader = this.deps.daChainReader;
    if (reader === undefined) return [];
    try {
      const mismatch = daParamsMismatch(
        await reader.fetchDaParams(),
        this.deps.config.daParams,
      );
      return mismatch === undefined
        ? []
        : [`${L1_DA_PARAMS_MISMATCH}: ${mismatch}`];
    } catch (error) {
      return [
        `${L1_DA_PARAMS_UNAVAILABLE}: ${error instanceof Error ? error.message : String(error)}`,
      ];
    }
  }

  private l1SourceAuthoritySha256(): string {
    return l1SourceAuthorityDigest(this.deps.config);
  }

  /**
   * Records where this tick saw each header, when that changed. Informational:
   * a decision is bound by its own signature row, and an observation of a
   * header with a persisted decision is kept by the store's merge.
   */
  private async persistL1SourceState(
    records: readonly StateQueueHeaderRecord[],
    signed: ReadonlySet<string>,
  ): Promise<void> {
    const prior = await this.deps.store.getL1SourceState();
    const known = new Map(
      (prior?.observations ?? []).map((observation) => [
        observation.headerHash,
        observation,
      ]),
    );
    const observations = records
      .map((record) =>
        this.observedDecision(
          record,
          signed.has(record.headerHash) ||
            known.get(record.headerHash)?.hasPersistedDecision === true,
        ),
      )
      .sort((left, right) => left.headerHash.localeCompare(right.headerHash));
    const proposed = new Set(observations.map(({ headerHash }) => headerHash));
    if (
      prior !== undefined &&
      observations.every(
        (observation) =>
          JSON.stringify(known.get(observation.headerHash)) ===
          JSON.stringify(observation),
      ) &&
      prior.observations.every(
        ({ headerHash, hasPersistedDecision }) =>
          hasPersistedDecision || proposed.has(headerHash),
      )
    )
      return;
    await this.deps.store.saveL1SourceState({
      schemaVersion: 1,
      sourceMode: "local_node",
      network: this.deps.config.network,
      authoritySha256: this.l1SourceAuthoritySha256(),
      status: "healthy",
      observations,
      observedAt: this.nowIso(),
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
    };
  }

  private async fetchVerifyAndSign(
    record: StateQueueHeaderRecord,
    payloadFetches: CommitteePayloadFetchObservation[],
  ): Promise<SignedHeaderResult | undefined> {
    const verified = await this.fetchVerifyPayload(record, payloadFetches);
    if (verified === undefined) {
      return undefined;
    }
    return signVerifiedCommitteePayload(
      this.deps,
      record,
      verified,
      (decision) => {
        this.promiseAdmission = decision;
        this.writeEvent({ event: "da_promise_admission", ...decision });
      },
    );
  }

  private async fetchVerifyPayload(
    record: StateQueueHeaderRecord,
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
    record: StateQueueHeaderRecord,
    payloadRecord: DaPayloadRecord,
  ): Promise<VerifiedDaPayload> {
    if (payloadRecord.validationStatus === "conflicted") {
      throw new Error(payloadRecord.validationError ?? "payload conflict");
    }
    if (
      payloadRecord.validationStatus === "malformed_da" ||
      payloadRecord.validationStatus === "root_mismatch"
    ) {
      const reverifyKey = `${payloadRecord.headerHash}:${payloadRecord.payloadSha256}`;
      if (this.reverifiedRejectedPayloads.has(reverifyKey)) {
        throw new Error(
          payloadRecord.validationError ??
            `stored DA payload is ${payloadRecord.validationStatus}`,
        );
      }
      this.reverifiedRejectedPayloads.add(reverifyKey);
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
    record: StateQueueHeaderRecord,
    payloadRecord: DaPayloadRecord,
  ): Promise<VerifiedDaPayload> {
    // Established before verification: an unavailable parent state is not a
    // verdict on this payload, so it never marks the payload rejected.
    const preBlockUtxos = await resolvePreBlockUtxos({
      header: record.header,
      getDaPayload: (headerHash) => this.deps.store.getDaPayload(headerHash),
      payloadSource: this.deps.payloadSource,
    });
    try {
      const verificationOptions = {
        payloadSchemaVersion: 1,
        stateQueueOutRef: record.stateQueueOutRef,
        preBlockUtxos,
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
    // output the header has since moved from follows the header, as its
    // decision observation does this tick.
    const signature: DaSignatureRecord =
      validatedSignature.validation.stateQueueOutRef ===
        record.stateQueueOutRef &&
      validatedSignature.l1ChainPoint.blockHash ===
        record.observedChainPoint.blockHash
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
      sourceMode: "local_node",
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
        sourceMode: "local_node",
        network: this.deps.config.network,
        authoritySha256: this.l1SourceAuthoritySha256(),
        status: "healthy",
        observations,
        observedAt: now,
      },
      ...(signature === undefined ? {} : { signature }),
    });
    return effect;
  }

  private coordinatorPostFailedMessage(
    record: Pick<DaSignatureRecord, "headerHash" | "signerIndex">,
  ): string {
    return coordinatorPostFailedMessage(this.deps.coordinator, record);
  }

  private shouldRepublishExistingSignatureForHeader(
    record: DaSignatureRecord,
    status: StateQueueHeaderRecord["status"],
  ): boolean {
    return shouldRepublishSignatureForHeader(
      this.deps.coordinator,
      record,
      status,
    );
  }
}
