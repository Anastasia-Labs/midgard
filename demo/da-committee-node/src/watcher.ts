import type { WatcherConfig } from "./config.js";
import type { AttestationCoordinator } from "./coordinator/coordinator.js";
import type { SubmitterReconciler } from "./coordinator/submitter-reconciler.js";
import {
  daPayloadSha256,
  DaPayloadValidationError,
  type VerifiedDaPayloadV1,
  verifyDaPayloadV1AgainstHeader,
} from "./da/payload.js";
import type {
  DaPayloadCandidate,
  DaPayloadFetchFailure,
  DaPayloadSource,
} from "./da/source.js";
import type {
  DaPayloadRecord,
  DaSignatureRecord,
  StateQueueHeaderRecord,
} from "./domain.js";
import type { DaAttestationChainReader } from "./l1/da-attestation-reader.js";
import {
  scanStateQueue,
  type StateQueueProvider,
} from "./l1/state-queue-scanner.js";
import {
  type DaSigner,
  type DaSignerValidation,
  signDaAttestation,
} from "./signer.js";
import {
  hasPayloadBytes,
  type L1ObservedDecision,
  type L1SourceState,
  type WatcherStore,
} from "./store.js";
import { hexToBytes } from "./utils/hex.js";

export type WatcherServiceDeps = {
  readonly config: WatcherConfig;
  readonly store: WatcherStore;
  readonly stateQueueProvider: StateQueueProvider;
  readonly payloadSource: DaPayloadSource;
  readonly signer?: DaSigner;
  readonly signerValidation?: DaSignerValidation;
  readonly coordinator?: AttestationCoordinator;
  readonly submitterReconciler?: Pick<SubmitterReconciler, "reconcileHeader">;
  readonly daChainReader?: DaAttestationChainReader;
};

export type WatcherTickResult = {
  readonly scannedHeaders: number;
  readonly signedHeaders: number;
  readonly reconciledHeaders: number;
  readonly skippedHeaders: number;
  readonly payloadFetches: readonly WatcherPayloadFetchObservation[];
  readonly errors: readonly string[];
};

export type WatcherPayloadFetchObservation = {
  readonly headerHash: string;
  readonly status: "missing_da" | "available" | "fetch_failed";
  readonly sourcePeerIds: readonly string[];
  readonly detail?: string;
};

export type WatcherL1SubmitterPreflightSnapshot = {
  readonly status: "ready" | "funded" | "failed" | "not_required" | "not_run";
  readonly detail?: unknown;
  readonly error?: string;
};

export type WatcherReadinessPeerSnapshot = {
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
  readonly l1SubmitterPreflight: WatcherL1SubmitterPreflightSnapshot;
};

export type WatcherReadinessSnapshot = {
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
  readonly peer: WatcherReadinessPeerSnapshot;
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
  readonly publishError?: string;
};

type CoordinatorPublishResult = {
  readonly broadcastStatus: DaSignatureRecord["broadcastStatus"];
  readonly error?: string;
};

export class WatcherService {
  private readonly deps: WatcherServiceDeps;
  private tickInFlight?: Promise<WatcherTickResult>;
  private lastTick:
    | {
        readonly status: "ok" | "degraded" | "failed";
        readonly startedAt: string;
        readonly finishedAt: string;
        readonly result?: WatcherTickResult;
        readonly error?: string;
      }
    | undefined;

  constructor(deps: WatcherServiceDeps) {
    this.deps = deps;
  }

  async initialize(): Promise<void> {
    await this.deps.store.initDeployment({
      fingerprint: this.deps.config.deploymentFingerprint,
      manifestSha256: this.deps.config.deploymentManifestSha256,
      contractDeploymentInfoSha256:
        this.deps.config.contractDeploymentInfoSha256,
      manifestRaw: this.deps.config.deploymentManifestRaw,
    });
    const l1State = await this.deps.store.getL1SourceState();
    if (
      l1State !== undefined &&
      (l1State.network !== this.deps.config.network ||
        l1State.sourceMode !== this.l1SourceMode())
    ) {
      await this.quarantineL1Source(
        `l1_source_configuration_changed: stored=${l1State.sourceMode}/${l1State.network}, configured=${this.l1SourceMode()}/${this.deps.config.network}`,
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
          throw new Error(
            "on-chain DA committee does not match watcher config",
          );
        }
        if (
          daParams.committeeSignersHash !==
          this.deps.config.daParams.committeeSignersHash
        ) {
          throw new Error(
            "on-chain DA committee_signers_hash does not match watcher config",
          );
        }
        if (daParams.threshold !== this.deps.config.daParams.threshold) {
          throw new Error(
            "on-chain DA threshold does not match watcher config",
          );
        }
      } catch (error) {
        await this.quarantineL1Source(
          `l1_da_params_observation_failed: ${
            error instanceof Error ? error.message : String(error)
          }`,
          l1State,
        );
        throw error;
      }
    }
  }

  async tick(): Promise<WatcherTickResult> {
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
      readonly l1SubmitterPreflight?: WatcherL1SubmitterPreflightSnapshot;
    } = {},
  ): Promise<WatcherReadinessSnapshot> {
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
    const scanner: WatcherReadinessSnapshot["scanner"] = {
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
      deployment.fingerprint !== this.deps.config.deploymentFingerprint
    ) {
      reasons.push(
        `store deployment fingerprint ${deployment.fingerprint} does not match configured ${this.deps.config.deploymentFingerprint}`,
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
      reasons.push("last watcher tick completed with errors");
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
        ...(deployment?.fingerprint === undefined
          ? {}
          : { storeFingerprint: deployment.fingerprint }),
        storeMatchesConfigured:
          deployment?.fingerprint === this.deps.config.deploymentFingerprint,
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

  private async tickOnceWithStatus(): Promise<WatcherTickResult> {
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

  private async tickOnce(): Promise<WatcherTickResult> {
    const priorL1State = await this.deps.store.getL1SourceState();
    if (priorL1State?.status === "quarantined") {
      return quarantinedTickResult(priorL1State);
    }
    let records: Awaited<ReturnType<typeof scanStateQueue>>;
    try {
      records = await scanStateQueue(this.deps.stateQueueProvider, {
        deploymentFingerprint: this.deps.config.deploymentFingerprint,
        daAttestationPolicyId: this.deps.config.daAttestationPolicyId,
        finalityDepth: this.deps.config.finalityDepth,
        consensusProfile: this.deps.config.consensusProfile,
      });
    } catch (error) {
      const reason = `l1_source_observation_failed: ${
        error instanceof Error ? error.message : String(error)
      }`;
      await this.quarantineL1Source(reason, priorL1State);
      throw error;
    }
    const transitionFailure = l1ObservationTransitionFailure(
      priorL1State,
      records,
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
    const errors: string[] = [];
    const payloadFetches: WatcherPayloadFetchObservation[] = [];
    let signedHeaders = 0;
    let reconciledHeaders = 0;
    let skippedHeaders = 0;
    for (const record of records) {
      await this.deps.store.upsertStateQueueHeader(record);
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
      const existingSignature = await this.deps.store.getDaSignature({
        headerHash: record.headerHash,
        signerIndex: this.deps.config.signerIndex,
      });
      if (existingSignature !== undefined) {
        if (
          this.shouldRepublishExistingSignatureForHeader(
            existingSignature,
            record.status,
          )
        ) {
          const published = await this.publishSignature(existingSignature);
          if (published.broadcastStatus !== existingSignature.broadcastStatus) {
            await this.deps.store.saveDaSignature({
              ...existingSignature,
              broadcastStatus: published.broadcastStatus,
            });
          }
          if (published.broadcastStatus === "post_failed") {
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
        const { signature, publishError } = signed;
        await this.deps.store.saveDaSignature(signature);
        signedHeaders += 1;
        if (signature.broadcastStatus === "post_failed") {
          errors.push(
            publishError ?? this.coordinatorPostFailedMessage(signature),
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
    await this.persistHealthyL1SourceState(records);
    return result;
  }

  private l1SourceMode(): L1SourceState["sourceMode"] {
    return this.deps.config.l1Source.sourceMode;
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
      status: "quarantined",
      observations: previous?.observations ?? [],
      observedAt: previous?.observedAt ?? now,
      quarantineReason: reason,
      quarantinedAt: now,
    };
    await this.deps.store.quarantineL1Decisions(state);
    return state;
  }

  private async persistHealthyL1SourceState(
    records: Awaited<ReturnType<typeof scanStateQueue>>,
  ): Promise<void> {
    const submissions = await this.deps.store.listL1Submissions();
    const submittedHeaders = new Set(
      submissions.map(({ headerHash }) => headerHash),
    );
    const observations = await Promise.all(
      records.map(
        async (record): Promise<L1ObservedDecision> => ({
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
          hasPersistedDecision:
            submittedHeaders.has(record.headerHash) ||
            (await this.deps.store.listDaSignatures(record.headerHash)).length >
              0,
        }),
      ),
    );
    await this.deps.store.saveL1SourceState({
      schemaVersion: 1,
      sourceMode: this.l1SourceMode(),
      network: this.deps.config.network,
      status: "healthy",
      observations: observations.sort((left, right) =>
        left.headerHash.localeCompare(right.headerHash),
      ),
      observedAt: new Date().toISOString(),
    });
  }

  private async fetchVerifyAndSign(
    record: Awaited<ReturnType<typeof scanStateQueue>>[number],
    payloadFetches: WatcherPayloadFetchObservation[],
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
    const signatureWitness = signDaAttestation({
      signer: this.deps.signer,
      signerIndex: this.deps.config.signerIndex,
      headerHash: record.headerHash,
    });
    const signature: DaSignatureRecord = {
      deploymentFingerprint: this.deps.config.deploymentFingerprint,
      headerHash: record.headerHash,
      signerIndex: this.deps.config.signerIndex,
      signatureWitness,
      payloadHash: verified.payloadSha256,
      committeeSignersHash: this.deps.signerValidation.committeeSignersHash,
      signedAt: new Date().toISOString(),
      broadcastStatus: "local",
      source: "local",
      verifiedAt: new Date().toISOString(),
      l1ChainPoint: record.observedChainPoint,
      validation: verified.validation,
    };
    const published = await this.publishSignature(signature);
    return {
      signature: {
        ...signature,
        broadcastStatus: published.broadcastStatus,
      },
      ...(published.error === undefined
        ? {}
        : { publishError: published.error }),
    };
  }

  private async fetchVerifyPayload(
    record: Awaited<ReturnType<typeof scanStateQueue>>[number],
    payloadFetches: WatcherPayloadFetchObservation[],
  ): Promise<VerifiedDaPayloadV1 | undefined> {
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
  ): Promise<VerifiedDaPayloadV1> {
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
  ): Promise<VerifiedDaPayloadV1> {
    try {
      const verificationOptions = {
        payloadSchemaVersion: 1,
        stateQueueOutRef: record.stateQueueOutRef,
      } as const;
      const verified = await verifyDaPayloadV1AgainstHeader(
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
    payloadFetches: WatcherPayloadFetchObservation[],
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
    if (reconciler === undefined) {
      return false;
    }
    const result = await reconciler.reconcileHeader(record);
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

const l1ObservationTransitionFailure = (
  previous: L1SourceState | undefined,
  current: Awaited<ReturnType<typeof scanStateQueue>>,
): string | undefined => {
  if (previous === undefined) {
    return undefined;
  }
  const currentByHeader = new Map(
    current.map((record) => [record.headerHash, record] as const),
  );
  for (const prior of previous.observations) {
    if (!prior.hasPersistedDecision) {
      continue;
    }
    const observed = currentByHeader.get(prior.headerHash);
    if (observed === undefined) {
      return `l1_source_decision_disappeared:${prior.headerHash}`;
    }
    const expectedAttestationAdvance =
      (prior.stateQueueStatus === "unattested" ||
        prior.stateQueueStatus === "attesting") &&
      observed.status === "attested" &&
      prior.slot !== undefined &&
      observed.observedChainPoint.slot !== undefined &&
      observed.observedChainPoint.slot > prior.slot &&
      observed.stateQueueOutRef !== prior.stateQueueOutRef &&
      observed.finalized;
    if (
      !expectedAttestationAdvance &&
      (observed.stateQueueOutRef !== prior.stateQueueOutRef ||
        observed.status !== prior.stateQueueStatus ||
        observed.observedChainPoint.slot !== prior.slot ||
        observed.observedChainPoint.blockHash !== prior.blockHash)
    ) {
      return `l1_source_decision_forked:${prior.headerHash}`;
    }
    if (!expectedAttestationAdvance && !observed.finalized) {
      return `l1_source_decision_lost_finality:${prior.headerHash}`;
    }
  }
  return undefined;
};

const quarantinedTickResult = (state: L1SourceState): WatcherTickResult => ({
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
): WatcherPayloadFetchObservation => {
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
