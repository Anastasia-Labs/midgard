import { type DeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import type { PromiseCapacityEvidence } from "./availability/promise-capacity-evidence.js";
import type {
  DaAttestationCandidateRecord,
  DaPayloadRecord,
  DaPeerBroadcastRecord,
  DaPeerHealthRecord,
  DaPeerNonceRecord,
  DaSignatureRecord,
  DaSignatureRecordV1,
  DaStoredConflictEvidenceRecord,
  DaStoredPayloadRecord,
  L1SubmissionRecord,
  StateQueueHeaderRecord,
} from "./domain.js";
import type { SignedHeader } from "./l1/follower/obligations.js";
import type { CommitteePinTargets } from "./l1/follower/retention-pins.js";
import type {
  PromiseStoreResourceLimits,
  PromiseStoreResourceUsage,
} from "./store/promise-resource-usage.js";
import type { CommitteeRetirementCertificate } from "./store/retirement-certificate.js";
import type {
  CommitteeRetirementBreachPoint,
  CommitteeRetirementFloor,
  CommitteeRetirementGuard,
  CommitteeRetirementSnapshot,
} from "./store/retirement-model.js";

export type RetainedPayloadPruneRequest = {
  readonly headerHash: string;
  /** The release clock of the caller's view: its latest final block's time. */
  readonly finalBlockTimeMs: number | null;
  readonly retentionDays?: number;
  readonly automaticRecoveryMaxDepth?: number;
  readonly deploymentFingerprint?: string;
  /** Header hash in the L1 `ConfirmedState` datum of the caller's view. */
  readonly confirmedHeadHash: string;
  /** Every header hash live in the L1 state queue of the caller's view. */
  readonly liveQueueHeaderHashes: ReadonlySet<string>;
};

export type StoreData = {
  readonly promiseCapacityEvidence: Record<string, PromiseCapacityEvidence>;
  readonly deployment?: CommitteeDeploymentRecord;
  readonly chainCursor?: L1SourceState;
  readonly retirementFloor?: CommitteeRetirementFloor;
  readonly stateQueueHeaders: Record<string, StateQueueHeaderRecord>;
  readonly daPayloads: Record<string, DaStoredPayloadRecord>;
  readonly daSignatures: Record<string, DaSignatureRecordV1>;
  readonly daConflictEvidence: Record<string, DaStoredConflictEvidenceRecord>;
  readonly daAttestationCandidates: Record<
    string,
    DaAttestationCandidateRecord
  >;
  readonly l1Submissions: Record<string, L1SubmissionRecord>;
  readonly peerBroadcasts: Record<string, DaPeerBroadcastRecord>;
  readonly peerHealth: Record<string, DaPeerHealthRecord>;
  readonly peerNonces: Record<string, DaPeerNonceRecord>;
  readonly decisionOutbox: Record<string, DecisionOutboxRecord>;
};

export type DecisionOutboxStatus =
  | "pending"
  | "published"
  | "failed"
  | "reconciled";

/**
 * Refuses an attempt at a decision effect while this store instance already
 * has an attempt at the same effect in flight: a live attempt is never run
 * twice. The caller defers the effect to a later tick.
 *
 * Only an attempt this instance began and has not yet completed conflicts.
 * A store instance writes only while it holds the store's instance lock (a
 * Postgres session advisory lock), so a pending attempt it did not begin was
 * begun by an earlier holder of that lock, whose session has ended, and is
 * retried at once.
 */
export class DecisionEffectInFlightError extends Error {
  readonly effectId: string;

  constructor(effectId: string) {
    super(
      `decision outbox pending attempt is still in flight in this committee node process: ${effectId}`,
    );
    this.name = "DecisionEffectInFlightError";
    this.effectId = effectId;
  }
}

/**
 * The attempts at decision effects that one store instance has begun and not
 * yet completed, by effect id: the in-process half of decision-effect mutual
 * exclusion. The store's instance lock is the cross-process half.
 */
export class InFlightDecisionAttempts {
  private readonly attempts = new Map<string, number>();

  /**
   * Records `effect` as in flight, or throws `DecisionEffectInFlightError`
   * when an attempt at the same effect already is. Called inside the store's
   * atomic section, after every other check of the begin has passed.
   */
  claim(effect: Pick<DecisionOutboxRecord, "effectId" | "attemptCount">): void {
    if (this.attempts.has(effect.effectId)) {
      throw new DecisionEffectInFlightError(effect.effectId);
    }
    this.attempts.set(effect.effectId, effect.attemptCount);
  }

  /**
   * Ends attempt `attemptCount` at `effectId`, whether or not it completed
   * durably: its side effects are over once its caller has stopped.
   */
  has(effectId: string): boolean {
    return this.attempts.has(effectId);
  }

  release(effectId: string, attemptCount: number): void {
    if (this.attempts.get(effectId) === attemptCount) {
      this.attempts.delete(effectId);
    }
  }
}

export type DecisionOutboxRecord = {
  readonly schemaVersion: 1;
  readonly effectId: string;
  readonly deploymentFingerprint: string;
  /**
   * `external_providers` only on a terminal record (published, reconciled
   * or failed) a build from before the L1 follower wrote: the store open
   * leaves those rows as they are. A pending record is always `local_node`.
   */
  readonly sourceMode: L1SourceState["sourceMode"] | "external_providers";
  readonly network: string;
  readonly effectKind: "signature_publish" | "l1_reconcile";
  readonly headerHash: string;
  readonly stateQueueOutRef: string;
  readonly signerIndex?: number;
  readonly slot?: number;
  readonly blockHash?: string;
  readonly finalized: true;
  readonly status: DecisionOutboxStatus;
  readonly attemptCount: number;
  readonly createdAt: string;
  readonly updatedAt: string;
  readonly lastError?: string;
};

export type L1ObservedStatus = StateQueueHeaderRecord["status"];

/**
 * Where the committee last saw a header on L1 (the follower's landed queue).
 * Informational: a rollback below k moves it with the chain, and a decision
 * is bound by its own signature row (class B), never by this observation.
 */
export type L1ObservedDecision = {
  readonly headerHash: string;
  readonly stateQueueOutRef: string;
  readonly stateQueueStatus: L1ObservedStatus;
  readonly slot?: number;
  readonly blockHash?: string;
  readonly finalized: boolean;
  readonly hasPersistedDecision: boolean;
};

export type L1SourceState = {
  readonly schemaVersion: 1;
  readonly sourceMode: "local_node";
  readonly network: string;
  readonly authoritySha256: string;
  readonly status: "healthy";
  readonly observations: readonly L1ObservedDecision[];
  readonly observedAt: string;
};

export type CommitteeDeploymentRecord = {
  readonly marker: DeploymentMarker;
  readonly manifestSha256: string;
  readonly contractDeploymentInfoSha256: string;
  readonly manifestRaw: string;
};

/**
 * Readiness counts. Totals are trigger-maintained counters; the two
 * "missing" counts cover only headers that are still unattested or
 * attesting, read through a partial index.
 */
export type CommitteeStoreReadinessCounts = {
  /** Stored state-queue header rows. */
  readonly discoveredHeaders: number;
  /** Open headers whose payload is absent or not verified. */
  readonly missingPayloads: number;
  /** Stored payload rows whose validation status is verified. */
  readonly verifiedPayloads: number;
  /** Open headers with a verified payload and no submitted L1 attestation. */
  readonly verifiedPayloadsMissingL1Attestation: number;
  /** Stored DA signature rows, local and peer. */
  readonly signatures: number;
  /** Stored L1 attestation submission rows. */
  readonly l1AttestationSubmissions: number;
  /** Distinct headers with a submitted or confirmed L1 attestation. */
  readonly submittedOrConfirmedL1Attestations: number;
};

export interface CommitteeStore {
  retirementDiscoveryActive(): boolean;
  withRetirementDiscovery<T>(run: () => Promise<T>): Promise<T>;
  getRetirementFloor(): Promise<CommitteeRetirementFloor | undefined>;
  captureRetirementGuard(): CommitteeRetirementGuard;
  assertRetirementGuard(
    token: CommitteeRetirementGuard,
    record?: StateQueueHeaderRecord,
  ): void;
  readRetirementSnapshot(): Promise<CommitteeRetirementSnapshot>;
  /**
   * Every L1 point and submission the stored records read again, as the
   * follower's retention pin targets (plan §11).
   */
  readL1PinTargets(): Promise<CommitteePinTargets>;
  applyRetirementCertificate(
    certificate: CommitteeRetirementCertificate,
  ): Promise<readonly string[]>;
  recordRetirementBreach(
    reason: string,
    observedAt: CommitteeRetirementBreachPoint,
  ): Promise<void>;
  withRetainedHeaderPin<T>(
    headerHash: string,
    run: () => Promise<T>,
  ): Promise<T>;

  promiseStoreResourceUsage(
    limits?: PromiseStoreResourceLimits,
  ): Promise<PromiseStoreResourceUsage>;
  getPromiseCapacityEvidence(
    key: string,
  ): Promise<PromiseCapacityEvidence | undefined>;
  savePromiseCapacityEvidence(
    record: PromiseCapacityEvidence,
    expectedPointId?: string,
  ): Promise<PromiseCapacityEvidence>;
  close?(): Promise<void>;
  initDeployment(args: {
    readonly marker: DeploymentMarker;
    readonly manifestSha256: string;
    readonly contractDeploymentInfoSha256: string;
    readonly manifestRaw: string;
  }): Promise<void>;
  getDeployment(): Promise<CommitteeDeploymentRecord | undefined>;
  getL1SourceState(): Promise<L1SourceState | undefined>;
  saveL1SourceState(state: L1SourceState): Promise<void>;
  getDecisionOutbox(
    effectId: string,
  ): Promise<DecisionOutboxRecord | undefined>;
  listDecisionOutbox(
    headerHash?: string,
  ): Promise<readonly DecisionOutboxRecord[]>;
  /**
   * Durably begins an attempt at a decision effect and holds it in flight
   * until `completeDecisionEffect` for that attempt returns or throws. Throws
   * `DecisionEffectInFlightError` while this instance has an attempt at the
   * same effect in flight; a pending attempt left by an earlier instance is
   * retried at once.
   */
  beginDecisionEffect(args: {
    readonly effect: DecisionOutboxRecord;
    readonly sourceState: L1SourceState;
    readonly signature?: DaSignatureRecord;
  }): Promise<void>;
  completeDecisionEffect(args: {
    readonly effectId: string;
    readonly expectedAttemptCount: number;
    readonly status: Exclude<DecisionOutboxStatus, "pending">;
    readonly updatedAt: string;
    readonly lastError?: string;
    readonly signature?: DaSignatureRecord;
  }): Promise<void>;
  upsertStateQueueHeader(record: StateQueueHeaderRecord): Promise<void>;
  /**
   * The headers whose L1 outcome is not settled yet: every header not
   * recorded as merged or removed. A terminal record is written only once
   * its exit is final, so the settled history is never listed. Read
   * through a partial index, so the tick's read does not grow with it.
   */
  listUnsettledStateQueueHeaders(): Promise<readonly StateQueueHeaderRecord[]>;
  /** The stored records of `headerHashes` (absent ones are left out). */
  getStateQueueHeaders(
    headerHashes: readonly string[],
  ): Promise<readonly StateQueueHeaderRecord[]>;
  /**
   * The readiness probe's counts. Totals come from counters the store keeps
   * as it writes; the per-header counts read only headers not yet final.
   * Neither lists the store, so the probe's cost does not grow with it.
   */
  readinessCounts(): Promise<CommitteeStoreReadinessCounts>;
  /**
   * Every header this member signed, with the end time it signed under: the
   * `signed` input of the committee's obligations projection
   * (`readCommitteeView`). It is read from the member's own signature rows
   * (class B), which a follower reset or rewind never erases; only
   * retirement prunes them, with their header.
   */
  listSignedDecisions(): Promise<readonly SignedHeader[]>;
  getStateQueueHeader(
    headerHash: string,
  ): Promise<StateQueueHeaderRecord | undefined>;
  saveDaPayload(record: DaPayloadRecord): Promise<DaPayloadRecord>;
  getDaPayload(headerHash: string): Promise<DaPayloadRecord | undefined>;
  /** Q54 retention enforcement: full retained DA payload set. */
  listDaPayloads(): Promise<readonly DaStoredPayloadRecord[]>;
  /**
   * Removes one retained DA payload only if the core retention decision,
   * re-evaluated inside the store's write boundary, still prunes it.
   */
  deleteDaPayloadIfPrunable(
    request: RetainedPayloadPruneRequest,
  ): Promise<boolean>;
  saveDaSignature(record: DaSignatureRecord): Promise<void>;
  getDaSignature(args: {
    readonly headerHash: string;
    readonly availabilityCommitmentDigest: string;
    readonly signerIndex: number;
  }): Promise<DaSignatureRecord | undefined>;
  listDaSignatures(headerHash?: string): Promise<readonly DaSignatureRecord[]>;
  saveDaConflictEvidence(
    record: DaStoredConflictEvidenceRecord,
  ): Promise<boolean>;
  listDaConflictEvidence(
    headerHash?: string,
  ): Promise<readonly DaStoredConflictEvidenceRecord[]>;
  saveDaAttestationCandidate(
    record: DaAttestationCandidateRecord,
  ): Promise<void>;
  listDaAttestationCandidates(
    headerHash?: string,
  ): Promise<readonly DaAttestationCandidateRecord[]>;
  saveL1Submission(record: L1SubmissionRecord): Promise<void>;
  listL1Submissions(): Promise<readonly L1SubmissionRecord[]>;
  savePeerBroadcast(record: DaPeerBroadcastRecord): Promise<void>;
  getPeerBroadcast(args: {
    readonly peerId: string;
    readonly headerHash: string;
    readonly availabilityCommitmentDigest: string;
    readonly signerIndex: number;
  }): Promise<DaPeerBroadcastRecord | undefined>;
  listPeerBroadcasts(
    headerHash?: string,
  ): Promise<readonly DaPeerBroadcastRecord[]>;
  savePeerHealth(record: DaPeerHealthRecord): Promise<void>;
  listPeerHealth(): Promise<readonly DaPeerHealthRecord[]>;
  recordPeerNonce(record: DaPeerNonceRecord): Promise<boolean>;
}

export const signatureKey = (
  headerHash: string,
  availabilityCommitmentDigest: string,
  signerIndex: number,
): string =>
  `${headerHash}:${availabilityCommitmentDigest}:${signerIndex.toString()}`;

export const conflictEvidenceKey = (
  record: Pick<
    DaStoredConflictEvidenceRecord,
    "deploymentFingerprint" | "evidenceHash"
  >,
): string => `${record.deploymentFingerprint}:${record.evidenceHash}`;

export const peerBroadcastKey = (
  peerId: string,
  headerHash: string,
  availabilityCommitmentDigest: string,
  signerIndex: number,
): string =>
  `${peerId}:${headerHash}:${availabilityCommitmentDigest}:${signerIndex.toString()}`;

export const peerNonceKey = (
  deploymentFingerprint: string,
  signerIndex: number,
  nonce: string,
): string => `${deploymentFingerprint}:${signerIndex.toString()}:${nonce}`;

export const hasPayloadBytes = (record: DaPayloadRecord): boolean =>
  record.payloadSha256.length > 0 && record.payloadCborHex.length > 0;
