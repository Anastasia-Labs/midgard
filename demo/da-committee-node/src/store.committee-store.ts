import { type DeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

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
import type { StateQueueReplayAnchor } from "./l1/state-queue-scanner.js";
import type { StateQueueOutputStep } from "./l1/terminal-retention-observation.js";

export type RetainedPayloadPruneRequest = {
  readonly headerHash: string;
  readonly nowMs: number;
  readonly retentionDays?: number;
  /** Header hash in the L1 `ConfirmedState` datum of the caller's view. */
  readonly confirmedHeadHash: string;
  /** Every header hash live in the L1 state queue of the caller's view. */
  readonly liveQueueHeaderHashes: ReadonlySet<string>;
};

export type StoreData = {
  readonly deployment?: CommitteeDeploymentRecord;
  readonly chainCursor?: L1SourceState;
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
 * A store instance holds its store's instance lock for its whole life (the
 * JSON store's exclusive lock file, the Postgres store's session advisory
 * lock), so a pending attempt it did not begin was begun by an earlier holder
 * of that lock, which is gone, and is retried at once.
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
  readonly sourceMode: L1SourceState["sourceMode"];
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
  readonly quarantineReason?: string;
  readonly quarantinedAt?: string;
};

/**
 * The status of an observation made where the node saw only authenticated
 * replay, which carries each header's outputs but not their datums: a
 * catch-up that moved a header to an output the snapshot does not show it at.
 * It is never guessed. A later observation of the same output fills it in, as
 * does one that final replay explains moving the header on from there, and
 * neither may contradict the status known before it; no decision binds to an
 * observation with this status.
 */
export const UNKNOWN_STATE_QUEUE_STATUS = "unknown";

export type L1ObservedStatus =
  | StateQueueHeaderRecord["status"]
  | typeof UNKNOWN_STATE_QUEUE_STATUS;

export type L1ObservedDecision = {
  readonly headerHash: string;
  readonly stateQueueOutRef: string;
  readonly stateQueueStatus: L1ObservedStatus;
  /**
   * Present exactly when `stateQueueStatus` is unknown: the last status the
   * node knew this header by, which a later observation filling the unknown
   * status in may not contradict (see `persistedDecisionTransition`).
   */
  readonly lastKnownStatus?: StateQueueHeaderRecord["status"];
  readonly slot?: number;
  readonly blockHash?: string;
  readonly finalized: boolean;
  readonly hasPersistedDecision: boolean;
  /**
   * The final authenticated replay steps, oldest first, that moved or removed
   * this header's output in the scan that made this observation. Present only
   * when there were any; they are what lets a persisted decision's output or
   * status change (see `persistedDecisionTransition`).
   */
  readonly authenticatedSteps?: readonly StateQueueOutputStep[];
};

export type L1SourceState = {
  readonly schemaVersion: 1;
  readonly sourceMode: "local_node" | "external_providers";
  readonly network: string;
  readonly authoritySha256: string;
  readonly status: "healthy" | "quarantined";
  readonly observations: readonly L1ObservedDecision[];
  readonly observedAt: string;
  readonly stateQueueReplayAnchor?: StateQueueReplayAnchor;
  readonly quarantineReason?: string;
  readonly quarantinedAt?: string;
};

export type CommitteeDeploymentRecord = {
  readonly marker: DeploymentMarker;
  readonly manifestSha256: string;
  readonly contractDeploymentInfoSha256: string;
  readonly manifestRaw: string;
};

export interface CommitteeStore {
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
  quarantineL1Decisions(state: L1SourceState): Promise<void>;
  upsertStateQueueHeader(record: StateQueueHeaderRecord): Promise<void>;
  listStateQueueHeaders(): Promise<readonly StateQueueHeaderRecord[]>;
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
