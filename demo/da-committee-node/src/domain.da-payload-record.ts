import * as SDK from "@al-ft/midgard-sdk";

export type ChainPoint = {
  readonly slot?: number;
  readonly blockHash?: string;
  readonly blockHeight?: number;
  readonly observedAt?: string;
  readonly depth?: number;
  readonly finalized?: boolean;
  readonly providerSource?: string;
};

export type Header = SDK.Header;

export type ObservedStateQueueNode = {
  readonly outRef: string;
  readonly assetName: string;
  readonly linkedListKey: string | "Empty";
  readonly rawDatumCbor?: string;
  readonly header: Header;
  readonly daAttestation: SDK.DaAvailabilityStateQueueStatus;
  readonly chainPoint: ChainPoint;
};

/**
 * One atomically observed state-queue view. The confirmed root is retained in
 * the observation so disappearance of a block node can only become a terminal
 * outcome when the root observation itself is final.
 */
export type ObservedStateQueueSnapshot = {
  readonly nodes: readonly ObservedStateQueueNode[];
  readonly confirmedHeaderHash: string;
  readonly confirmedStateOutRef: string;
  readonly observedChainPoint: ChainPoint;
  /**
   * Block height of the chain tip the snapshot was read at, when the source
   * can read it at that same point. The depths of the snapshot's outputs are
   * counted from this tip, and replayed history is judged final against it
   * too, so one tick judges all finality at one tip.
   */
  readonly tipBlockNo?: number;
};

export type StateQueueHeaderStatus =
  | "unattested"
  | "attesting"
  | "attested"
  | "merged"
  | "removed"
  | "conflicted";

export type StateQueueHeaderRecord = {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly stateQueueOutRef: string;
  readonly blockAssetName: string;
  readonly rawStateQueueDatumCbor?: string;
  readonly header: Header;
  readonly computedHeaderHash: string;
  readonly daAttestation: SDK.DaAvailabilityStateQueueStatus;
  readonly observedChainPoint: ChainPoint;
  readonly finalized: boolean;
  readonly status: StateQueueHeaderStatus;
  readonly validationErrors: readonly string[];
  readonly updatedAt: string;
};

export type DaPayloadRecord = {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly payloadSchemaVersion: 1;
  readonly payloadCborHex: string;
  readonly payloadSha256: string;
  readonly sourcePeerId: string;
  readonly fetchedAt: string;
  readonly payloadFetchStatus?:
    | "not_attempted"
    | "missing_da"
    | "available"
    | "fetch_failed";
  readonly verifiedAt?: string;
  readonly rootSummary?: PayloadRootSet;
  readonly validationStatus:
    | "fetched"
    | "verified"
    | "missing_da"
    | "malformed_da"
    | "root_mismatch"
    | "conflicted";
  readonly conflictStatus?: "none" | "conflicting_bytes";
  readonly validationError?: string;
};

export type DaStoredPayloadRecord = Omit<DaPayloadRecord, "rootSummary"> & {
  readonly rootSummary?: DaStoredPayloadRootSet;
};

export type PayloadRootSet = {
  readonly utxosRoot: string;
  readonly withdrawalsRoot: string;
  readonly forcedTransactionsRoot: string;
  readonly transactionsRoot: string;
  readonly depositsRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
};

export type PayloadCountSet = {
  readonly withdrawalCount: bigint;
  readonly forcedTransactionCount: bigint;
  readonly l2TransactionCount: bigint;
  readonly depositCount: bigint;
  readonly totalEventCount: bigint;
  readonly transitionStepCount: bigint;
};

export type ValidationSummary = {
  readonly payloadVersion: number;
  readonly rootsMatch: boolean;
  readonly stateQueueOutRef: string;
  readonly headerHash: string;
  readonly rootSummary: PayloadRootSet;
  readonly countSummary: PayloadCountSet;
  readonly l1Header: {
    readonly startTime: string;
    readonly endTime: string;
    readonly operatorVkey: string;
    readonly prevHeaderHash: string;
    readonly protocolVersion: string;
  };
};

export type DaStoredPayloadRootSet = PayloadRootSet & {
  readonly validationTracesRoot: string;
};

export type DaStoredPayloadCountSet = PayloadCountSet & {
  readonly validationTraceCount: bigint;
};

export type DaStoredValidationSummary = Omit<
  ValidationSummary,
  "rootSummary" | "countSummary"
> & {
  readonly rootSummary: DaStoredPayloadRootSet;
  readonly countSummary: DaStoredPayloadCountSet;
};

export type DaSignatureRecord = {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly signerIndex: number;
  readonly signatureWitness: string;
  readonly availabilityCommitmentCbor: string;
  readonly availabilityCommitmentDigest: string;
  readonly payloadHash: string;
  readonly committeeSignersHash: string;
  readonly signedAt: string;
  readonly broadcastStatus: "local" | "posted" | "post_failed";
  readonly source?: "local" | "peer";
  readonly sourcePeer?: string;
  readonly receivedAt?: string;
  readonly verifiedAt?: string;
  readonly l1ChainPoint: ChainPoint;
  readonly validation: ValidationSummary;
};

export type DaSignatureRecordV1 = Omit<
  DaSignatureRecord,
  "source" | "validation"
> & {
  readonly source: "local" | "peer";
  readonly validation: DaStoredValidationSummary;
};

export type DaStoredConflictEvidenceRecord = {
  readonly conflictSchemaVersion: 1;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly commitmentDigest: string;
  readonly conflictingHeaderHash: string;
  readonly conflictingCommitmentDigest: string;
  readonly signerIndex: number;
  readonly evidenceKind: "equivocation";
  readonly evidenceHash: string;
  readonly compactEvidenceCborHex: string;
  readonly reporterPeerId: string;
  readonly receivedAt: string;
};

export type DaAttestationCandidateRecord = {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly outRef: string;
  readonly datumCbor: string;
  readonly attestationCount: number;
  readonly threshold: number;
  readonly committeeSignersHash: string;
  readonly bitmap: string;
  readonly observedChainPoint: ChainPoint;
  readonly status: "initialized" | "signed" | "threshold" | "burned" | "stale";
};

export type L1SubmissionRecord = {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly txKind: "init" | "add_signatures" | "apply";
  readonly txHash: string;
  readonly inputsUsed: readonly string[];
  readonly submittedAt: string;
  readonly confirmedAt?: string;
  readonly resultStatus: "submitted" | "confirmed" | "failed";
  readonly failureCause?: string;
};

export type DaCommitteeMember = {
  readonly index: number;
  readonly vkey: string;
  readonly canSubmitL1: boolean;
};

export type DaPeerBroadcastRecord = {
  readonly deploymentFingerprint: string;
  readonly peerId: string;
  readonly headerHash: string;
  readonly availabilityCommitmentDigest: string;
  readonly signerIndex: number;
  readonly status: "pending" | "posted" | "failed";
  readonly attempts: number;
  readonly nextAttemptAt?: string;
  readonly lastAttemptAt?: string;
  readonly lastSuccessAt?: string;
  readonly lastError?: string;
  readonly updatedAt: string;
};

export type DaPeerHealthRecord = {
  readonly peerId: string;
  readonly signerIndex?: number;
  readonly lastSuccessAt?: string;
  readonly lastFailureAt?: string;
  readonly lastError?: string;
  readonly consecutiveFailures: number;
  readonly updatedAt: string;
};

export type DaPeerNonceRecord = {
  readonly deploymentFingerprint: string;
  readonly signerIndex: number;
  readonly nonce: string;
  readonly timestampMs: number;
  readonly receivedAt: string;
};

export const payloadRootKeys = [
  "utxosRoot",
  "withdrawalsRoot",
  "forcedTransactionsRoot",
  "transactionsRoot",
  "depositsRoot",
  "transitionTraceRoot",
  "eventToStepRoot",
  "validationTracesRoot",
] as const;

export const chainPointKeys = [
  "slot",
  "blockHash",
  "blockHeight",
  "observedAt",
  "depth",
  "finalized",
  "providerSource",
] as const;

export const payloadRecordRequiredKeys = [
  "deploymentFingerprint",
  "headerHash",
  "payloadSchemaVersion",
  "payloadCborHex",
  "payloadSha256",
  "sourcePeerId",
  "fetchedAt",
  "validationStatus",
] as const;

export const payloadRecordOptionalKeys = [
  "payloadFetchStatus",
  "verifiedAt",
  "rootSummary",
  "conflictStatus",
  "validationError",
] as const;
