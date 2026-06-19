import type * as SDK from "@al-ft/midgard-sdk";

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
  readonly daAttestation: string;
  readonly chainPoint: ChainPoint;
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
  readonly daAttestation: string;
  readonly observedChainPoint: ChainPoint;
  readonly finalized: boolean;
  readonly status: StateQueueHeaderStatus;
  readonly validationErrors: readonly string[];
  readonly updatedAt: string;
};

export type DaPayloadRecord = {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly payloadCborHex: string;
  readonly payloadSha256: string;
  readonly sourceEndpoint: string;
  readonly fetchedAt: string;
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

export type PayloadRootSet = {
  readonly utxosRoot: string;
  readonly transactionsRoot: string;
  readonly depositsRoot: string;
  readonly withdrawalsRoot: string;
};

export type ValidationSummary = {
  readonly payloadVersion: number;
  readonly rootsMatch: boolean;
  readonly stateQueueOutRef: string;
  readonly headerHash: string;
  readonly rootSummary: PayloadRootSet;
  readonly l1Header: {
    readonly startTime: string;
    readonly endTime: string;
    readonly operatorVkey: string;
    readonly prevHeaderHash: string;
    readonly protocolVersion: string;
  };
};

export type DaSignatureRecord = {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly signerIndex: number;
  readonly signatureWitness: string;
  readonly payloadHash: string;
  readonly committeeSignersHash: string;
  readonly signedAt: string;
  readonly broadcastStatus: "local" | "posted" | "post_failed";
  readonly source?: "local" | "peer" | "legacy";
  readonly sourcePeer?: string;
  readonly receivedAt?: string;
  readonly verifiedAt?: string;
  readonly l1ChainPoint: ChainPoint;
  readonly validation: ValidationSummary;
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
  readonly baseUrls: readonly string[];
  readonly canSubmitL1: boolean;
};

export type DaPeerConfig = {
  readonly signerIndex?: number;
  readonly baseUrl: string;
};

export type DaPeerBroadcastRecord = {
  readonly deploymentFingerprint: string;
  readonly peerBaseUrl: string;
  readonly headerHash: string;
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
  readonly peerBaseUrl: string;
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
