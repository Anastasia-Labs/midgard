import {
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
} from "@al-ft/midgard-fault-proofs";

import type {
  DbEvidence,
  RawEvidenceRef,
  TransactionEvidence,
} from "../e2e/summary.js";

export const E2E_AUTHENTICATED_L1_TX_OBSERVATION_SCHEMA_VERSION =
  "midgard-e2e-authenticated-l1-tx-observation-v1" as const;

export const E2E_STATE_CORRECTION_RECOVERY_OBSERVATION_SCHEMA_VERSION =
  "midgard-e2e-state-correction-recovery-observation-v1" as const;

export const E2E_STATE_CORRECTION_FINAL_SNAPSHOT_SCHEMA_VERSION =
  "midgard-e2e-state-correction-final-snapshot-v1" as const;

export type StateCorrectionIndependentSourcePaths = {
  readonly deploymentManifestPath: string;
  readonly blueprintPath: string;
  readonly cataloguePath: string;
  readonly parametersPath: string;
  readonly workflowJournalDirectories: readonly string[];
  readonly l1ObservationPaths: readonly string[];
  readonly recoveryObservationPaths: readonly string[];
  readonly finalSnapshotPath: string;
};

export type StateCorrectionIndependentEvidence = {
  readonly db: readonly DbEvidence[];
  readonly transactions: readonly TransactionEvidence[];
  readonly rawEvidence: readonly RawEvidenceRef[];
  readonly notes: readonly string[];
};

/**
 * Non-artifact authority. Production callers must re-read the configured local
 * Cardano sources and database; returning success from data loaded out of the
 * evidence directory is not an implementation of this port.
 */
export interface StateCorrectionIndependentAuthority {
  authenticateTransaction(input: {
    readonly txHash: string;
    readonly kupoOutputIndex: number;
    readonly includedAt: ChainPoint;
    readonly observedAtTip: ChainPoint & {
      readonly confirmationDepth: number;
    };
    readonly rawSourceDigests: {
      readonly kupoResponseSha256: string;
      readonly ogmiosBlockResponseSha256: string;
      readonly ogmiosTipResponseSha256: string;
    };
  }): Promise<void>;
  authenticateFinalState(input: {
    readonly manifestId: string;
    readonly observedAt: ChainPoint & { readonly confirmationDepth: number };
    readonly stateQueueDepth: number;
    readonly unfinishedMutationJobs: number;
    readonly pendingFinalizations: number;
    readonly retainedProofTokens: readonly {
      readonly unit: string;
      readonly outRef: string;
    }[];
    readonly economics: readonly {
      readonly familyId: string;
      readonly removalTxHash: string;
      readonly kupoOutputIndex: number;
      readonly includedAt: ChainPoint;
      readonly referencedProofTokenOutRef: string;
      readonly operatorCredential: string;
      readonly proverCredential: string;
      readonly operatorBondInputOutRef: string | null;
      readonly operatorBondInputLovelace: string;
      readonly proverRewardOutputOutRef: string | null;
      readonly removalFeeLovelace: string;
      readonly slashedLovelace: string;
      readonly proverRewardLovelace: string;
    }[];
    readonly withdrawalReservePayout: {
      readonly payoutConcludeTxHash: string;
      readonly kupoOutputIndex: number;
      readonly includedAt: ChainPoint;
      readonly destination: string;
      readonly payoutValueSha256: string;
      readonly reserveValueSha256: string;
    };
    readonly snapshotDigest: string;
    readonly rawSourceDigests: {
      readonly kupoStateQueueResponseSha256: string;
      readonly kupoProofTokenResponseSha256s: readonly string[];
      readonly ogmiosTipResponseSha256: string;
      readonly nodeDatabaseExportSha256: string;
    };
  }): Promise<void>;
}

export type JsonValue =
  | null
  | boolean
  | number
  | string
  | readonly JsonValue[]
  | { readonly [key: string]: JsonValue };

export type ChainPoint = {
  readonly slot: string;
  readonly blockHash: string;
};

export type AuthenticatedL1TxObservation = {
  readonly schemaVersion: typeof E2E_AUTHENTICATED_L1_TX_OBSERVATION_SCHEMA_VERSION;
  readonly runId: string;
  readonly network: "Preprod";
  readonly manifestId: string;
  readonly txHash: string;
  readonly includedAt: ChainPoint;
  readonly observedAtTip: ChainPoint & { readonly confirmationDepth: number };
  readonly authentication: {
    readonly source: "local-kupmios-ogmios";
    readonly kupoResponsePath: string;
    readonly kupoResponseSha256: string;
    readonly ogmiosBlockResponsePath: string;
    readonly ogmiosBlockResponseSha256: string;
    readonly ogmiosTipResponsePath: string;
    readonly ogmiosTipResponseSha256: string;
  };
};

export type RecoveryObservation = {
  readonly schemaVersion: typeof E2E_STATE_CORRECTION_RECOVERY_OBSERVATION_SCHEMA_VERSION;
  readonly runId: string;
  readonly manifestId: string;
  readonly id: string;
  readonly beforeJournalSha256: string;
  readonly afterJournalSha256: string;
  readonly duplicateSubmissionCount: number;
  readonly lostEvidenceCount: number;
  readonly verifiedBeforeReconciliationCount: number;
  readonly unrecoverableWorkflowCount: number;
  readonly manualRepairCount: number;
  readonly terminalState: "recovered";
  readonly watcherState: "ready_after_reconciliation";
};

export type FinalSnapshot = {
  readonly schemaVersion: typeof E2E_STATE_CORRECTION_FINAL_SNAPSHOT_SCHEMA_VERSION;
  readonly runId: string;
  readonly network: "Preprod";
  readonly manifestId: string;
  readonly observedAt: ChainPoint & { readonly confirmationDepth: number };
  readonly authentication: {
    readonly source: "local-kupmios-ogmios-and-node-db";
    readonly kupoStateQueueResponsePath: string;
    readonly kupoStateQueueResponseSha256: string;
    readonly kupoProofTokenResponses: readonly {
      readonly unit: string;
      readonly outRef: string;
      readonly responsePath: string;
      readonly responseSha256: string;
    }[];
    readonly ogmiosTipResponsePath: string;
    readonly ogmiosTipResponseSha256: string;
    readonly nodeDatabaseExportPath: string;
    readonly nodeDatabaseExportSha256: string;
  };
  readonly stateQueue: {
    readonly depth: number;
    readonly fraudulentHeaderHashes: readonly string[];
  };
  readonly jobs: {
    readonly unfinishedMutationJobs: number;
    readonly pendingFinalizations: number;
  };
  readonly watcher: {
    readonly readiness: "ready";
    readonly verification: "resumed_after_reconciliation";
  };
  readonly economics: readonly {
    readonly familyId: string;
    readonly removalTxHash: string;
    readonly proofTokenUnit: string;
    readonly proofTokenOutRef: string;
    readonly removalReferencedProofTokenOutRef: string;
    readonly proofTokenFinalState: "retained";
    readonly operatorCredential: string;
    readonly proverCredential: string;
    readonly operatorBondInputOutRef: string | null;
    readonly operatorBondInputLovelace: string;
    readonly proverRewardOutputOutRef: string | null;
    readonly removalFeeLovelace: string;
    readonly slashedLovelace: string;
    readonly proverRewardLovelace: string;
    readonly duplicateRewardCount: number;
  }[];
  readonly withdrawalReservePayout: {
    readonly withdrawalOrderTxHash: string;
    readonly reserveTxHash: string;
    readonly payoutInitTxHash: string;
    readonly payoutAddTxHashes: readonly string[];
    readonly payoutConcludeTxHash: string;
    readonly destination: string;
    readonly payoutValueSha256: string;
    readonly reserveValueSha256: string;
    readonly status: "paid";
  };
  readonly forcedClassifications: readonly {
    readonly direction: "valid-marked-invalid" | "invalid-marked-valid";
    readonly evidenceTxHash: string;
    readonly correctionTxHash: string;
    readonly canonicalClassification: "valid" | "invalid";
    readonly finalClassification: "valid" | "invalid";
  }[];
};

export type NodeDatabaseExport = Pick<
  FinalSnapshot,
  | "runId"
  | "manifestId"
  | "stateQueue"
  | "jobs"
  | "watcher"
  | "economics"
  | "withdrawalReservePayout"
  | "forcedClassifications"
> & {
  readonly schemaVersion: "midgard-e2e-state-correction-node-db-export-v1";
};

export type LoadedWorkflow = {
  readonly directory: string;
  readonly digest: string;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
  readonly terminal: FraudProofWorkflowTerminal;
  readonly confirmedTxHashes: ReadonlySet<string>;
  readonly entryPaths: readonly string[];
};

export const record = (
  value: unknown,
  field: string,
): Readonly<Record<string, unknown>> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

export const exactKeys = (
  value: Readonly<Record<string, unknown>>,
  keys: readonly string[],
  field: string,
): void => {
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(
      `${field} must contain exactly: ${expected.join(", ")}; found: ${actual.join(", ")}`,
    );
  }
};

export const canonicalString = (value: unknown, field: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value !== value.trim()
  ) {
    throw new Error(`${field} must be a non-empty canonical string`);
  }
  return value;
};

export const exactString = <T extends string>(
  value: unknown,
  expected: T,
  field: string,
): T => {
  if (value !== expected) {
    throw new Error(`${field} must be ${expected}`);
  }
  return expected;
};

export const nonNegativeInteger = (value: unknown, field: string): number => {
  if (!Number.isSafeInteger(value) || Number(value) < 0) {
    throw new Error(`${field} must be a non-negative safe integer`);
  }
  return Number(value);
};

export const positiveInteger = (value: unknown, field: string): number => {
  const parsed = nonNegativeInteger(value, field);
  if (parsed < 1) throw new Error(`${field} must be positive`);
  return parsed;
};

export const lowerHex = (
  value: unknown,
  bytes: number,
  field: string,
): string => {
  const parsed = canonicalString(value, field);
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(parsed)) {
    throw new Error(`${field} must be ${bytes.toString()}-byte lowercase hex`);
  }
  return parsed;
};
