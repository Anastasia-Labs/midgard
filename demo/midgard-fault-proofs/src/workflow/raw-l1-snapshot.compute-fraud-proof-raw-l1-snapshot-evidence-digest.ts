import { createHash } from "node:crypto";

import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import type { EvidenceProvenance } from "@al-ft/midgard-sdk";

export const FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION =
  "midgard-fraud-proof-raw-l1-snapshot-v1" as const;

export const FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY =
  "midgard-fraud-proof-raw-l1-snapshot-authority-v1" as const;

/** Action inclusion and durable release evidence use the same authentication. */
export type FraudProofL1ObservationDepth =
  | "inclusion"
  | "release_finality"
  | "recovery_finality";

export type FraudProofRawL1ComputationStepRole =
  | "computation_thread_step_01"
  | "computation_thread_step_02"
  | "computation_thread_step_03"
  | "computation_thread_step_04"
  | "computation_thread_step_05"
  | "computation_thread_step_06"
  | "computation_thread_step_07"
  | "computation_thread_step_08"
  | "computation_thread_step_09"
  | "computation_thread_step_10"
  | "computation_thread_step_11"
  | "computation_thread_step_12"
  | "computation_thread_step_13"
  | "computation_thread_step_14"
  | "computation_thread_step_15"
  | "computation_thread_step_16"
  | "computation_thread_step_17";

export type FraudProofRawL1ScopeRole =
  | "deposit_event"
  | "withdrawal_event"
  | "deposit_history_data"
  | "withdrawal_history_data"
  | "forced_transaction_event"
  | "hub_oracle"
  | "state_queue"
  | FraudProofRawL1ComputationStepRole
  | "permanent_proof_token"
  | "active_operator_directory"
  | "retired_operator_directory"
  | "scheduler"
  | "proof_chunk"
  | "field_publication"
  | "field_certificate";

export type FraudProofRawL1SnapshotRequest = {
  readonly deploymentIdentityDigest: string;
  readonly blueprintHash: string;
  readonly finalityPolicyDigest: string;
  readonly headerHash: string;
  readonly scopes: readonly {
    readonly role: FraudProofRawL1ScopeRole;
    readonly address: string;
  }[];
  /** Exact units whose create/spend history must be returned. */
  readonly historyUnits: readonly string[];
};

export type FraudProofRawL1Point = {
  readonly slot: string;
  readonly blockHash: string;
  readonly blockNo: string;
  readonly pointId: string;
};

export type FraudProofRawL1Utxo = {
  readonly outRef: string;
  readonly outputCbor: string;
  readonly datumCbor: string | null;
  readonly referenceScriptCbor: string | null;
};

export type FraudProofRawL1Transaction = {
  readonly txHash: string;
  readonly bodyCbor: string;
  readonly witnessSetCbor: string;
  readonly redeemersCbor: string | null;
  readonly isValid: true;
  readonly inclusionPoint: FraudProofRawL1Point;
  readonly confirmationDepth: number;
  /** Every ordinary input resolved to the exact output bytes it consumed. */
  readonly resolvedInputs: readonly FraudProofRawL1Utxo[];
  /** Every reference input resolved to the exact output bytes it referenced. */
  readonly resolvedReferenceInputs: readonly FraudProofRawL1Utxo[];
};

export type FraudProofRawL1UnitHistory = {
  readonly unit: string;
  /** The unit's history was scanned from origin rather than an arbitrary cursor. */
  readonly fromGenesis: true;
  readonly completeThroughPointId: string;
  readonly transactionHashes: readonly string[];
};

export type FraudProofRawL1Snapshot = {
  readonly schemaVersion: typeof FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION;
  readonly deploymentIdentityDigest: string;
  readonly blueprintHash: string;
  readonly finalityPolicyDigest: string;
  readonly headerHash: string;
  readonly provenance: EvidenceProvenance & {
    readonly trustClass: "authenticated_cardano_l1";
    readonly sourceMode: "local_chain_follower";
    readonly boundaryPoint: FraudProofRawL1Point;
    readonly tipPoint: FraudProofRawL1Point;
  };
  readonly cursor: {
    readonly point: FraudProofRawL1Point;
    readonly tip: FraudProofRawL1Point;
    readonly confirmationDepth: number;
    readonly rollbackCursor: string;
  };
  readonly scopes: readonly {
    readonly role: FraudProofRawL1ScopeRole;
    readonly address: string;
    readonly utxos: readonly FraudProofRawL1Utxo[];
  }[];
  readonly historyUnits: readonly string[];
  readonly history: readonly FraudProofRawL1UnitHistory[];
  readonly transactions: readonly FraudProofRawL1Transaction[];
};

/**
 * Stable content identity after full snapshot admission. This does not grant
 * authority or replace the snapshot's live finality/rollback binding. Preserve
 * all address coverage, unit membership and exact transaction evidence; omit
 * only capture progress and confirmation counters that advance without changing
 * any of that evidence.
 */
export const computeFraudProofRawL1SnapshotEvidenceDigest = (
  snapshot: FraudProofRawL1Snapshot,
): string => {
  const orderedUtxos = (values: readonly FraudProofRawL1Utxo[]) =>
    [...values].sort((left, right) => left.outRef.localeCompare(right.outRef));
  const transcript = {
    schemaVersion: "midgard-fraud-proof-raw-l1-evidence-v1",
    snapshotSchemaVersion: snapshot.schemaVersion,
    deploymentIdentityDigest: snapshot.deploymentIdentityDigest,
    blueprintHash: snapshot.blueprintHash,
    finalityPolicyDigest: snapshot.finalityPolicyDigest,
    headerHash: snapshot.headerHash,
    provenance: {
      trustClass: snapshot.provenance.trustClass,
      sourceId: snapshot.provenance.sourceId,
      grade: snapshot.provenance.grade,
      sourceMode: snapshot.provenance.sourceMode,
    },
    scopes: snapshot.scopes
      .map((scope) => ({
        role: scope.role,
        address: scope.address,
        utxos: orderedUtxos(scope.utxos),
      }))
      .sort((left, right) => left.role.localeCompare(right.role)),
    historyUnits: [...snapshot.historyUnits].sort(),
    history: snapshot.history
      .map((entry) => ({
        unit: entry.unit,
        fromGenesis: entry.fromGenesis,
        transactionHashes: [...entry.transactionHashes].sort(),
      }))
      .sort((left, right) => left.unit.localeCompare(right.unit)),
    transactions: snapshot.transactions
      .map((transaction) => ({
        txHash: transaction.txHash,
        bodyCbor: transaction.bodyCbor,
        witnessSetCbor: transaction.witnessSetCbor,
        redeemersCbor: transaction.redeemersCbor,
        isValid: transaction.isValid,
        inclusionPoint: transaction.inclusionPoint,
        resolvedInputs: orderedUtxos(transaction.resolvedInputs),
        resolvedReferenceInputs: orderedUtxos(
          transaction.resolvedReferenceInputs,
        ),
      }))
      .sort((left, right) => left.txHash.localeCompare(right.txHash)),
  };
  return createHash("sha256")
    .update(canonicalJson(transcript, "raw L1 immutable evidence"))
    .digest("hex");
};

/**
 * Provider-specific implementations return untrusted bytes. Admission and all
 * stage/terminal derivation stay in this package.
 */
export interface FraudProofRawL1SnapshotAuthority {
  readonly authorityVersion: typeof FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY;
  capture(request: FraudProofRawL1SnapshotRequest): Promise<unknown>;
}

const HEX_32 = /^[0-9a-f]{64}$/u;

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const OUT_REF = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u;

export const NATURAL = /^(0|[1-9][0-9]*)$/u;

export const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;

const UNIT = /^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u;

export const MAX_COLLECTION_SIZE = 100_000;

export const RAW_L1_SCOPE_ROLES = new Set<FraudProofRawL1ScopeRole>([
  "deposit_event",
  "withdrawal_event",
  "deposit_history_data",
  "withdrawal_history_data",
  "forced_transaction_event",
  "hub_oracle",
  "state_queue",
  "computation_thread_step_01",
  "computation_thread_step_02",
  "computation_thread_step_03",
  "computation_thread_step_04",
  "computation_thread_step_05",
  "computation_thread_step_06",
  "computation_thread_step_07",
  "computation_thread_step_08",
  "computation_thread_step_09",
  "computation_thread_step_10",
  "computation_thread_step_11",
  "computation_thread_step_12",
  "computation_thread_step_13",
  "computation_thread_step_14",
  "computation_thread_step_15",
  "computation_thread_step_16",
  "computation_thread_step_17",

  "permanent_proof_token",
  "active_operator_directory",
  "retired_operator_directory",
  "scheduler",
  "proof_chunk",
  "field_publication",
  "field_certificate",
]);

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${label} must be a plain object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

export const exact = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  const parsed = record(value, label);
  const actual = Object.keys(parsed).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has missing or unknown fields`);
  }
  return parsed;
};

export const string = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.trim().length === 0 ||
    value.trim() !== value
  ) {
    throw new Error(`${label} must be a canonical non-empty string`);
  }
  return value;
};

export const digest = (value: unknown, label: string): string => {
  const parsed = string(value, label);
  if (!HEX_32.test(parsed)) throw new Error(`${label} must be 32-byte hex`);
  return parsed;
};

export const assetUnit = (value: unknown, label: string): string => {
  const parsed = string(value, label);
  if (!UNIT.test(parsed))
    throw new Error(`${label} must be a canonical asset unit`);
  return parsed;
};
