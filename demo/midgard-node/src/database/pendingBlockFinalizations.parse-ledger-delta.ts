import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { type DeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import * as DepositsDB from "./deposits.js";
import * as ForcedTransactionsDB from "./forcedTransactions.js";
import {
  type MemberRecord,
  PENDING_BLOCK_FINALIZATION_VERSION,
  PendingBlockFinalizationReplayKind,
  type RetainedRootMemberInput,
  type Row,
  UtxoColumns,
  type UtxoInput,
  type WithdrawalMemberRecord,
} from "./pendingBlockFinalizations.columns.js";
import { exactRecord } from "./utils/exact-record.js";
import * as TxTable from "./utils/tx.js";
import * as WithdrawalsDB from "./withdrawals.js";

export type Record = Row & {
  readonly depositEventIds: readonly Buffer[];
  readonly forcedTransactionEventIds: readonly Buffer[];
  readonly withdrawalEventIds: readonly Buffer[];
  readonly mempoolTxIds: readonly Buffer[];
  readonly depositMembers: readonly MemberRecord[];
  readonly forcedTransactionMembers: readonly MemberRecord[];
  readonly withdrawalMembers: readonly WithdrawalMemberRecord[];
  readonly txMembers: readonly MemberRecord[];
  readonly transitionTraceMembers: readonly MemberRecord[];
  readonly eventToStepMembers: readonly MemberRecord[];
  readonly validationTraceMembers: readonly MemberRecord[];
  readonly validationTraceWitnessMembers: readonly MemberRecord[];
  readonly ledgerDelta: LedgerDeltaInput;
  readonly utxoPayloadAggregate?: UtxoPayloadSizeAggregate;
  readonly nativeMpfReplay?: NativeMpfReplayInput;
};

export type UtxoPayloadSizeAggregate = {
  readonly entryCount: number;
  readonly encodedTupleBytes: number;
};

export type LedgerDeltaInput = {
  readonly spent: readonly Buffer[];
  readonly produced: readonly UtxoInput[];
};

export type NativeMpfReplayInput = {
  readonly schema: 1;
  readonly ownerBinarySha256: Buffer;
  readonly baseRoot: Buffer;
  readonly candidateRoot: Buffer;
  readonly eventLog: Buffer;
  readonly eventLogDigest: Buffer;
  readonly eventRoots: Buffer;
  readonly eventCount: number;
};

export type PendingBlockFinalizationMetadata = {
  readonly deploymentMarker: DeploymentMarker;
  readonly consensusProfileId: typeof MIDGARD_CONSENSUS_PROFILE_ID;
  readonly stateQueueLeaseToken: string;
  readonly baseSnapshotId: string;
  readonly baseTailOutRef: string;
  readonly baseTailHeaderHash: Buffer;
  readonly baseTailDatumCbor: string;
  readonly baseRoots: {
    readonly utxosRoot: string;
    readonly forcedTransactionsRoot: string;
    readonly transactionsRoot: string;
    readonly depositsRoot: string;
    readonly withdrawalsRoot: string;
  };
  readonly blockStartTime: Date;
  readonly expectedRoots: {
    readonly utxosRoot: string;
    readonly forcedTransactionsRoot: string;
    readonly transactionsRoot: string;
    readonly depositsRoot: string;
    readonly withdrawalsRoot: string;
    readonly transitionTraceRoot: string;
    readonly eventToStepRoot: string;
    readonly validationTracesRoot: string;
  };
  readonly expectedCounts: {
    readonly withdrawalCount: bigint;
    readonly forcedTransactionCount: bigint;
    readonly l2TransactionCount: bigint;
    readonly depositCount: bigint;
    readonly totalEventCount: bigint;
    readonly transitionStepCount: bigint;
    readonly validationTraceCount: bigint;
  };
};

export type PendingBlockFinalization = {
  readonly version: typeof PENDING_BLOCK_FINALIZATION_VERSION;
  readonly metadata: PendingBlockFinalizationMetadata;
  readonly replay:
    | {
        readonly kind: typeof PendingBlockFinalizationReplayKind.LedgerDelta;
        readonly ledgerDelta: LedgerDeltaInput;
      }
    | {
        readonly kind: typeof PendingBlockFinalizationReplayKind.LedgerDeltaWithNativeMpf;
        readonly ledgerDelta: LedgerDeltaInput;
        readonly nativeMpfReplay: NativeMpfReplayInput;
      };
};

export type PrepareInput = {
  readonly preparedTxHash?: Buffer;
  readonly headerHash: Buffer;
  readonly headerCbor: Buffer;
  readonly metadata: PendingBlockFinalizationMetadata;
  readonly blockEndTime: Date;
  readonly depositEventIds: readonly Buffer[];
  readonly depositEntries: readonly DepositsDB.Entry[];
  readonly forcedTransactionEventIds: readonly Buffer[];
  readonly forcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
  readonly withdrawalEventIds: readonly Buffer[];
  readonly withdrawalEntries: readonly WithdrawalsDB.Entry[];
  readonly mempoolTxIds: readonly Buffer[];
  readonly mempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly mempoolTxProgramMaterialSidecars?: readonly {
    readonly txId: Buffer;
    readonly sidecarCbor: Buffer;
  }[];
  readonly mempoolTxSourceTable: string;
  readonly transitionTraceMembers: readonly RetainedRootMemberInput[];
  readonly eventToStepMembers: readonly RetainedRootMemberInput[];
  readonly validationTraceMembers: readonly RetainedRootMemberInput[];
  readonly validationTraceWitnessMembers: readonly RetainedRootMemberInput[];
  readonly ledgerDelta: LedgerDeltaInput;
  readonly utxoPayloadAggregate?: UtxoPayloadSizeAggregate;
  readonly nativeMpfReplay?: NativeMpfReplayInput;
};

export const exactBytes = (
  value: unknown,
  label: string,
  length?: number,
): Buffer => {
  if (
    !(value instanceof Uint8Array) ||
    (length === undefined ? value.length === 0 : value.length !== length)
  ) {
    throw new Error(
      length === undefined
        ? `${label} must be non-empty bytes`
        : `${label} must contain exactly ${length.toString()} bytes`,
    );
  }
  return Buffer.from(value);
};

export const exactNonEmptyString = (value: unknown, label: string): string => {
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`${label} must be a non-empty string`);
  }
  return value;
};

export const exactHex = (
  value: unknown,
  bytes: number,
  label: string,
): string => {
  if (
    typeof value !== "string" ||
    !new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value)
  ) {
    throw new Error(
      `${label} must be exactly ${bytes.toString()} lowercase hex bytes`,
    );
  }
  return value;
};

export const exactDate = (value: unknown, label: string): Date => {
  if (!(value instanceof Date) || !Number.isFinite(value.getTime())) {
    throw new Error(`${label} must be a valid Date`);
  }
  return new Date(value.getTime());
};

export const exactNonNegativeBigInt = (
  value: unknown,
  label: string,
): bigint => {
  if (typeof value !== "bigint" || value < 0n) {
    throw new Error(`${label} must be a non-negative bigint`);
  }
  return value;
};

export const parseLedgerDelta = (value: unknown): LedgerDeltaInput => {
  const candidate = exactRecord(
    value,
    ["spent", "produced"],
    "PendingBlockFinalizationV1 ledgerDelta",
  );
  if (!Array.isArray(candidate.spent)) {
    throw new Error(
      "PendingBlockFinalizationV1 ledgerDelta.spent must be an array",
    );
  }
  if (!Array.isArray(candidate.produced)) {
    throw new Error(
      "PendingBlockFinalizationV1 ledgerDelta.produced must be an array",
    );
  }
  const spent = candidate.spent.map((outref, index) =>
    exactBytes(
      outref,
      `PendingBlockFinalizationV1 ledgerDelta.spent[${index.toString()}]`,
    ),
  );
  const produced = candidate.produced.map((entry, index) => {
    const item = exactRecord(
      entry,
      [UtxoColumns.OUTREF, UtxoColumns.OUTPUT],
      `PendingBlockFinalizationV1 ledgerDelta.produced[${index.toString()}]`,
    );
    return {
      [UtxoColumns.OUTREF]: exactBytes(
        item[UtxoColumns.OUTREF],
        `PendingBlockFinalizationV1 ledgerDelta.produced[${index.toString()}].outref`,
      ),
      [UtxoColumns.OUTPUT]: exactBytes(
        item[UtxoColumns.OUTPUT],
        `PendingBlockFinalizationV1 ledgerDelta.produced[${index.toString()}].output`,
      ),
    };
  });
  const spentIds = new Set(spent.map((outref) => outref.toString("hex")));
  const producedIds = new Set(
    produced.map((entry) => entry[UtxoColumns.OUTREF].toString("hex")),
  );
  if (
    spentIds.size !== spent.length ||
    producedIds.size !== produced.length ||
    [...spentIds].some((outref) => producedIds.has(outref))
  ) {
    throw new Error(
      "PendingBlockFinalizationV1 ledgerDelta outrefs must be unique and disjoint",
    );
  }
  return { spent, produced };
};

export const assertNativeMpfReplay = (replay: NativeMpfReplayInput): void => {
  const exact = (value: Buffer, bytes: number, field: string): void => {
    if (value.byteLength !== bytes) {
      throw new Error(
        `${field} must contain exactly ${bytes.toString()} bytes`,
      );
    }
  };
  if (replay.schema !== 1) throw new Error("mpf_owner_schema must equal 1");
  exact(replay.ownerBinarySha256, 32, "mpf_owner_binary_sha256");
  exact(replay.baseRoot, 32, "mpf_replay_base_root");
  exact(replay.candidateRoot, 32, "mpf_replay_candidate_root");
  exact(replay.eventLogDigest, 32, "mpf_replay_event_log_digest");
  if (replay.eventLog.byteLength < 92) {
    throw new Error("mpf_replay_event_log is truncated");
  }
  if (
    !Number.isSafeInteger(replay.eventCount) ||
    replay.eventCount < 0 ||
    replay.eventRoots.byteLength !== replay.eventCount * 32
  ) {
    throw new Error("mpf_replay_event_roots/count mismatch");
  }
};
