import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import {
  makeDeploymentMarker,
  MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import {
  keyValuePhasRoot,
  ledgerOutputToInsertBatchOp,
} from "../src/mpf/index.js";
import { buildAuthenticatedRootFromEncodedEntries } from "../src/workers/commit-block-header/transition-roots.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";
import { deterministicFixtureBytes } from "./utils.js";

export const fixture = (label: string, length: number): Buffer =>
  deterministicFixtureBytes(`da-payload:${label}`, length);

export const ledgerRoot = (entries: readonly [Buffer, Buffer][]) => {
  const operations = entries.map(([outRef, outputCbor]) =>
    ledgerOutputToInsertBatchOp({ outRef, outputCbor }),
  );
  return keyValuePhasRoot(
    operations.map((operation) => operation.key),
    operations.map((operation) => operation.value),
  );
};

export const sourceRoot = (
  domain: SDK.RootDomain,
  entries: readonly [Buffer, Buffer][],
) =>
  buildAuthenticatedRootFromEncodedEntries(
    domain,
    entries.map(([key, value]) => ({ key, value })),
  ).pipe(Effect.map((built) => built.root));

export type TestRoots = {
  readonly utxosRoot: string;
  readonly withdrawalsRoot: string;
  readonly forcedTransactionsRoot: string;
  readonly transactionsRoot: string;
  readonly depositsRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
  readonly validationTracesRoot: string;
};

type TestCounts = {
  readonly withdrawalCount: bigint;
  readonly forcedTransactionCount: bigint;
  readonly l2TransactionCount: bigint;
  readonly depositCount: bigint;
  readonly totalEventCount: bigint;
  readonly transitionStepCount: bigint;
  readonly validationTraceCount: bigint;
};

export const countsFromLengths = ({
  withdrawals = 0,
  forcedTransactions = 0,
  transactions = 0,
  deposits = 0,
}: {
  readonly withdrawals?: number;
  readonly forcedTransactions?: number;
  readonly transactions?: number;
  readonly deposits?: number;
}): TestCounts => {
  const total = BigInt(
    withdrawals + forcedTransactions + transactions + deposits,
  );
  return {
    withdrawalCount: BigInt(withdrawals),
    forcedTransactionCount: BigInt(forcedTransactions),
    l2TransactionCount: BigInt(transactions),
    depositCount: BigInt(deposits),
    totalEventCount: total,
    transitionStepCount: total,
    validationTraceCount: 0n,
  };
};

export const headerFor = (
  roots: TestRoots,
  counts: TestCounts,
): SDK.Header => ({
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  utxosRoot: roots.utxosRoot,
  withdrawalsRoot: roots.withdrawalsRoot,
  forcedTransactionsRoot: roots.forcedTransactionsRoot,
  transactionsRoot: roots.transactionsRoot,
  depositsRoot: roots.depositsRoot,
  transitionTraceRoot: roots.transitionTraceRoot,
  eventToStepRoot: roots.eventToStepRoot,
  validationTracesRoot: roots.validationTracesRoot,
  withdrawalCount: counts.withdrawalCount,
  forcedTransactionCount: counts.forcedTransactionCount,
  l2TransactionCount: counts.l2TransactionCount,
  depositCount: counts.depositCount,
  totalEventCount: counts.totalEventCount,
  transitionStepCount: counts.transitionStepCount,
  validationTraceCount: counts.validationTraceCount,
  startTime: 1n,
  endTime: 2n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "11".repeat(28),
  operatorVkey: "22".repeat(28),
  protocolVersion: 1n,
});

const headerCbor = (header: SDK.Header): Buffer =>
  Buffer.from(LucidData.to(header as never, SDK.Header as never), "hex");

export const retainedPairs = (
  label: string,
  count: number,
): readonly [Buffer, Buffer][] =>
  Array.from({ length: count }, (_, index) => [
    fixture(`${label}-key-${index}`, 4),
    fixture(`${label}-value-${index}`, 16),
  ]);

export const LEDGER_OUTPUT_CBOR = Buffer.from(
  "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  "hex",
);

export const ledgerEntries = (
  label: string,
  count: number,
): readonly [Buffer, Buffer][] =>
  Array.from({ length: count }, (_, index) => [
    makeOutRefCbor(fixture(`${label}-tx-id-${index}`, 32)),
    Buffer.from(LEDGER_OUTPUT_CBOR),
  ]);

export const member = (
  headerHash: Buffer,
  key: Buffer,
  value: Buffer,
  ordinal: number,
): PendingBlockFinalizationsDB.MemberRecord => ({
  [PendingBlockFinalizationsDB.MemberColumns.HEADER_HASH]: headerHash,
  [PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID]: key,
  [PendingBlockFinalizationsDB.MemberColumns.ORDINAL]: ordinal,
  [PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR]: value,
  [PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_SHA256]: fixture(
    `member-sha-${ordinal}`,
    32,
  ),
  [PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE]: "test_source",
  [PendingBlockFinalizationsDB.MemberColumns.SOURCE_ID]: key,
  [PendingBlockFinalizationsDB.MemberColumns.SOURCE_TIMESTAMP]: new Date(
    "2026-06-12T00:00:00.000Z",
  ),
});

export const record = ({
  headerHash,
  utxoEntries = [],
  depositMembers,
  forcedTransactionMembers = [],
  withdrawalMembers,
  txMembers = [],
  transitionTraceMembers,
  eventToStepMembers,
  roots,
  counts,
  header,
  consensusProfileId = MIDGARD_CONSENSUS_PROFILE_ID,
}: {
  readonly headerHash: Buffer;
  readonly utxoEntries?: readonly [Buffer, Buffer][];
  readonly depositMembers: readonly PendingBlockFinalizationsDB.MemberRecord[];
  readonly forcedTransactionMembers?: readonly PendingBlockFinalizationsDB.MemberRecord[];
  readonly withdrawalMembers: readonly PendingBlockFinalizationsDB.MemberRecord[];
  readonly txMembers?: readonly PendingBlockFinalizationsDB.MemberRecord[];
  readonly transitionTraceMembers: readonly PendingBlockFinalizationsDB.MemberRecord[];
  readonly eventToStepMembers: readonly PendingBlockFinalizationsDB.MemberRecord[];
  readonly roots: TestRoots;
  readonly counts: TestCounts;
  readonly header: SDK.Header;
  readonly consensusProfileId?: typeof MIDGARD_CONSENSUS_PROFILE_ID;
}): PendingBlockFinalizationsDB.Record => {
  const blockStart = new Date("2026-06-12T00:00:00.000Z");
  const blockEnd = new Date("2026-06-12T00:00:10.000Z");
  return {
    [PendingBlockFinalizationsDB.Columns.HEADER_HASH]: headerHash,
    [PendingBlockFinalizationsDB.Columns.HEADER_CBOR]: headerCbor(header),
    [PendingBlockFinalizationsDB.Columns.FORMAT_VERSION]:
      PendingBlockFinalizationsDB.PENDING_BLOCK_FINALIZATION_VERSION,
    [PendingBlockFinalizationsDB.Columns.REPLAY_KIND]:
      PendingBlockFinalizationsDB.PendingBlockFinalizationReplayKind
        .LedgerDelta,
    [PendingBlockFinalizationsDB.Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION]:
      MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
    [PendingBlockFinalizationsDB.Columns.DEPLOYMENT_MANIFEST_ID]:
      makeDeploymentMarker("de".repeat(32)).manifestId,
    [PendingBlockFinalizationsDB.Columns.CONSENSUS_PROFILE_ID]:
      consensusProfileId,
    [PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH]: null,
    [PendingBlockFinalizationsDB.Columns.STATE_QUEUE_LEASE_TOKEN]: "lease",
    [PendingBlockFinalizationsDB.Columns.BASE_SNAPSHOT_ID]: "snapshot",
    [PendingBlockFinalizationsDB.Columns.BASE_TAIL_OUT_REF]: "base#0",
    [PendingBlockFinalizationsDB.Columns.BASE_TAIL_HEADER_HASH]: fixture(
      "base-header",
      28,
    ),
    [PendingBlockFinalizationsDB.Columns.BASE_TAIL_DATUM_CBOR]: "d87980",
    [PendingBlockFinalizationsDB.Columns.BASE_UTXOS_ROOT]:
      SDK.EMPTY_MERKLE_TREE_ROOT,
    [PendingBlockFinalizationsDB.Columns.BASE_FORCED_TRANSACTIONS_ROOT]:
      SDK.EMPTY_MERKLE_TREE_ROOT,
    [PendingBlockFinalizationsDB.Columns.BASE_TRANSACTIONS_ROOT]:
      SDK.EMPTY_MERKLE_TREE_ROOT,
    [PendingBlockFinalizationsDB.Columns.BASE_DEPOSITS_ROOT]:
      SDK.EMPTY_MERKLE_TREE_ROOT,
    [PendingBlockFinalizationsDB.Columns.BASE_WITHDRAWALS_ROOT]:
      SDK.EMPTY_MERKLE_TREE_ROOT,
    [PendingBlockFinalizationsDB.Columns.BLOCK_START_TIME]: blockStart,
    [PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME]: blockEnd,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT]: roots.utxosRoot,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_FORCED_TRANSACTIONS_ROOT]:
      roots.forcedTransactionsRoot,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSACTIONS_ROOT]:
      roots.transactionsRoot,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSITS_ROOT]:
      roots.depositsRoot,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWALS_ROOT]:
      roots.withdrawalsRoot,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSITION_TRACE_ROOT]:
      roots.transitionTraceRoot,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_EVENT_TO_STEP_ROOT]:
      roots.eventToStepRoot,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACES_ROOT]:
      roots.validationTracesRoot,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWAL_COUNT]:
      counts.withdrawalCount,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_FORCED_TRANSACTION_COUNT]:
      counts.forcedTransactionCount,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_L2_TRANSACTION_COUNT]:
      counts.l2TransactionCount,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSIT_COUNT]:
      counts.depositCount,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_TOTAL_EVENT_COUNT]:
      counts.totalEventCount,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSITION_STEP_COUNT]:
      counts.transitionStepCount,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACE_COUNT]:
      counts.validationTraceCount,
    [PendingBlockFinalizationsDB.Columns.LEDGER_DELTA_SPENT]: [],
    [PendingBlockFinalizationsDB.Columns.LEDGER_DELTA_PRODUCED]:
      utxoEntries.map(([outref, output]) => ({
        outref: outref.toString("hex"),
        output: output.toString("hex"),
      })),
    [PendingBlockFinalizationsDB.Columns.STATUS]:
      PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
    [PendingBlockFinalizationsDB.Columns.OBSERVED_CONFIRMED_AT_MS]: 1n,
    [PendingBlockFinalizationsDB.Columns.CREATED_AT]: blockStart,
    [PendingBlockFinalizationsDB.Columns.UPDATED_AT]: blockStart,
    depositEventIds: depositMembers.map(
      (entry) => entry[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
    ),
    withdrawalEventIds: withdrawalMembers.map(
      (entry) => entry[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
    ),
    forcedTransactionEventIds: forcedTransactionMembers.map(
      (entry) => entry[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
    ),
    mempoolTxIds: txMembers.map(
      (entry) => entry[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
    ),
    depositMembers,
    forcedTransactionMembers,
    withdrawalMembers: withdrawalMembers.map((member) => ({
      ...member,
      [PendingBlockFinalizationsDB.WithdrawalMemberColumns
        .CLASSIFICATION_REVISION]: 0,
      [PendingBlockFinalizationsDB.WithdrawalMemberColumns.VALIDITY]:
        "WithdrawalIsValid",
      [PendingBlockFinalizationsDB.WithdrawalMemberColumns.VALIDITY_DETAIL]: {},
      [PendingBlockFinalizationsDB.WithdrawalMemberColumns
        .CLASSIFICATION_SHA256]: Buffer.alloc(32),
    })),
    txMembers,
    transitionTraceMembers,
    eventToStepMembers,
    validationTraceMembers: [],
    validationTraceWitnessMembers: [],
    ledgerDelta: {
      spent: [],
      produced: utxoEntries.map(([outref, output]) => ({
        [PendingBlockFinalizationsDB.UtxoColumns.OUTREF]: outref,
        [PendingBlockFinalizationsDB.UtxoColumns.OUTPUT]: output,
      })),
    },
  };
};

export type JournalFixtureOptions = {
  readonly utxoEntries?: readonly [Buffer, Buffer][];
  readonly depositEntries?: readonly [Buffer, Buffer][];
  readonly forcedTransactionEntries?: readonly [Buffer, Buffer][];
  readonly withdrawalEntries?: readonly [Buffer, Buffer][];
  readonly txEntries?: readonly [Buffer, Buffer][];
  readonly transitionTraceEntries?: readonly [Buffer, Buffer][];
  readonly eventToStepEntries?: readonly [Buffer, Buffer][];
  readonly rootOverrides?: Partial<TestRoots>;
  readonly recordRootOverrides?: Partial<TestRoots>;
};
