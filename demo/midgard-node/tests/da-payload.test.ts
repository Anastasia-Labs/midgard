import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { DaPayloadsDB, PendingBlockFinalizationsDB } from "@/database/index.js";
import { buildDaPayloadInsert } from "@/workers/commit-block-header/da-payload.js";
import { backfillMissingDaPayloadsFromFinalizedJournals } from "@/workers/commit-block-header/da-payload-backfill.js";
import { buildAuthenticatedRootFromEncodedEntries } from "@/workers/commit-block-header/transition-roots.js";
import { keyValuePhasRoot } from "@/workers/utils/mpf.js";

import { deterministicFixtureBytes } from "./utils.js";

const fixture = (label: string, length: number): Buffer =>
  deterministicFixtureBytes(`da-payload:${label}`, length);

const root = (entries: readonly [Buffer, Buffer][]) =>
  keyValuePhasRoot(
    entries.map(([key]) => key),
    entries.map(([, value]) => value),
  );

const sourceRoot = (
  domain: SDK.RootDomain,
  entries: readonly [Buffer, Buffer][],
) =>
  buildAuthenticatedRootFromEncodedEntries(
    domain,
    entries.map(([key, value]) => ({ key, value })),
  ).pipe(Effect.map((built) => built.root));

type TestRoots = {
  readonly utxosRoot: string;
  readonly withdrawalsRoot: string;
  readonly forcedTransactionsRoot: string;
  readonly transactionsRoot: string;
  readonly depositsRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
};

type TestCounts = {
  readonly withdrawalCount: bigint;
  readonly forcedTransactionCount: bigint;
  readonly l2TransactionCount: bigint;
  readonly depositCount: bigint;
  readonly totalEventCount: bigint;
  readonly transitionStepCount: bigint;
};

const countsFromLengths = ({
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
  };
};

const headerFor = (roots: TestRoots, counts: TestCounts): SDK.Header => ({
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  utxosRoot: roots.utxosRoot,
  withdrawalsRoot: roots.withdrawalsRoot,
  forcedTransactionsRoot: roots.forcedTransactionsRoot,
  transactionsRoot: roots.transactionsRoot,
  depositsRoot: roots.depositsRoot,
  transitionTraceRoot: roots.transitionTraceRoot,
  eventToStepRoot: roots.eventToStepRoot,
  withdrawalCount: counts.withdrawalCount,
  forcedTransactionCount: counts.forcedTransactionCount,
  l2TransactionCount: counts.l2TransactionCount,
  depositCount: counts.depositCount,
  totalEventCount: counts.totalEventCount,
  transitionStepCount: counts.transitionStepCount,
  startTime: 1n,
  endTime: 2n,
  prevHeaderHash: "11".repeat(28),
  operatorVkey: "22".repeat(28),
  protocolVersion: 1n,
});

const headerCbor = (header: SDK.Header): Buffer =>
  Buffer.from(LucidData.to(header as never, SDK.Header as never), "hex");

const retainedPairs = (
  label: string,
  count: number,
): readonly [Buffer, Buffer][] =>
  Array.from({ length: count }, (_, index) => [
    fixture(`${label}-key-${index}`, 4),
    fixture(`${label}-value-${index}`, 16),
  ]);

const member = (
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

const utxoMember = (
  headerHash: Buffer,
  key: Buffer,
  value: Buffer,
  ordinal: number,
): PendingBlockFinalizationsDB.UtxoRecord => ({
  [PendingBlockFinalizationsDB.UtxoColumns.HEADER_HASH]: headerHash,
  [PendingBlockFinalizationsDB.UtxoColumns.OUTREF]: key,
  [PendingBlockFinalizationsDB.UtxoColumns.ORDINAL]: ordinal,
  [PendingBlockFinalizationsDB.UtxoColumns.OUTPUT]: value,
});

const record = ({
  headerHash,
  utxoMembers = [],
  depositMembers,
  forcedTransactionMembers = [],
  withdrawalMembers,
  txMembers = [],
  transitionTraceMembers,
  eventToStepMembers,
  roots,
  counts,
  header,
}: {
  readonly headerHash: Buffer;
  readonly utxoMembers?: readonly PendingBlockFinalizationsDB.UtxoRecord[];
  readonly depositMembers: readonly PendingBlockFinalizationsDB.MemberRecord[];
  readonly forcedTransactionMembers?: readonly PendingBlockFinalizationsDB.MemberRecord[];
  readonly withdrawalMembers: readonly PendingBlockFinalizationsDB.MemberRecord[];
  readonly txMembers?: readonly PendingBlockFinalizationsDB.MemberRecord[];
  readonly transitionTraceMembers: readonly PendingBlockFinalizationsDB.MemberRecord[];
  readonly eventToStepMembers: readonly PendingBlockFinalizationsDB.MemberRecord[];
  readonly roots: {
    readonly utxosRoot: string;
    readonly forcedTransactionsRoot: string;
    readonly transactionsRoot: string;
    readonly depositsRoot: string;
    readonly withdrawalsRoot: string;
    readonly transitionTraceRoot: string;
    readonly eventToStepRoot: string;
  };
  readonly counts: TestCounts;
  readonly header: SDK.Header;
}): PendingBlockFinalizationsDB.Record => {
  const blockStart = new Date("2026-06-12T00:00:00.000Z");
  const blockEnd = new Date("2026-06-12T00:00:10.000Z");
  return {
    [PendingBlockFinalizationsDB.Columns.HEADER_HASH]: headerHash,
    [PendingBlockFinalizationsDB.Columns.HEADER_CBOR]: headerCbor(header),
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
    withdrawalMembers,
    txMembers,
    transitionTraceMembers,
    eventToStepMembers,
    utxoMembers,
  };
};

describe("DaPayloadV2 builder", () => {
  it("builds a canonical payload whose roots, counts, and header match the journal", async () => {
    const utxoEntries: readonly [Buffer, Buffer][] = [
      [fixture("utxo-b", 34), fixture("utxo-value-b", 41)],
      [fixture("utxo-a", 34), fixture("utxo-value-a", 39)],
    ];
    const depositEntries: readonly [Buffer, Buffer][] = [
      [fixture("deposit", 34), fixture("deposit-info", 48)],
    ];
    const withdrawalEntries: readonly [Buffer, Buffer][] = [
      [fixture("withdrawal", 34), fixture("withdrawal-info", 52)],
    ];
    const transitionTraceEntries = retainedPairs("trace", 2);
    const eventToStepEntries = retainedPairs("event-to-step", 2);
    const counts = countsFromLengths({
      withdrawals: withdrawalEntries.length,
      deposits: depositEntries.length,
    });
    const roots = await Effect.runPromise(
      Effect.all({
        utxosRoot: root(utxoEntries),
        forcedTransactionsRoot: Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT),
        transactionsRoot: Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT),
        depositsRoot: sourceRoot(SDK.ROOT_DOMAINS.deposits, depositEntries),
        withdrawalsRoot: sourceRoot(
          SDK.ROOT_DOMAINS.withdrawals,
          withdrawalEntries,
        ),
        transitionTraceRoot: sourceRoot(
          SDK.ROOT_DOMAINS.transitionTrace,
          transitionTraceEntries,
        ),
        eventToStepRoot: sourceRoot(
          SDK.ROOT_DOMAINS.eventToStep,
          eventToStepEntries,
        ),
      }),
    );
    const header = headerFor(roots, counts);
    const headerHash = Buffer.from(
      await Effect.runPromise(SDK.hashBlockHeader(header)),
      "hex",
    );
    const pending = record({
      headerHash,
      utxoMembers: utxoEntries.map(([key, value], index) =>
        utxoMember(headerHash, key, value, index),
      ),
      depositMembers: depositEntries.map(([key, value], index) =>
        member(headerHash, key, value, index),
      ),
      withdrawalMembers: withdrawalEntries.map(([key, value], index) =>
        member(headerHash, key, value, index),
      ),
      transitionTraceMembers: transitionTraceEntries.map(
        ([key, value], index) => member(headerHash, key, value, index),
      ),
      eventToStepMembers: eventToStepEntries.map(([key, value], index) =>
        member(headerHash, key, value, index),
      ),
      roots,
      counts,
      header,
    });

    const insert = await Effect.runPromise(
      buildDaPayloadInsert({
        record: pending,
      }),
    );
    const payload = SDK.decodeDaPayloadV2(insert.payload_cbor);

    expect(payload.block_body.header_hash).toBe(headerHash.toString("hex"));
    expect(payload.block_body.header).toEqual(header);
    expect(payload.block_body.utxos.map(([key]) => key)).toEqual(
      [...payload.block_body.utxos.map(([key]) => key)].sort(),
    );
    expect(insert.utxos_root).toBe(roots.utxosRoot);
    expect(insert.forced_transactions_root).toBe(roots.forcedTransactionsRoot);
    expect(insert.transactions_root).toBe(roots.transactionsRoot);
    expect(insert.deposits_root).toBe(roots.depositsRoot);
    expect(insert.withdrawals_root).toBe(roots.withdrawalsRoot);
    expect(insert.transition_trace_root).toBe(roots.transitionTraceRoot);
    expect(insert.event_to_step_root).toBe(roots.eventToStepRoot);
    expect(insert.deposit_count).toBe(1n);
    expect(insert.withdrawal_count).toBe(1n);
    expect(insert.total_event_count).toBe(2n);
    expect(insert.payload_sha256.toString("hex")).toBe(
      SDK.daPayloadHashHex(insert.payload_cbor),
    );
  });

  it("backfills a missing DA payload from complete journal payload members", async () => {
    const utxoEntries: readonly [Buffer, Buffer][] = [
      [fixture("backfill-utxo-b", 34), fixture("backfill-utxo-value-b", 41)],
      [fixture("backfill-utxo-a", 34), fixture("backfill-utxo-value-a", 39)],
    ];
    const depositEntries: readonly [Buffer, Buffer][] = [
      [fixture("backfill-deposit", 34), fixture("backfill-deposit-info", 48)],
    ];
    const transitionTraceEntries = retainedPairs("backfill-trace", 1);
    const eventToStepEntries = retainedPairs("backfill-event-to-step", 1);
    const counts = countsFromLengths({ deposits: depositEntries.length });
    const roots = await Effect.runPromise(
      Effect.all({
        utxosRoot: root(utxoEntries),
        forcedTransactionsRoot: Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT),
        transactionsRoot: Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT),
        depositsRoot: sourceRoot(SDK.ROOT_DOMAINS.deposits, depositEntries),
        withdrawalsRoot: Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT),
        transitionTraceRoot: sourceRoot(
          SDK.ROOT_DOMAINS.transitionTrace,
          transitionTraceEntries,
        ),
        eventToStepRoot: sourceRoot(
          SDK.ROOT_DOMAINS.eventToStep,
          eventToStepEntries,
        ),
      }),
    );
    const header = headerFor(roots, counts);
    const headerHash = Buffer.from(
      await Effect.runPromise(SDK.hashBlockHeader(header)),
      "hex",
    );
    const pending = record({
      headerHash,
      utxoMembers: utxoEntries.map(([key, value], index) =>
        utxoMember(headerHash, key, value, index),
      ),
      depositMembers: depositEntries.map(([key, value], index) =>
        member(headerHash, key, value, index),
      ),
      withdrawalMembers: [],
      transitionTraceMembers: transitionTraceEntries.map(
        ([key, value], index) => member(headerHash, key, value, index),
      ),
      eventToStepMembers: eventToStepEntries.map(([key, value], index) =>
        member(headerHash, key, value, index),
      ),
      roots,
      counts,
      header,
    });
    const inserts: DaPayloadsDB.InsertInput[] = [];

    const summary = await Effect.runPromise(
      backfillMissingDaPayloadsFromFinalizedJournals({
        deps: {
          retrieveMissingRecords: () => Effect.succeed([pending]),
          upsertAvailable: (input) =>
            Effect.sync(() => {
              inserts.push(input);
            }),
        },
      }),
    );

    expect(summary).toEqual({
      scanned: 1,
      backfilled: [headerHash.toString("hex")],
      skipped: [],
    });
    expect(inserts).toHaveLength(1);
    expect(
      SDK.decodeDaPayloadV2(inserts[0]!.payload_cbor).block_body.header_hash,
    ).toBe(headerHash.toString("hex"));
  });

  it("skips backfill when the journal is missing committed UTxO members", async () => {
    const counts = countsFromLengths({});
    const roots = {
      utxosRoot: "00".repeat(32),
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    };
    const header = headerFor(roots, counts);
    const headerHash = Buffer.from(
      await Effect.runPromise(SDK.hashBlockHeader(header)),
      "hex",
    );
    const pending = record({
      headerHash,
      depositMembers: [],
      withdrawalMembers: [],
      transitionTraceMembers: [],
      eventToStepMembers: [],
      roots,
      counts,
      header,
    });
    const inserts: DaPayloadsDB.InsertInput[] = [];

    const summary = await Effect.runPromise(
      backfillMissingDaPayloadsFromFinalizedJournals({
        deps: {
          retrieveMissingRecords: () => Effect.succeed([pending]),
          upsertAvailable: (input) =>
            Effect.sync(() => {
              inserts.push(input);
            }),
        },
      }),
    );

    expect(summary).toEqual({
      scanned: 1,
      backfilled: [],
      skipped: [
        {
          headerHash: headerHash.toString("hex"),
          reason: "journal has incomplete payload members",
        },
      ],
    });
    expect(inserts).toEqual([]);
  });

  it("rejects a payload whose recomputed roots do not match the journal", async () => {
    const utxoEntries: readonly [Buffer, Buffer][] = [
      [fixture("bad-utxo", 34), fixture("bad-utxo-value", 41)],
    ];
    const depositEntries: readonly [Buffer, Buffer][] = [
      [fixture("bad-deposit", 34), fixture("bad-deposit-info", 48)],
    ];
    const transitionTraceEntries = retainedPairs("bad-trace", 1);
    const eventToStepEntries = retainedPairs("bad-event-to-step", 1);
    const counts = countsFromLengths({ deposits: depositEntries.length });
    const roots = await Effect.runPromise(
      Effect.all({
        utxosRoot: root(utxoEntries),
        forcedTransactionsRoot: Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT),
        transactionsRoot: Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT),
        depositsRoot: sourceRoot(SDK.ROOT_DOMAINS.deposits, depositEntries),
        withdrawalsRoot: Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT),
        transitionTraceRoot: sourceRoot(
          SDK.ROOT_DOMAINS.transitionTrace,
          transitionTraceEntries,
        ),
        eventToStepRoot: sourceRoot(
          SDK.ROOT_DOMAINS.eventToStep,
          eventToStepEntries,
        ),
      }),
    );
    const header = headerFor(roots, counts);
    const headerHash = Buffer.from(
      await Effect.runPromise(SDK.hashBlockHeader(header)),
      "hex",
    );
    const pending = record({
      headerHash,
      utxoMembers: utxoEntries.map(([key, value], index) =>
        utxoMember(headerHash, key, value, index),
      ),
      depositMembers: depositEntries.map(([key, value], index) =>
        member(headerHash, key, value, index),
      ),
      withdrawalMembers: [],
      transitionTraceMembers: transitionTraceEntries.map(
        ([key, value], index) => member(headerHash, key, value, index),
      ),
      eventToStepMembers: eventToStepEntries.map(([key, value], index) =>
        member(headerHash, key, value, index),
      ),
      roots: {
        ...roots,
        depositsRoot: "00".repeat(32),
      },
      counts,
      header,
    });

    const result = await Effect.runPromise(
      Effect.either(
        buildDaPayloadInsert({
          record: pending,
        }),
      ),
    );

    expect(result._tag).toBe("Left");
  });
});
