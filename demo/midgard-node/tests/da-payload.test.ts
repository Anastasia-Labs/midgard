import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile-v1";
import {
  DaPayloadContentEncoding,
  decodeDaPayloadEnvelope,
  unwrapDaPayload,
} from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import {
  makeDeploymentMarker,
  MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { describe, expect, it } from "vitest";

import {
  DaPayloadsDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import { DatabaseError } from "../src/database/utils/common.js";
import {
  keyValuePhasRoot,
  ledgerOutputToInsertBatchOp,
} from "../src/mpf/index.js";
import { buildDaPayloadInsert } from "../src/workers/commit-block-header/da-payload.js";
import { backfillMissingDaPayloadsFromFinalizedJournals } from "../src/workers/commit-block-header/da-payload-backfill.js";
import { buildAuthenticatedRootFromEncodedEntries } from "../src/workers/commit-block-header/transition-roots.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";
import { deterministicFixtureBytes } from "./utils.js";

const fixture = (label: string, length: number): Buffer =>
  deterministicFixtureBytes(`da-payload:${label}`, length);

const ledgerRoot = (entries: readonly [Buffer, Buffer][]) => {
  const operations = entries.map(([outRef, outputCbor]) =>
    ledgerOutputToInsertBatchOp({ outRef, outputCbor }),
  );
  return keyValuePhasRoot(
    operations.map((operation) => operation.key),
    operations.map((operation) => operation.value),
  );
};

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
    validationTraceCount: 0n,
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

const retainedPairs = (
  label: string,
  count: number,
): readonly [Buffer, Buffer][] =>
  Array.from({ length: count }, (_, index) => [
    fixture(`${label}-key-${index}`, 4),
    fixture(`${label}-value-${index}`, 16),
  ]);

const LEDGER_OUTPUT_CBOR = Buffer.from(
  "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  "hex",
);

const ledgerEntries = (
  label: string,
  count: number,
): readonly [Buffer, Buffer][] =>
  Array.from({ length: count }, (_, index) => [
    makeOutRefCbor(fixture(`${label}-tx-id-${index}`, 32)),
    Buffer.from(LEDGER_OUTPUT_CBOR),
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

const record = ({
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
    withdrawalMembers,
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

type JournalFixtureOptions = {
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

const sourceRootOrEmpty = (
  domain: SDK.RootDomain,
  entries: readonly [Buffer, Buffer][],
) =>
  entries.length === 0
    ? Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT)
    : sourceRoot(domain, entries);

const buildJournalFixture = async ({
  utxoEntries = [],
  depositEntries = [],
  forcedTransactionEntries = [],
  withdrawalEntries = [],
  txEntries = [],
  transitionTraceEntries = [],
  eventToStepEntries = [],
  rootOverrides = {},
  recordRootOverrides = {},
}: JournalFixtureOptions): Promise<{
  readonly roots: TestRoots;
  readonly header: SDK.Header;
  readonly headerHash: Buffer;
  readonly pending: PendingBlockFinalizationsDB.Record;
}> => {
  const counts = countsFromLengths({
    withdrawals: withdrawalEntries.length,
    forcedTransactions: forcedTransactionEntries.length,
    transactions: txEntries.length,
    deposits: depositEntries.length,
  });
  const roots: TestRoots = {
    validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    ...(await Effect.runPromise(
      Effect.all({
        utxosRoot:
          utxoEntries.length === 0
            ? Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT)
            : ledgerRoot(utxoEntries),
        forcedTransactionsRoot: sourceRootOrEmpty(
          SDK.ROOT_DOMAINS.forcedTransactionsV1,
          forcedTransactionEntries,
        ),
        transactionsRoot: sourceRootOrEmpty(
          SDK.ROOT_DOMAINS.transactionsV1,
          txEntries,
        ),
        depositsRoot: sourceRootOrEmpty(
          SDK.ROOT_DOMAINS.deposits,
          depositEntries,
        ),
        withdrawalsRoot: sourceRootOrEmpty(
          SDK.ROOT_DOMAINS.withdrawals,
          withdrawalEntries,
        ),
        transitionTraceRoot: sourceRootOrEmpty(
          SDK.ROOT_DOMAINS.transitionTrace,
          transitionTraceEntries,
        ),
        eventToStepRoot: sourceRootOrEmpty(
          SDK.ROOT_DOMAINS.eventToStep,
          eventToStepEntries,
        ),
      }),
    )),
    ...rootOverrides,
  };
  const header = headerFor(roots, counts);
  const headerHash = Buffer.from(
    await Effect.runPromise(SDK.hashBlockHeader(header)),
    "hex",
  );
  const memberRecords = (entries: readonly [Buffer, Buffer][]) =>
    entries.map(([key, value], index) => member(headerHash, key, value, index));

  return {
    roots,
    header,
    headerHash,
    pending: record({
      headerHash,
      utxoEntries,
      depositMembers: memberRecords(depositEntries),
      forcedTransactionMembers: memberRecords(forcedTransactionEntries),
      withdrawalMembers: memberRecords(withdrawalEntries),
      txMembers: memberRecords(txEntries),
      transitionTraceMembers: memberRecords(transitionTraceEntries),
      eventToStepMembers: memberRecords(eventToStepEntries),
      roots: {
        ...roots,
        ...recordRootOverrides,
      },
      counts,
      header,
    }),
  };
};

describe("DaPayloadV1 builder", () => {
  it("builds canonical V1 journals with distinct preimage sidecars", async () => {
    const counts = countsFromLengths({});
    const roots: TestRoots = {
      utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
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
      consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
    });

    const insert = await Effect.runPromise(
      buildDaPayloadInsert({
        record: pending,
        utxos: [],
        envelope: { mode: "identity", zstdLevel: 3 },
      }),
    );
    const unwrapped = await unwrapDaPayload(insert.payload_cbor, {
      maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
    });
    expect(decodeDaPayloadEnvelope(insert.payload_cbor).contentEncoding).toBe(
      DaPayloadContentEncoding.identity,
    );
    const payload = SDK.decodeDaPayload(unwrapped.innerBytes);

    expect(insert.version).toBe(1);
    expect(payload.version).toBe(SDK.DA_PAYLOAD_VERSION);
    expect(payload.block_body.transaction_preimages).toEqual([]);
    expect(payload.block_body.forced_transaction_preimages).toEqual([]);
    expect(payload.block_body.header).toEqual(header);
  });

  it("builds a canonical payload whose roots, counts, and header match the journal", async () => {
    const utxoEntries = ledgerEntries("utxo", 2);
    const depositEntries: readonly [Buffer, Buffer][] = [
      [fixture("deposit", 34), fixture("deposit-info", 48)],
    ];
    const withdrawalEntries: readonly [Buffer, Buffer][] = [
      [fixture("withdrawal", 34), fixture("withdrawal-info", 52)],
    ];
    const transitionTraceEntries = retainedPairs("trace", 2);
    const eventToStepEntries = retainedPairs("event-to-step", 2);
    const { pending, roots, header, headerHash } = await buildJournalFixture({
      utxoEntries,
      depositEntries,
      withdrawalEntries,
      transitionTraceEntries,
      eventToStepEntries,
    });

    const insert = await Effect.runPromise(
      buildDaPayloadInsert({
        record: pending,
        utxos: utxoEntries.map(([outref, output]) => ({ outref, output })),
      }),
    );
    const identityUnwrapped = await unwrapDaPayload(insert.payload_cbor, {
      maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
    });
    const payload = SDK.decodeDaPayload(identityUnwrapped.innerBytes);

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

    const zstdInsert = await Effect.runPromise(
      buildDaPayloadInsert({
        record: pending,
        utxos: utxoEntries.map(([outref, output]) => ({ outref, output })),
        envelope: { mode: "zstd", zstdLevel: 3 },
      }),
    );
    const unwrapped = await unwrapDaPayload(zstdInsert.payload_cbor, {
      maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
    });
    expect(
      decodeDaPayloadEnvelope(zstdInsert.payload_cbor).contentEncoding,
    ).toBe(DaPayloadContentEncoding.zstd);
    expect(zstdInsert.version).toBe(1);
    expect(unwrapped.innerBytes).toEqual(identityUnwrapped.innerBytes);
    expect(zstdInsert.payload_sha256.toString("hex")).toBe(
      SDK.daPayloadHashHex(zstdInsert.payload_cbor),
    );
  });

  it("backfills a missing DA payload from complete journal payload members", async () => {
    const utxoEntries = ledgerEntries("backfill-utxo", 2);
    const depositEntries: readonly [Buffer, Buffer][] = [
      [fixture("backfill-deposit", 34), fixture("backfill-deposit-info", 48)],
    ];
    const transitionTraceEntries = retainedPairs("backfill-trace", 1);
    const eventToStepEntries = retainedPairs("backfill-event-to-step", 1);
    const { pending, headerHash } = await buildJournalFixture({
      utxoEntries,
      depositEntries,
      transitionTraceEntries,
      eventToStepEntries,
    });
    const inserts: DaPayloadsDB.InsertInput[] = [];

    const summary = await Effect.runPromise(
      backfillMissingDaPayloadsFromFinalizedJournals({
        deps: {
          retrieveMissingRecords: () => Effect.succeed([pending]),
          materializeUtxos: () =>
            Effect.succeed(
              utxoEntries.map(([outref, output]) => ({ outref, output })),
            ),
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
    const backfilled = await unwrapDaPayload(inserts[0]!.payload_cbor, {
      maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
    });
    expect(
      SDK.decodeDaPayload(backfilled.innerBytes).block_body.header_hash,
    ).toBe(headerHash.toString("hex"));
  });

  it("skips backfill when the V1 delta chain cannot be materialized", async () => {
    const { pending, headerHash } = await buildJournalFixture({
      rootOverrides: {
        utxosRoot: "00".repeat(32),
      },
    });
    const inserts: DaPayloadsDB.InsertInput[] = [];

    const summary = await Effect.runPromise(
      backfillMissingDaPayloadsFromFinalizedJournals({
        deps: {
          retrieveMissingRecords: () => Effect.succeed([pending]),
          materializeUtxos: () =>
            Effect.fail(
              new DatabaseError({
                table: PendingBlockFinalizationsDB.tableName,
                message: "V1 delta chain is incomplete",
                cause: "test fixture",
              }),
            ),
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
          reason: expect.stringContaining("V1 delta chain is incomplete"),
        },
      ],
    });
    expect(inserts).toEqual([]);
  });

  it("reports requested journals excluded from DA payload backfill by status", async () => {
    const { pending, headerHash } = await buildJournalFixture({
      depositEntries: [
        [fixture("backfill-excluded-deposit", 34), fixture("info", 48)],
      ],
    });
    const abandoned = {
      ...pending,
      [PendingBlockFinalizationsDB.Columns.STATUS]:
        PendingBlockFinalizationsDB.Status.Abandoned,
    };

    const summary = await Effect.runPromise(
      backfillMissingDaPayloadsFromFinalizedJournals({
        headerHash,
        deps: {
          retrieveMissingRecords: () => Effect.succeed([]),
          retrieveJournalByHeaderHash: () =>
            Effect.succeed(Option.some(abandoned)),
          materializeUtxos: () => Effect.succeed([]),
          upsertAvailable: () => Effect.void,
        },
      }),
    );

    expect(summary).toEqual({
      scanned: 0,
      backfilled: [],
      skipped: [
        {
          headerHash: headerHash.toString("hex"),
          reason:
            "journal excluded by status: abandoned; revive and complete local finalization before DA payload backfill",
        },
      ],
    });
  });

  it("rejects a payload whose recomputed roots do not match the journal", async () => {
    const utxoEntries = ledgerEntries("bad-utxo", 1);
    const depositEntries: readonly [Buffer, Buffer][] = [
      [fixture("bad-deposit", 34), fixture("bad-deposit-info", 48)],
    ];
    const transitionTraceEntries = retainedPairs("bad-trace", 1);
    const eventToStepEntries = retainedPairs("bad-event-to-step", 1);
    const { pending } = await buildJournalFixture({
      utxoEntries,
      depositEntries,
      transitionTraceEntries,
      eventToStepEntries,
      recordRootOverrides: {
        depositsRoot: "00".repeat(32),
      },
    });

    const result = await Effect.runPromise(
      Effect.either(
        buildDaPayloadInsert({
          record: pending,
          utxos: utxoEntries.map(([outref, output]) => ({ outref, output })),
        }),
      ),
    );

    expect(result._tag).toBe("Left");
  });
});
