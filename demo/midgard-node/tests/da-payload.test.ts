import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  MempoolLedgerDB,
  PendingBlockFinalizationsDB,
  DaPayloadsDB,
} from "@/database/index.js";
import { buildDaPayloadInsert } from "@/workers/commit-block-header/da-payload.js";
import { backfillMissingDaPayloadsFromFinalizedJournals } from "@/workers/commit-block-header/da-payload-backfill.js";
import { keyValuePhasRoot } from "@/workers/utils/mpf.js";

import { deterministicFixtureBytes } from "./utils.js";

const fixture = (label: string, length: number): Buffer =>
  deterministicFixtureBytes(`da-payload:${label}`, length);

const root = (entries: readonly [Buffer, Buffer][]) =>
  keyValuePhasRoot(
    entries.map(([key]) => key),
    entries.map(([, value]) => value),
  );

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

const utxo = (
  key: Buffer,
  value: Buffer,
  index: number,
): MempoolLedgerDB.EntryWithTimeStamp => ({
  [MempoolLedgerDB.Columns.TX_ID]: fixture(`utxo-tx-${index}`, 32),
  [MempoolLedgerDB.Columns.OUTREF]: key,
  [MempoolLedgerDB.Columns.OUTPUT]: value,
  [MempoolLedgerDB.Columns.ADDRESS]: "addr_test1vptest",
  [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
  [MempoolLedgerDB.Columns.TIMESTAMPTZ]: new Date("2026-06-12T00:00:00.000Z"),
});

const record = ({
  headerHash,
  depositMembers,
  withdrawalMembers,
  roots,
}: {
  readonly headerHash: Buffer;
  readonly depositMembers: readonly PendingBlockFinalizationsDB.MemberRecord[];
  readonly withdrawalMembers: readonly PendingBlockFinalizationsDB.MemberRecord[];
  readonly roots: {
    readonly utxosRoot: string;
    readonly transactionsRoot: string;
    readonly depositsRoot: string;
    readonly withdrawalsRoot: string;
  };
}): PendingBlockFinalizationsDB.Record => {
  const blockStart = new Date("2026-06-12T00:00:00.000Z");
  const blockEnd = new Date("2026-06-12T00:00:10.000Z");
  return {
    [PendingBlockFinalizationsDB.Columns.HEADER_HASH]: headerHash,
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
    [PendingBlockFinalizationsDB.Columns.BASE_TRANSACTIONS_ROOT]:
      SDK.EMPTY_MERKLE_TREE_ROOT,
    [PendingBlockFinalizationsDB.Columns.BASE_DEPOSITS_ROOT]:
      SDK.EMPTY_MERKLE_TREE_ROOT,
    [PendingBlockFinalizationsDB.Columns.BASE_WITHDRAWALS_ROOT]:
      SDK.EMPTY_MERKLE_TREE_ROOT,
    [PendingBlockFinalizationsDB.Columns.BLOCK_START_TIME]: blockStart,
    [PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME]: blockEnd,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT]: roots.utxosRoot,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSACTIONS_ROOT]:
      roots.transactionsRoot,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSITS_ROOT]:
      roots.depositsRoot,
    [PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWALS_ROOT]:
      roots.withdrawalsRoot,
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
    mempoolTxIds: [],
    depositMembers,
    withdrawalMembers,
    txMembers: [],
  };
};

describe("DaPayloadV1 builder", () => {
  it("builds a headerless canonical payload whose roots match the journal", async () => {
    const headerHash = fixture("header", 28);
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
    const roots = await Effect.runPromise(
      Effect.all({
        utxosRoot: root(utxoEntries),
        transactionsRoot: Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT),
        depositsRoot: root(depositEntries),
        withdrawalsRoot: root(withdrawalEntries),
      }),
    );
    const pending = record({
      headerHash,
      depositMembers: depositEntries.map(([key, value], index) =>
        member(headerHash, key, value, index),
      ),
      withdrawalMembers: withdrawalEntries.map(([key, value], index) =>
        member(headerHash, key, value, index),
      ),
      roots,
    });

    const insert = await Effect.runPromise(
      buildDaPayloadInsert({
        record: pending,
        utxos: utxoEntries.map(([key, value], index) =>
          utxo(key, value, index),
        ),
      }),
    );
    const payload = SDK.decodeDaPayloadV1(insert.payload_cbor);

    expect(payload.header_hash).toBe(headerHash.toString("hex"));
    expect("header" in payload).toBe(false);
    expect(payload.block_body.utxos.map(([key]) => key)).toEqual(
      [...payload.block_body.utxos.map(([key]) => key)].sort(),
    );
    expect(insert.utxos_root).toBe(roots.utxosRoot);
    expect(insert.transactions_root).toBe(roots.transactionsRoot);
    expect(insert.deposits_root).toBe(roots.depositsRoot);
    expect(insert.withdrawals_root).toBe(roots.withdrawalsRoot);
    expect(insert.payload_sha256.toString("hex")).toBe(
      SDK.daPayloadHashHex(insert.payload_cbor),
    );
  });

  it("backfills a missing DA payload only when current UTxOs prove the journal roots", async () => {
    const headerHash = fixture("backfill-header", 28);
    const utxoEntries: readonly [Buffer, Buffer][] = [
      [fixture("backfill-utxo-b", 34), fixture("backfill-utxo-value-b", 41)],
      [fixture("backfill-utxo-a", 34), fixture("backfill-utxo-value-a", 39)],
    ];
    const depositEntries: readonly [Buffer, Buffer][] = [
      [fixture("backfill-deposit", 34), fixture("backfill-deposit-info", 48)],
    ];
    const roots = await Effect.runPromise(
      Effect.all({
        utxosRoot: root(utxoEntries),
        transactionsRoot: Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT),
        depositsRoot: root(depositEntries),
        withdrawalsRoot: Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT),
      }),
    );
    const pending = record({
      headerHash,
      depositMembers: depositEntries.map(([key, value], index) =>
        member(headerHash, key, value, index),
      ),
      withdrawalMembers: [],
      roots,
    });
    const inserts: DaPayloadsDB.InsertInput[] = [];

    const summary = await Effect.runPromise(
      backfillMissingDaPayloadsFromFinalizedJournals({
        deps: {
          retrieveMissingRecords: () => Effect.succeed([pending]),
          retrieveUtxos: Effect.succeed(
            utxoEntries.map(([key, value], index) => utxo(key, value, index)),
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
    expect(SDK.decodeDaPayloadV1(inserts[0]!.payload_cbor).header_hash).toBe(
      headerHash.toString("hex"),
    );
  });

  it("skips historical DA payload backfill when the available UTxOs do not match the journal roots", async () => {
    const headerHash = fixture("backfill-bad-header", 28);
    const utxoEntries: readonly [Buffer, Buffer][] = [
      [
        fixture("backfill-bad-utxo", 34),
        fixture("backfill-bad-utxo-value", 41),
      ],
    ];
    const pending = record({
      headerHash,
      depositMembers: [],
      withdrawalMembers: [],
      roots: {
        utxosRoot: "00".repeat(32),
        transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
    });
    const inserts: DaPayloadsDB.InsertInput[] = [];

    const summary = await Effect.runPromise(
      backfillMissingDaPayloadsFromFinalizedJournals({
        deps: {
          retrieveMissingRecords: () => Effect.succeed([pending]),
          retrieveUtxos: Effect.succeed(
            utxoEntries.map(([key, value], index) => utxo(key, value, index)),
          ),
          upsertAvailable: (input) =>
            Effect.sync(() => {
              inserts.push(input);
            }),
        },
      }),
    );

    expect(summary.scanned).toBe(1);
    expect(summary.backfilled).toEqual([]);
    expect(summary.skipped).toHaveLength(1);
    expect(summary.skipped[0]?.headerHash).toBe(headerHash.toString("hex"));
    expect(summary.skipped[0]?.reason).toContain(
      "Refusing to persist DA payload",
    );
    expect(inserts).toEqual([]);
  });

  it("rejects a payload whose recomputed roots do not match the journal", async () => {
    const headerHash = fixture("bad-header", 28);
    const utxoEntries: readonly [Buffer, Buffer][] = [
      [fixture("bad-utxo", 34), fixture("bad-utxo-value", 41)],
    ];
    const depositEntries: readonly [Buffer, Buffer][] = [
      [fixture("bad-deposit", 34), fixture("bad-deposit-info", 48)],
    ];
    const roots = await Effect.runPromise(
      Effect.all({
        utxosRoot: root(utxoEntries),
        transactionsRoot: Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT),
        depositsRoot: root(depositEntries),
        withdrawalsRoot: Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT),
      }),
    );
    const pending = record({
      headerHash,
      depositMembers: depositEntries.map(([key, value], index) =>
        member(headerHash, key, value, index),
      ),
      withdrawalMembers: [],
      roots: {
        ...roots,
        depositsRoot: "00".repeat(32),
      },
    });

    const result = await Effect.runPromise(
      Effect.either(
        buildDaPayloadInsert({
          record: pending,
          utxos: utxoEntries.map(([key, value], index) =>
            utxo(key, value, index),
          ),
        }),
      ),
    );

    expect(result._tag).toBe("Left");
  });
});
