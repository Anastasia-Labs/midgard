/**
 * Real journals, landed rows and folds for the landed-merge walk's tests.
 * Every block is empty, so a finalization moves no ledger row and only the
 * walk varies.
 */
import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";
import { expect } from "vitest";

import {
  BlocksDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import { foldMerge } from "../src/landed-blocks/confirmed-merges.js";
import {
  Frontier,
  insertRow,
  type LandedBlockRow,
} from "../src/landed-blocks/store.js";
import { Globals } from "../src/services/index.js";
import { prepareUnderTestLease } from "./helpers/mutation-lease.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

export const DURABLE_ROOT = "ab".repeat(32);

/**
 * Restores the worker's empty application state with the confirmed-ledger
 * frontier at genesis (as landed-block processing bootstraps it before any
 * block lands), then runs `effect` with an Architecture G native owner
 * whose diagnostics `diagnostics` answers.
 */
export const isolatedDb = <A, E>(
  effect: Effect.Effect<A, E, any>,
  diagnostics: () => Promise<{
    readonly durableRoot: string;
    readonly activeGenerations: number;
  }> = async () => ({ durableRoot: DURABLE_ROOT, activeGenerations: 1 }),
) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      yield* Frontier.upsert({
        headerHash: SDK.GENESIS_HEADER_HASH,
        utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      });
      const globals = yield* Globals;
      yield* Ref.set(globals.NATIVE_MPF_OWNER, { diagnostics } as never);
      return yield* effect;
    }).pipe(Effect.provide(Globals.Default)),
  ) as Effect.Effect<A, E, never>;

const emptyRoots = {
  utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
};
const expectedRoots = {
  ...emptyRoots,
  transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
};
const expectedCounts = {
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
};

/** An empty block's header; `index` sets its time window. */
const blockHeader = (index: number, prevHeaderHash: string): SDK.Header => ({
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  ...expectedRoots,
  ...expectedCounts,
  startTime: BigInt(index * 1_000),
  endTime: BigInt(index * 1_000 + 999),
  blockSlot: BigInt(index),
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash,
  operatorVkey: "22".repeat(28),
  protocolVersion: 1n,
});

export type Block = {
  readonly header: SDK.Header;
  readonly headerHash: string;
};

export const block = (index: number, prev: string) =>
  Effect.map(SDK.hashBlockHeader(blockHeader(index, prev)), (headerHash) => ({
    header: blockHeader(index, prev),
    headerHash,
  }));

/** An empty block's processed landed row on `parentHeaderHash`. */
const emptyRow = (
  headerHash: string,
  parentHeaderHash: string,
  kind: LandedBlockRow["kind"],
  applied: boolean,
): LandedBlockRow => ({
  headerHash,
  parentHeaderHash,
  parentUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  kind,
  state: "processed",
  applied,
  spent: [],
  produced: [],
  depositIds: [],
  withdrawals: [],
  forcedIds: [],
  txIds: [],
});

/**
 * Journals `stored` under `headerHash` on `base` (by default its header's
 * parent) with one block-row tx, its block landed and, when `status` is
 * applied, applied.
 */
export const journal = ({
  headerHash,
  stored,
  status,
  base = stored.prevHeaderHash,
}: {
  readonly headerHash: string;
  readonly stored: SDK.Header;
  readonly base?: string;
  readonly status:
    | typeof PendingBlockFinalizationsDB.OwnBlockStatus.Applied
    | typeof PendingBlockFinalizationsDB.OwnBlockStatus.Landed;
}) =>
  Effect.gen(function* () {
    const hash = Buffer.from(headerHash, "hex");
    const blockStartTime = new Date("2026-06-12T00:00:00.000Z");
    yield* prepareUnderTestLease({
      headerHash: hash,
      headerCbor: Buffer.from(
        LucidData.to(stored as never, SDK.Header as never),
        "hex",
      ),
      metadata: {
        deploymentMarker: makeDeploymentMarker("de".repeat(32)),
        consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
        stateQueueLeaseToken: "lease-token",
        baseSnapshotId: "snapshot",
        baseTailOutRef: `${headerHash}#0`,
        baseTailHeaderHash: Buffer.from(base, "hex"),
        baseTailDatumCbor: "d87980",
        baseRoots: emptyRoots,
        blockStartTime,
        expectedRoots,
        expectedCounts,
      },
      blockEndTime: new Date(blockStartTime.getTime() + 60_000),
      blockTime: new Date(blockStartTime.getTime() + 60_000),
      depositEventIds: [],
      depositEntries: [],
      forcedTransactionEventIds: [],
      forcedTransactionEntries: [],
      withdrawalEventIds: [],
      withdrawalEntries: [],
      mempoolTxIds: [],
      mempoolTxs: [],
      mempoolTxSourceTable: "none",
      transitionTraceMembers: [],
      eventToStepMembers: [],
      validationTraceMembers: [],
      ledgerDelta: { spent: [], produced: [] },
    });
    yield* insertRow(
      emptyRow(
        headerHash,
        stored.prevHeaderHash,
        "own",
        status === PendingBlockFinalizationsDB.OwnBlockStatus.Applied,
      ),
    );
    yield* BlocksDB.insert(hash, [
      Buffer.from(headerHash.padEnd(64, "0"), "hex"),
    ]);
  });

export const finalizedJournal = ({ header, headerHash }: Block) =>
  journal({
    headerHash,
    stored: header,
    status: PendingBlockFinalizationsDB.OwnBlockStatus.Applied,
  });

/**
 * L1's confirmed state naming `headerHash`, whose header ends at `endTime`
 * (a merge carries its header's end time into the confirmed state).
 */
export const confirmedAt = (
  headerHash: string,
  endTime: bigint,
  utxoRoot: string = SDK.EMPTY_MERKLE_TREE_ROOT,
): SDK.ConfirmedState => ({
  headerHash,
  prevHeaderHash: SDK.GENESIS_HEADER_HASH,
  utxoRoot,
  startTime: 0n,
  endTime,
  protocolVersion: 1n,
});

/** L1's confirmed state at this node's block. */
export const confirmedAtBlock = (
  { header, headerHash }: Block,
  utxoRoot: string = SDK.EMPTY_MERKLE_TREE_ROOT,
) => confirmedAt(headerHash, header.endTime, utxoRoot);

/** Another operator's empty block on `parentHeaderHash`, as landed-block
 * processing records it. */
export const foreignRow = (
  headerHash: string,
  parentHeaderHash: string,
): LandedBlockRow => emptyRow(headerHash, parentHeaderHash, "foreign", true);

/** This node's block `block`'s processed, applied landed row. */
export const ownRow = ({ header, headerHash }: Block): LandedBlockRow =>
  emptyRow(headerHash, header.prevHeaderHash, "own", true);

/**
 * Folds the processed `row` into `confirmed_ledger` at the frontier, as
 * landed-block processing does once its merge landed: its landed row goes
 * and only its retained fold names its parent.
 */
export const fold = (row: LandedBlockRow) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(foldMerge(row, null));
  });

const statusOf = (headerHash: string) =>
  PendingBlockFinalizationsDB.retrieveStatusByHeaderHash(
    Buffer.from(headerHash, "hex"),
  );

const blockRows = (headerHash: string) =>
  Effect.map(
    BlocksDB.retrieveTxHashesByHeaderHash(Buffer.from(headerHash, "hex")),
    (rows) => rows.length,
  );

/**
 * The merge finalized: its block is folded; the block rows stay until its
 * fold is final (`releaseFinalFolds`).
 */
export const expectFinalized = (headerHash: string) =>
  Effect.gen(function* () {
    expect(yield* statusOf(headerHash)).toBe(
      PendingBlockFinalizationsDB.OwnBlockStatus.Folded,
    );
    expect(yield* blockRows(headerHash)).toBe(1);
  });

/** The merge did not finalize: its block is not folded and its block rows
 * are still there. */
export const expectUntouched = (headerHash: string) =>
  Effect.gen(function* () {
    expect(yield* statusOf(headerHash)).not.toBe(
      PendingBlockFinalizationsDB.OwnBlockStatus.Folded,
    );
    expect(yield* blockRows(headerHash)).toBe(1);
  });
