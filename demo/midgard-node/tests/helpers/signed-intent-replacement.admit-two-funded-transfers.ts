import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, vi } from "vitest";

import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import { commitConfirmRecoverAndMerge } from "../deposit-flow-emulator-shared.js";
import {
  admitTransfer,
  buildDepositorTransfer,
  depositorL2Utxos,
  type Lifecycle,
  read,
  readLocalFinalizationJob,
  readSqlLedgerRoot,
  settleWithin,
  submitDeposit,
} from "./correction-rewind-scenario.js";

/** Shared steps of the signed-intent replacement emulator tests ("whichever
 * lands wins"): actual deployed validators, the production history owner and
 * Architecture G, and emulator transactions. */

export const C = Pending.Columns;

/** The release's own plan domain, never the signed-header recovery's. */
export const SIGNED_INTENT_RELEASE_DOMAIN =
  "midgard-history-signed-intent-release-intent-v1";

export const UNLANDED: readonly string[] = [
  Pending.Status.PendingSubmission,
  Pending.Status.SubmittedLocalFinalizationPending,
  Pending.Status.SubmittedUnconfirmed,
];

export type Handle =
  | Lifecycle
  | Awaited<ReturnType<Lifecycle["restartRuntime"]>>;

export const resetSharedRows = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DELETE FROM state_queue_terminal_observer_states`;
      yield* sql`DELETE FROM event_history_recovery_plans`;
    }),
  );

/** The signed upper validity bound (TTL, exclusive), in slots. */
export const signedTtl = (cbor: Buffer) => {
  const tx = CML.Transaction.from_cbor_bytes(cbor);
  const body = tx.body();
  const ttl = body.ttl();
  body.free();
  tx.free();
  if (ttl === undefined) throw new Error("A commit is signed with a TTL");
  return Number(ttl);
};

export const readPlans = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ state: string; intent: string }>`
        SELECT state, intent FROM event_history_recovery_plans
        ORDER BY created_at`;
      return rows.map((row) => ({
        state: row.state,
        intent: JSON.parse(row.intent) as {
          domain: string;
          headerHash: string;
          signedTransactionHash: string;
          targetRoot: string;
        },
      }));
    }),
  );

export const readLeaseStatus = (token: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ status: string }>`
        SELECT status FROM state_queue_mutation_leases WHERE token = ${token}`;
      expect(rows).toHaveLength(1);
      return rows[0]!.status;
    }),
  );

/** A commit process killed after handing its block to L1 never releases its
 * state-queue mutation lease. */
export const holdLeaseAsCrashed = (token: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const updated = yield* sql`UPDATE state_queue_mutation_leases
        SET status = 'active', released_at = NULL,
          expires_at = NOW() + INTERVAL '10 minutes'
        WHERE token = ${token} RETURNING token`;
      expect(updated).toHaveLength(1);
    }),
  );

export const retireCrashedLease = (token: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`UPDATE state_queue_mutation_leases
        SET status = 'released', released_at = NOW()
        WHERE token = ${token} AND status = 'active'`;
    }),
  );

/** Two independent L2 outputs funded through a merged deposit block, each
 * spent by an admitted L2 transfer that is not committed yet. */
export const admitTwoFundedTransfers = async (lifecycle: Lifecycle) => {
  const h = lifecycle;
  const funding = [
    await submitDeposit(h, 20_000_000n),
    await submitDeposit(h, 9_000_000n),
  ];
  await lifecycle.deployment.chain.awaitLedgerTime(Math.max(...funding) + 1000);
  await nextPoint(h);
  await commitConfirmRecoverAndMerge({
    fixture: h.fixture,
    lucidService: h.lucidService,
    globals: h.globals,
    production: h.production,
  });
  await h.synchronize();
  const byAmount = async (lovelace: bigint) => {
    const found = (await depositorL2Utxos(lifecycle)).filter(
      (utxo) => utxo.assets.lovelace === lovelace,
    );
    expect(found).toHaveLength(1);
    return found[0]!;
  };
  const first = await buildDepositorTransfer(
    lifecycle,
    [await byAmount(20_000_000n)],
    5_000_000n,
  );
  const second = await buildDepositorTransfer(
    lifecycle,
    [await byAmount(9_000_000n)],
    4_000_000n,
  );
  expect(await admitTransfer(lifecycle, first)).toBe("accepted");
  expect(await admitTransfer(lifecycle, second)).toBe("accepted");
  const txIds = [first.txId, second.txId]
    .map((id) => id.toString("hex"))
    .sort();
  return { first, second, txIds };
};

/** Direct journal surgery; returns the previous values. */
export const updateJournal = (
  headerHash: string,
  fields: Readonly<Record<string, unknown>>,
) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const key = Buffer.from(headerHash, "hex");
      const rows = yield* sql<Record<string, unknown>>`
        SELECT * FROM pending_block_finalizations WHERE header_hash = ${key}`;
      expect(rows).toHaveLength(1);
      const before = Object.fromEntries(
        Object.keys(fields).map((column) => [column, rows[0]![column]]),
      );
      const updated = yield* sql`UPDATE pending_block_finalizations
        SET ${sql.update(fields as Record<string, never>)}
        WHERE header_hash = ${key} RETURNING header_hash`;
      expect(updated).toHaveLength(1);
      return before;
    }),
  );

/** Raw journal columns: a journal whose signed bytes do not decode is
 * refused by the canonical record loader, so it is read column by column. */
export const readJournalColumns = (headerHash: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        status: string;
        correction_transition_digest: string | null;
      }>`SELECT status, correction_transition_digest
        FROM pending_block_finalizations
        WHERE header_hash = ${Buffer.from(headerHash, "hex")}`;
      expect(rows).toHaveLength(1);
      return rows[0]!;
    }),
  );

export const readMempoolTxIds = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ tx_id: Buffer }>`SELECT tx_id FROM mempool`;
      return rows.map((row) => row.tx_id.toString("hex")).sort();
    }),
  );

/** How many times each transaction is committed locally. */
export const readImmutableCounts = (txIds: readonly string[]) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ tx_id: Buffer }>`SELECT tx_id FROM immutable
        WHERE tx_id IN ${sql.in(txIds.map((id) => Buffer.from(id, "hex")))}`;
      return Object.fromEntries(
        txIds.map((id) => [
          id,
          rows.filter((row) => row.tx_id.toString("hex") === id).length,
        ]),
      );
    }),
  );

export const readDepositHeader = (eventId: Buffer) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ projected_header_hash: Buffer | null }>`
        SELECT projected_header_hash FROM deposits_utxos
        WHERE event_id = ${eventId}`;
      expect(rows).toHaveLength(1);
      return rows[0]!.projected_header_hash?.toString("hex") ?? null;
    }),
  );

export const nativeRoot = async (handle: Pick<Handle, "evidence">) => {
  const native = (await handle.evidence()).native;
  if (native === undefined) throw new Error("Native owner is not open");
  return native.durableRoot;
};

/** The next authenticated source point, one L1 block later. Bounded, so a
 * wedged owner fails here. */
export const nextPoint = async (handle: Handle) => {
  handle.fixture.emulator.awaitBlock(1);
  vi.setSystemTime(new Date(handle.fixture.emulator.now()));
  await synchronizeWithin(handle);
};

/** Advance L1 (and the faked wall clock) to `slot` without appending any
 * source point. */
export const advanceL1ToSlot = (handle: Handle, slot: number) => {
  const delta = slot - handle.fixture.emulator.slot;
  expect(delta).toBeGreaterThan(0);
  handle.fixture.emulator.awaitSlot(delta);
  vi.setSystemTime(new Date(handle.fixture.emulator.now()));
};

/** Move L1 to exactly `slot` so the next sealed source point's head is that
 * slot. The emulator derives block height from the slot, so a point inside
 * the last point's height band gets the next height; transport heights are
 * synthetic anyway. Nothing may be pending in the emulator. */
export const moveToExactSlot = (handle: Handle, slot: number) => {
  const { emulator } = handle.fixture;
  expect(Object.keys(emulator.mempool)).toHaveLength(0);
  advanceL1ToSlot(handle, slot);
  const last = handle.batches.at(-1)!;
  expect(slot).toBeGreaterThan(last.observedSlot);
  emulator.blockHeight = Math.max(
    emulator.blockHeight,
    last.observedHeight + 1,
  );
};

/** Wait (real time) for a restarted owner to open its gate without any new
 * source point after the restart. */
export const awaitOwnerReady = async (handle: Handle) => {
  const deadline = performance.now() + 120_000;
  for (;;) {
    const frontier = Effect.runSync(handle.production.owner.frontier);
    if (frontier.ready) return frontier;
    if (performance.now() >= deadline)
      throw new Error(
        `Restarted history owner never became ready: ${JSON.stringify(frontier)}`,
      );
    await new Promise((resolve) => setTimeout(resolve, 50));
  }
};

export const snapshotUnreplaced = async (headerHash: string) => {
  const journal = await readJournalColumns(headerHash);
  return {
    status: journal.status,
    correction: journal.correction_transition_digest,
    job: (await readLocalFinalizationJob(headerHash)) !== undefined,
    plans: await readPlans(),
    sqlRoot: (await readSqlLedgerRoot()).root_hex,
  };
};

/** The signed-intent journal is exactly as it was: still active, nothing
 * replaced. */
export const expectUnreplaced = async (
  headerHash: string,
  before: Awaited<ReturnType<typeof snapshotUnreplaced>>,
) => {
  const now = await snapshotUnreplaced(headerHash);
  expect(now).toEqual(before);
  expect(UNLANDED).toContain(now.status);
};

/** One synchronization that must converge within `ms` of real time. */
export const synchronizeWithin = (h: Handle, ms = 120_000) =>
  settleWithin(h.synchronize(), ms);
