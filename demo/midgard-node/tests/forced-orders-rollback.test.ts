/**
 * Forced rows across rollbacks (N10b), on a simulated chain with the
 * follower store in the node database as in production.
 *
 * A rollback that removes an order removes its follower key and its order
 * row. The hook then deletes the node row without a header that no
 * unfinished block journal holds; a row such a journal holds is an orphan
 * the event-history recovery counts (`l1_events_orphan_recovery`) until the
 * journal is disposed of. The working-ledger rebase disposes of such a
 * journal (I3: it includes an event whose admission left the chain) with
 * no operator step; the hook then deletes the row. An order that lands
 * again ends as exactly one row, the same row.
 */
import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { ForcedTransactionsDB } from "../src/database/index.js";
import { EVENTS_ORPHAN_RECOVERY } from "../src/l1-events/driver.js";
import type { LandedLedger } from "../src/landed-blocks/ledger.js";
import {
  disposeJournals,
  ownJournalDisposition,
} from "../src/landed-blocks/own-journals.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { FORCED_CONFIG } from "./helpers/forced-orders-chain.js";
import {
  honest,
  horizon,
  INCLUSION,
  inlineOrder,
  label,
  nodeFollowerLifecycle,
  orphans,
  rows,
} from "./helpers/forced-orders-node-chain.js";
import {
  db,
  ingestionHook,
  UNCHANGED,
} from "./helpers/forced-orders-node-store.js";
import { provideDatabaseLayers } from "./utils.js";

const follow = nodeFollowerLifecycle();

const ZERO_ROOT = "00".repeat(32);
const EMPTY_MERKLE_ROOT =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";
const HEADER = Buffer.alloc(28, 0x7e);

/**
 * A modeled block journal holding the forced row of `txOrderId`, as a build
 * leaves it: the row `projected` without a header, the journal unfinished.
 */
const journal = (txOrderId: Buffer, status: string) =>
  db(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const at = new Date(1_000_000);
      yield* sql`INSERT INTO pending_block_finalizations ${sql.insert({
        header_hash: HEADER,
        submitted_tx_hash: null,
        block_end_time: new Date(at.getTime() + 1_000),
        status,
        observed_confirmed_at_ms: null,
        state_queue_lease_token: "forced-rollback-test",
        base_snapshot_id: "forced-rollback-test",
        base_tail_out_ref: "base#0",
        base_tail_header_hash: Buffer.alloc(28, 0xbb),
        base_tail_datum_cbor: "d87980",
        base_utxos_root: ZERO_ROOT,
        base_transactions_root: ZERO_ROOT,
        base_deposits_root: ZERO_ROOT,
        base_withdrawals_root: ZERO_ROOT,
        block_start_time: at,
        expected_utxos_root: ZERO_ROOT,
        expected_transactions_root: ZERO_ROOT,
        expected_deposits_root: ZERO_ROOT,
        expected_withdrawals_root: ZERO_ROOT,
        base_forced_transactions_root: ZERO_ROOT,
        expected_forced_transactions_root: ZERO_ROOT,
        header_cbor: Buffer.from("a0", "hex"),
        format_version: 1,
        replay_kind: "ledger_delta_v1",
        deployment_marker_schema_version: "midgard-deployment-marker-v1",
        deployment_manifest_id: "de".repeat(32),
        expected_transition_trace_root: ZERO_ROOT,
        expected_event_to_step_root: ZERO_ROOT,
        expected_withdrawal_count: 0n,
        expected_forced_transaction_count: 0n,
        expected_l2_transaction_count: 0n,
        expected_deposit_count: 0n,
        expected_total_event_count: 0n,
        expected_transition_step_count: 0n,
        consensus_profile_id: "midgard-consensus-v1",
        expected_validation_traces_root: EMPTY_MERKLE_ROOT,
        expected_validation_trace_count: 0n,
        ledger_delta_spent: "[]",
        ledger_delta_produced: "[]",
      } as never)}`;
      const payload = ForcedTransactionsDB.encodeForcedTransactionJournalMember(
        {
          sourceValueCbor: Buffer.from([1]),
          canonicalTransactionCbor: Buffer.from([2]),
          programMaterialSidecarCbor: Buffer.from([3]),
        },
      );
      yield* sql`INSERT INTO pending_block_finalization_forced_transactions ${sql.insert(
        {
          header_hash: HEADER,
          member_id: txOrderId,
          ordinal: 0,
          payload_cbor: payload,
          payload_sha256: createHash("sha256").update(payload).digest(),
          source_table: ForcedTransactionsDB.tableName,
          source_id: txOrderId,
          source_time_stamp_tz: at,
        } as never,
      )}`;
      yield* sql`UPDATE ${sql(ForcedTransactionsDB.tableName)}
        SET status = 'projected' WHERE tx_order_id = ${txOrderId}`;
    }),
  );

/** No landed blocks: the journal's base is not on the processed chain. */
const NO_LANDED: LandedLedger = {
  frontier: { headerHash: "00".repeat(28), utxosRoot: ZERO_ROOT },
  confirmed: [],
  chain: [],
};

/** The working-ledger rebase's own-journal disposition, then its disposal. */
const disposeOrphanHolders = () =>
  Effect.runPromise(
    provideDatabaseLayers(
      withFollowerWrite(
        Effect.gen(function* () {
          const disposition = yield* ownJournalDisposition([], NO_LANDED);
          yield* disposeJournals(disposition.dispose);
          return disposition;
        }),
      ),
    ),
  );

const journalStatus = () =>
  db(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const [row] = yield* sql<{ status: string }>`
        SELECT status FROM pending_block_finalizations
        WHERE header_hash = ${HEADER}`;
      return row?.status;
    }),
  );

describe("forced rows across rollbacks (N10b)", () => {
  it("an order whose block rolls back and never returns leaves no row, and the horizon drops it", async () => {
    const { store, chain } = await follow();
    const order = inlineOrder(chain.chain, honest());
    await chain.forward([order]);
    const { hook, logs } = ingestionHook(store, FORCED_CONFIG);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await rows()).toHaveLength(1);
    expect(await horizon()).toBeNull();
    await chain.backward(1);
    // Until the hook runs, the row without its order caps the horizon below
    // its inclusion time: no block may take it.
    expect(await horizon()).toBe(Number(INCLUSION) - 1);
    expect(await orphans()).toBe(0);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await rows()).toEqual([]);
    expect(logs.join("\n")).toContain(
      `deleted 1 forced row(s) whose order left the chain: ${label(order)}`,
    );
    expect(await horizon()).toBeNull();
    // An empty block on the new branch: the order never returns, nothing
    // comes back.
    await chain.forward([]);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await rows()).toEqual([]);
  });

  it("an honest order a rollback does not reach is ingested and kept unchanged", async () => {
    const { store, chain } = await follow();
    const order = inlineOrder(chain.chain, honest());
    await chain.forward([order]);
    await chain.forward([]);
    const { hook, logs } = ingestionHook(store, FORCED_CONFIG);
    expect(await hook(UNCHANGED)).toBeUndefined();
    const before = await rows();
    expect(before).toHaveLength(1);
    expect(Buffer.from(before[0]!.native_tx_cbor)).toEqual(honest());
    expect(before[0]!.status).toBe(ForcedTransactionsDB.Status.Awaiting);
    await chain.backward(1);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await rows()).toEqual(before);
    expect(logs.join("\n")).not.toMatch(/deleted/u);
    expect(await horizon()).toBeNull();
    expect(await orphans()).toBe(0);
  });

  it("an order that lands again before the hook runs keeps its one row", async () => {
    const { store, chain } = await follow();
    const order = inlineOrder(chain.chain, honest());
    await chain.forward([order]);
    const { hook, logs } = ingestionHook(store, FORCED_CONFIG);
    expect(await hook(UNCHANGED)).toBeUndefined();
    const before = await rows();
    await chain.backward(1);
    await chain.forward([]);
    await chain.forward([order]);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await rows()).toEqual(before);
    expect(logs.join("\n")).not.toMatch(/deleted/u);
    expect(await horizon()).toBeNull();
  });

  it("an order that lands again after the hook deleted its row is ingested to the same row", async () => {
    const { store, chain } = await follow();
    const order = inlineOrder(chain.chain, honest());
    await chain.forward([order]);
    const { hook, logs } = ingestionHook(store, FORCED_CONFIG);
    expect(await hook(UNCHANGED)).toBeUndefined();
    const before = await rows();
    await chain.backward(1);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await rows()).toEqual([]);
    await chain.forward([order]);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await rows()).toEqual(before);
    expect(logs.join("\n")).toMatch(/deleted 1 forced row/u);
    expect(await horizon()).toBeNull();
  });

  it("deletes nothing while the follower is not caught up", async () => {
    const { store, chain } = await follow();
    const order = inlineOrder(chain.chain, honest());
    await chain.forward([order]);
    const { hook } = ingestionHook(store, FORCED_CONFIG);
    expect(await hook(UNCHANGED)).toBeUndefined();
    const before = await rows();
    await chain.backward(1);
    const behind = ingestionHook(store, FORCED_CONFIG, {
      caughtUp: () => false,
    });
    expect(await behind.hook(UNCHANGED)).toBeUndefined();
    expect(await rows()).toEqual(before);
    // The row still caps the horizon below its inclusion time.
    expect(await horizon()).toBe(Number(INCLUSION) - 1);
  });

  it("a row an unfinished block journal holds is routed to the orphan recovery, never deleted in place, and recovered once the journal is disposed of", async () => {
    const { store, chain } = await follow();
    const order = inlineOrder(chain.chain, honest());
    await chain.forward([order]);
    const { hook, logs } = ingestionHook(store, FORCED_CONFIG);
    expect(await hook(UNCHANGED)).toBeUndefined();
    const [row] = await rows();
    await journal(Buffer.from(row!.tx_order_id), "submitted_unconfirmed");
    // Journaled and its order live: no orphan.
    expect(await orphans()).toBe(0);
    expect(await hook(UNCHANGED)).toBeUndefined();
    await chain.backward(1);
    expect(await orphans()).toBe(1);
    for (let attempt = 0; attempt < 2; attempt += 1) {
      expect(await hook(UNCHANGED)).toEqual({
        reason: EVENTS_ORPHAN_RECOVERY,
        detail: expect.stringContaining(label(order)) as unknown,
      });
      const kept = await rows();
      expect(kept).toHaveLength(1);
      expect(kept[0]!.status).toBe(ForcedTransactionsDB.Status.Projected);
      expect(kept[0]!.projected_header_hash).toBeNull();
    }
    expect(logs.join("\n")).not.toMatch(/deleted/u);
    // The rebase disposes of the journal for its orphaned member, which
    // clears the orphan; the hook then deletes the row.
    expect(await disposeOrphanHolders()).toEqual({
      dispose: [
        {
          headerHash: HEADER.toString("hex"),
          cause: "it includes an event whose admission left the chain",
          active: true,
        },
      ],
      revive: [],
    });
    expect(await journalStatus()).toBe("abandoned");
    expect(await orphans()).toBe(0);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await rows()).toEqual([]);
    expect(await horizon()).toBeNull();
  });

  it("a journaled row whose order lands again is no orphan and stays in its journal", async () => {
    const { store, chain } = await follow();
    const order = inlineOrder(chain.chain, honest());
    await chain.forward([order]);
    const { hook } = ingestionHook(store, FORCED_CONFIG);
    expect(await hook(UNCHANGED)).toBeUndefined();
    const [row] = await rows();
    await journal(Buffer.from(row!.tx_order_id), "submitted_unconfirmed");
    const before = await rows();
    await chain.backward(1);
    expect(await orphans()).toBe(1);
    await chain.forward([order]);
    expect(await orphans()).toBe(0);
    // The rebase keeps a journal whose forced member is canonical again.
    expect((await disposeOrphanHolders()).dispose).toEqual([]);
    expect(await journalStatus()).toBe("submitted_unconfirmed");
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await rows()).toEqual(before);
  });
});
