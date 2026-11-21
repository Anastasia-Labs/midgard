/**
 * Forced rows across rollbacks (N10b), on a simulated chain with the
 * follower store in the node database as in production.
 *
 * A rollback that removes an order removes its follower key and its order
 * row. The hook then deletes the node row without a header that no live
 * block journal holds. A journal that includes the order is built at a
 * commit anchor above it (the follower block it was built at, with every
 * event it includes below it), so the rollback takes the anchor too and the
 * journal is dead (I3) at once, with no operator step: the hook deletes the
 * row in place. An order that lands again ends as exactly one row, the
 * same row.
 */
import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  ForcedTransactionsDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import { Frontier } from "../src/landed-blocks/store.js";
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
import { simCommitAnchor } from "./helpers/landed-blocks-sim.own.js";

const follow = nodeFollowerLifecycle();

const ZERO_ROOT = "00".repeat(32);
const EMPTY_MERKLE_ROOT =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";
const HEADER = Buffer.alloc(28, 0x7e);

const BASE = Buffer.alloc(28, 0xbb);

/**
 * A modeled block journal holding the forced row of `txOrderId`, as a build
 * leaves it: the row `projected` without a header, the journal live on the
 * confirmed-ledger frontier, its commit anchor the follower's tip (d = 0).
 */
const journal = (txOrderId: Buffer) =>
  db(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const at = new Date(1_000_000);
      const anchor = yield* simCommitAnchor;
      yield* Frontier.upsert({
        headerHash: BASE.toString("hex"),
        utxosRoot: ZERO_ROOT,
      });
      yield* sql`INSERT INTO pending_block_finalizations ${sql.insert({
        header_hash: HEADER,
        block_end_time: new Date(at.getTime() + 1_000),
        block_time: new Date(at.getTime() + 1_000),
        state_queue_lease_token: "forced-rollback-test",
        base_snapshot_id: "forced-rollback-test",
        base_tail_out_ref: "base#0",
        base_tail_header_hash: BASE,
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
        commit_anchor_hash: anchor.hash,
        commit_anchor_height: anchor.height,
        commit_anchor_slot: anchor.slot,
        ledger_delta_spent: "[]",
        ledger_delta_produced: "[]",
      } as never)}`;
      const payload = ForcedTransactionsDB.encodeForcedTransactionJournalMember(
        {
          sourceValueCbor: Buffer.from([1]),
          canonicalTransactionCbor: Buffer.from([2]),
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

/** Why the journal is dead, if it is. */
const deadCause = () =>
  db(
    Effect.map(PendingBlockFinalizationsDB.deadJournals, (dead) =>
      dead.causes.get(HEADER.toString("hex")),
    ),
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

  it("a rollback that takes a journaled order takes the journal's commit anchor: the journal is dead and the row is deleted in place", async () => {
    const { store, chain } = await follow();
    const order = inlineOrder(chain.chain, honest());
    await chain.forward([order]);
    // The commit's anchor: a block above the order's.
    await chain.forward([]);
    const { hook, logs } = ingestionHook(store, FORCED_CONFIG);
    expect(await hook(UNCHANGED)).toBeUndefined();
    const [row] = await rows();
    await journal(Buffer.from(row!.tx_order_id));
    // Journaled and its order live: the journal is live, no orphan.
    expect(await deadCause()).toBeUndefined();
    expect(await orphans()).toBe(0);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(await rows()).toHaveLength(1);
    await chain.backward(2);
    expect(await deadCause()).toBe("its commit anchor left the chain");
    expect(await orphans()).toBe(0);
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(logs.join("\n")).toContain(
      `deleted 1 forced row(s) whose order left the chain: ${label(order)}`,
    );
    expect(await rows()).toEqual([]);
    expect(await horizon()).toBeNull();
  });

  it("a journaled row whose order lands again is no orphan; its journal dies with its anchor and the row stays one row", async () => {
    const { store, chain } = await follow();
    const order = inlineOrder(chain.chain, honest());
    await chain.forward([order]);
    await chain.forward([]);
    const { hook } = ingestionHook(store, FORCED_CONFIG);
    expect(await hook(UNCHANGED)).toBeUndefined();
    const [row] = await rows();
    await journal(Buffer.from(row!.tx_order_id));
    await chain.backward(2);
    await chain.forward([order]);
    await chain.forward([]);
    expect(await orphans()).toBe(0);
    // The relanded chain is a new branch: the anchor is not on it, so the
    // journal is dead; the row is no orphan and stays.
    expect(await deadCause()).toBe("its commit anchor left the chain");
    expect(await hook(UNCHANGED)).toBeUndefined();
    // The same one row, with no header: the next commit selects it.
    const after = await rows();
    expect(after).toHaveLength(1);
    expect(after[0]!.tx_order_id).toEqual(row!.tx_order_id);
    expect(after[0]!.projected_header_hash).toBeNull();
  });
});
