/**
 * Node-table fixtures for the commit predicate's tests: a block journal row
 * for `BLOCK` with its commit anchor, the follower block an anchor names,
 * and listed events.
 */
import { currentViewIn } from "@al-ft/midgard-l1-follower";
import { eventKeyOfId } from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { CommitAnchor } from "../../src/database/commit-anchor.js";
import { db } from "./forced-orders-node-store.js";
import type { PredicateScenario } from "./intent-predicates-scenario.js";

const ZERO_ROOT = "00".repeat(32);
const EMPTY_MERKLE_ROOT =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";
export const BLOCK = Buffer.alloc(28, 0x7e);

/** A block journal row for `BLOCK` in `status`, with its commit anchor. */
export const journalRow = (status: string, anchor: CommitAnchor | null) =>
  db(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const at = new Date(1_000_000);
      yield* sql`INSERT INTO pending_block_finalizations ${sql.insert({
        header_hash: BLOCK,
        submitted_tx_hash: null,
        block_end_time: at,
        status,
        observed_confirmed_at_ms: null,
        state_queue_lease_token: "predicate-test",
        base_snapshot_id: "predicate-test",
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
        commit_anchor_hash: anchor?.hash ?? null,
        commit_anchor_height: anchor?.height ?? null,
        commit_anchor_slot: anchor?.slot ?? null,
      } as never)}`;
    }),
  );

/** A listed event: its admission key and its event row, at `height`. */
export const listEvent = async (
  s: PredicateScenario,
  kind: "deposit" | "withdrawal",
  eventId: Buffer,
  height: number,
) => {
  const key = eventKeyOfId(eventId);
  const origin = Buffer.concat([Buffer.alloc(32, 0x0e), Buffer.from([0, 1])]);
  await s.sql(
    "INSERT INTO l1_event_keys (kind, key, origin_outref, first_canonical_slot) VALUES (?, ?, ?, ?)",
    [kind, key, origin, 0],
  );
  const bytes = Buffer.from("00", "hex");
  await s.sql(
    `INSERT INTO node_l1_events (kind, event_key, event_id, inclusion_time, facts_cbor, payload_cbor, original_assets_cbor, admission_tx_hash, admission_output_index, admission_tx_index, admitted_block_hash, admitted_height, admitted_slot)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
    [
      kind,
      key,
      eventId,
      0,
      bytes,
      bytes,
      bytes,
      origin.subarray(0, 32),
      1,
      0,
      Buffer.alloc(32),
      height,
      0,
    ],
  );
  return { key, origin };
};

/** An event id: the CBOR of an output reference. */
export const eventIdOf = (byte: number): Buffer =>
  Buffer.from(
    Data.to(
      {
        transactionId: byte.toString(16).padStart(2, "0").repeat(32),
        outputIndex: 0n,
      },
      SDK.OutputReference,
    ),
    "hex",
  );

export const viewHeight = async (s: PredicateScenario): Promise<number> => {
  const view = await s.store.transaction("read", (tx) =>
    currentViewIn(tx, s.store.dialect),
  );
  if (view === null) throw new Error("no view");
  return view.height;
};

/** The follower block at `height`, as a commit anchor. */
export const anchorAt = async (
  s: PredicateScenario,
  height: number,
): Promise<CommitAnchor> => {
  const [row] = await s.sql(
    "SELECT hash, slot FROM l1_blocks WHERE height = ?",
    [height],
  );
  if (row === undefined)
    throw new Error(`no follower block at height ${height.toString()}`);
  return {
    hash: Buffer.from(row.hash as Uint8Array),
    height,
    slot: Number(row.slot),
  };
};
