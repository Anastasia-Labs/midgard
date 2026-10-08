/**
 * The node's §8.4 family predicates over the follower's projections (I1),
 * each in both polarities, on the node database:
 *
 * - commit: its block journal row is live, the tail it spends is still the
 *   landed tail, the scheduler names this operator at the lower validity
 *   bound, and every included event is canonical and at least d blocks
 *   below the view; each failing condition is `false`;
 * - merge: the header it names is the queue's head, and it spends the head
 *   and the root;
 * - attestation: the header it names is landed;
 * - correction: the removed header is landed and the target is still
 *   unattested and timed out at the lower validity bound;
 * - payout absorb/initialize: the event it names is listed and not
 *   retired; fund/conclude: every input is still a live fact;
 * - a family whose target state is not in the facts throws
 *   `FamilyPredicateUnavailable` (held, never resent or abandoned).
 *
 * The operator-set transitions are in
 * `l1-follower-intent-predicates-operators.test.ts`.
 */
import { createHash } from "node:crypto";

import { currentViewIn, type FactStore } from "@al-ft/midgard-l1-follower";
import { eventKeyOfId } from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { FamilyPredicateUnavailable } from "../src/services/l1-follower.intent-predicates.js";
import { db } from "./helpers/forced-orders-node-store.js";
import {
  FOREIGN,
  openPredicateScenario,
  outRefText,
  OWN,
  type PredicateScenario,
} from "./helpers/intent-predicates-scenario.js";
import {
  loadOperatorSetChainFixture,
  type OperatorSetChainFixture,
} from "./helpers/operator-set-chain.js";

const opened: FactStore[] = [];
let fixture: OperatorSetChainFixture;
beforeAll(async () => {
  fixture = await loadOperatorSetChainFixture();
}, 120_000);
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

const open = () => openPredicateScenario(fixture, opened);

const ZERO_ROOT = "00".repeat(32);
const EMPTY_MERKLE_ROOT =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";
const BLOCK = Buffer.alloc(28, 0x7e);

/** A block journal row for `BLOCK` in `status`. */
const journalRow = (status: string) =>
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
      } as never)}`;
    }),
  );

/** A journal member table row for `BLOCK`. */
const member = (
  table:
    | "pending_block_finalization_deposits"
    | "pending_block_finalization_forced_transactions",
  memberId: Buffer,
  identity: { key: Buffer; origin: Buffer } | null,
) =>
  db(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const payload = Buffer.from("member");
      yield* sql`INSERT INTO ${sql(table)} ${sql.insert({
        header_hash: BLOCK,
        member_id: memberId,
        ordinal: 0,
        payload_cbor: payload,
        payload_sha256: createHash("sha256").update(payload).digest(),
        source_table: "predicate_test",
        source_id: memberId,
        source_time_stamp_tz: new Date(1_000_000),
        ...(identity === null
          ? {}
          : { l1_event_key: identity.key, l1_origin_outref: identity.origin }),
      } as never)}`;
    }),
  );

/** A listed event: its admission key and its event row, at `height`. */
const listEvent = async (
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
const eventIdOf = (byte: number): Buffer =>
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

const viewHeight = async (s: PredicateScenario): Promise<number> => {
  const view = await s.store.transaction("read", (tx) =>
    currentViewIn(tx, s.store.dialect),
  );
  if (view === null) throw new Error("no view");
  return view.height;
};

describe("the node's §8.4 predicates over the follower's projections", () => {
  it("commit: wanted while its journal row, tail, shift and included events hold; each failing one is false", async () => {
    const s = await open();
    await s.land(s.lists.insert(s.live(), "active", OWN));
    await s.land(s.lists.shift(s.live(), OWN, 0n));
    const tail = outRefText(s.head);
    const commit = await s.record(
      "commit",
      `commit:tail=${tail}`,
      s.spend([s.head]),
      BLOCK,
    );

    // No journal row for the header: not wanted.
    expect(await s.verdict(commit)).toBe(false);
    await journalRow("pending_submission");
    expect(await s.verdict(commit)).toBe(true);

    // An included deposit, canonical and d below the view.
    const eventId = eventIdOf(0x31);
    const { key, origin } = await listEvent(s, "deposit", eventId, 1);
    await member("pending_block_finalization_deposits", eventId, {
      key,
      origin,
    });
    expect(await s.verdict(commit)).toBe(true);
    // Admitted within d of the view.
    const height = await viewHeight(s);
    await s.sql("UPDATE node_l1_events SET admitted_height = ?", [height - 1]);
    expect(await s.verdict(commit)).toBe(false);
    await s.sql("UPDATE node_l1_events SET admitted_height = ?", [height - 2]);
    expect(await s.verdict(commit)).toBe(true);
    // No longer canonical: the admission key is gone.
    await s.sql("DELETE FROM l1_event_keys");
    expect(await s.verdict(commit)).toBe(false);
    await s.sql(
      "INSERT INTO l1_event_keys (kind, key, origin_outref, first_canonical_slot) VALUES (?, ?, ?, ?)",
      ["deposit", key, origin, 0],
    );
    expect(await s.verdict(commit)).toBe(true);

    // A forced member with no forced row: false.
    await member(
      "pending_block_finalization_forced_transactions",
      Buffer.alloc(32, 0x44),
      null,
    );
    expect(await s.verdict(commit)).toBe(false);
    await s.sql("DELETE FROM pending_block_finalization_forced_transactions");
    expect(await s.verdict(commit)).toBe(true);

    // The scheduler hands the shift to another operator: false.
    await s.land(s.lists.insert(s.live(), "active", FOREIGN));
    await s.land(s.lists.shift(s.live(), FOREIGN, 0n));
    expect(await s.verdict(commit)).toBe(false);
    await s.land(s.lists.shift(s.live(), OWN, 0n));
    expect(await s.verdict(commit)).toBe(true);
    // Its lower validity bound before the shift's start (the appointment
    // the node submitted just before the commit): still wanted.
    expect(
      await s.verdict(commit, {
        slotToPosixMs: () => -1,
      }),
    ).toBe(true);
    // Its lower validity bound past the shift: false.
    expect(
      await s.verdict(commit, {
        slotToPosixMs: () => Number(SDK.SHIFT_DURATION_MS),
      }),
    ).toBe(false);

    // The journal row abandoned: false.
    await db(
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) =>
          sql`UPDATE pending_block_finalizations SET status = 'abandoned'`,
      ),
    );
    expect(await s.verdict(commit)).toBe(false);
  });

  it("commit: a tail that is no longer the landed tail is false", async () => {
    const s = await open();
    await s.land(s.lists.insert(s.live(), "active", OWN));
    await s.land(s.lists.shift(s.live(), OWN, 0n));
    await journalRow("pending_submission");
    const stale = await s.record(
      "commit",
      `commit:tail=${outRefText(s.root)}`,
      s.spend([s.root]),
      BLOCK,
    );
    expect(await s.verdict(stale)).toBe(false);
    // The tail named but not spent: false.
    const unspent = await s.record(
      "commit",
      `commit:tail=${outRefText(s.head)}`,
      s.spend([s.spare(0)]),
      BLOCK,
    );
    expect(await s.verdict(unspent)).toBe(false);
  });

  it("merge: wanted while its header is the head and it spends the head and the root", async () => {
    const s = await open();
    const header = Buffer.from(s.firstHash, "hex");
    const merge = await s.record(
      "merge",
      `merge:head=${s.firstHash}`,
      s.spend([s.root, s.head]),
      header,
    );
    expect(await s.verdict(merge)).toBe(true);
    const other = await s.record(
      "merge",
      "merge:head=other",
      s.spend([s.root, s.head], { nonce: s.nonce() }),
      Buffer.alloc(28, 0xee),
    );
    expect(await s.verdict(other)).toBe(false);
    const headOnly = await s.record(
      "merge",
      `merge:head=${s.firstHash}:head-only`,
      s.spend([s.head]),
      header,
    );
    expect(await s.verdict(headOnly)).toBe(false);
  });

  it("attest: wanted while its header is landed", async () => {
    const s = await open();
    const landed = await s.record(
      "attest",
      `attest:${s.firstHash}:apply`,
      s.spend([s.spare(0)]),
      Buffer.from(s.firstHash, "hex"),
    );
    expect(await s.verdict(landed)).toBe(true);
    const absent = await s.record(
      "attest",
      "attest:absent:apply",
      s.spend([s.spare(1)]),
      Buffer.alloc(28, 0xee),
    );
    expect(await s.verdict(absent)).toBe(false);
  });

  it("correction: wanted while the removed header is landed and the target is unattested and timed out", async () => {
    const s = await open();
    const deadline = Number(s.first.endTime + SDK.DA_ATTESTATION_TIMEOUT_MS);
    const correction = await s.record(
      "correction",
      `correction:${s.firstHash}:remove-last:${s.firstHash}`,
      s.spend([s.head, s.spare(0)]),
      Buffer.from(s.firstHash, "hex"),
    );
    expect(await s.verdict(correction, { slotToPosixMs: () => deadline })).toBe(
      true,
    );
    expect(
      await s.verdict(correction, { slotToPosixMs: () => deadline - 1 }),
    ).toBe(false);
    const gone = await s.record(
      "correction",
      `correction:${s.firstHash}:prune-descendant:${"ee".repeat(28)}`,
      s.spend([s.spare(1)]),
      Buffer.alloc(28, 0xee),
    );
    expect(await s.verdict(gone, { slotToPosixMs: () => deadline })).toBe(
      false,
    );
  });

  it("payout absorb/initialize: wanted while the event is listed and not retired", async () => {
    const s = await open();
    const deposit = eventIdOf(0x51);
    const withdrawal = eventIdOf(0x52);
    const absorb = await s.record(
      "settlement",
      `settlement:deposit:${deposit.toString("hex")}:absorb`,
      s.spend([s.spare(0)]),
      deposit,
    );
    const initialize = await s.record(
      "reserve_payout",
      `reserve_payout:${withdrawal.toString("hex")}:initialize`,
      s.spend([s.spare(1)]),
      withdrawal,
    );
    expect(await s.verdict(absorb)).toBe(false);
    expect(await s.verdict(initialize)).toBe(false);
    await listEvent(s, "deposit", deposit, 1);
    await listEvent(s, "withdrawal", withdrawal, 1);
    expect(await s.verdict(absorb)).toBe(true);
    expect(await s.verdict(initialize)).toBe(true);
    await s.sql("UPDATE node_l1_events SET retired_slot = 0");
    expect(await s.verdict(absorb)).toBe(false);
    expect(await s.verdict(initialize)).toBe(false);
  });

  it("payout fund/conclude: wanted while every input is live", async () => {
    const s = await open();
    const eventId = eventIdOf(0x53);
    const conclude = await s.record(
      "settlement",
      `settlement:withdrawal:${eventId.toString("hex")}:conclude`,
      s.spend([s.spare(0)]),
      eventId,
    );
    expect(await s.verdict(conclude)).toBe(true);
    // Another transaction spends the payout input.
    await s.driver.forward([s.spend([s.spare(0)], { nonce: s.nonce() })]);
    expect(await s.verdict(conclude)).toBe(false);
  });

  it("a family whose target state is not in the facts is held, never decided", async () => {
    const s = await open();
    for (const [i, family] of [
      "script_reward_registration",
      "phas_membership",
      "reference_funding",
    ].entries()) {
      const hash = await s.record(
        family,
        `${family}:test`,
        s.spend([s.spare(i)]),
        null,
      );
      await expect(s.verdict(hash)).rejects.toBeInstanceOf(
        FamilyPredicateUnavailable,
      );
    }
  });
});
