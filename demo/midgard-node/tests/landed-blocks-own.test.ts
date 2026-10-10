/**
 * This node's own landed blocks (plan §7.3, §15 N3), processed against stub
 * ports on the node database: an own block is adopted from its journal and
 * never replayed, exactly once across runs; one whose journal is abandoned
 * is adopted unapplied for the rebase to revive, and the blocks after it
 * wait for its local finalization; a journal that does not describe the
 * landed block, or whose delta misses its header's root, holds the local
 * fault by name and records nothing; a merged own block folds
 * into `confirmed_ledger` once its journal is locally applied (N5), once,
 * whether the merge fiber folded it first or not; a foreign block after it
 * replays on the journal's
 * post-state, and one that misses its header's root is never adopted.
 * The holds by retry class: the waits (DA peers, the follower's facts, a
 * revival) are retried on the driver's backoff; a verdict on the view and a
 * replay failure not known to be transient are `notRetried`, re-read on the
 * next follower change.
 */
import "./utils.js";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { isRetriedHold, notRetried } from "../src/l1-events/driver.js";
import {
  foldMerge,
  retrieveMergeLinks,
} from "../src/landed-blocks/confirmed-merges.js";
import {
  combineHolds,
  CONFIRMED_LEDGER_OWN_BLOCK_PENDING,
  LANDED_BLOCK_AWAITING_DA,
  LANDED_BLOCK_DA_REFETCH_PENDING,
  LANDED_BLOCK_EVENT_UNKNOWN,
  LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING,
  LANDED_BLOCK_FORCED_ORDER_PENDING,
  LANDED_BLOCK_INVALID,
  LANDED_BLOCK_OWN_JOURNAL_MISMATCH,
  LANDED_BLOCK_OWN_REVIVAL_PENDING,
  LANDED_BLOCK_REBASE_PENDING,
  LANDED_BLOCK_REPLAY_FAILED,
  LANDED_BLOCK_REPLAY_INCOMPLETE,
  LANDED_BLOCKS_WAITING,
} from "../src/landed-blocks/holds.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import { blockedRebaseHold } from "../src/landed-blocks/rebase-target.js";
import type { ReplayOutcome } from "../src/landed-blocks/replay.js";
import { Frontier, retrieveRows } from "../src/landed-blocks/store.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { hex32 } from "./helpers/state-queue-sim.fixtures.js";
import {
  A1,
  AFTER_A,
  type Block,
  confirmedAt,
  confirmedKeys,
  fixture,
  G0,
  GENESIS,
  harness,
  hex,
  inNode,
  ports,
  queueOf,
  sortedKeys,
} from "./landed-blocks-own.fixture.js";

describe("own landed blocks", () => {
  it("adopts an own block from its journal exactly once, never replaying it, and replays the next foreign block on its post-state", async () => {
    const { a, f, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    await inNode(
      Effect.gen(function* () {
        const process = (nodes: readonly Block[]) =>
          processLandedQueue(ports(state), queueOf(genesisState, nodes));
        for (let run = 0; run < 3; run++) {
          expect(yield* process([a])).toBeUndefined();
          const rows = yield* retrieveRows;
          expect(rows).toHaveLength(1);
          expect(rows[0]).toMatchObject({
            headerHash: a.hash,
            kind: "own",
            state: "processed",
            applied: true,
            parentHeaderHash: SDK.GENESIS_HEADER_HASH,
            utxosRoot: a.header.utxosRoot,
          });
          expect(rows[0]!.spent.map(hex)).toEqual([hex(G0.outref)]);
          expect(sortedKeys(rows[0]!.produced)).toEqual(sortedKeys([A1]));
        }
        expect(state.replays).toHaveLength(0);
        expect(state.rebaseRequests).toBe(0);
        expect(yield* confirmedKeys).toEqual(sortedKeys(GENESIS));

        const held = yield* process([a, f]);
        expect(held?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
        expect(state.replays.map((input) => input.headerHash)).toEqual([
          f.hash,
        ]);
        expect(sortedKeys(state.replays[0]!.parentEntries)).toEqual(
          sortedKeys(AFTER_A),
        );
        const rows = yield* retrieveRows;
        expect(
          rows.map((row) => [row.headerHash, row.kind, row.applied]),
        ).toEqual([
          [a.hash, "own", true],
          [f.hash, "foreign", false],
        ]);
        // Processed once: later runs neither replay nor re-record either block.
        yield* process([a, f]);
        expect(state.replays).toHaveLength(1);
        expect(yield* retrieveRows).toHaveLength(2);
      }),
    );
  }, 120_000);

  it("writes nothing when the follower left the run's view before its write", async () => {
    const { a, f, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    await inNode(
      Effect.gen(function* () {
        const process = (nodes: readonly Block[]) =>
          processLandedQueue(ports(state), queueOf(genesisState, nodes));
        // The bootstrap write.
        state.viewHeld = false;
        expect(yield* process([a])).toEqual({
          reason: LANDED_BLOCKS_WAITING,
          detail: "view moved",
        });
        expect(yield* Frontier.retrieve).toBeUndefined();
        expect(yield* retrieveRows).toHaveLength(0);
        state.viewHeld = true;
        expect(yield* process([a])).toBeUndefined();
        expect(yield* retrieveRows).toHaveLength(1);
        // A processed block's write.
        state.viewHeld = false;
        expect(yield* process([a, f])).toEqual({
          reason: LANDED_BLOCKS_WAITING,
          detail: "view moved",
        });
        expect((yield* retrieveRows).map((row) => row.headerHash)).toEqual([
          a.hash,
        ]);
        expect(state.rebaseRequests).toBe(0);
        state.viewHeld = true;
        expect((yield* process([a, f]))?.reason).toBe(
          LANDED_BLOCK_REBASE_PENDING,
        );
        expect((yield* retrieveRows).map((row) => row.headerHash)).toEqual([
          a.hash,
          f.hash,
        ]);
      }),
    );
  }, 120_000);

  it("adopts an own block whose journal is abandoned unapplied for the rebase to revive, and holds the blocks after it until its revival is finalized", async () => {
    const { a, f, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, { ...journalA, status: "abandoned" });
    await inNode(
      Effect.gen(function* () {
        const process = (nodes: readonly Block[]) =>
          processLandedQueue(ports(state), queueOf(genesisState, nodes));
        const held = yield* process([a, f]);
        expect(held?.reason).toBe(LANDED_BLOCK_OWN_REVIVAL_PENDING);
        // A wait on this node's own commit path: retried on the backoff.
        expect(isRetriedHold(held!)).toBe(true);
        expect(held?.detail).toContain(a.hash);
        expect(held?.detail).toContain(`also ${LANDED_BLOCK_REBASE_PENDING}:`);
        const rows = yield* retrieveRows;
        expect(rows).toHaveLength(1);
        expect(rows[0]).toMatchObject({
          headerHash: a.hash,
          kind: "own",
          state: "processed",
          applied: false,
        });
        expect(state.replays).toHaveLength(0);
        expect(state.rebaseRequests).toBe(1);
        // Revived by the rebase, not yet finalized locally: still held.
        state.journals.set(a.hash, { ...journalA, revived: true });
        expect((yield* process([a, f]))?.reason).toBe(
          LANDED_BLOCK_OWN_REVIVAL_PENDING,
        );
        expect(state.replays).toHaveLength(0);
        // Finalized locally: the next block is processed.
        state.journals.set(a.hash, { ...journalA, status: "locally_applied" });
        expect((yield* process([a, f]))?.reason).toBe(
          LANDED_BLOCK_REBASE_PENDING,
        );
        expect(state.replays.map((input) => input.headerHash)).toEqual([
          f.hash,
        ]);
        expect(yield* retrieveRows).toHaveLength(2);
      }),
    );
  }, 120_000);

  it("never adopts an own block whose journal does not describe the landed block, holding a local fault", async () => {
    const { a, f, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, {
      ...journalA,
      expectedUtxosRoot: f.header.utxosRoot,
    });
    await inNode(
      Effect.gen(function* () {
        const held = yield* processLandedQueue(
          ports(state),
          queueOf(genesisState, [a, f]),
        );
        expect(held?.reason).toBe(LANDED_BLOCK_OWN_JOURNAL_MISMATCH);
        expect(isRetriedHold(held!)).toBe(false);
        expect(held?.detail).toContain(a.hash);
        expect(yield* retrieveRows).toHaveLength(0);
        expect(state.replays).toHaveLength(0);
      }),
    );
  }, 120_000);

  it("recomputes an own block's root from its journal delta: a delta that misses the header's root holds a local fault, never landed_block_invalid", async () => {
    const { a, f, genesisState, journalA } = await fixture();
    const state = harness();
    // The journal names the right base and root, but its delta keeps G0.
    state.journals.set(a.hash, { ...journalA, spent: [] });
    await inNode(
      Effect.gen(function* () {
        const held = yield* processLandedQueue(
          ports(state),
          queueOf(genesisState, [a, f]),
        );
        expect(held?.reason).toBe(LANDED_BLOCK_OWN_JOURNAL_MISMATCH);
        expect(held?.detail).toContain(
          `its header commits ${a.header.utxosRoot}`,
        );
        expect(held?.detail).not.toContain(LANDED_BLOCK_INVALID);
        expect(yield* retrieveRows).toHaveLength(0);
        expect(state.replays).toHaveLength(0);
        // The journal's delta reaches the root: adopted, as in the first case.
        state.journals.set(a.hash, journalA);
        yield* processLandedQueue(ports(state), queueOf(genesisState, [a]));
        expect((yield* retrieveRows).map((row) => row.headerHash)).toEqual([
          a.hash,
        ]);
      }),
    );
  }, 120_000);

  it("never adopts a foreign block that replays to another root than its header's", async () => {
    const { a, f, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    state.replayRoot = hex32(7);
    await inNode(
      Effect.gen(function* () {
        const held = yield* processLandedQueue(
          ports(state),
          queueOf(genesisState, [a, f]),
        );
        expect(held?.reason).toBe(LANDED_BLOCK_INVALID);
        // A verdict: no timer replays it again at the same view.
        expect(isRetriedHold(held!)).toBe(false);
        expect(held?.detail).toContain(f.hash);
        expect(held?.detail).toContain(hex32(7));
        expect((yield* retrieveRows).map((row) => row.headerHash)).toEqual([
          a.hash,
        ]);
        expect(state.rebaseRequests).toBe(0);
      }),
    );
  }, 120_000);

  it("retries the waits a foreign block's replay ends in and not its verdicts or unclassified failures", async () => {
    const { a, f, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    const refused = Object.assign(new Error("connect ECONNREFUSED"), {
      code: "ECONNREFUSED",
    });
    const cases: readonly (readonly [
      Effect.Effect<ReplayOutcome, unknown>,
      string,
      boolean,
    ])[] = [
      [
        Effect.succeed({ kind: "missing", detail: "x" }),
        LANDED_BLOCK_AWAITING_DA,
        true,
      ],
      [
        Effect.succeed({ kind: "da_refetch_pending", detail: "x" }),
        LANDED_BLOCK_DA_REFETCH_PENDING,
        true,
      ],
      [
        Effect.succeed({ kind: "event_unknown", detail: "x" }),
        LANDED_BLOCK_EVENT_UNKNOWN,
        true,
      ],
      [
        Effect.succeed({ kind: "forced_order_pending", detail: "x" }),
        LANDED_BLOCK_FORCED_ORDER_PENDING,
        true,
      ],
      [
        Effect.succeed({ kind: "incomplete", detail: "x" }),
        LANDED_BLOCK_REPLAY_INCOMPLETE,
        false,
      ],
      [
        Effect.succeed({ kind: "invalid", detail: "x" }),
        LANDED_BLOCK_INVALID,
        false,
      ],
      [Effect.fail(refused), LANDED_BLOCK_REPLAY_FAILED, true],
      [Effect.fail(new Error("boom")), LANDED_BLOCK_REPLAY_FAILED, false],
    ];
    await inNode(
      Effect.gen(function* () {
        for (const [ends, reason, retried] of cases) {
          state.replayEnds = ends;
          const held = yield* processLandedQueue(
            ports(state),
            queueOf(genesisState, [a, f]),
          );
          expect([held?.reason, isRetriedHold(held!)]).toEqual([
            reason,
            retried,
          ]);
        }
        expect(state.replays.map((input) => input.headerHash)).toEqual(
          cases.map(() => f.hash),
        );
      }),
    );
  }, 120_000);

  it("retries a rebase blocked on landed processing, and not one blocked on a missing schema", () => {
    const pending = blockedRebaseHold({ kind: "blocked", detail: "d" });
    expect([pending.reason, isRetriedHold(pending)]).toEqual([
      LANDED_BLOCK_REBASE_PENDING,
      true,
    ]);
    const schema = blockedRebaseHold({
      kind: "blocked",
      detail: "d",
      reason: LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING,
    });
    expect([schema.reason, isRetriedHold(schema)]).toEqual([
      LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING,
      false,
    ]);
  });

  it("retries a combined hold when any hold in it is retried, and not when none is", () => {
    const wait = { reason: LANDED_BLOCK_REBASE_PENDING, detail: "w" };
    const verdict = () =>
      notRetried({ reason: LANDED_BLOCK_INVALID, detail: "v" });
    expect(isRetriedHold(combineHolds([verdict(), wait])!)).toBe(true);
    expect(isRetriedHold(combineHolds([verdict(), verdict()])!)).toBe(false);
    expect(isRetriedHold(combineHolds([verdict()])!)).toBe(false);
  });

  it("folds a merged own block into confirmed_ledger once its journal is locally applied", async () => {
    const { a, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    const merged = queueOf(confirmedAt(a, ""), []);
    await inNode(
      Effect.gen(function* () {
        yield* processLandedQueue(ports(state), queueOf(genesisState, [a]));
        // Merged, but not locally applied yet: pending, by name.
        const pending = yield* processLandedQueue(ports(state), merged);
        expect(pending?.reason).toBe(CONFIRMED_LEDGER_OWN_BLOCK_PENDING);
        expect(pending?.detail).toContain(a.hash);
        expect((yield* Frontier.retrieve)?.headerHash).toBe(
          SDK.GENESIS_HEADER_HASH,
        );
        expect(yield* confirmedKeys).toEqual(sortedKeys(GENESIS));
        expect((yield* retrieveMergeLinks).size).toBe(0);

        state.journals.set(a.hash, { ...journalA, status: "locally_applied" });
        expect(yield* processLandedQueue(ports(state), merged)).toBeUndefined();
        expect(yield* Frontier.retrieve).toEqual({
          headerHash: a.hash,
          utxosRoot: a.header.utxosRoot,
        });
        expect(yield* confirmedKeys).toEqual(sortedKeys(AFTER_A));
        expect(yield* retrieveRows).toHaveLength(0);
        expect(state.replays).toHaveLength(0);
        // The fold is retained, with the merge that made `a` the root.
        expect((yield* retrieveMergeLinks).get(a.hash)?.merge).toEqual({
          slot: 0,
          outRef: merged.root!.outRef,
        });
      }),
    );
  }, 120_000);

  it("passes a merged own block the merge fiber already folded without folding it again", async () => {
    const { a, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, { ...journalA, status: "locally_applied" });
    const merged = queueOf(confirmedAt(a, ""), []);
    await inNode(
      Effect.gen(function* () {
        yield* processLandedQueue(ports(state), queueOf(genesisState, [a]));
        // The merge fiber folded it first, through the same fold, with no
        // merge point (it reads no queue history).
        const [row] = yield* retrieveRows;
        yield* withFollowerWrite(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            yield* sql.withTransaction(foldMerge(row!, null));
          }),
        );
        expect((yield* retrieveMergeLinks).get(a.hash)?.merge).toBeNull();
        expect(yield* processLandedQueue(ports(state), merged)).toBeUndefined();
        expect((yield* Frontier.retrieve)?.headerHash).toBe(a.hash);
        expect(yield* confirmedKeys).toEqual(sortedKeys(AFTER_A));
        // Landed processing fills in its merge point.
        expect((yield* retrieveMergeLinks).get(a.hash)?.merge).toEqual({
          slot: 0,
          outRef: merged.root!.outRef,
        });
      }),
    );
  }, 120_000);
});
