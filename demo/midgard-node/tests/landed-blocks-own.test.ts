/**
 * This node's own landed blocks (plan §7.3, §15 N3), processed against stub
 * ports on the node database: an own block is adopted from its journal and
 * never replayed, exactly once across runs, unapplied, as a foreign block
 * is, and the rebase is asked to apply it (the rebase applies own and
 * foreign blocks alike; an abandoned journal's block is revived by that
 * rebase, with nothing held after it); a journal that does not describe the
 * landed block, or whose delta misses its header's root, holds the local
 * fault by name and records nothing; a merged own block folds into
 * `confirmed_ledger` once the rebase applied it (N5), once, whether the
 * merge fiber folded it first or not, and waits for that rebase until then;
 * a foreign block after it replays on the journal's post-state, and one
 * that misses its header's root is never adopted.
 * The holds by retry class: the waits (DA peers, the follower's facts) and
 * a foreign import's local failures are retried on the driver's backoff,
 * the local failures with the `landed_block_local_fault` alarm; a verdict
 * on the view and a replay failure not known to be transient are
 * `notRetried`, re-read on the next follower change; a follower store whose
 * origin is too late for the blocks after the landed frontier is fatal.
 */

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Metric } from "effect";
import { describe, expect, it } from "vitest";

import {
  isFatalHold,
  isRetriedHold,
  notRetried,
} from "../src/l1-events/driver.js";
import {
  foldMerge,
  retrieveMergeLinks,
} from "../src/landed-blocks/confirmed-merges.js";
import {
  combineHolds,
  LANDED_BLOCK_AWAITING_DA,
  LANDED_BLOCK_DA_REFETCH_PENDING,
  LANDED_BLOCK_EVENT_UNKNOWN,
  LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING,
  LANDED_BLOCK_FORCED_ORDER_PENDING,
  LANDED_BLOCK_INVALID,
  LANDED_BLOCK_LOCAL_FAULT,
  LANDED_BLOCK_ORIGIN_AFTER_FRONTIER,
  LANDED_BLOCK_OWN_JOURNAL_MISMATCH,
  LANDED_BLOCK_REBASE_PENDING,
  LANDED_BLOCK_REPLAY_FAILED,
  LANDED_BLOCK_REPLAY_INCOMPLETE,
  LANDED_BLOCKS_WAITING,
} from "../src/landed-blocks/holds.js";
import { processedChain } from "../src/landed-blocks/ledger.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import { blockedRebaseHold } from "../src/landed-blocks/rebase-target.js";
import type { ReplayOutcome } from "../src/landed-blocks/replay.js";
import { landedBlockLocalFaultCounter } from "../src/landed-blocks/replay-outcome-hold.js";
import {
  Frontier,
  markApplied,
  retrieveRows,
} from "../src/landed-blocks/store.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { simDigest } from "./helpers/landed-blocks-sim.universe.js";
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

/**
 * The processed rows in landed order: the chain from genesis by parent
 * links, as processing reads it (`processed_at` is a wall-clock stamp, not
 * an order).
 */
const landedRows = Effect.map(retrieveRows, (rows) =>
  processedChain(rows, SDK.GENESIS_HEADER_HASH),
);

describe("own landed blocks", () => {
  it("adopts an own block from its journal unapplied exactly once, never replaying it, asks the rebase to apply it, and replays the next foreign block on its post-state", async () => {
    const { a, f, genesisState, journalA } = await fixture();
    const state = harness();
    const withdrawal = {
      id: simDigest("own:withdrawal-a").toString("hex"),
      validity: "WithdrawalIsValid",
      detail: null,
      settlement: "00",
    };
    state.journals.set(a.hash, { ...journalA, withdrawals: [withdrawal] });
    await inNode(
      Effect.gen(function* () {
        const process = (nodes: readonly Block[]) =>
          processLandedQueue(ports(state), queueOf(genesisState, nodes));
        for (let run = 1; run <= 3; run++) {
          // The rebase applies it, as it does a foreign block: until it
          // ran, the run holds for it.
          expect((yield* process([a]))?.reason).toBe(
            LANDED_BLOCK_REBASE_PENDING,
          );
          expect(state.rebaseRequests).toBe(run);
          const rows = yield* retrieveRows;
          expect(rows).toHaveLength(1);
          expect(rows[0]).toMatchObject({
            headerHash: a.hash,
            kind: "own",
            state: "processed",
            applied: false,
            parentHeaderHash: SDK.GENESIS_HEADER_HASH,
            utxosRoot: a.header.utxosRoot,
            withdrawals: [withdrawal],
          });
          expect(rows[0]!.spent.map(hex)).toEqual([hex(G0.outref)]);
          expect(sortedKeys(rows[0]!.produced)).toEqual(sortedKeys([A1]));
        }
        expect(state.replays).toHaveLength(0);
        expect(yield* confirmedKeys).toEqual(sortedKeys(GENESIS));

        const held = yield* process([a, f]);
        expect(held?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
        expect(state.replays.map((input) => input.headerHash)).toEqual([
          f.hash,
        ]);
        expect(sortedKeys(state.replays[0]!.parentEntries)).toEqual(
          sortedKeys(AFTER_A),
        );
        const rows = yield* landedRows;
        expect(
          rows.map((row) => [row.headerHash, row.kind, row.applied]),
        ).toEqual([
          [a.hash, "own", false],
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
        expect((yield* process([a]))?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
        expect(yield* retrieveRows).toHaveLength(1);
        expect(state.rebaseRequests).toBe(1);
        // A processed block's write.
        state.viewHeld = false;
        expect(yield* process([a, f])).toEqual({
          reason: LANDED_BLOCKS_WAITING,
          detail: "view moved",
        });
        expect((yield* retrieveRows).map((row) => row.headerHash)).toEqual([
          a.hash,
        ]);
        expect(state.rebaseRequests).toBe(1);
        state.viewHeld = true;
        expect((yield* process([a, f]))?.reason).toBe(
          LANDED_BLOCK_REBASE_PENDING,
        );
        expect((yield* landedRows).map((row) => row.headerHash)).toEqual([
          a.hash,
          f.hash,
        ]);
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
        const waiting = yield* processLandedQueue(
          ports(state),
          queueOf(genesisState, [a, f]),
        );
        expect(waiting?.reason).toBe(LANDED_BLOCK_INVALID);
        // The own block before it still waits for the rebase, which the
        // driver retries.
        expect(waiting?.detail).toContain(LANDED_BLOCK_REBASE_PENDING);
        expect(isRetriedHold(waiting!)).toBe(true);
        expect(state.rebaseRequests).toBe(1);
        // What the rebase marks once it applied `a` (the fixture's never runs).
        yield* markApplied([a.hash]);
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
      }),
    );
  }, 120_000);

  it("retries the waits and local failures a foreign block's replay ends in and not its verdicts or unclassified failures", async () => {
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
      // The `landed_block_local_fault` alarm's kind, when it is raised.
      ("local_fault" | "incomplete")?,
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
        Effect.succeed({ kind: "local_fault", detail: "x" }),
        LANDED_BLOCK_LOCAL_FAULT,
        true,
        "local_fault",
      ],
      [
        Effect.succeed({ kind: "incomplete", detail: "x" }),
        LANDED_BLOCK_REPLAY_INCOMPLETE,
        true,
        "incomplete",
      ],
      [
        Effect.succeed({ kind: "invalid", detail: "x" }),
        LANDED_BLOCK_INVALID,
        false,
      ],
      [
        Effect.succeed({ kind: "origin_after_frontier", detail: "x" }),
        LANDED_BLOCK_ORIGIN_AFTER_FRONTIER,
        false,
      ],
      [Effect.fail(refused), LANDED_BLOCK_REPLAY_FAILED, true],
      [Effect.fail(new Error("boom")), LANDED_BLOCK_REPLAY_FAILED, false],
    ];
    await inNode(
      Effect.gen(function* () {
        const alarms = (kind: "local_fault" | "incomplete") =>
          Metric.value(
            Metric.tagged(landedBlockLocalFaultCounter, "kind", kind),
          ).pipe(Effect.map(({ count }) => count));
        // `a` adopted and applied, as the rebase leaves it: what holds is
        // the foreign block's replay alone.
        yield* processLandedQueue(ports(state), queueOf(genesisState, [a]));
        yield* markApplied([a.hash]);
        for (const [ends, reason, retried, alarm] of cases) {
          state.replayEnds = ends;
          const before = {
            local_fault: yield* alarms("local_fault"),
            incomplete: yield* alarms("incomplete"),
          };
          const held = yield* processLandedQueue(
            ports(state),
            queueOf(genesisState, [a, f]),
          );
          expect([held?.reason, isRetriedHold(held!)]).toEqual([
            reason,
            retried,
          ]);
          // Only a late-origin store stops the node at once.
          expect(isFatalHold(held!)).toBe(
            reason === LANDED_BLOCK_ORIGIN_AFTER_FRONTIER,
          );
          expect({
            local_fault: (yield* alarms("local_fault")) - before.local_fault,
            incomplete: (yield* alarms("incomplete")) - before.incomplete,
          }).toEqual({
            local_fault: alarm === "local_fault" ? 1n : 0n,
            incomplete: alarm === "incomplete" ? 1n : 0n,
          });
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

  it("folds a merged own block into confirmed_ledger once the rebase applied it, and not before", async () => {
    const { a, genesisState, journalA } = await fixture();
    const state = harness();
    state.journals.set(a.hash, journalA);
    const merged = queueOf(confirmedAt(a, ""), []);
    await inNode(
      Effect.gen(function* () {
        yield* processLandedQueue(ports(state), queueOf(genesisState, [a]));
        // Merged, but not applied by the rebase yet: the fold waits for it.
        const pending = yield* processLandedQueue(ports(state), merged);
        expect(pending?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
        expect((yield* Frontier.retrieve)?.headerHash).toBe(
          SDK.GENESIS_HEADER_HASH,
        );
        expect(yield* confirmedKeys).toEqual(sortedKeys(GENESIS));
        expect((yield* retrieveMergeLinks).size).toBe(0);

        // What the rebase marks once it applied `a` (the fixture's never runs).
        yield* markApplied([a.hash]);
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
    state.journals.set(a.hash, journalA);
    const merged = queueOf(confirmedAt(a, ""), []);
    await inNode(
      Effect.gen(function* () {
        yield* processLandedQueue(ports(state), queueOf(genesisState, [a]));
        yield* markApplied([a.hash]);
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
