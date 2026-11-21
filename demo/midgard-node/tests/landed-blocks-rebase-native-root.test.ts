/**
 * The landed-block rebase is also due when the native MPF's durable root is
 * not the target's end root (the live journal's expected root, else the
 * processed tip's), on the node database with the modelled native MPF owner
 * of `landed-blocks-rebase.fixture.ts`:
 *
 * - a commit's journal is live from its insert, and the native MPF reaches
 *   its root only at the promotion after the commit worker, inside the
 *   commit's producer (`runAtFollowerView`). While that producer runs, the
 *   follower's due check leaves the drift alone: no recompute starts, the
 *   gate's epoch stays, and the promotion lands as it would;
 * - a producer that ended without promoting leaves the drift, and the next
 *   due check applies the live journal forward; nothing of it is undone;
 * - without a native owner the drift is never read;
 * - a deposit-only live journal that dies has no effect for the rebase to
 *   dispose of, yet the native MPF holds its root: the rebase restores the
 *   base's root.
 */
import { SqlClient } from "@effect/sql";
import { Effect, Option, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { rebasePlan } from "../src/landed-blocks/rebase-target.js";
import {
  readFollowerWriteGate,
  runAtFollowerView,
} from "../src/services/follower-write-gate.js";
import type { Globals } from "../src/services/globals.js";
import { testDriverRecompute } from "./helpers/driver-recompute.js";
import {
  insertOwnJournal,
  simCommitTxHash,
} from "./helpers/landed-blocks-sim.own.js";
import {
  attempt,
  BLOCK,
  E1,
  entry,
  expectRebased,
  freshNative,
  hex,
  type Native,
  processOf,
  R1,
  root,
  run,
  seed,
} from "./landed-blocks-rebase.fixture.js";

const OWN = "c1".repeat(28);
const R2 = root(0x12);
const DEPOSIT = entry("deposit", 5_000_000n);

/** The live journal of a deposit-only own block on the processed tip. */
const ownJournal = insertOwnJournal({
  headerHash: OWN,
  baseHeaderHash: BLOCK,
  baseUtxosRoot: R1,
  expectedUtxosRoot: R2,
  spent: [],
  produced: [DEPOSIT],
  txIds: [],
  at: new Date("2026-10-01T00:00:00.000Z"),
});

/** The processed foreign block, applied by a first rebase: the native root is R1. */
const landed = async (native: Native) => {
  const globals = await processOf(native);
  await seed(globals);
  expectRebased(await attempt(globals));
  expect(native.durableRoot).toBe(R1);
  // The own block's delta reaches its journal's root.
  native.reaches = R2;
  return globals;
};

const rebaseIfDue = Effect.flatMap(testDriverRecompute(), (recompute) =>
  recompute.rebaseIfDue("native root drift test"),
);

const liveHeader = PendingBlockFinalizationsDB.retrieveLive().pipe(
  Effect.map(
    Option.map((record) =>
      hex(record[PendingBlockFinalizationsDB.Columns.HEADER_HASH]),
    ),
  ),
);

/**
 * The commit's producer from its journal insert: the plan with the native
 * owner sees the drift, and the follower's due check runs meanwhile.
 * `promote` stands for the parent's promotion before the producer ends.
 */
const commitWindow = (globals: Globals, native: Native, promote: boolean) =>
  run(
    globals,
    runAtFollowerView(
      Effect.gen(function* () {
        yield* ownJournal;
        const owner = yield* Ref.get(globals.NATIVE_MPF_OWNER);
        const seen = yield* rebasePlan(owner);
        const before = yield* readFollowerWriteGate;
        const hold = yield* rebaseIfDue;
        const after = yield* readFollowerWriteGate;
        const root = native.durableRoot;
        if (promote) native.durableRoot = R2;
        return {
          drift: seen.kind === "ready" ? seen.drift : undefined,
          hold,
          root,
          epochKept: after.epoch === before.epoch,
          pending: after.pending,
        };
      }),
    ),
  );

describe("native MPF root drift", () => {
  it("a commit's producer between its journal insert and its promotion holds the drift", async () => {
    const native = freshNative();
    const globals = await landed(native);
    expect(await commitWindow(globals, native, true)).toEqual({
      drift: { durableRoot: R1, targetRoot: R2 },
      hold: undefined,
      root: R1,
      epochKept: true,
      pending: undefined,
    });
    // Promoted: nothing is due, and the journal stays live.
    const shown = await attempt(globals);
    expect(shown.hold).toBeUndefined();
    expect(shown.due).toBe("none");
    expect(native.durableRoot).toBe(R2);
    expect(await run(globals, liveHeader)).toEqual(Option.some(OWN));
  });

  it("a producer that ended without promoting leaves the drift, and the rebase applies the live journal forward", async () => {
    const native = freshNative();
    const globals = await landed(native);
    expect(await commitWindow(globals, native, false)).toMatchObject({
      hold: undefined,
      root: R1,
      epochKept: true,
    });
    const shown = await attempt(globals);
    expect(shown.hold).toBeUndefined();
    expect(shown.due).toBe("none");
    expect(native.durableRoot).toBe(R2);
    expect(await run(globals, liveHeader)).toEqual(Option.some(OWN));
  });

  it("without a native owner the drift is never read", async () => {
    const native = freshNative();
    const globals = await landed(native);
    await Effect.runPromise(Ref.set(globals.NATIVE_MPF_OWNER, undefined));
    await run(globals, runAtFollowerView(ownJournal));
    const before = await run(globals, readFollowerWriteGate);
    expect(await run(globals, rebaseIfDue)).toBeUndefined();
    expect(await run(globals, rebasePlan())).toEqual({ kind: "none" });
    expect((await run(globals, readFollowerWriteGate)).epoch).toBe(
      before.epoch,
    );
    expect(native.durableRoot).toBe(R1);
  });

  it("a deposit-only live journal that dies: the rebase returns the native root to the base", async () => {
    const native = freshNative();
    const globals = await landed(native);
    await commitWindow(globals, native, true);
    expect(native.durableRoot).toBe(R2);
    // S6 pruned the commit's intent: its journal is dead, with no effect
    // pending for the rebase to dispose of.
    await run(
      globals,
      Effect.flatMap(
        SqlClient.SqlClient,
        (sql) =>
          sql`DELETE FROM l1_intents WHERE tx_hash = ${simCommitTxHash(Buffer.from(OWN, "hex"))}`,
      ),
    );
    expect(await run(globals, rebasePlan())).toEqual({ kind: "none" });
    const shown = await attempt(globals);
    expect(shown.hold).toBeUndefined();
    expect(shown.due).toBe("none");
    expect(native.durableRoot).toBe(R1);
    expect(shown.working).toEqual([hex(E1.outref)]);
    expect(await run(globals, liveHeader)).toEqual(Option.none());
  });
});
