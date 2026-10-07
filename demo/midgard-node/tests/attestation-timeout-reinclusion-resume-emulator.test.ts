// Startup, multi-block and interrupted correction rewinds, split from
// attestation-timeout-reinclusion-emulator.test.ts so each file stays within
// the per-file budget. Every test opens its own scenario.
import "node:util";
import "@effect/sql";
import "effect";
import "vitest";
import "../src/database/pendingBlockFinalizations.js";
import "./helpers/correction-rewind-scenario.js";
import "./attestation-timeout-reinclusion-emulator.expect-rewound-and-recommitted.js";

import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  C,
  captureRemoved,
  expectOwedAndUnreincluded,
  expectRewoundAndRecommitted,
  expectUnobservedRemoval,
  failureText,
  MARKER_REFUSAL,
  nativeRoot,
  openRemovedTailOverRetainedBlock,
  openRemovedTwoBlockSuffix,
  restartAfter,
} from "./attestation-timeout-reinclusion-emulator.expect-rewound-and-recommitted.js";
import {
  closeLifecycle,
  commitNextBlock,
  type Lifecycle,
  openCorrectionRewindScenario,
  read,
  readDeposits,
  readJournal,
  readObserver,
  readRecoveryPlans,
  readSqlLedgerRoot,
} from "./helpers/correction-rewind-scenario.js";
import {
  assertClosedCorrectionGate,
  assertUnboundRemovedLocalRoot,
  assertUnretainedCorrectionRoot,
} from "./helpers/correction-rewind-source-gate.js";

it("does not rewind at startup for a removal accepted while the node was down but not yet final, and until then the next commit fails closed", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  let h: Pick<Lifecycle, "close"> = scenario.h;
  try {
    const removed = await captureRemoved(scenario.headers);
    const cursorQueue = (await readObserver()).cursorQueue;
    let removal: Awaited<ReturnType<typeof scenario.removeTail>> | undefined;
    const restarted = await restartAfter(scenario, async () => {
      removal = await scenario.removeTail(scenario.headers[0]!, {
        observe: false,
      });
    });
    h = restarted;
    await expectUnobservedRemoval(removed, restarted, cursorQueue);
    expect(
      (await scenario.tick(restarted.globals)).admittedTransactionHashes,
    ).toEqual([]);
    await scenario.nextSourceBlock(restarted);
    // The observer now tracks the removal as pending; nothing is admitted,
    // owed or rewound.
    expect((await readObserver()).admitted).toEqual([]);
    await expectUnobservedRemoval(removed, restarted);
    const later = await scenario.deposit(14_000_000n, restarted);
    await restarted.deployment.chain.awaitLedgerTime(later + 1000);
    await scenario.nextSourceBlock(restarted);
    const queueBefore = await scenario.readQueue();
    expect(await failureText(commitNextBlock(restarted))).toContain(
      MARKER_REFUSAL,
    );
    expect(await scenario.readQueue()).toEqual(queueBefore);
    expect(await readRecoveryPlans()).toEqual([]);
    expect(await nativeRoot(restarted)).toBe(removed[0]!.expected);
    // Once final, the same node rewinds and the same commit succeeds.
    await scenario.awaitRemovalFinality(restarted);
    expect(
      (await scenario.tick(restarted.globals)).admittedTransactionHashes,
    ).toEqual([removal!.accepted.transaction.txHash]);
    await scenario.nextSourceBlock(restarted);
    const rewoundNativeRoot = await nativeRoot(restarted);
    const rewoundSqlRoot = (await readSqlLedgerRoot()).root_hex;
    const next = await commitNextBlock(restarted);
    await expectRewoundAndRecommitted({
      scenario,
      removed,
      rewoundNativeRoot,
      rewoundSqlRoot,
      next,
      laterDeposits: 1,
    });
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);

it("rewinds a removed two-block suffix to the earliest block's base and commits both reincluded deposits in order", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 2 });
  const { h } = scenario;
  try {
    const removed = await captureRemoved(scenario.headers);
    expect(removed[1]!.base).toBe(removed[0]!.expected);
    expect(removed[0]!.base).not.toBe(removed[1]!.base);
    // A timed-out suffix is removed tail first.
    const second = await scenario.removeTail(scenario.headers[1]!);
    const first = await scenario.removeTail(scenario.headers[0]!);
    await scenario.awaitRemovalFinality();
    expect((await scenario.tick(h.globals)).admittedTransactionHashes).toEqual([
      second.accepted.transaction.txHash,
      first.accepted.transaction.txHash,
    ]);
    await scenario.nextSourceBlock();
    const rewoundNativeRoot = await nativeRoot(h);
    const rewoundSqlRoot = (await readSqlLedgerRoot()).root_hex;
    const next = await commitNextBlock(h);
    await expectRewoundAndRecommitted({
      scenario,
      removed,
      rewoundNativeRoot,
      rewoundSqlRoot,
      next,
    });
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);

it("resumes a two-block rewind interrupted after its plan was persisted and before the native root moved", async () => {
  const { scenario, removed } = await openRemovedTwoBlockSuffix();
  let h: Pick<Lifecycle, "close"> = scenario.h;
  try {
    const owner = await Effect.runPromise(
      Ref.get(scenario.h.globals.NATIVE_MPF_OWNER),
    );
    if (owner === undefined) throw new Error("Native owner is not open");
    // The process dies at the first native mutation.
    owner.restoreCanonicalRoot = () =>
      Promise.reject(new Error("injected crash before native restore"));
    expect(await failureText(scenario.nextSourceBlock())).toContain(
      "injected crash before native restore",
    );
    expect(await nativeRoot(scenario.h)).toBe(removed.at(-1)!.expected);
    await expectOwedAndUnreincluded(removed);
    await assertClosedCorrectionGate(scenario.h);
    // The restarted process resumes the persisted plan at startup.
    const restarted = await scenario.h.restartRuntime();
    h = restarted;
    const rewoundNativeRoot = await nativeRoot(restarted);
    const rewoundSqlRoot = (await readSqlLedgerRoot()).root_hex;
    const next = await commitNextBlock(restarted);
    await expectRewoundAndRecommitted({
      scenario,
      removed,
      rewoundNativeRoot,
      rewoundSqlRoot,
      next,
    });
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);

it("resumes a rewind to a retained non-empty base interrupted after the native root moved and before reinclusion committed", async () => {
  const { scenario, removed } = await openRemovedTailOverRetainedBlock();
  let h: Pick<Lifecycle, "close"> = scenario.h;
  const dropProbe = () =>
    read(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DROP TRIGGER IF EXISTS rewind_crash_probe
          ON pending_block_finalizations`;
        yield* sql`DROP FUNCTION IF EXISTS rewind_crash_probe()`;
      }),
    );
  try {
    // The SQL reinclusion transaction dies at its first journal abandonment,
    // after the native restore acknowledged.
    await read(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql.unsafe(`CREATE FUNCTION rewind_crash_probe() RETURNS trigger
          LANGUAGE plpgsql AS $$ BEGIN
            RAISE EXCEPTION 'injected crash between rewind and reinclusion';
          END $$`);
        yield* sql.unsafe(`CREATE TRIGGER rewind_crash_probe
          BEFORE UPDATE ON pending_block_finalizations FOR EACH ROW
          WHEN (NEW.status = 'abandoned' AND OLD.status <> 'abandoned')
          EXECUTE FUNCTION rewind_crash_probe()`);
      }),
    );
    expect(await failureText(scenario.nextSourceBlock())).toContain(
      "injected crash between rewind and reinclusion",
    );
    // Native rewound; journal, deposit and SQL marker untouched; plan retained.
    const target = removed[0]!.base;
    expect(await nativeRoot(scenario.h)).toBe(target);
    expect((await readSqlLedgerRoot()).root_hex).not.toBe(target);
    await expectOwedAndUnreincluded(removed);
    await dropProbe();
    const restarted = await scenario.h.restartRuntime();
    h = restarted;
    expect(await nativeRoot(restarted)).toBe(target);
    const engine = await readSqlLedgerRoot();
    expect(engine.root_hex).toBe(target);
    // The replay base's own payload aggregate, from the retained journal.
    expect(engine.utxo_payload_entry_count).not.toBeNull();
    const journal = await readJournal(removed[0]!.headerHash);
    expect(journal[C.STATUS]).toBe(Pending.Status.Abandoned);
    expect(journal[C.CORRECTION_TRANSITION_DIGEST]).toBe(
      (await readObserver()).admitted[0]!.transitionDigest,
    );
    const plans = await readRecoveryPlans();
    expect(plans.map(({ state }) => state)).toEqual(["applied"]);
    expect((await readDeposits()).at(-1)).toEqual({
      status: "projected",
      projectedHeader: null,
    });
    // The retained predecessor is unattested and, in this profile, timed out
    // before its successor could be removed: the node correctly refuses to
    // commit on it until it is removed too.
    expect(await failureText(commitNextBlock(restarted))).toContain(
      "Commit paused until expired unattested suffix is corrected",
    );
  } finally {
    try {
      await dropProbe();
    } finally {
      await closeLifecycle(h);
    }
  }
}, 600_000);

it(
  "fails closed with a specific error when the rewind base is not retained, and never moves the marker or reincludes",
  assertUnretainedCorrectionRoot,
  600_000,
);

it(
  "refuses a removed native root whose local journal parent is unbound and leaves native and SQL unchanged",
  assertUnboundRemovedLocalRoot,
  600_000,
);
