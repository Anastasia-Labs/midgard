import "node:util";
import "@effect/sql";
import "effect";
import "vitest";
import "../src/database/pendingBlockFinalizations.js";
import "../src/fibers/speculative-commit-builder.js";
import "./helpers/correction-rewind-scenario.js";
import "./helpers/speculative-ready-candidate.js";
import "./attestation-timeout-reinclusion-emulator.expect-rewound-and-recommitted.js";

import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { shutdownSpeculativeCommitSession } from "../src/fibers/speculative-commit-builder.js";
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
  type Scenario,
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
  readObserverRow,
  readRecoveryPlans,
  readSqlLedgerRoot,
  restoreObserverRow,
} from "./helpers/correction-rewind-scenario.js";
import {
  assertClosedCorrectionGate,
  assertUnboundRemovedLocalRoot,
  assertUnretainedCorrectionRoot,
} from "./helpers/correction-rewind-source-gate.js";
import {
  expectInvalidatedByT1,
  installReadyCandidate,
} from "./helpers/speculative-ready-candidate.js";

it("rewinds the native ledger and commits a removed block's reincluded deposit on the correct base in the same process", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  const { h } = scenario;
  try {
    const removed = await captureRemoved(scenario.headers);
    const removal = await scenario.removeTail(scenario.headers[0]!);
    await scenario.awaitRemovalFinality();
    expect((await scenario.tick(h.globals)).admittedTransactionHashes).toEqual([
      removal.accepted.transaction.txHash,
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
    await h.synchronize();
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);

it("with speculation on, a correction rewind invalidates a Ready candidate (T1) and releases its fork before the owner rewinds, then ends where the flag-off rewind ends", async () => {
  const previous = process.env.SPECULATIVE_COMMIT_BUILD;
  process.env.SPECULATIVE_COMMIT_BUILD = "true";
  let scenario: Scenario | undefined;
  try {
    scenario = await openCorrectionRewindScenario({ blocks: 1 });
    const { h } = scenario;
    expect(h.production.nodeConfig.SPECULATIVE_COMMIT_BUILD).toBe(true);
    const removed = await captureRemoved(scenario.headers);
    const owner = await Effect.runPromise(Ref.get(h.globals.NATIVE_MPF_OWNER));
    if (owner === undefined) throw new Error("Native owner is not open");
    // The candidate is built on the removed block: its fork's base is the
    // removed block's root, which is the owner's durable root until the
    // rewind moves it.
    const removedRoot = removed[0]!.expected;
    expect((await owner.diagnostics()).durableRoot).toBe(removedRoot);
    const installed = await installReadyCandidate({
      globals: h.globals,
      owner,
      baseHeaderHash: removed[0]!.headerHash,
      nowMs: h.fixture.emulator.now(),
      maxAttempts: h.production.nodeConfig.SPECULATIVE_REBUILD_MAX_ATTEMPTS,
      observe: async () => (await owner.diagnostics()).durableRoot,
    });
    expect(installed.root).toBe(removedRoot);

    const removal = await scenario.removeTail(scenario.headers[0]!);
    await scenario.awaitRemovalFinality();
    expect((await scenario.tick(h.globals)).admittedTransactionHashes).toEqual([
      removal.accepted.transaction.txHash,
    ]);
    // Not before the rewind is owed and prepared.
    expect(installed.worker.instructions).toEqual([]);
    await scenario.nextSourceBlock();

    // T1 reached the worker, the session is gone, and the candidate's fork
    // was released while the owner still held the removed block's root.
    expectInvalidatedByT1({ ...installed, globals: h.globals });
    expect(installed.released.observed).toBe(removedRoot);

    // Parity with the flag-off rewind above.
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
    await h.synchronize();
  } finally {
    try {
      await Effect.runPromise(shutdownSpeculativeCommitSession());
      if (scenario !== undefined) await closeLifecycle(scenario.h);
    } finally {
      if (previous === undefined) delete process.env.SPECULATIVE_COMMIT_BUILD;
      else process.env.SPECULATIVE_COMMIT_BUILD = previous;
    }
  }
}, 600_000);

it("does not rewind before the removal is final, and until then the next commit fails closed instead of committing on the removed block's root", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  const { h } = scenario;
  try {
    const removed = await captureRemoved(scenario.headers);
    const removal = await scenario.removeTail(scenario.headers[0]!);
    // Depth 1 < release depth: nothing is admitted, owed or rewound, even
    // across forward appends.
    expect((await scenario.tick(h.globals)).admittedTransactionHashes).toEqual(
      [],
    );
    await scenario.nextSourceBlock();
    expect(await nativeRoot(h)).toBe(removed[0]!.expected);
    expect(await readRecoveryPlans()).toEqual([]);
    expect((await readJournal(removed[0]!.headerHash))[C.STATUS]).toBe(
      Pending.Status.Finalized,
    );
    // A new deposit gives the next block content. Without an admitted
    // removal nothing is owed or rewound.
    const later = await scenario.deposit(14_000_000n);
    await h.deployment.chain.awaitLedgerTime(later + 1000);
    await scenario.nextSourceBlock();
    expect(await nativeRoot(h)).toBe(removed[0]!.expected);
    expect(await readRecoveryPlans()).toEqual([]);
    const queueBefore = await scenario.readQueue();
    // The native root still holds the removed block's outputs, which no
    // canonical block carries: the commit is refused, nothing is submitted.
    expect(await failureText(commitNextBlock(h))).toContain(MARKER_REFUSAL);
    expect(await scenario.readQueue()).toEqual(queueBefore);
    expect(await readRecoveryPlans()).toEqual([]);
    expect(await nativeRoot(h)).toBe(removed[0]!.expected);
    expect((await readDeposits())[0]).toEqual({
      status: "projected",
      projectedHeader: removed[0]!.headerHash,
    });
    // Once final, the rewind happens and the same commit succeeds.
    await scenario.awaitRemovalFinality();
    expect((await scenario.tick(h.globals)).admittedTransactionHashes).toEqual([
      removal.accepted.transaction.txHash,
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
      laterDeposits: 1,
    });
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);

it("rewinds after a restart that preceded local reconciliation (the live state), and replays an unsaved observer idempotently", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  let h: Pick<Lifecycle, "close"> = scenario.h;
  try {
    const removed = await captureRemoved(scenario.headers);
    const removal = await scenario.removeTail(scenario.headers[0]!);
    await scenario.awaitRemovalFinality();
    const observerBefore = await readObserverRow();
    // Removal final on L1; journal finalized; observer cursor pre-removal;
    // native root at the removed block's root. The process restarts.
    const restarted = await scenario.h.restartRuntime();
    h = restarted;
    expect(await nativeRoot(restarted)).toBe(removed[0]!.expected);
    const admitted = await scenario.tick(restarted.globals);
    expect(admitted.admittedTransactionHashes).toEqual([
      removal.accepted.transaction.txHash,
    ]);
    const saved = await readObserverRow();
    // Crash before the observer save: the replayed tick admits the same
    // transition and saves the same state.
    await restoreObserverRow(observerBefore);
    expect(
      (await scenario.tick(restarted.globals)).admittedTransactionHashes,
    ).toEqual([removal.accepted.transaction.txHash]);
    expect(await readObserverRow()).toEqual(saved);
    await scenario.nextSourceBlock(restarted);
    const rewoundNativeRoot = await nativeRoot(restarted);
    const rewoundSqlRoot = (await readSqlLedgerRoot()).root_hex;
    // An observer replay after the rewind (at a greater depth, so with a new
    // transition digest) re-admits the removal but owes nothing: the removed
    // journal keeps the digest it was abandoned under, and no second rewind
    // is planned.
    const observerAtRewind = await readObserver();
    const abandonedUnder = (await readJournal(removed[0]!.headerHash))[
      C.CORRECTION_TRANSITION_DIGEST
    ];
    expect(abandonedUnder).not.toBeNull();
    await restoreObserverRow(observerBefore);
    expect(
      (await scenario.tick(restarted.globals)).admittedTransactionHashes,
    ).toEqual([removal.accepted.transaction.txHash]);
    expect(
      (await scenario.tick(restarted.globals)).admittedTransactionHashes,
    ).toEqual([]);
    await scenario.nextSourceBlock(restarted);
    expect(await readRecoveryPlans()).toHaveLength(1);
    expect(
      (await readJournal(removed[0]!.headerHash))[
        C.CORRECTION_TRANSITION_DIGEST
      ],
    ).toBe(abandonedUnder);
    expect(await nativeRoot(restarted)).toBe(rewoundNativeRoot);
    const next = await commitNextBlock(restarted);
    await expectRewoundAndRecommitted({
      scenario,
      removed,
      rewoundNativeRoot,
      rewoundSqlRoot,
      next,
      observerAtRewind,
    });
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);

it("recovers at startup when its block was removed and the removal became final on L1 while the node was down, then commits the reincluded deposit", async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  let h: Pick<Lifecycle, "close"> = scenario.h;
  try {
    const removed = await captureRemoved(scenario.headers);
    const cursorQueue = (await readObserver()).cursorQueue;
    let removal: Awaited<ReturnType<typeof scenario.removeTail>> | undefined;
    // No node service runs from before the removal until after its finality:
    // the node first sees the removal on L1 when it starts.
    const restarted = await restartAfter(scenario, async () => {
      removal = await scenario.removeTail(scenario.headers[0]!, {
        observe: false,
      });
      await scenario.awaitRemovalFinality(undefined, { observe: false });
    });
    h = restarted;
    await expectUnobservedRemoval(removed, restarted, cursorQueue);
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
    });
  } finally {
    await closeLifecycle(h);
  }
}, 600_000);

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
