import "node:util";
import "@effect/sql";
import "effect";
import "vitest";
import "../src/database/pendingBlockFinalizations.js";
import "./helpers/correction-rewind-scenario.js";
import "./attestation-timeout-reinclusion-emulator.expect-rewound-and-recommitted.js";

import { expect, it } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  C,
  captureRemoved,
  expectRewoundAndRecommitted,
  expectUnobservedRemoval,
  failureText,
  MARKER_REFUSAL,
  nativeRoot,
  restartAfter,
} from "./attestation-timeout-reinclusion-emulator.expect-rewound-and-recommitted.js";
import {
  closeLifecycle,
  commitNextBlock,
  type Lifecycle,
  openCorrectionRewindScenario,
  readDeposits,
  readJournal,
  readObserver,
  readObserverRow,
  readRecoveryPlans,
  readSqlLedgerRoot,
  restoreObserverRow,
} from "./helpers/correction-rewind-scenario.js";

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
