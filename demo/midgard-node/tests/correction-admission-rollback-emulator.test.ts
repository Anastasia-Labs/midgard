/**
 * §15 N4 acceptance on the emulator (plan §7.4, §5.5 P7 and P8): a
 * correction is admitted when its removal lands, a rollback of it below k
 * recomputes with no halt and no intervention, the same removal landing
 * again rewinds exactly once, and commit, merge and settlement then resume
 * on the parent with every root equal to a fresh replay.
 *
 * The removal is an ordinary L1 transaction from the operator's wallet; the
 * node learns of it, and of its rollback, only through the follower's facts
 * (the landed state queue). The rollback discards the removal's block and
 * the empty blocks after it (`rollBackEmulatorChain`); the node's follower
 * store rewinds onto the restored chain at the next synchronization and
 * applies the blocks after it. A rollback deeper than k is not hostable
 * here: the emulator follower host replays a chain from its origin when the
 * store refuses a rewind, and the follower refuses such a rollback before
 * any fact changes (the fork
 * simulator's beyond-k rewind and the readiness route's `rollback_beyond_k`
 * case cover that polarity).
 */
import "./helpers/follower-emulator-installed.js";

import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import {
  advanceEmulatorPastLatestBlockEndTime,
  commitConfirmRecoverAndMerge,
  SDK,
} from "./deposit-flow-emulator-shared.js";
import {
  commitLocallyAppliedBlock,
  expectOneRoot,
  finalizeLocally,
  type Lifecycle,
  observe,
  submitDeposit,
  synchronizeBounded,
} from "./helpers/correction-admission-scenario.js";
import {
  captureEmulatorChain,
  rollBackEmulatorChain,
} from "./helpers/emulator-rollback.js";
import { prepareTimedOutTailRemoval } from "./helpers/history-timeout-correction-fixture.js";
import { openProductionLifecycle } from "./helpers/production-lifecycle.js";

/** No journal has this header: `observe` before any block is committed. */
const NO_BLOCK = "00".repeat(32);

/** Advance `blocks` L1 blocks, then let the follower driver synchronize. */
const advance = async (h: Lifecycle, blocks: number) => {
  if (blocks > 0) h.fixture.emulator.awaitBlock(blocks);
  vi.setSystemTime(new Date(h.fixture.emulator.now()));
  await synchronizeBounded(h);
};

/** Wallet views pin predicted change; a rewritten chain invalidates them. */
const clearWalletPins = (h: Lifecycle) => {
  h.lucidService.api.clearUTxOOverride();
  h.fixture.operatorLucid.clearUTxOOverride();
  h.fixture.depositorLucid.clearUTxOOverride();
};

it("admits a landed correction, recomputes its rollback at cd + 1 and its relanding exactly once, then commits, merges and settles on the parent", async () => {
  const h = await openProductionLifecycle({ landedBlocks: true });
  const { fixture } = h;
  try {
    const { confirmationDepth: cd, automaticRecoveryMaxDepth: k } =
      h.deployment.manifest.l1Finality;
    expect(cd).toBeGreaterThan(1);
    expect(cd + 1).toBeLessThan(k);
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    await advance(h, 0);
    const start = await observe(h, NO_BLOCK);
    expect(start.sqlRoot).toBe(start.nativeRoot);
    const base = start.nativeRoot;

    // B1: an own block carrying deposit D1, applied locally, never attested.
    const b1 = await commitLocallyAppliedBlock(
      h,
      await submitDeposit(h, 12_000_000n),
    );
    const applied = await observe(h, b1);
    const b1Root = expectOneRoot(applied);
    expect(b1Root).not.toBe(base);
    expect(applied.journal).toMatchObject({
      status: "locally_applied",
      corrected: false,
    });
    expect(applied.deposits).toEqual([
      { status: "projected", projectedHeader: b1 },
    ]);
    expect(applied.landedTip).toBe(b1);
    expect(applied.liveness).toEqual([]);
    expect(applied.landedHold).toBeUndefined();

    // The removal lands: admitted at once, at depth 1.
    const removal = await prepareTimedOutTailRemoval({
      fixture,
      targetHeaderHash: b1,
    });
    const beforeRemoval = captureEmulatorChain(fixture.emulator);
    const landed = await removal.submit();
    const depth = () =>
      fixture.emulator.blockHeight - landed.acceptedHeight + 1;
    await advance(h, 0);
    expect(depth()).toBe(1);
    const admitted = await observe(h, b1);
    expect(expectOneRoot(admitted)).toBe(base);
    expect(admitted.journal).toMatchObject({ status: "abandoned" });
    // D1 is pending again: released to awaiting, and at once re-projected
    // (it is due) with no header, for the next block to carry.
    expect(admitted.deposits).toEqual([
      { status: "projected", projectedHeader: null },
    ]);
    expect(admitted.landed.map((row) => row.headerHash)).not.toContain(b1);
    expect(admitted.landedTip).not.toBe(b1);
    // At most one native child restart for the rewind; the process is this one.
    expect(admitted.childRestarts - applied.childRestarts).toBeLessThanOrEqual(
      1,
    );
    expect(admitted.liveness).toEqual([]);
    expect(admitted.landedHold).toBeUndefined();

    // Deeper, to cd and then cd + 1: nothing more happens.
    await advance(h, cd - depth());
    expect(depth()).toBe(cd);
    expect(await observe(h, b1)).toEqual(admitted);
    await advance(h, 1);
    expect(depth()).toBe(cd + 1);
    expect(await observe(h, b1)).toEqual(admitted);

    // Rolled back at depth cd + 1 (below k): B1 is back on L1, so the node
    // adopts it again and the rebase revives it. Nothing halts.
    expect(rollBackEmulatorChain(fixture.emulator, beforeRemoval)).toBe(
      fixture.emulator.blockHeight - beforeRemoval.blockHeight,
    );
    clearWalletPins(h);
    expect(
      (
        await fixture.operatorLucid.transactionStatus(
          landed.accepted.transaction.txHash,
        )
      ).status,
    ).not.toBe("confirmed");
    await advance(h, 1);
    const revived = await observe(h, b1);
    expect(revived.liveness).toEqual([]);
    expect(revived.landedHold).toBeUndefined();
    expect(revived.landedTip).toBe(b1);
    expect(revived.deposits).toEqual([
      { status: "projected", projectedHeader: b1 },
    ]);
    expect(expectOneRoot(revived)).toBe(b1Root);
    expect(revived.childRestarts - admitted.childRestarts).toBeLessThanOrEqual(
      1,
    );
    // Revived: its journal is the commit path's again, local finalization
    // owed. B1 is still past its DA attestation deadline, so the commit path
    // defers (it pauses until correction removes the expired suffix): a
    // refusal, never a halt.
    expect(revived.journal).toMatchObject({
      status: "observed_waiting_stability",
      corrected: true,
    });
    await expect(finalizeLocally(h, b1)).rejects.toThrow(
      "Commit paused until expired unattested suffix is corrected",
    );
    const deferred = await observe(h, b1);
    expect(deferred.liveness).toEqual([]);
    expect(expectOneRoot(deferred)).toBe(b1Root);

    // The same removal lands again: one rewind, exactly as the first.
    const relanded = await fixture.emulator.submitTx(landed.signedCbor);
    expect(relanded).toBe(landed.accepted.transaction.txHash);
    expect(await fixture.operatorLucid.awaitTx(relanded)).toBe(true);
    clearWalletPins(h);
    await advance(h, 0);
    const readmitted = await observe(h, b1);
    expect(expectOneRoot(readmitted)).toBe(base);
    expect(readmitted.journal).toMatchObject({ status: "abandoned" });
    expect(readmitted.deposits).toEqual(admitted.deposits);
    expect(readmitted.landed).toEqual(admitted.landed);
    expect(
      readmitted.childRestarts - deferred.childRestarts,
    ).toBeLessThanOrEqual(1);
    expect(readmitted.liveness).toEqual([]);
    expect(readmitted.landedHold).toBeUndefined();
    for (let block = 0; block < 2; block++) {
      await advance(h, 1);
      expect(await observe(h, b1)).toEqual(readmitted);
    }

    // P7: the next commit builds on the parent, never on the removed B1, and
    // carries D1; merge and settlement follow with no intervention.
    const merged = await commitConfirmRecoverAndMerge({
      fixture,
      lucidService: h.lucidService,
      globals: h.globals,
      production: h.production,
    });
    const b2 = merged.queuedHeaderHash;
    expect(b2).not.toBe(b1);
    expect(merged.queuedHeader.prevHeaderHash).not.toBe(b1);
    expect(merged.settlementUtxo).toBeDefined();
    await advance(h, 1);
    const settled = await observe(h, b2);
    expect(settled.liveness).toEqual([]);
    expect(settled.landedHold).toBeUndefined();
    expect(settled.journal).toMatchObject({ status: "locally_applied" });
    expect(settled.landedTip).toBe(b2);
    expect(settled.deposits.map((row) => row.projectedHeader)).toEqual([b2]);
    // The ledger root equals a fresh replay, and the root L1 accepted for B2.
    expect(expectOneRoot(settled)).toBe(merged.queuedHeader.utxosRoot);
    expect(
      await Effect.runPromise(SDK.hashBlockHeader(merged.queuedHeader)),
    ).toBe(b2);
  } finally {
    await h.close();
    vi.useRealTimers();
  }
});
