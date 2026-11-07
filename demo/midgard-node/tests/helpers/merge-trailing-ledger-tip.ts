import type { SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import { Effect, Either } from "effect";
import { expect } from "vitest";

import { mergeAction } from "../../src/fibers/merge.js";
import { listSlotAwareDueWork } from "../../src/fibers/slot-aware-due-work.js";
import {
  advanceEmulatorToDueWork,
  Database,
  emulatorStateQueueSnapshot,
  mergeMaturityWindow,
} from "../deposit-flow-emulator-shared.js";
import type { MergeLifecycle } from "../merge-landed-finalization-emulator.test.js";

/** Slots the ledger tip is short of the merge's lower bound (devnet r4: 20). */
const TIP_SHORT_OF_BOUND = 20;

type Submission = { readonly accepted: string } | { readonly refused: string };

/**
 * Devnet r4, on the real validators: the merge of a mature block, with the
 * local node between blocks. The L1 clock and the wall-clock submit slot are
 * past the merge's validity lower bound (block end time plus the maturity
 * window); the ledger's latest block, which the mempool judges the bound at,
 * is 20 slots short of it. The merge is deferred to due work and nothing is
 * sent (the node sent it, and the ledger refused it, OutsideValidityInterval);
 * a retry while the tip still trails stays deferred, although wall time is
 * past the due slot. Once the tip reaches the bound, the same merge lands on
 * the first send, and is finalized exactly once.
 */
export const assertMergeWaitsForLedgerTip = async (
  m: MergeLifecycle,
  hooks: { l1ClockLeadSlots: number },
  expectFinalizedOnce: (
    m: MergeLifecycle,
    headerHash: string,
    attempts: number,
  ) => Promise<void>,
) => {
  const { fixture } = m;
  const block = await m.commitDepositBlock(12_000_000n, false);
  const tail = (
    await Effect.runPromise(
      emulatorStateQueueSnapshot(
        fixture.operatorLucid,
        fixture.contracts.stateQueue,
        "startup",
      ).pipe(Effect.provide(Database.layer)),
    )
  ).tailCommitBase;
  expect(tail.headerHash).toBe(block);
  const window = mergeMaturityWindow(
    fixture.operatorLucid,
    tail.blockEndTimeMs,
  );
  const validFromSlot = Number(
    fixture.operatorLucid.unixTimeToSlot(window.validFromUnixTime),
  );
  const readyAfterSlot = Number(
    fixture.operatorLucid.unixTimeToSlot(window.readyAfterUnixTime),
  );
  const tipSlot = validFromSlot - TIP_SHORT_OF_BOUND;
  expect(fixture.emulator.slot).toBeLessThan(tipSlot);
  fixture.emulator.awaitSlot(tipSlot - fixture.emulator.slot);
  await m.h.synchronize();

  // The L1 clock and the submit slot run one slot past the merge's
  // readiness, which is 20 s past its lower bound.
  hooks.l1ClockLeadSlots = readyAfterSlot + 1 - tipSlot;
  const trailing = {
    ...m.h.lucidService,
    submitSlotSnapshot: () =>
      Effect.sync(
        (): SubmitSlotSnapshot => ({
          source: "l1_node_tip",
          currentSlot: fixture.emulator.slot + hooks.l1ClockLeadSlots,
          ledgerTipSlot: fixture.emulator.slot,
          observedAtMs: Date.now(),
          slotLengthMs: 1_000,
        }),
      ),
  };
  const submissions: Submission[] = [];
  const submitThrough = fixture.emulator.submitTx;
  fixture.emulator.submitTx = async (signedCbor) => {
    try {
      const txHash = await submitThrough.call(fixture.emulator, signedCbor);
      submissions.push({ accepted: txHash });
      return txHash;
    } catch (error) {
      submissions.push({ refused: String(error) });
      throw error;
    }
  };
  try {
    const deferred = await m.run(Effect.either(mergeAction(true)), trailing);
    expect(Either.isRight(deferred) && deferred.right).toMatchObject({
      status: "skipped_oldest_block_local_ledger_not_ready",
      headerHash: block,
      reason: expect.stringContaining(
        `local_ledger_slot=${tipSlot.toString()},target_slot=${(validFromSlot + 2).toString()}`,
      ),
    });
    expect(submissions).toEqual([]);
    const dueWork = listSlotAwareDueWork().filter(
      (entry) => entry.kind === "merge_submit_validity",
    );
    expect(dueWork).toHaveLength(1);
    expect(dueWork[0]).toMatchObject({
      observedSlot: tipSlot,
      dueSlot: validFromSlot + 2,
    });

    // Wall time is now past the due slot; the ledger tip is not.
    const stillTrailing = await m.run(
      Effect.either(mergeAction(true)),
      trailing,
    );
    expect(Either.isRight(stillTrailing) && stillTrailing.right).toMatchObject({
      status: "skipped_oldest_block_local_ledger_not_ready",
      reason: expect.stringContaining("merge_due_work_not_due"),
    });
    expect(submissions).toEqual([]);

    await advanceEmulatorToDueWork(fixture, dueWork[0]!);
    await m.h.synchronize();
    const merged = await m.mergeUntilSubmitted(trailing);
    expect(Either.isRight(merged) && merged.right).toMatchObject({
      status: "merged",
      headerHash: block,
    });
    expect(submissions).toHaveLength(1);
    expect(submissions[0]).toHaveProperty("accepted");
  } finally {
    fixture.emulator.submitTx = submitThrough;
    hooks.l1ClockLeadSlots = 0;
  }
  expect(await m.queuedBlocks()).toBe(0);
  await expectFinalizedOnce(m, block, 1);
};
