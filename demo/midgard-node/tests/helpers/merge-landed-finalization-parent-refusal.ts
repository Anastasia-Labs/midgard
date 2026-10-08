import { inspect } from "node:util";

import { expect } from "vitest";

import { DepositsDB } from "../../src/database/index.js";
import { CONFIRMED_MERGE_JOURNAL_UNBOUND } from "../../src/transactions/state-queue/merge-to-confirmed-state.finalize-confirmed-merge-program.js";
import type { MergeLifecycle } from "../merge-landed-finalization-emulator.test.js";

const FOREIGN_BASE = Buffer.from("ff".repeat(28), "hex");

/**
 * A merge lands while the node is down, and its retained journal names a
 * base other than the landed header's parent. The merge fiber's catch-up
 * (the landed merge's local finalization) refuses that journal by name on
 * every attempt and writes nothing: no fold, no first frontier at the
 * journal's base, no finalization job. With the journal's own base back, the
 * same catch-up folds the merge exactly once.
 */
export const assertLandedMergeParentRefusal = async (
  m: MergeLifecycle,
  hooks: { readonly confirmedFinalizations: readonly unknown[] },
  expectFinalizedOnce: (
    m: MergeLifecycle,
    headerHash: string,
    attempts: number,
  ) => Promise<void>,
) => {
  const { fixture } = m;
  const block = await m.commitDepositBlock(12_000_000n);
  const held = await m.expireMergeConfirmation(block);
  const header = Buffer.from(block, "hex");
  const confirmedRows = () =>
    m.sqlRun(
      (sql) => sql`SELECT outref,output FROM confirmed_ledger ORDER BY outref`,
    );
  const temporalRows = () =>
    m.sqlRun(
      (sql) => sql`SELECT
        (SELECT count(*) FROM node_confirmed_merges)::int AS merges,
        (SELECT count(*) FROM node_confirmed_ledger_frontier
          WHERE header_hash = ${FOREIGN_BASE})::int AS foreign_frontier`,
    );
  const [journal] = await m.sqlRun(
    (sql) => sql<{ readonly base_tail_header_hash: Buffer }>`
      SELECT base_tail_header_hash FROM pending_block_finalizations
      WHERE header_hash = ${header}`,
  );
  const before = await confirmedRows();
  await m.restart(async () => {
    expect(await fixture.emulator.submitTx(held.txCbor)).toBe(held.txHash);
    fixture.emulator.awaitBlock(1);
    await m.sqlRun(
      (sql) => sql`UPDATE pending_block_finalizations
        SET base_tail_header_hash = ${FOREIGN_BASE}
        WHERE header_hash = ${header}`,
    );
  });
  expect(await m.queuedBlocks()).toBe(0);
  for (let attempt = 1; attempt <= 2; attempt += 1) {
    const failure = await m.catchUp().then(
      () => undefined,
      (error: unknown) => error,
    );
    // Effect's FiberFailure keeps the nested cause behind its inspect
    // representation; Error.cause formatting alone loses the refusal reason.
    const refusal = inspect(failure, { depth: 20 });
    expect(refusal).toContain(CONFIRMED_MERGE_JOURNAL_UNBOUND);
    expect(refusal).toContain(`journal_base=${"ff".repeat(28)}/`);
    expect(await confirmedRows()).toEqual(before);
    expect(await temporalRows()).toEqual([{ merges: 0, foreign_frontier: 0 }]);
    expect(await m.mergeJob(block)).toBeUndefined();
    await m.expectDeposits(block, DepositsDB.Status.Projected);
  }
  expect(hooks.confirmedFinalizations).toEqual([]);

  // The journal's own base back: the same catch-up folds the merge once.
  await m.sqlRun(
    (sql) => sql`UPDATE pending_block_finalizations
      SET base_tail_header_hash = ${journal!.base_tail_header_hash}
      WHERE header_hash = ${header}`,
  );
  expect((await m.catchUp()).status).toBe("skipped_merge_candidate_changed");
  await expectFinalizedOnce(m, block, 1);
  expect(await confirmedRows()).not.toEqual(before);
  expect(hooks.confirmedFinalizations).toEqual([]);
};
