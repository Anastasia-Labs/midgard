import { Effect, Either } from "effect";
import { expect } from "vitest";

import { DepositsDB } from "../../src/database/index.js";
import { mergeAction } from "../../src/fibers/merge.js";
import type { MergeLifecycle } from "../merge-landed-finalization-emulator.test.js";

const FOREIGN_BASE = Buffer.from("ff".repeat(28), "hex");

/**
 * A merge lands while the node is down, and its retained journal names a
 * base other than the landed header's parent. Landed-block processing folds
 * the merged block at its canonical parent on restart, without the journal,
 * and its deposits are consumed: the block is folded. Nothing reads that
 * journal's base: a further driver run and a merge attempt over the empty
 * queue write nothing, so no further fold and no frontier at that base.
 */
export const assertLandedMergeFoldedByProcessing = async (
  m: MergeLifecycle,
  expectFinalizedOnce: (m: MergeLifecycle, headerHash: string) => Promise<void>,
) => {
  const { fixture } = m;
  const block = await m.commitDepositBlock(12_000_000n);
  const held = await m.abandonMergeConfirmation(block);
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
  const landed = await confirmedRows();
  expect(landed).not.toEqual(before);
  const landedTemporal = await temporalRows();
  expect(landedTemporal).toMatchObject([{ foreign_frontier: 0 }]);
  await expectFinalizedOnce(m, block);
  const unchanged = async () => {
    expect(await confirmedRows()).toEqual(landed);
    expect(await temporalRows()).toEqual(landedTemporal);
    await expectFinalizedOnce(m, block);
    await m.expectDeposits(block, DepositsDB.Status.Consumed);
  };
  await m.h.synchronize();
  await unchanged();
  expect(Either.isRight(await m.run(Effect.either(mergeAction(true))))).toBe(
    true,
  );
  await unchanged();
};
