import { inspect } from "node:util";

import { expect } from "vitest";

import { DepositsDB } from "../../src/database/index.js";
import type { MergeLifecycle } from "../merge-landed-finalization-emulator.test.js";

export const assertLandedMergeParentRefusal = async (
  m: MergeLifecycle,
  confirmedFinalizations: () => readonly unknown[],
) => {
  const { fixture } = m;
  const block = await m.commitDepositBlock(12_000_000n);
  const held = await m.expireMergeConfirmation(block);
  const confirmedRows = () =>
    m.sqlRun(
      (sql) => sql`SELECT outref,output FROM confirmed_ledger ORDER BY outref`,
    );
  const before = await confirmedRows();
  const failure = await m
    .restart(async () => {
      expect(await fixture.emulator.submitTx(held.txCbor)).toBe(held.txHash);
      fixture.emulator.awaitBlock(1);
      await m.sqlRun(
        (sql) => sql`UPDATE pending_block_finalizations
        SET base_tail_header_hash = ${Buffer.from("ff".repeat(28), "hex")}
        WHERE header_hash = ${Buffer.from(block, "hex")}`,
      );
    })
    .then(
      () => undefined,
      (error: unknown) => error,
    );
  // Effect's FiberFailure keeps the nested owner cause behind its inspect
  // representation; Error.cause formatting alone loses the refusal reason.
  expect(inspect(failure, { depth: 20 })).toContain(
    "Landed merge recovery journal differs from its canonical header/parent/root",
  );
  expect(await m.authorityState()).not.toBe("ready");
  expect(await confirmedRows()).toEqual(before);
  expect(await m.queuedBlocks()).toBe(0);
  expect(await m.mergeJob(block)).toBeUndefined();
  await m.expectDeposits(block, DepositsDB.Status.Projected);
  expect(confirmedFinalizations()).toEqual([]);
};
