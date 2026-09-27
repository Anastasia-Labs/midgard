import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { signedIntentReplacementDigest } from "../src/services/canonical-journal-recovery.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  commitConfirmRecoverAndMerge,
} from "./deposit-flow-emulator-shared.js";
import {
  admitTransfer,
  buildDepositorTransfer,
  closeLifecycle,
  commitAndLocallyFinalizeNextBlock,
  depositorL2Utxos,
  finalizeLocally,
  insertForcedTransfer,
  read,
  readJournal,
  submitDeposit,
  submitUnlandedBlock,
  submitWithdrawal,
} from "./helpers/correction-rewind-scenario.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import {
  expectReplaced,
  landSignedCommitAsFork,
  moveToExactSlot,
  nativeRoot,
  readImmutableCounts,
  resetSharedRows,
  signedTtl,
  synchronizeWithin,
} from "./helpers/signed-intent-replacement.js";

/**
 * Every member kind of a replaced signed commit: an L2 transfer, a
 * withdrawal, a forced transaction and a deposit. Whichever commit lands
 * carries each of them, and each is committed exactly once: the replacement
 * when E never lands, E itself when E wins its base slot after its
 * replacement was signed. Actual deployed validators, the production history
 * owner and Architecture G, and emulator transactions.
 */

const C = Pending.Columns;
const hex = (buffer: Buffer) => buffer.toString("hex");

type Lifecycle = Awaited<
  ReturnType<typeof openHistoryProductionOwnerLifecycle>
>;

const AMOUNTS = {
  transfer: 20_000_000n,
  withdrawal: 12_000_000n,
  forced: 15_000_000n,
  deposit: 17_000_000n,
  transferPayment: 5_000_000n,
  forcedPayment: 4_000_000n,
} as const;

/** Merge three funding deposits, then commit a block E carrying a transfer,
 * a withdrawal and a forced transaction spending their outputs, and a new
 * deposit; E's signed commit is handed to L1 and lost. */
const loseContentCommit = async (h: Lifecycle) => {
  const { fixture, lucidService, globals, production } = h;
  const funding = [
    await submitDeposit(h, AMOUNTS.transfer),
    await submitDeposit(h, AMOUNTS.withdrawal),
    await submitDeposit(h, AMOUNTS.forced),
  ];
  await h.deployment.chain.awaitLedgerTime(Math.max(...funding) + 1000);
  vi.setSystemTime(fixture.emulator.now());
  await h.synchronize();
  await commitConfirmRecoverAndMerge({
    fixture,
    lucidService,
    globals,
    production,
  });
  await h.synchronize();
  const byAmount = async (lovelace: bigint) => {
    const found = (await depositorL2Utxos(h)).filter(
      (utxo) => utxo.assets.lovelace === lovelace,
    );
    expect(found).toHaveLength(1);
    return found[0]!;
  };
  const transferInput = await byAmount(AMOUNTS.transfer);
  const withdrawn = await byAmount(AMOUNTS.withdrawal);
  const forcedInput = await byAmount(AMOUNTS.forced);
  const depositTime = await submitDeposit(h, AMOUNTS.deposit);
  const withdrawalTime = await submitWithdrawal(h, withdrawn);
  const transfer = await buildDepositorTransfer(
    h,
    [transferInput],
    AMOUNTS.transferPayment,
  );
  expect(await admitTransfer(h, transfer)).toBe("accepted");
  const forcedTx = await buildDepositorTransfer(
    h,
    [forcedInput],
    AMOUNTS.forcedPayment,
  );
  const forced = await insertForcedTransfer(h, forcedTx);
  const lost = await submitUnlandedBlock(
    h,
    Math.max(depositTime, withdrawalTime, forced.inclusionTime),
  );
  const journal = await readJournal(lost.submittedHeaderHash);
  expect(journal.mempoolTxIds.map(hex)).toEqual([hex(transfer.txId)]);
  expect(journal.withdrawalEventIds).toHaveLength(1);
  expect(journal.forcedTransactionEventIds.map(hex)).toEqual([
    hex(forced.eventId),
  ]);
  expect(journal.depositEventIds).toHaveLength(1);
  return {
    header: lost.submittedHeaderHash,
    journal,
    ttl: signedTtl(journal[C.SIGNED_TX_CBOR]!),
    transferId: hex(transfer.txId),
  };
};

/** Where each non-transfer member of `journal` is projected, by kind. */
const readMemberHeaders = (journal: Pending.Record) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const header = (rows: readonly { h: Buffer | null }[]) => {
        expect(rows).toHaveLength(1);
        return rows[0]!.h === null ? null : hex(rows[0]!.h);
      };
      return {
        withdrawal: header(
          yield* sql<{ h: Buffer | null }>`SELECT projected_header_hash AS h
            FROM withdrawal_utxos
            WHERE event_id = ${journal.withdrawalEventIds[0]!}`,
        ),
        forced: header(
          yield* sql<{ h: Buffer | null }>`SELECT projected_header_hash AS h
            FROM forced_transaction_utxos
            WHERE tx_order_id = ${journal.forcedTransactionEventIds[0]!}`,
        ),
        deposit: header(
          yield* sql<{ h: Buffer | null }>`SELECT projected_header_hash AS h
            FROM deposits_utxos
            WHERE event_id = ${journal.depositEventIds[0]!}`,
        ),
      };
    }),
  );

const readMempool = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ tx_id: Buffer }>`SELECT tx_id FROM mempool`;
      return rows.map((row) => hex(row.tx_id));
    }),
  );

const sameMembers = (a: Pending.Record, b: Pending.Record) => {
  expect(a.mempoolTxIds.map(hex)).toEqual(b.mempoolTxIds.map(hex));
  expect(a.withdrawalEventIds.map(hex)).toEqual(b.withdrawalEventIds.map(hex));
  expect(a.forcedTransactionEventIds.map(hex)).toEqual(
    b.forcedTransactionEventIds.map(hex),
  );
  expect(a.depositEventIds.map(hex)).toEqual(b.depositEventIds.map(hex));
};

describe.sequential("signed-intent replacement of every member kind", () => {
  it("reopens a lost commit's transfer, withdrawal, forced transaction and deposit, and its replacement commits each exactly once", async () => {
    const h = await openHistoryProductionOwnerLifecycle();
    try {
      await resetSharedRows();
      await advanceEmulatorPastLatestBlockEndTime(h.fixture);
      const E = await loseContentCommit(h);
      moveToExactSlot(h, E.ttl);
      await synchronizeWithin(h, 240_000);
      await expectReplaced(E.journal, { handle: h });
      // Every member is pending again, none committed.
      expect(await readMemberHeaders(E.journal)).toEqual({
        withdrawal: null,
        forced: null,
        deposit: null,
      });
      expect(await readMempool()).toContain(E.transferId);
      expect(await readImmutableCounts([E.transferId])).toEqual({
        [E.transferId]: 0,
      });

      const nextHeader = await commitAndLocallyFinalizeNextBlock(h);
      const next = await readJournal(nextHeader);
      expect(next[C.BASE_UTXOS_ROOT]).toBe(E.journal[C.BASE_UTXOS_ROOT]);
      sameMembers(next, E.journal);
      expect(await readMemberHeaders(E.journal)).toEqual({
        withdrawal: nextHeader,
        forced: nextHeader,
        deposit: nextHeader,
      });
      expect(await readImmutableCounts([E.transferId])).toEqual({
        [E.transferId]: 1,
      });
      expect(await readMempool()).not.toContain(E.transferId);
      expect(await nativeRoot(h)).toBe(next[C.EXPECTED_UTXOS_ROOT]);
    } finally {
      await closeLifecycle(h);
    }
  }, 1_200_000);

  it("revives a replaced commit of every member kind that wins its base slot, abandons its replacement, and commits each member exactly once", async () => {
    const h = await openHistoryProductionOwnerLifecycle();
    try {
      await resetSharedRows();
      await advanceEmulatorPastLatestBlockEndTime(h.fixture);
      const E = await loseContentCommit(h);
      moveToExactSlot(h, E.ttl);
      await synchronizeWithin(h, 240_000);
      await expectReplaced(E.journal, { handle: h });
      // NEW_E: the same members on the same base, handed to L1 and lost.
      // This deliberately skips a production step. The node's pre-lease
      // alignment (alignCommitSchedulerBeforeMutationWorker in
      // src/fibers/block-commitment.ts) would Rewind the scheduler here, since
      // E's TTL sits late in the shift, so a production NEW_E references the
      // refreshed scheduler UTxO. That is harmless: E's validTo is at or
      // before its shift end (schedulerStateCoversCommitTarget), and a
      // refresh's validFrom is at or after it
      // (resolveSchedulerRefreshValidityWindow in
      // src/workers/utils/scheduler-refresh.ts; onchain scheduler.ak
      // validate_end_of_shift_and_get_operators), so a fork that includes E
      // orders the refresh after E. The emulator cannot place an unobserved E
      // before that refresh, so the test skips the alignment to keep E's
      // reference inputs unspent and E landable below. Keeping them unspent is
      // a harness precondition, not a production property.
      const lost = await submitUnlandedBlock(
        h,
        h.fixture.emulator.now() - 1000,
        { alignScheduler: false },
      );
      const N = await readJournal(lost.submittedHeaderHash);
      expect(N[C.BASE_TAIL_OUT_REF]).toBe(E.journal[C.BASE_TAIL_OUT_REF]);
      sameMembers(N, E.journal);
      const nTtl = signedTtl(N[C.SIGNED_TX_CBOR]!);

      // The chain now followed included E before its TTL.
      await landSignedCommitAsFork(h, E.journal[C.SIGNED_TX_CBOR]!);
      if (h.fixture.emulator.slot < nTtl) moveToExactSlot(h, nTtl);
      await synchronizeWithin(h, 240_000);
      expect((await readJournal(E.header))[C.STATUS]).toBe(
        Pending.Status.ObservedWaitingStability,
      );
      const abandoned = await readJournal(N[C.HEADER_HASH].toString("hex"));
      expect(abandoned[C.STATUS]).toBe(Pending.Status.Abandoned);
      expect(abandoned[C.CORRECTION_TRANSITION_DIGEST]).toBe(
        signedIntentReplacementDigest(N),
      );
      // Every member is taken back by E.
      expect(await readMemberHeaders(E.journal)).toEqual({
        withdrawal: E.header,
        forced: E.header,
        deposit: E.header,
      });

      await finalizeLocally(h, E.header);
      expect(await nativeRoot(h)).toBe(E.journal[C.EXPECTED_UTXOS_ROOT]);
      expect(await readMemberHeaders(E.journal)).toEqual({
        withdrawal: E.header,
        forced: E.header,
        deposit: E.header,
      });
      expect(await readImmutableCounts([E.transferId])).toEqual({
        [E.transferId]: 1,
      });
      expect(await readMempool()).not.toContain(E.transferId);
    } finally {
      await closeLifecycle(h);
    }
  }, 1_200_000);
});
