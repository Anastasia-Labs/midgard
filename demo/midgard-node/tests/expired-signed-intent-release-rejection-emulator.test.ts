import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { expect, it, vi } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { journalAbandonment } from "../src/services/canonical-journal-recovery.js";
import {
  REWIND_REJECT_CODE_BATCH_MEMBER,
  REWIND_REJECT_CODE_REOPENED_DEPOSIT_INPUT,
} from "../src/services/state-queue-correction-ledger-restore.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  commitConfirmRecoverAndMerge,
} from "./deposit-flow-emulator-shared.js";
import {
  admitTransfer,
  admitTransfersTogether,
  buildDepositorTransfer,
  closeLifecycle,
  depositorL2Utxos,
  flushWriteBehind,
  read,
  readAcceptanceTraces,
  readJournal,
  submitDeposit,
  submitUnlandedBlock,
} from "./helpers/correction-rewind-scenario.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import {
  moveToExactSlot,
  signedTtl,
} from "./helpers/signed-intent-replacement.js";

/**
 * The replacement of a signed commit that missed its validity window reopens
 * the lost block through the shared correction reinclusion path, so it undoes
 * the acceptance of every pending transaction that spent the lost block's
 * deposit exactly as a rewind does. Actual deployed validators, the
 * production history owner and admission queue, and emulator transactions.
 */

const C = Pending.Columns;

type Lifecycle = Awaited<
  ReturnType<typeof openHistoryProductionOwnerLifecycle>
>;
type Handle = Lifecycle | Awaited<ReturnType<Lifecycle["restartRuntime"]>>;

const resetSharedRows = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DELETE FROM state_queue_terminal_observer_states`;
      yield* sql`DELETE FROM event_history_recovery_plans`;
    }),
  );

const readMempoolTxIds = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{ tx_id: Buffer }>`SELECT tx_id FROM mempool`;
      return rows.map((row) => row.tx_id.toString("hex")).sort();
    }),
  );

const hex = (value: Buffer) => value.toString("hex");

/** Direct surgery: the header assignment block confirmation performs. */
const assignDepositHeader = (eventId: Buffer, headerHash: string) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const updated = yield* sql`UPDATE deposits_utxos
        SET projected_header_hash = ${Buffer.from(headerHash, "hex")}
        WHERE event_id = ${eventId} AND projected_header_hash IS NULL
        RETURNING event_id`;
      expect(updated).toHaveLength(1);
    }),
  );

it("replacing a lost commit whose deposit a pending transaction spent rejects its whole accepted batch and leaves no acceptance trace or ledger repair wedge", async () => {
  const lifecycle = await openHistoryProductionOwnerLifecycle();
  let h: Handle = lifecycle;
  try {
    await resetSharedRows();
    await advanceEmulatorPastLatestBlockEndTime(h.fixture);
    // An L2 output independent of the lost block, funded through a merged
    // deposit block.
    const funding = await submitDeposit(h, 9_000_000n);
    await h.deployment.chain.awaitLedgerTime(funding + 1000);
    vi.setSystemTime(h.fixture.emulator.now());
    await h.synchronize();
    await commitConfirmRecoverAndMerge({
      fixture: h.fixture,
      lucidService: h.lucidService,
      globals: h.globals,
      production: h.production,
    });
    await h.synchronize();
    const byAmount = async (lovelace: bigint) => {
      const found = (await depositorL2Utxos(h)).filter(
        (utxo) => utxo.assets.lovelace === lovelace,
      );
      expect(found).toHaveLength(1);
      return found[0]!;
    };
    const independent = await byAmount(9_000_000n);

    // A deposit block signed and handed to L1, which never includes it.
    const inclusion = await submitDeposit(h, 12_000_000n);
    const lost = await submitUnlandedBlock(lifecycle, inclusion);
    const header = lost.submittedHeaderHash;
    const journal = await readJournal(header);
    expect(journal.depositEventIds).toHaveLength(1);
    expect(journal.mempoolTxIds).toHaveLength(0);
    // Unreachable through the node today, so forced: only block
    // confirmation assigns a deposit its header (which makes its L2 output
    // spendable), and a replaced commit was never observed on L1; and the
    // admission queue pauses while a submission awaits local finalization.
    // The assignment is forced while no runtime runs (the next one hydrates
    // it), and the pause is lifted for the one admission below, so the
    // replacement's shared rejection path runs on a real accepted batch.
    h = await lifecycle.restartRuntime({
      afterStop: () => assignDepositHeader(journal.depositEventIds[0]!, header),
    });

    // One batch accepts a spend of the lost block's deposit output with an
    // independent spend.
    const onDeposit = await buildDepositorTransfer(
      h,
      [await byAmount(12_000_000n)],
      6_000_000n,
    );
    const coBatched = await buildDepositorTransfer(
      h,
      [independent],
      4_000_000n,
    );
    const paused = h.globals.LOCAL_FINALIZATION_PENDING;
    expect(Effect.runSync(Ref.get(paused))).toBe(true);
    Effect.runSync(Ref.set(paused, false));
    expect(await admitTransfersTogether(h, [onDeposit, coBatched])).toEqual([
      "accepted",
      "accepted",
    ]);
    Effect.runSync(Ref.set(paused, true));
    await flushWriteBehind(h);
    const txIds = [onDeposit.txId, coBatched.txId];
    const batch = txIds.map(hex).sort();
    const traces = () =>
      readAcceptanceTraces({
        txIds,
        depositEventIds: journal.depositEventIds,
      });
    const accepted = await traces();
    expect(accepted.admissions).toEqual({
      [hex(onDeposit.txId)]: { status: "accepted", code: null },
      [hex(coBatched.txId)]: { status: "accepted", code: null },
    });
    expect(accepted.addressHistory).toEqual(batch);
    expect(accepted.receipts).toEqual([{ txIds: batch, reversed: false }]);
    expect(await readMempoolTxIds()).toEqual(batch);

    // The first source point at the signed TTL shows the commit's base still
    // the queue tail: the lost block is replaced and reopened.
    moveToExactSlot(h, signedTtl(journal[C.SIGNED_TX_CBOR]!));
    await h.synchronize();
    const replaced = await readJournal(header);
    expect(replaced[C.STATUS]).toBe(Pending.Status.Abandoned);
    expect(journalAbandonment(replaced)).toBe("replacement");

    // The spend of the reopened deposit is rejected at its rule and its
    // batch co-member with it, since the batch's receipt is one inverse; the
    // whole acceptance is undone and neither repair wedge is left.
    expect(await traces()).toEqual({
      admissions: {
        [hex(onDeposit.txId)]: {
          status: "rejected",
          code: REWIND_REJECT_CODE_REOPENED_DEPOSIT_INPUT,
        },
        [hex(coBatched.txId)]: {
          status: "rejected",
          code: REWIND_REJECT_CODE_BATCH_MEMBER,
        },
      },
      rejections: {
        [hex(onDeposit.txId)]: REWIND_REJECT_CODE_REOPENED_DEPOSIT_INPUT,
        [hex(coBatched.txId)]: REWIND_REJECT_CODE_BATCH_MEMBER,
      },
      addressHistory: [],
      receipts: [{ txIds: batch, reversed: true }],
      incompleteReceipts: [],
      publishedDependencies: [],
    });
    expect(await readMempoolTxIds()).toEqual([]);
    // The independent output is spendable again, by a fresh submission.
    expect(
      (await depositorL2Utxos(h)).some((utxo) =>
        utxo.outrefCbor.equals(independent.outrefCbor),
      ),
    ).toBe(true);
    const resubmitted = await buildDepositorTransfer(
      h,
      [independent],
      3_000_000n,
    );
    expect(await admitTransfer(h, resubmitted)).toBe("accepted");
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);
