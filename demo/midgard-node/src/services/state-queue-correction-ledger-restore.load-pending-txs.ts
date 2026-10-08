import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { Effect, Option } from "effect";

import {
  DepositsDB,
  MempoolLedgerDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import * as Ledger from "../database/utils/ledger.js";
import {
  failure,
  hex,
  type LedgerRow,
  type RejectionCodes,
} from "./working-ledger-recompute.js";

/**
 * Speculative-ledger repair after a state-queue correction reopened removed
 * blocks. Runs inside the reinclusion transaction, after every removed
 * block's payloads returned to the pending sets.
 *
 * Two local effects outlive a removed block and are undone here:
 *
 * 1. Local finalization deleted each valid withdrawal's L2 output from
 *    `mempool_ledger`. The withdrawal is pending again, so its output is
 *    restored with its exact bytes (and deposit origin, when it had one).
 * 2. A reopened deposit's output is no longer spendable until the deposit is
 *    committed again, and a transaction spending it in the same block is
 *    refused at commit. Every pending transaction that spends such an output,
 *    directly or through another refused transaction's output, is rejected
 *    now, and its ledger effects are reversed from exact before-images.
 *
 * A rejection undoes the whole acceptance: the admission becomes terminally
 * rejected, its address history goes, and every acceptance receipt it belongs
 * to is marked reversed. A receipt is the inverse of one accepted batch and
 * cannot be split, so a rejection widens to every member of a batch it
 * touches (`E_REWIND_BATCH_MEMBER`); a batch with a member that already left
 * the pending sets cannot be reversed at all, and the repair refuses.
 *
 * A before-image this node cannot prove is never guessed: the repair fails and
 * the whole reinclusion transaction rolls back.
 */

export const REWIND_REJECT_CODE_REOPENED_DEPOSIT_INPUT =
  "E_REWIND_REOPENED_DEPOSIT_INPUT";

export const REWIND_REJECT_CODE_DEPENDENT_INPUT = "E_REWIND_DEPENDENT_INPUT";

/** A transaction accepted in one batch with a rejected transaction: the
 * batch's acceptance receipt is reversed as a whole. */
export const REWIND_REJECT_CODE_BATCH_MEMBER = "E_REWIND_BATCH_MEMBER";

export const REJECTIONS: RejectionCodes = {
  direct: {
    code: REWIND_REJECT_CODE_REOPENED_DEPOSIT_INPUT,
    detail:
      "Transaction spends the output of a deposit reopened by a state-queue correction",
  },
  dependent: {
    code: REWIND_REJECT_CODE_DEPENDENT_INPUT,
    detail:
      "Transaction spends an output of a transaction rejected after a state-queue correction",
  },
  batch: {
    code: REWIND_REJECT_CODE_BATCH_MEMBER,
    detail:
      "Transaction was accepted in one batch with a transaction rejected after a state-queue correction",
  },
};

const ROOT_TAIL_HEADER_HASH = Buffer.alloc(28);

export type WithdrawalLedgerRestore = Readonly<{
  /** 38-byte ledger key of the withdrawn L2 output. */
  outRef: Buffer;
  /** The withdrawal's l2_outref (Data OutputReference); a deposit's event id
   * uses the same encoding. */
  l2OutRefData: Buffer;
  /** Header whose base state held the output. */
  baseTailHeaderHash: Buffer;
}>;

export type LedgerRestoreResult = Readonly<{
  restoredWithdrawalOutputs: number;
  rejectedTransactions: readonly Buffer[];
}>;

/** The deposit whose ledger output is `outRef`, if `eventId` names one. */
export const depositRow = (eventId: Buffer, outRef: Buffer) =>
  Effect.gen(function* () {
    const deposit = yield* DepositsDB.retrieveByEventId(eventId);
    if (Option.isNone(deposit)) return undefined;
    const entry = yield* DepositsDB.toMempoolLedgerEntry(deposit.value);
    if (!entry[Ledger.Columns.OUTREF].equals(outRef)) return undefined;
    return {
      row: {
        [MempoolLedgerDB.Columns.TX_ID]: entry[Ledger.Columns.TX_ID],
        [MempoolLedgerDB.Columns.OUTREF]: entry[Ledger.Columns.OUTREF],
        [MempoolLedgerDB.Columns.OUTPUT]: entry[Ledger.Columns.OUTPUT],
        [MempoolLedgerDB.Columns.ADDRESS]: entry[Ledger.Columns.ADDRESS],
        [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: entry.source_event_id,
      } satisfies LedgerRow,
      deposit: deposit.value,
    };
  });

/** An output produced by an unmerged ancestor block, from its journal's
 * retained ledger delta. The walk follows base-tail links to the root. */
export const ancestorRow = (outRef: Buffer, baseTailHeaderHash: Buffer) =>
  Effect.gen(function* () {
    const seen = new Set<string>();
    let cursor = baseTailHeaderHash;
    while (!cursor.equals(ROOT_TAIL_HEADER_HASH) && !seen.has(hex(cursor))) {
      seen.add(hex(cursor));
      const journal =
        yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(cursor);
      if (Option.isNone(journal)) return undefined;
      const record = journal.value;
      const member = record.ledgerDelta.produced.find((produced) =>
        produced[PendingBlockFinalizationsDB.UtxoColumns.OUTREF].equals(outRef),
      );
      if (member !== undefined) {
        const output = member[PendingBlockFinalizationsDB.UtxoColumns.OUTPUT];
        return yield* Effect.try({
          try: () =>
            ({
              [MempoolLedgerDB.Columns.TX_ID]: Buffer.from(
                decodeMidgardSpendInputItem(outRef).txId,
              ),
              [MempoolLedgerDB.Columns.OUTREF]: Buffer.from(outRef),
              [MempoolLedgerDB.Columns.OUTPUT]: Buffer.from(output),
              [MempoolLedgerDB.Columns.ADDRESS]: encodeMidgardAddressText(
                decodeMidgardTxOutput(output).address,
              ),
              [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
            }) satisfies LedgerRow,
          catch: (cause) =>
            failure("Retained journal output is not canonical", {
              outRef: hex(outRef),
              cause,
            }),
        });
      }
      cursor =
        record[PendingBlockFinalizationsDB.Columns.BASE_TAIL_HEADER_HASH];
    }
    return undefined;
  });
