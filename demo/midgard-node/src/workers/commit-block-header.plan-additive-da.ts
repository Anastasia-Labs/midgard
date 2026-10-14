import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import type { MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  DepositsDB,
  ForcedTransactionsDB,
  WithdrawalsDB,
} from "../database/index.js";
import type { MinimalEntry } from "../database/utils/ledger.js";
import {
  Columns as TxColumns,
  type EntryWithTimeStamp,
} from "../database/utils/tx.js";
import { CommitWorkerInvariantError } from "./commit-block-header.pending-user-event-counts-up-to.js";
import {
  type CommitFrameEventContribution,
  selectAdditiveCommitFrame,
} from "./utils/commit-block-planner.additive-frame.js";
import type { CommitEventWindow } from "./utils/commit-block-planner.event-window.js";
import {
  additiveDaFixedBytes,
  additiveEventTraceBytes,
  daEntryTupleBytes,
  forcedInclusionValueBytesUpperBound,
  ordinarySourceBytes,
  withdrawalValueBytesUpperBound,
} from "./utils/commit-da-material.encoding.js";
import {
  type DaMaterialTransaction,
  potentialCommitProgramSources,
  potentialEventProgramBytes,
  potentialProgramSourceBytes,
} from "./utils/commit-da-material.program-sources.js";

const eventKey = (
  id: Buffer,
  kind: "deposits" | "withdrawals" | "forcedTransactions",
): SDK.EventKey => {
  const ref = LucidData.from(id.toString("hex"), SDK.OutputReference);
  switch (kind) {
    case "deposits":
      return { DepositEventKey: { deposit_id: ref } };
    case "withdrawals":
      return { WithdrawalEventKey: { withdrawal_id: ref } };
    case "forcedTransactions":
      return { ForcedTransactionEventKey: { tx_order_id: ref } };
  }
};

/** Source/encoding-only budget: no validation replay, mutable verdict read,
 * largest-window sidecar assumption, or shared-material sizing credit. */
export const commitDaSourceContributions = (input: {
  readonly base: readonly MinimalEntry[];
  readonly deposits: readonly DepositsDB.Entry[];
  readonly withdrawals: readonly WithdrawalsDB.Entry[];
  readonly forced: readonly ForcedTransactionsDB.Entry[];
  readonly candidates: readonly EntryWithTimeStamp[];
  readonly profile: MidgardConsensusProfile;
}) => {
  const forced: DaMaterialTransaction[] = input.forced.map((row) => ({
    canonical: decodeMidgardForcedTxFullFromCanonicalCbor(
      row[ForcedTransactionsDB.Columns.NATIVE_TX_CBOR],
    ),
    sourceKind: "forced",
  }));
  const ordinary: DaMaterialTransaction[] = input.candidates.map((row) => ({
    canonical: decodeMidgardNativeTxFullFromCanonicalCbor(row[TxColumns.TX]),
    sourceKind: "normal",
  }));
  const normalSources = potentialCommitProgramSources(input.base, [
    ...forced,
    ...ordinary,
  ]);
  const normalSourceBytes = potentialProgramSourceBytes(normalSources);
  const events: CommitFrameEventContribution[] = [];
  for (const row of input.deposits) {
    const id = row[DepositsDB.Columns.ID];
    events.push({
      kind: "deposits",
      inclusionTimeMs: row[DepositsDB.Columns.INCLUSION_TIME].getTime(),
      bytes:
        daEntryTupleBytes(id, row[DepositsDB.Columns.INFO]) +
        additiveEventTraceBytes(eventKey(id, "deposits"), "Deposit", false),
    });
  }
  for (const row of input.withdrawals) {
    const id = row[WithdrawalsDB.Columns.ID];
    events.push({
      kind: "withdrawals",
      inclusionTimeMs: row[WithdrawalsDB.Columns.INCLUSION_TIME].getTime(),
      bytes:
        daEntryTupleBytes(
          id,
          withdrawalValueBytesUpperBound(
            row[WithdrawalsDB.Columns.RAW_EVENT_INFO],
          ),
        ) +
        additiveEventTraceBytes(
          eventKey(id, "withdrawals"),
          "Withdrawal",
          false,
        ),
    });
  }
  for (const [index, row] of input.forced.entries()) {
    const id = row[ForcedTransactionsDB.Columns.TX_ORDER_ID];
    const transaction = forced[index]!;
    events.push({
      kind: "forcedTransactions",
      inclusionTimeMs:
        row[ForcedTransactionsDB.Columns.INCLUSION_TIME].getTime(),
      bytes:
        daEntryTupleBytes(
          id,
          forcedInclusionValueBytesUpperBound(transaction),
        ) +
        daEntryTupleBytes(
          id,
          row[ForcedTransactionsDB.Columns.NATIVE_TX_CBOR],
        ) +
        additiveEventTraceBytes(
          eventKey(id, "forcedTransactions"),
          "ForcedTransaction",
          true,
        ),
    });
  }
  const ordinaryBytes = input.candidates.map((row, index) => {
    const id = row[TxColumns.TX_ID];
    return (
      ordinarySourceBytes(id, row[TxColumns.TX], input.profile) +
      additiveEventTraceBytes(
        { L2TransactionEventKey: { tx_id: id.toString("hex") } },
        "L2Transaction",
        true,
      ) +
      potentialEventProgramBytes(ordinary[index]!, normalSourceBytes)
    );
  });
  return { events, ordinaryBytes, fixedBytes: additiveDaFixedBytes() };
};

export const planAdditiveCommitDa = (input: {
  readonly window: Extract<CommitEventWindow, { endTimeMs: number }>;
  readonly base: readonly MinimalEntry[];
  readonly candidates: readonly EntryWithTimeStamp[];
  readonly profile: MidgardConsensusProfile;
  readonly maxInnerBytes: number;
}) =>
  Effect.gen(function* () {
    const end = new Date(input.window.endTimeMs);
    const deposits = yield* DepositsDB.retrievePendingHeaderEntriesUpTo(end);
    const withdrawals =
      yield* WithdrawalsDB.retrievePendingHeaderEntriesUpTo(end);
    const forced =
      yield* ForcedTransactionsDB.retrievePendingHeaderEntriesUpTo(end);
    if (
      deposits.length !== input.window.counts.deposits ||
      withdrawals.length !== input.window.counts.withdrawals ||
      forced.length !== input.window.counts.forcedTransactions
    ) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "DA source set differs from the complete-instant count authority",
        }),
      );
    }
    const contributions = yield* Effect.try({
      try: () =>
        commitDaSourceContributions({
          ...input,
          deposits,
          withdrawals,
          forced,
        }),
      catch: (cause) =>
        new CommitWorkerInvariantError({
          message: `Cannot derive conservative DA source budget: ${String(cause)}`,
        }),
    });
    return selectAdditiveCommitFrame({
      ...contributions,
      initialWindow: input.window,
      maxInnerBytes: input.maxInnerBytes,
    });
  });
