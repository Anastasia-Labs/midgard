/**
 * Commit-stage rejection codes and the resolution of per-transaction deltas for commit.
 */

import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Metric, Option } from "effect";

import * as CekProgramMaterialDB from "../database/cekProgramMaterial.js";
import * as DepositsDB from "../database/deposits.js";
import * as MempoolDB from "../database/mempool.js";
import * as MempoolLedgerDB from "../database/mempoolLedger.js";
import * as MempoolTxDeltasDB from "../database/mempoolTxDeltas.js";
import * as ProcessedMempoolDB from "../database/processedMempool.js";
import * as TxRejectionsDB from "../database/txRejections.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import * as Tx from "../database/utils/tx.js";
import { Database } from "../services/index.js";
import { findSpentAndProducedUTxOs } from "../utils.js";

export const COMMIT_REJECT_CODE_DECODE_FAILED = "E_COMMIT_CBOR_DESERIALIZATION";

export const COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT =
  "E_COMMIT_WITHDRAWN_REFERENCE_INPUT";

export const COMMIT_REJECT_CODE_SAME_BLOCK_DEPOSIT_INPUT =
  "E_COMMIT_SAME_BLOCK_DEPOSIT_INPUT";

export const COMMIT_REJECT_CODE_FORCED_TRANSACTION_INPUT =
  "E_COMMIT_FORCED_TRANSACTION_INPUT";

export const COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT =
  "E_COMMIT_SPENDS_REJECTED_OUTPUT";

export type ResolvedTxDeltaForCommit =
  | {
      readonly _tag: "Decoded";
      readonly spent: readonly Buffer[];
      readonly produced: readonly Ledger.MinimalEntry[];
    }
  | {
      readonly _tag: "Rejected";
      readonly rejection: TxRejectionsDB.EntryNoTimestamp;
    };

export const commitTxDeltaCacheHitCounter = Metric.counter(
  "commit_tx_delta_cache_hit_total",
  {
    description:
      "Commit candidates resolved from the best-effort mempool tx-delta cache",
    bigint: true,
    incremental: true,
  },
);

export const commitTxDeltaFallbackDecodedCounter = Metric.counter(
  "commit_tx_delta_fallback_decoded_total",
  {
    description:
      "Commit candidates successfully decoded from canonical CBOR after a tx-delta cache miss",
    bigint: true,
    incremental: true,
  },
);

export const resolveTxDeltaForCommit = (
  entry: Tx.EntryWithTimeStamp,
  existingDelta: MempoolTxDeltasDB.TxDelta | undefined,
): Effect.Effect<ResolvedTxDeltaForCommit, never> =>
  Effect.gen(function* () {
    if (existingDelta !== undefined) {
      return {
        _tag: "Decoded",
        spent: existingDelta.spent.map((outRef) => Buffer.from(outRef)),
        produced: existingDelta.produced.map((deltaEntry) => ({
          [Ledger.Columns.OUTREF]: Buffer.from(
            deltaEntry[Ledger.Columns.OUTREF],
          ),
          [Ledger.Columns.OUTPUT]: Buffer.from(
            deltaEntry[Ledger.Columns.OUTPUT],
          ),
        })),
      };
    }

    const txId = entry[Tx.Columns.TX_ID];
    const txCbor = entry[Tx.Columns.TX];
    const decoded = yield* findSpentAndProducedUTxOs(txCbor, txId).pipe(
      Effect.either,
    );
    if (decoded._tag === "Left") {
      return {
        _tag: "Rejected",
        rejection: {
          [TxRejectionsDB.Columns.TX_ID]: Buffer.from(txId),
          [TxRejectionsDB.Columns.REJECT_CODE]:
            COMMIT_REJECT_CODE_DECODE_FAILED,
          [TxRejectionsDB.Columns.REJECT_DETAIL]: decoded.left.message,
        },
      };
    }

    return {
      _tag: "Decoded",
      spent: decoded.right.spent,
      produced: decoded.right.produced,
    };
  });

/** The admitted ledger effects of a transaction the commit stage rejected. */
export type CommitStageRejectedTxEffects = {
  readonly txId: Buffer;
  readonly spent: readonly Buffer[];
  readonly produced: readonly Ledger.MinimalEntry[];
};

/**
 * Resolves an outref (hex) against the block post-state: the output when it
 * is still unspent after the block, `null` when the block consumes it, and
 * `undefined` when the block's ledger never holds it.
 */
export type CommitStageInputPostState = (
  outRefHex: string,
) => Buffer | null | undefined;

export type CommitStageLedgerRevert = {
  readonly rejected: readonly CommitStageRejectedTxEffects[];
  readonly resolveInputPostState: CommitStageInputPostState;
};

/**
 * The block post-state over its base ledger and the outputs it inserts. Every
 * input of the reverted set resolves here, including the committed inputs of
 * pending descendants the revert rejects with it.
 */
export const commitStageInputPostState =
  ({
    baseLedgerOutputs,
    insertedOutputs,
    spentOutRefHexes,
  }: {
    readonly baseLedgerOutputs: ReadonlyMap<string, Buffer>;
    readonly insertedOutputs: ReadonlyMap<string, Buffer>;
    readonly spentOutRefHexes: ReadonlySet<string>;
  }): CommitStageInputPostState =>
  (outRefHex) =>
    spentOutRefHexes.has(outRefHex)
      ? null
      : (insertedOutputs.get(outRefHex) ?? baseLedgerOutputs.get(outRefHex));

const hex = (value: Buffer) => value.toString("hex");

const ledgerRow = (
  outRef: Buffer,
  output: Buffer,
): Effect.Effect<MempoolLedgerDB.EntryNoTimeStamp, DatabaseError> =>
  Effect.try({
    try: () => ({
      [MempoolLedgerDB.Columns.TX_ID]: Buffer.from(
        decodeMidgardSpendInputItem(outRef).txId,
      ),
      [MempoolLedgerDB.Columns.OUTREF]: Buffer.from(outRef),
      [MempoolLedgerDB.Columns.OUTPUT]: Buffer.from(output),
      [MempoolLedgerDB.Columns.ADDRESS]: encodeMidgardAddressText(
        decodeMidgardTxOutput(output).address,
      ),
      [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
    }),
    catch: (cause) =>
      new DatabaseError({
        table: MempoolLedgerDB.tableName,
        message: "A restored ledger output is not canonical",
        cause: { outRef: hex(outRef), cause },
      }),
  });

/** A deposit output is restored as the deposit's own row, so a deposit
 * projected past the committed tip stays accounted to that deposit. A deposit's
 * ledger outref is its event id. The caller unconsumes the deposit. */
const withDepositOrigin = (row: MempoolLedgerDB.EntryNoTimeStamp) =>
  Effect.gen(function* () {
    const outRef = decodeMidgardSpendInputItem(
      row[MempoolLedgerDB.Columns.OUTREF],
    );
    const deposit = yield* DepositsDB.retrieveByEventId(
      Buffer.from(
        aikenSerialisedPlutusDataCbor(
          SDK.outputReferenceToPlutusDataCbor({
            txHash: Buffer.from(outRef.txId).toString("hex"),
            outputIndex: outRef.outputIndex,
          }),
        ),
        "hex",
      ),
    );
    if (Option.isNone(deposit)) return row;
    const entry = yield* DepositsDB.toMempoolLedgerEntry(deposit.value);
    if (
      !entry[Ledger.Columns.OUTREF].equals(
        row[MempoolLedgerDB.Columns.OUTREF],
      ) ||
      !entry[Ledger.Columns.OUTPUT].equals(row[MempoolLedgerDB.Columns.OUTPUT])
    )
      return row;
    return {
      [MempoolLedgerDB.Columns.TX_ID]: entry[Ledger.Columns.TX_ID],
      [MempoolLedgerDB.Columns.OUTREF]: entry[Ledger.Columns.OUTREF],
      [MempoolLedgerDB.Columns.OUTPUT]: entry[Ledger.Columns.OUTPUT],
      [MempoolLedgerDB.Columns.ADDRESS]: entry[Ledger.Columns.ADDRESS],
      [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: entry.source_event_id,
    };
  });

/** Pending transactions outside `excluded`, with their spends and outputs. An
 * undecodable one spends nothing this node can name; the commit stage rejects
 * it on its own. */
const loadPendingTxEffects = (excluded: ReadonlySet<string>) =>
  Effect.gen(function* () {
    const entries = [
      ...(yield* Tx.retrieveAllEntries(MempoolDB.tableName)),
      ...(yield* Tx.retrieveAllEntries(ProcessedMempoolDB.tableName)),
    ].filter((entry) => !excluded.has(hex(entry[Tx.Columns.TX_ID])));
    const deltas = yield* MempoolTxDeltasDB.retrieveByTxIds(
      entries.map((entry) => entry[Tx.Columns.TX_ID]),
    );
    const pending: CommitStageRejectedTxEffects[] = [];
    for (const entry of entries) {
      const txId = entry[Tx.Columns.TX_ID];
      const resolved = yield* resolveTxDeltaForCommit(
        entry,
        deltas.get(hex(txId)),
      );
      if (resolved._tag === "Decoded")
        pending.push({
          txId,
          spent: resolved.spent,
          produced: resolved.produced,
        });
    }
    return pending;
  });

/**
 * Reverts the admitted `mempool_ledger` effects of commit-stage rejected
 * transactions, inside the caller's transaction. Every pending transaction
 * that spends one of their outputs, directly or transitively, is rejected with
 * them. Their outputs are deleted, and each input they spent from outside the
 * rejected set comes back when the block leaves it unspent, or when a pending
 * transaction produced it. Returns whether `mempool_ledger` changed.
 */
export const revertCommitStageRejectedLedgerEffects = ({
  rejected,
  resolveInputPostState,
}: CommitStageLedgerRevert): Effect.Effect<boolean, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (rejected.length === 0) return false;
    const sql = yield* SqlClient.SqlClient;
    const reverted = [...rejected];
    const revertedIds = new Set(reverted.map(({ txId }) => hex(txId)));
    const producerByOutRef = new Map(
      reverted.flatMap((tx) =>
        tx.produced.map(
          (entry) => [hex(entry[Ledger.Columns.OUTREF]), tx.txId] as const,
        ),
      ),
    );
    const spentByReverted = new Set(
      reverted.flatMap(({ spent }) => spent.map(hex)),
    );
    const present = new Set(
      (yield* MempoolLedgerDB.retrieveByTxOutRefs(
        [...producerByOutRef.keys()].map((key) => Buffer.from(key, "hex")),
      )).map((row) => hex(row[MempoolLedgerDB.Columns.OUTREF])),
    );
    // Pending transactions are needed only when one spent an output of the
    // rejected set (it is neither in the ledger nor spent inside the set), or
    // may have produced an input the block does not hold.
    const needsPending =
      [...producerByOutRef.keys()].some(
        (key) => !present.has(key) && !spentByReverted.has(key),
      ) ||
      [...spentByReverted].some(
        (key) =>
          !producerByOutRef.has(key) &&
          resolveInputPostState(key) === undefined,
      );
    const pending = needsPending
      ? yield* loadPendingTxEffects(revertedIds)
      : [];
    const cascaded: TxRejectionsDB.EntryNoTimestamp[] = [];
    for (let changed = true; changed; ) {
      changed = false;
      for (const tx of pending) {
        if (revertedIds.has(hex(tx.txId))) continue;
        const input = tx.spent.find((outRef) =>
          producerByOutRef.has(hex(outRef)),
        );
        if (input === undefined) continue;
        cascaded.push({
          [TxRejectionsDB.Columns.TX_ID]: Buffer.from(tx.txId),
          [TxRejectionsDB.Columns.REJECT_CODE]:
            COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT,
          [TxRejectionsDB.Columns.REJECT_DETAIL]:
            `Transaction spends L2 outref ${hex(
              input,
            )}, an output of transaction ${hex(
              producerByOutRef.get(hex(input))!,
            )}, which was rejected at commit`,
        });
        reverted.push(tx);
        revertedIds.add(hex(tx.txId));
        for (const outRef of tx.spent) spentByReverted.add(hex(outRef));
        for (const entry of tx.produced)
          producerByOutRef.set(hex(entry[Ledger.Columns.OUTREF]), tx.txId);
        changed = true;
      }
    }

    const produced = [...producerByOutRef.keys()].map((key) =>
      Buffer.from(key, "hex"),
    );
    const deleted =
      produced.length === 0
        ? []
        : yield* sql<{ readonly outref: Buffer }>`DELETE FROM ${sql(
            MempoolLedgerDB.tableName,
          )}
          WHERE ${sql(MempoolLedgerDB.Columns.OUTREF)} IN ${sql.in(produced)}
          RETURNING ${sql(MempoolLedgerDB.Columns.OUTREF)}`;

    const remaining = pending.filter(({ txId }) => !revertedIds.has(hex(txId)));
    const spentByRemaining = new Set(
      remaining.flatMap(({ spent }) => spent.map(hex)),
    );
    const producedByRemaining = new Map(
      remaining.flatMap(({ produced }) =>
        produced.map(
          (entry) =>
            [
              hex(entry[Ledger.Columns.OUTREF]),
              entry[Ledger.Columns.OUTPUT],
            ] as const,
        ),
      ),
    );
    const restored: MempoolLedgerDB.EntryNoTimeStamp[] = [];
    for (const key of spentByReverted) {
      if (producerByOutRef.has(key) || spentByRemaining.has(key)) continue;
      const postState = resolveInputPostState(key);
      const output =
        postState === undefined ? producedByRemaining.get(key) : postState;
      if (output === undefined || output === null) continue;
      restored.push(
        yield* withDepositOrigin(
          yield* ledgerRow(Buffer.from(key, "hex"), output),
        ),
      );
    }
    const inserted =
      restored.length === 0
        ? []
        : yield* sql<{ readonly outref: Buffer }>`INSERT INTO ${sql(
            MempoolLedgerDB.tableName,
          )} ${sql.insert(restored)}
          ON CONFLICT (${sql(MempoolLedgerDB.Columns.OUTREF)}) DO NOTHING
          RETURNING ${sql(MempoolLedgerDB.Columns.OUTREF)}`;
    // Admission marked a spent deposit consumed; its output is back.
    yield* DepositsDB.unconsumeByEventIds(
      restored.flatMap((row) => {
        const eventId = row[MempoolLedgerDB.Columns.SOURCE_EVENT_ID];
        return eventId === null ? [] : [eventId];
      }),
    );

    if (cascaded.length > 0) {
      const cascadedIds = cascaded.map(
        (entry) => entry[TxRejectionsDB.Columns.TX_ID],
      );
      yield* MempoolDB.clearTxs(cascadedIds);
      yield* ProcessedMempoolDB.clearTxs(cascadedIds);
      yield* TxRejectionsDB.insertMany(cascaded);
      yield* CekProgramMaterialDB.releaseAdmissionOwnership(cascadedIds);
    }
    return deleted.length > 0 || inserted.length > 0;
  }).pipe(
    sqlErrorToDatabaseError(
      MempoolLedgerDB.tableName,
      "Failed to revert commit-stage rejected ledger effects",
    ),
  );

/**
 * Applies a commit-stage rejection as one database mutation: the rejected
 * transactions leave both pending tables and their ledger effects are
 * reverted. CEK admission ownership is released only if all of it commits.
 * Returns whether `mempool_ledger` changed.
 */
export const persistCommitStageRejectedTransactions = ({
  rejectedTxHashes,
  rejectionEntries,
  ledgerRevert,
}: {
  readonly rejectedTxHashes: readonly Buffer[];
  readonly rejectionEntries: readonly TxRejectionsDB.EntryNoTimestamp[];
  readonly ledgerRevert: CommitStageLedgerRevert;
}): Effect.Effect<boolean, DatabaseError, Database> => {
  if (rejectedTxHashes.length === 0) return Effect.succeed(false);
  return Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* MempoolDB.clearTxs([...rejectedTxHashes]);
        yield* ProcessedMempoolDB.clearTxs([...rejectedTxHashes]);
        yield* TxRejectionsDB.insertMany(rejectionEntries);
        const reverted =
          yield* revertCommitStageRejectedLedgerEffects(ledgerRevert);
        yield* CekProgramMaterialDB.releaseAdmissionOwnership(rejectedTxHashes);
        return reverted;
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      MempoolDB.tableName,
      "Failed to persist commit-stage transaction rejections",
    ),
  );
};
