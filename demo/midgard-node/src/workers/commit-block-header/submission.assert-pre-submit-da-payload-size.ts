import {
  encodeMidgardCekProgramMaterialDaValue,
  mergeMidgardCekProgramMaterialSidecars,
} from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  type DaPayloadEmissionMode,
  maxDaPayloadInnerBytes,
  projectDaPayloadSizes,
} from "@al-ft/midgard-core/da-payload-sizing";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, Effect } from "effect";

import { readDaHardeningConfig } from "../../da/hardening-config.js";
import {
  DepositsDB,
  ForcedTransactionsDB,
  PendingBlockFinalizationsDB,
  TxUtils as TxTable,
  WithdrawalsDB,
} from "../../database/index.js";
import { DatabaseError } from "../../database/utils/common.js";
import { Columns as TxColumns } from "../../database/utils/tx.js";
import {
  encodeTransactionRootValue,
  type RetainedEventToStepMember,
  type RetainedTransitionTraceMember,
  type RetainedValidationTraceMember,
  type UtxoPayloadSizeAggregate,
} from "../../mpf/index.js";
import { Database } from "../../services/index.js";
import { TxSubmitError } from "../../transactions/utils.js";

export const COMMIT_STALE_OPERATOR_WALLET_VIEW_RETRIES = 1;

const daEntry = (key: Buffer, value: Buffer): SDK.DaPayloadEntry => [
  key.toString("hex"),
  value.toString("hex"),
];

export const forcedProgramMaterialSidecars = (
  entries: readonly ForcedTransactionsDB.Entry[],
): readonly Buffer[] =>
  entries.map((entry) => {
    const sidecar =
      entry[ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR];
    if (sidecar == null || sidecar.length === 0) {
      throw new Error(
        `V1 forced transaction ${entry[
          ForcedTransactionsDB.Columns.TX_ORDER_ID
        ].toString("hex")} is missing canonical CEK program material`,
      );
    }
    return Buffer.from(sidecar);
  });

export const daProgramMaterialFromSidecars = (
  sidecars: Iterable<Uint8Array>,
): readonly SDK.DaPayloadEntry[] =>
  mergeMidgardCekProgramMaterialSidecars(sidecars).map((entry) =>
    daEntry(
      Buffer.from(entry.root),
      encodeMidgardCekProgramMaterialDaValue(entry),
    ),
  );

export const assertPreSubmitDaPayloadSize = ({
  headerHash,
  header,
  utxoPayloadAggregate,
  includedDepositEntries,
  includedForcedTransactionEntries,
  includedWithdrawalEntries,
  processedMempoolTxs,
  transitionTraceMembers,
  eventToStepMembers,
  validationTraceMembers,
  cekProgramMaterial,
  envelopeMode = readDaHardeningConfig().envelopeMode,
}: {
  readonly headerHash: string;
  readonly header: SDK.Header;
  readonly utxoPayloadAggregate: UtxoPayloadSizeAggregate;
  readonly includedDepositEntries: readonly DepositsDB.Entry[];
  readonly includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
  readonly includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
  readonly processedMempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly transitionTraceMembers: readonly RetainedTransitionTraceMember[];
  readonly eventToStepMembers: readonly RetainedEventToStepMember[];
  readonly validationTraceMembers: readonly RetainedValidationTraceMember[];
  readonly cekProgramMaterial: readonly SDK.DaPayloadEntry[];
  readonly envelopeMode?: DaPayloadEmissionMode;
}): Effect.Effect<number, DatabaseError> =>
  Effect.gen(function* () {
    const transactionSources = yield* Effect.try({
      try: () =>
        processedMempoolTxs.map((entry) =>
          daEntry(
            entry[TxColumns.TX_ID],
            encodeTransactionRootValue(
              entry[TxColumns.TX],
              MIDGARD_CONSENSUS_PROFILE,
            ),
          ),
        ),
      catch: (cause) =>
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message: "Failed to derive V1 transaction sources for DA sizing",
          cause,
        }),
    });
    const forcedTransactionPreimages = includedForcedTransactionEntries.map(
      (entry) =>
        daEntry(
          entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
          entry[ForcedTransactionsDB.Columns.NATIVE_TX_CBOR],
        ),
    );
    const withdrawals = yield* Effect.forEach(
      includedWithdrawalEntries,
      (entry) => {
        const value = entry[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO];
        return value === null
          ? Effect.fail(
              new DatabaseError({
                table: PendingBlockFinalizationsDB.tableName,
                message:
                  "Cannot size DA payload for an unclassified withdrawal",
                cause: `withdrawal_event_id=${entry[
                  WithdrawalsDB.Columns.ID
                ].toString("hex")}`,
              }),
            )
          : Effect.succeed(
              daEntry(entry[WithdrawalsDB.Columns.ID], Buffer.from(value)),
            );
      },
    );
    const commonBody = {
      header_hash: headerHash,
      // The exact aggregate replaces this list in the sizing function.
      utxos: [],
      withdrawals,
      forced_transactions: includedForcedTransactionEntries.map((entry) =>
        daEntry(
          entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
          entry[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE],
        ),
      ),
      transactions: transactionSources,
      deposits: includedDepositEntries.map((entry) =>
        daEntry(entry[DepositsDB.Columns.ID], entry[DepositsDB.Columns.INFO]),
      ),
      transition_trace: transitionTraceMembers.map((entry) =>
        daEntry(entry.keyCbor, entry.valueCbor),
      ),
      event_to_step: eventToStepMembers.map((entry) =>
        daEntry(entry.keyCbor, entry.valueCbor),
      ),
      validation_trace_witnesses: validationTraceMembers.flatMap(
        (entry) => entry.witnesses,
      ),
    };
    const encodedBytes = SDK.daPayloadEncodedSizeFromUtxoAggregate(
      {
        version: SDK.DA_PAYLOAD_VERSION,
        block_body: {
          ...commonBody,
          header,
          transaction_preimages: processedMempoolTxs.map((entry) =>
            daEntry(entry[TxColumns.TX_ID], entry[TxColumns.TX]),
          ),
          forced_transaction_preimages: forcedTransactionPreimages,
          cek_program_material: [...cekProgramMaterial],
          validation_traces: validationTraceMembers.map((entry) =>
            daEntry(entry.keyCbor, entry.valueCbor),
          ),
          counts: {
            withdrawalCount: header.withdrawalCount,
            forcedTransactionCount: header.forcedTransactionCount,
            l2TransactionCount: header.l2TransactionCount,
            depositCount: header.depositCount,
            totalEventCount: header.totalEventCount,
            transitionStepCount: header.transitionStepCount,
            validationTraceCount: header.validationTraceCount,
          },
        },
      },
      utxoPayloadAggregate,
    );
    const projection = projectDaPayloadSizes(encodedBytes, envelopeMode);
    const effectiveInnerLimit = maxDaPayloadInnerBytes(envelopeMode);
    if (
      encodedBytes > effectiveInnerLimit ||
      projection.storedBytesUpperBound > DA_TRANSPORT_LIMITS.maxPayloadBytes ||
      projection.requestBytesUpperBound > DA_TRANSPORT_LIMITS.maxPayloadBytes
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to prepare or submit a block whose DA payload cannot fit the V1 submit frame",
          cause: `header_hash=${headerHash},envelope_mode=${envelopeMode},inner_bytes=${encodedBytes.toString()},stored_bytes_upper_bound=${projection.storedBytesUpperBound.toString()},request_bytes_upper_bound=${projection.requestBytesUpperBound.toString()},effective_inner_limit=${effectiveInnerLimit.toString()},max_frame_bytes=${DA_TRANSPORT_LIMITS.maxPayloadBytes.toString()},utxo_entry_count=${utxoPayloadAggregate.entryCount.toString()},utxo_encoded_tuple_bytes=${utxoPayloadAggregate.encodedTupleBytes.toString()}`,
        }),
      );
    }
    yield* Effect.logInfo(
      `da_payload_pre_submit_inner_bytes=${encodedBytes.toString()} da_payload_stored_bytes_upper_bound=${projection.storedBytesUpperBound.toString()} da_payload_request_bytes_upper_bound=${projection.requestBytesUpperBound.toString()} da_payload_effective_inner_limit=${effectiveInnerLimit.toString()} da_payload_envelope_mode=${envelopeMode} da_payload_frame_limit_bytes=${DA_TRANSPORT_LIMITS.maxPayloadBytes.toString()} utxo_entry_count=${utxoPayloadAggregate.entryCount.toString()} utxo_encoded_tuple_bytes=${utxoPayloadAggregate.encodedTupleBytes.toString()}`,
    );
    return encodedBytes;
  });

export class StaleOperatorWalletRetrySignal extends Data.TaggedError(
  "StaleOperatorWalletRetrySignal",
)<{
  readonly pendingHeaderHash: Buffer;
  readonly txSubmitError: TxSubmitError;
}> {}

export const maybeAbandonPreviousStaleAttempt = (
  previousPendingHeaderHash: Buffer | undefined,
  nextHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  previousPendingHeaderHash === undefined ||
  previousPendingHeaderHash.equals(nextHeaderHash)
    ? Effect.void
    : PendingBlockFinalizationsDB.markAbandoned(previousPendingHeaderHash).pipe(
        Effect.catchAll((cause) =>
          Effect.logWarning(
            `🔹 Previous stale pending journal was already cleared before retry (header=${previousPendingHeaderHash.toString(
              "hex",
            )}): ${formatUnknownError(cause)}`,
          ),
        ),
      );

export const assertCommitInputsWithinBlockEndTime = ({
  blockEndTimeMs,
  processedMempoolTxs = [],
  includedDepositEntries,
  includedForcedTransactionEntries,
  includedWithdrawalEntries,
}: {
  readonly blockEndTimeMs: number;
  readonly processedMempoolTxs?: readonly TxTable.EntryWithTimeStamp[];
  readonly includedDepositEntries: readonly DepositsDB.Entry[];
  readonly includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
  readonly includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
}): Effect.Effect<void, DatabaseError> =>
  Effect.gen(function* () {
    const violations = [
      ...processedMempoolTxs
        .filter(
          (entry) => entry[TxColumns.TIMESTAMPTZ].getTime() > blockEndTimeMs,
        )
        .map(
          (entry) =>
            `tx:${entry[TxColumns.TX_ID].toString("hex")}@${entry[
              TxColumns.TIMESTAMPTZ
            ].toISOString()}`,
        ),
      ...includedDepositEntries
        .filter(
          (entry) =>
            entry[DepositsDB.Columns.INCLUSION_TIME].getTime() > blockEndTimeMs,
        )
        .map(
          (entry) =>
            `deposit:${entry[DepositsDB.Columns.ID].toString("hex")}@${entry[
              DepositsDB.Columns.INCLUSION_TIME
            ].toISOString()}`,
        ),
      ...includedForcedTransactionEntries
        .filter(
          (entry) =>
            entry[ForcedTransactionsDB.Columns.INCLUSION_TIME].getTime() >
            blockEndTimeMs,
        )
        .map(
          (entry) =>
            `forced:${entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString(
              "hex",
            )}@${entry[
              ForcedTransactionsDB.Columns.INCLUSION_TIME
            ].toISOString()}`,
        ),
      ...includedWithdrawalEntries
        .filter(
          (entry) =>
            entry[WithdrawalsDB.Columns.INCLUSION_TIME].getTime() >
            blockEndTimeMs,
        )
        .map(
          (entry) =>
            `withdrawal:${entry[WithdrawalsDB.Columns.ID].toString(
              "hex",
            )}@${entry[WithdrawalsDB.Columns.INCLUSION_TIME].toISOString()}`,
        ),
    ];
    if (violations.length > 0) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to prepare a pending commit journal with inputs after the block end-time",
          cause: `block_end_time_ms=${blockEndTimeMs.toString()},violations=${violations.join(",")}`,
        }),
      );
    }
  });
