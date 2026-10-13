import {
  encodeMidgardCekProgramMaterialDaValue,
  mergeMidgardCekProgramMaterialSidecars,
} from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  type DaPayloadEmissionMode,
  daPayloadFramePressureStage,
  maxDaPayloadInnerBytes,
  projectDaPayloadSizes,
} from "@al-ft/midgard-core/da-payload-sizing";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { readDaHardeningConfig } from "../../da/hardening-config.js";
import {
  DepositsDB,
  ForcedTransactionsDB,
  PendingBlockFinalizationsDB,
  TxAdmissionsDB,
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
} from "../../mpf/index.js";
import type { Database } from "../../services/index.js";
import {
  type CommitDaFrameMeasurement,
  DA_PAYLOAD_UPPER_BOUND_HEADER,
  DA_PAYLOAD_UPPER_BOUND_HEADER_HASH,
} from "../utils/commit-block-planner.js";
import { measureDaPayloadPrefixes } from "./submission.measure-da-prefixes.js";

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

export type DaPayloadBlockContent = {
  readonly includedDepositEntries: readonly DepositsDB.Entry[];
  readonly includedForcedTransactionEntries: readonly ForcedTransactionsDB.Entry[];
  readonly includedWithdrawalEntries: readonly WithdrawalsDB.Entry[];
  readonly processedMempoolTxs: readonly TxTable.EntryWithTimeStamp[];
  readonly transitionTraceMembers: readonly RetainedTransitionTraceMember[];
  readonly eventToStepMembers: readonly RetainedEventToStepMember[];
  readonly validationTraceMembers: readonly RetainedValidationTraceMember[];
};

/** Exact inner DaPayloadV1 bytes of the block the node submits. */
export const commitDaPayloadForSizing = ({
  headerHash,
  header,
  includedDepositEntries,
  includedForcedTransactionEntries,
  includedWithdrawalEntries,
  processedMempoolTxs,
  transitionTraceMembers,
  eventToStepMembers,
  validationTraceMembers,
  cekProgramMaterial,
}: DaPayloadBlockContent & {
  readonly headerHash: string;
  readonly header: SDK.Header;
  readonly cekProgramMaterial: readonly SDK.DaPayloadEntry[];
}): Effect.Effect<SDK.DaPayload, DatabaseError> =>
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
      validation_trace_witnesses: [],
    };
    return {
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
    };
  });

const daPayloadInnerBytes = (
  content: Parameters<typeof commitDaPayloadForSizing>[0],
) =>
  commitDaPayloadForSizing(content).pipe(Effect.map(SDK.daPayloadEncodedSize));

export const DA_FRAME_REFUSAL_MESSAGE =
  "Refusing to prepare or submit a block whose DA payload cannot fit the V1 submit frame";

export const assertPreSubmitDaPayloadSize = ({
  headerHash,
  header,
  envelopeMode = readDaHardeningConfig().envelopeMode,
  ...content
}: DaPayloadBlockContent & {
  readonly headerHash: string;
  readonly header: SDK.Header;
  readonly cekProgramMaterial: readonly SDK.DaPayloadEntry[];
  readonly envelopeMode?: DaPayloadEmissionMode;
}): Effect.Effect<number, DatabaseError> =>
  Effect.gen(function* () {
    const encodedBytes = yield* daPayloadInnerBytes({
      headerHash,
      header,
      ...content,
    });
    const projection = projectDaPayloadSizes(encodedBytes, envelopeMode);
    const effectiveInnerLimit = maxDaPayloadInnerBytes(envelopeMode);
    const utilisation = (encodedBytes / effectiveInnerLimit).toFixed(4);
    const exceedsFrame = (innerBytes: number): boolean => {
      const sizes = projectDaPayloadSizes(innerBytes, envelopeMode);
      return (
        innerBytes > effectiveInnerLimit ||
        sizes.storedBytesUpperBound > DA_TRANSPORT_LIMITS.maxPayloadBytes ||
        sizes.requestBytesUpperBound > DA_TRANSPORT_LIMITS.maxPayloadBytes
      );
    };
    if (exceedsFrame(encodedBytes)) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message: DA_FRAME_REFUSAL_MESSAGE,
          cause: `header_hash=${headerHash},envelope_mode=${envelopeMode},inner_bytes=${encodedBytes.toString()},stored_bytes_upper_bound=${projection.storedBytesUpperBound.toString()},request_bytes_upper_bound=${projection.requestBytesUpperBound.toString()},effective_inner_limit=${effectiveInnerLimit.toString()},max_frame_bytes=${DA_TRANSPORT_LIMITS.maxPayloadBytes.toString()},frame_utilisation=${utilisation}`,
        }),
      );
    }
    yield* Effect.logInfo(
      `da_payload_pre_submit_inner_bytes=${encodedBytes.toString()} da_payload_stored_bytes_upper_bound=${projection.storedBytesUpperBound.toString()} da_payload_request_bytes_upper_bound=${projection.requestBytesUpperBound.toString()} da_payload_effective_inner_limit=${effectiveInnerLimit.toString()} da_payload_envelope_mode=${envelopeMode} da_payload_frame_limit_bytes=${DA_TRANSPORT_LIMITS.maxPayloadBytes.toString()} da_payload_frame_utilisation=${utilisation}`,
    );
    const pressureStage = daPayloadFramePressureStage(
      encodedBytes,
      effectiveInnerLimit,
    );
    if (pressureStage > 0) {
      const headroomBytes = effectiveInnerLimit - encodedBytes;
      yield* Effect.logWarning(
        `da_payload_frame_pressure=high header_hash=${headerHash} da_payload_frame_utilisation=${utilisation} da_payload_pressure_stage_percent=${pressureStage.toString()} da_payload_pre_submit_inner_bytes=${encodedBytes.toString()} da_payload_effective_inner_limit=${effectiveInnerLimit.toString()} da_payload_headroom_bytes=${headroomBytes.toString()}`,
      );
    }
    return encodedBytes;
  });

/**
 * Measures a built block before its header exists: the same content the
 * pre-submit check sizes, under the longest-encoding header, so a block this
 * admits is admitted there. Content the submit path would refuse for another
 * reason is left unmeasured, to be refused by that path as before.
 */
export const measureCommitDaPayloadUpperBound = ({
  rejectedTxIds,
  identityContext = Buffer.alloc(0),
  ...content
}: DaPayloadBlockContent & {
  readonly rejectedTxIds: readonly Buffer[];
  readonly identityContext?: Buffer;
}): Effect.Effect<CommitDaFrameMeasurement | undefined, never, Database> =>
  Effect.gen(function* () {
    const sidecars = yield* TxAdmissionsDB.retrieveProgramMaterialSidecars(
      content.processedMempoolTxs.map((entry) => entry[TxColumns.TX_ID]),
    );
    const byId = new Map(
      sidecars.map((entry) => [entry.txId.toString("hex"), entry.sidecarCbor]),
    );
    if (
      sidecars.length !== content.processedMempoolTxs.length ||
      byId.size !== sidecars.length
    )
      return yield* Effect.fail(
        "DA prefix program material identities are incomplete or duplicated",
      );
    const ordinarySidecars = yield* Effect.try(() =>
      content.processedMempoolTxs.map((entry) => {
        const sidecar = byId.get(entry[TxColumns.TX_ID].toString("hex"));
        if (sidecar === undefined)
          throw new Error("DA prefix program material identity is missing");
        return sidecar;
      }),
    );
    const forcedSidecars = yield* Effect.try(() =>
      forcedProgramMaterialSidecars(content.includedForcedTransactionEntries),
    );
    const payload = yield* commitDaPayloadForSizing({
      ...content,
      headerHash: DA_PAYLOAD_UPPER_BOUND_HEADER_HASH,
      header: DA_PAYLOAD_UPPER_BOUND_HEADER,
      cekProgramMaterial: [],
    });
    return yield* Effect.try(() =>
      measureDaPayloadPrefixes({
        payload,
        content,
        ordinarySidecars,
        forcedSidecars,
        identityContext,
        rejectedTxIds,
      }),
    );
  }).pipe(
    Effect.catchAll((cause) =>
      Effect.as(
        Effect.logWarning(
          `commit_da_frame_measurement=unavailable cause=${formatUnknownError(cause)}`,
        ),
        undefined,
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
