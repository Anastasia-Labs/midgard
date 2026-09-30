import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Duration, Effect, Metric } from "effect";

import { readDaHardeningConfig } from "../../da/hardening-config.js";
import {
  DaPayloadsDB,
  ForcedTransactionsDB,
  PendingBlockFinalizationsDB,
} from "../../database/index.js";
import { DatabaseError } from "../../database/utils/common.js";
import { encodeTransactionRootValue, type MpfError } from "../../mpf/index.js";
import {
  bufferEntry,
  computeDaPayloadRoots,
  daPayloadBytesEnvelopeGauge,
  daPayloadBytesUncompressedGauge,
  daPayloadCompressDurationTimer,
  daPayloadCompressionRatioGauge,
  expectedCounts,
  journalCekProgramMaterial,
  type PayloadUtxoEntry,
  sortedEntries,
} from "./da-payload.compute-da-payload-roots.js";
import {
  decodeHeader,
  verifyPayloadCommitments,
} from "./da-payload.verify-payload-commitments.js";

export const buildDaPayloadInsert = ({
  record,
  utxos,
  envelope,
}: {
  readonly record: PendingBlockFinalizationsDB.Record;
  readonly utxos: readonly PayloadUtxoEntry[];
  readonly envelope?: {
    readonly mode: "identity" | "zstd";
    readonly zstdLevel: number;
  };
}): Effect.Effect<DaPayloadsDB.InsertInput, DatabaseError | MpfError> =>
  Effect.gen(function* () {
    const payloadUtxos = utxos;
    const header = yield* decodeHeader(record);
    const counts = expectedCounts(record);
    const profileId =
      record[PendingBlockFinalizationsDB.Columns.CONSENSUS_PROFILE_ID];
    if (profileId !== MIDGARD_CONSENSUS_PROFILE.profileId) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to build DA payload for an unknown consensus profile",
          cause: `profile=${String(profileId)}`,
        }),
      );
    }
    const forcedMembers = yield* Effect.try({
      try: () =>
        record.forcedTransactionMembers.map((member) => ({
          key: Buffer.from(
            member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
          ),
          ...ForcedTransactionsDB.decodeForcedTransactionJournalMember(
            member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
          ),
        })),
      catch: (cause) =>
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to build V1 DA from a non-canonical ForcedTransactionJournalMemberV1",
          cause,
        }),
    });
    const cekProgramMaterial = yield* Effect.try({
      try: () => journalCekProgramMaterial(record, forcedMembers),
      catch: (cause) =>
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to build V1 DA without exact journaled CEK program material",
          cause,
        }),
    });
    const transactionSources = record.txMembers.map((member) =>
      bufferEntry(
        member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
        encodeTransactionRootValue(
          member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
          MIDGARD_CONSENSUS_PROFILE,
        ),
      ),
    );
    const commonBody = {
      header_hash:
        record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex"),
      utxos: sortedEntries(
        payloadUtxos.map((entry) => bufferEntry(entry.outref, entry.output)),
      ),
      withdrawals: sortedEntries(
        record.withdrawalMembers.map((member) =>
          bufferEntry(
            member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
            member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
          ),
        ),
      ),
      forced_transactions: sortedEntries(
        forcedMembers.map((member) =>
          bufferEntry(member.key, member.sourceValueCbor),
        ),
      ),
      transactions: sortedEntries(transactionSources),
      deposits: sortedEntries(
        record.depositMembers.map((member) =>
          bufferEntry(
            member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
            member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
          ),
        ),
      ),
      transition_trace: sortedEntries(
        record.transitionTraceMembers.map((member) =>
          bufferEntry(
            member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
            member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
          ),
        ),
      ),
      event_to_step: sortedEntries(
        record.eventToStepMembers.map((member) =>
          bufferEntry(
            member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
            member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
          ),
        ),
      ),
      validation_trace_witnesses: sortedEntries(
        record.validationTraceWitnessMembers.map((member) =>
          bufferEntry(
            member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
            member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
          ),
        ),
      ),
    } as const;
    const payload: SDK.DaPayload = {
      version: SDK.DA_PAYLOAD_VERSION,
      block_body: {
        ...commonBody,
        header,
        transaction_preimages: sortedEntries(
          record.txMembers.map((member) =>
            bufferEntry(
              member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
              member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
            ),
          ),
        ),
        forced_transaction_preimages: sortedEntries(
          forcedMembers.map((member) =>
            bufferEntry(member.key, member.canonicalTransactionCbor),
          ),
        ),
        cek_program_material: [...cekProgramMaterial],
        validation_traces: sortedEntries(
          record.validationTraceMembers.map((member) =>
            bufferEntry(
              member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
              member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
            ),
          ),
        ),
        counts,
      },
    };
    const roots = yield* computeDaPayloadRoots(payload);
    yield* verifyPayloadCommitments({ record, header, payload, roots });
    const innerPayloadCbor = SDK.encodeDaPayload(payload);
    const envelopeConfig =
      envelope ??
      (() => {
        const config = readDaHardeningConfig();
        return { mode: config.envelopeMode, zstdLevel: config.zstdLevel };
      })();
    const compressionStartedAt = Date.now();
    const payloadCbor = yield* Effect.tryPromise({
      try: () =>
        wrapDaPayload(innerPayloadCbor, {
          mode: envelopeConfig.mode,
          zstdLevel: envelopeConfig.zstdLevel,
        }),
      catch: (cause) =>
        new DatabaseError({
          table: DaPayloadsDB.tableName,
          message: "Failed to encode canonical V1 DA payload envelope",
          cause,
        }),
    });
    yield* daPayloadBytesUncompressedGauge(
      Effect.succeed(innerPayloadCbor.length),
    );
    yield* daPayloadBytesEnvelopeGauge(Effect.succeed(payloadCbor.length));
    yield* daPayloadCompressionRatioGauge(
      Effect.succeed(innerPayloadCbor.length / payloadCbor.length),
    );
    yield* Metric.update(
      daPayloadCompressDurationTimer,
      Duration.millis(Date.now() - compressionStartedAt),
    );
    return {
      [DaPayloadsDB.Columns.HEADER_HASH]:
        record[PendingBlockFinalizationsDB.Columns.HEADER_HASH],
      [DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID]: profileId,
      [DaPayloadsDB.Columns.VERSION]: 1 as const,
      [DaPayloadsDB.Columns.PAYLOAD_CBOR]: payloadCbor,
      [DaPayloadsDB.Columns.PAYLOAD_SHA256]: Buffer.from(
        SDK.daPayloadHashHex(payloadCbor),
        "hex",
      ),
      [DaPayloadsDB.Columns.UTXOS_ROOT]: roots.utxosRoot,
      [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]:
        roots.forcedTransactionsRoot,
      [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: roots.transactionsRoot,
      [DaPayloadsDB.Columns.DEPOSITS_ROOT]: roots.depositsRoot,
      [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: roots.withdrawalsRoot,
      [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]: roots.transitionTraceRoot,
      [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]: roots.eventToStepRoot,
      [DaPayloadsDB.Columns.VALIDATION_TRACES_ROOT]: roots.validationTracesRoot,
      [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: counts.withdrawalCount,
      [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]:
        counts.forcedTransactionCount,
      [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]: counts.l2TransactionCount,
      [DaPayloadsDB.Columns.DEPOSIT_COUNT]: counts.depositCount,
      [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: counts.totalEventCount,
      [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]: counts.transitionStepCount,
      [DaPayloadsDB.Columns.VALIDATION_TRACE_COUNT]:
        counts.validationTraceCount,
      [DaPayloadsDB.Columns.BLOCK_START_TIME]:
        record[PendingBlockFinalizationsDB.Columns.BLOCK_START_TIME],
      [DaPayloadsDB.Columns.BLOCK_END_TIME]:
        record[PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME],
    };
  });
