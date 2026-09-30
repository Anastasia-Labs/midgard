import { decodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import {
  requireCandidateHistory,
  withHistoryWrite,
} from "../services/event-history-producer.js";
import * as DepositsDB from "./deposits.js";
import * as ForcedTransactionsDB from "./forcedTransactions.js";
import {
  ACTIVE_STATUSES,
  Columns,
  depositsTableName,
  eventToStepTableName,
  forcedTransactionsTableName,
  MemberColumns,
  PENDING_BLOCK_FINALIZATION_VERSION,
  PendingBlockFinalizationReplayKind,
  type Row,
  Status,
  tableName,
  transitionTraceTableName,
  txsTableName,
  UtxoColumns,
  validationTracesTableName,
  validationTraceWitnessesTableName,
  WithdrawalMemberColumns,
  withdrawalsTableName,
} from "./pendingBlockFinalizations.columns.js";
import {
  forcedTransactionMemberEntry,
  retainedRootMemberEntry,
  withdrawalMemberEntry,
} from "./pendingBlockFinalizations.decode-pending-block-finalization-row.js";
import {
  exactBytes,
  exactDate,
  type PrepareInput,
} from "./pendingBlockFinalizations.parse-ledger-delta.js";
import {
  assertSameIdSet,
  depositMemberEntry,
  parsePendingBlockFinalization,
  txMemberEntry,
} from "./pendingBlockFinalizations.parse-pending-block-finalization.js";
import { withdrawalMemberToAssignment } from "./pendingBlockFinalizations.retrieve-finalized-missing-da-payloads.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import * as TxTable from "./utils/tx.js";
import * as WithdrawalsDB from "./withdrawals.js";

export const preparePendingSubmission = (
  input: PrepareInput,
  options?: {
    /**
     * Runs after the active-journal guard and inside the same SQL transaction
     * as the pending journal insert. Used by speculative submission to lock,
     * revalidate, and project its exact source snapshot atomically.
     */
    readonly beforeJournalInsert?: Effect.Effect<void, DatabaseError, Database>;
  },
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const pendingV1 = yield* Effect.try({
      try: () => {
        const pending = parsePendingBlockFinalization({
          version: PENDING_BLOCK_FINALIZATION_VERSION,
          metadata: input.metadata,
          replay:
            input.nativeMpfReplay === undefined
              ? {
                  kind: PendingBlockFinalizationReplayKind.LedgerDelta,
                  ledgerDelta: input.ledgerDelta,
                }
              : {
                  kind: PendingBlockFinalizationReplayKind.LedgerDeltaWithNativeMpf,
                  ledgerDelta: input.ledgerDelta,
                  nativeMpfReplay: input.nativeMpfReplay,
                },
        });
        exactBytes(
          input.headerHash,
          "PendingBlockFinalizationV1 headerHash",
          28,
        );
        exactBytes(input.headerCbor, "PendingBlockFinalizationV1 headerCbor");
        const blockEndTime = exactDate(
          input.blockEndTime,
          "PendingBlockFinalizationV1 blockEndTime",
        );
        if (
          blockEndTime.getTime() <= pending.metadata.blockStartTime.getTime()
        ) {
          throw new Error(
            "PendingBlockFinalizationV1 block window must be increasing",
          );
        }
        return pending;
      },
      catch: (cause) =>
        new DatabaseError({
          table: tableName,
          message:
            "Refusing to prepare a non-canonical PendingBlockFinalizationV1",
          cause,
        }),
    });
    const metadata = pendingV1.metadata;
    const ledgerDelta = pendingV1.replay.ledgerDelta;
    const nativeMpfReplay =
      pendingV1.replay.kind ===
      PendingBlockFinalizationReplayKind.LedgerDeltaWithNativeMpf
        ? pendingV1.replay.nativeMpfReplay
        : undefined;
    const deploymentMarker = metadata.deploymentMarker;
    yield* assertSameIdSet(
      tableName,
      "deposit",
      input.depositEventIds,
      input.depositEntries.map((entry) => entry[DepositsDB.Columns.ID]),
    );
    yield* assertSameIdSet(
      tableName,
      "forced transaction",
      input.forcedTransactionEventIds,
      input.forcedTransactionEntries.map(
        (entry) => entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
      ),
    );
    yield* assertSameIdSet(
      tableName,
      "withdrawal",
      input.withdrawalEventIds,
      input.withdrawalEntries.map((entry) => entry[WithdrawalsDB.Columns.ID]),
    );
    yield* assertSameIdSet(
      tableName,
      "mempool tx",
      input.mempoolTxIds,
      input.mempoolTxs.map((entry) => entry[TxTable.Columns.TX_ID]),
    );
    const depositMembers = input.depositEntries.map((entry, ordinal) =>
      depositMemberEntry(input.headerHash, entry, ordinal),
    );
    const forcedTransactionMembers = yield* Effect.forEach(
      input.forcedTransactionEntries,
      (entry, ordinal) =>
        forcedTransactionMemberEntry(input.headerHash, entry, ordinal),
    );
    const withdrawalMembers = yield* Effect.forEach(
      input.withdrawalEntries,
      (entry, ordinal) =>
        withdrawalMemberEntry(input.headerHash, entry, ordinal),
    );
    const programMaterialByTxId = new Map<string, Buffer>();
    for (const material of input.mempoolTxProgramMaterialSidecars ?? []) {
      const txIdHex = material.txId.toString("hex");
      if (programMaterialByTxId.has(txIdHex)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: txsTableName,
            message:
              "Refusing to prepare duplicate V1 transaction program material",
            cause: `tx_id=${txIdHex}`,
          }),
        );
      }
      yield* Effect.try({
        try: () => decodeMidgardCekProgramMaterialSidecar(material.sidecarCbor),
        catch: (cause) =>
          new DatabaseError({
            table: txsTableName,
            message:
              "Refusing to journal malformed V1 transaction program material",
            cause,
          }),
      });
      programMaterialByTxId.set(txIdHex, Buffer.from(material.sidecarCbor));
    }
    if (
      programMaterialByTxId.size !== input.mempoolTxs.length ||
      input.mempoolTxs.some(
        (entry) =>
          !programMaterialByTxId.has(
            entry[TxTable.Columns.TX_ID].toString("hex"),
          ),
      )
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: txsTableName,
          message:
            "V1 pending journal requires one canonical program-material sidecar per normal transaction",
          cause: `transactions=${input.mempoolTxs.length.toString()},sidecars=${programMaterialByTxId.size.toString()}`,
        }),
      );
    }
    const txMembers = input.mempoolTxs.map((entry, ordinal) =>
      txMemberEntry(
        input.headerHash,
        entry,
        ordinal,
        input.mempoolTxSourceTable,
        programMaterialByTxId.get(entry[TxTable.Columns.TX_ID].toString("hex")),
      ),
    );
    const transitionTraceMembers = input.transitionTraceMembers.map(
      (entry, ordinal) =>
        retainedRootMemberEntry({
          headerHash: input.headerHash,
          entry,
          ordinal,
          sourceTable: transitionTraceTableName,
          blockEndTime: input.blockEndTime,
        }),
    );
    const eventToStepMembers = input.eventToStepMembers.map((entry, ordinal) =>
      retainedRootMemberEntry({
        headerHash: input.headerHash,
        entry,
        ordinal,
        sourceTable: eventToStepTableName,
        blockEndTime: input.blockEndTime,
      }),
    );
    const validationTraceMembers = input.validationTraceMembers.map(
      (entry, ordinal) =>
        retainedRootMemberEntry({
          headerHash: input.headerHash,
          entry,
          ordinal,
          sourceTable: validationTracesTableName,
          blockEndTime: input.blockEndTime,
        }),
    );
    const validationTraceWitnessMembers =
      input.validationTraceWitnessMembers.map((entry, ordinal) =>
        retainedRootMemberEntry({
          headerHash: input.headerHash,
          entry,
          ordinal,
          sourceTable: validationTraceWitnessesTableName,
          blockEndTime: input.blockEndTime,
        }),
      );
    yield* withHistoryWrite(
      Effect.gen(function* () {
        const candidateHistory = yield* requireCandidateHistory;
        if (
          (Option.isSome(candidateHistory) &&
            input.preparedTxHash === undefined) ||
          (input.preparedTxHash !== undefined &&
            input.preparedTxHash.length !== 32)
        )
          return yield* Effect.fail(
            new DatabaseError({
              table: tableName,
              message:
                "Production pending journal requires the exact prepared transaction body hash",
              cause: input.headerHash.toString("hex"),
            }),
          );
        const activeRows = yield* sql<Row>`SELECT * FROM ${sql(tableName)}
          WHERE ${sql(Columns.STATUS)} IN ${sql.in(ACTIVE_STATUSES)}
          LIMIT 1`;
        const active = activeRows[0];
        if (
          active !== undefined &&
          !active[Columns.HEADER_HASH].equals(input.headerHash)
        ) {
          return yield* Effect.fail(
            new DatabaseError({
              table: tableName,
              message:
                "Refusing to prepare a new pending block while another active pending-finalization record exists",
              cause: `active_header_hash=${active[Columns.HEADER_HASH].toString(
                "hex",
              )},requested_header_hash=${input.headerHash.toString("hex")}`,
            }),
          );
        }
        if (options?.beforeJournalInsert !== undefined) {
          yield* options.beforeJournalInsert;
        }
        yield* WithdrawalsDB.assertClassificationSnapshots(
          withdrawalMembers.map(withdrawalMemberToAssignment),
        );
        if (active !== undefined) {
          yield* sql`DELETE FROM ${sql(tableName)}
            WHERE ${sql(Columns.HEADER_HASH)} = ${input.headerHash}
              AND ${sql(Columns.STATUS)} = ${Status.PendingSubmission}
              AND ${sql(Columns.INTENDED_TX_HASH)} IS NULL`;
        }
        yield* sql`DELETE FROM ${sql(tableName)}
          WHERE ${sql(Columns.HEADER_HASH)} = ${input.headerHash}
            AND ${sql(Columns.STATUS)} = ${Status.Abandoned}
            AND ${sql(Columns.SUBMITTED_TX_HASH)} IS NULL
      AND ${sql(Columns.INTENDED_TX_HASH)} IS NULL`;
        yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert({
          [Columns.HEADER_HASH]: input.headerHash,
          [Columns.HEADER_CBOR]: input.headerCbor,
          [Columns.FORMAT_VERSION]: pendingV1.version,
          [Columns.REPLAY_KIND]: pendingV1.replay.kind,
          [Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION]:
            deploymentMarker.schemaVersion,
          [Columns.DEPLOYMENT_MANIFEST_ID]: deploymentMarker.manifestId,
          [Columns.CONSENSUS_PROFILE_ID]: metadata.consensusProfileId,
          [Columns.PREPARED_TX_HASH]: input.preparedTxHash ?? null,
          [Columns.SUBMITTED_TX_HASH]: null,
          [Columns.STATE_QUEUE_LEASE_TOKEN]: metadata.stateQueueLeaseToken,
          [Columns.BASE_SNAPSHOT_ID]: metadata.baseSnapshotId,
          [Columns.BASE_TAIL_OUT_REF]: metadata.baseTailOutRef,
          [Columns.BASE_TAIL_HEADER_HASH]: metadata.baseTailHeaderHash,
          [Columns.BASE_TAIL_DATUM_CBOR]: metadata.baseTailDatumCbor,
          [Columns.BASE_UTXOS_ROOT]: metadata.baseRoots.utxosRoot,
          [Columns.BASE_FORCED_TRANSACTIONS_ROOT]:
            metadata.baseRoots.forcedTransactionsRoot,
          [Columns.BASE_TRANSACTIONS_ROOT]: metadata.baseRoots.transactionsRoot,
          [Columns.BASE_DEPOSITS_ROOT]: metadata.baseRoots.depositsRoot,
          [Columns.BASE_WITHDRAWALS_ROOT]: metadata.baseRoots.withdrawalsRoot,
          [Columns.BLOCK_START_TIME]: metadata.blockStartTime,
          [Columns.BLOCK_END_TIME]: input.blockEndTime,
          [Columns.EXPECTED_UTXOS_ROOT]: metadata.expectedRoots.utxosRoot,
          [Columns.EXPECTED_FORCED_TRANSACTIONS_ROOT]:
            metadata.expectedRoots.forcedTransactionsRoot,
          [Columns.EXPECTED_TRANSACTIONS_ROOT]:
            metadata.expectedRoots.transactionsRoot,
          [Columns.EXPECTED_DEPOSITS_ROOT]: metadata.expectedRoots.depositsRoot,
          [Columns.EXPECTED_WITHDRAWALS_ROOT]:
            metadata.expectedRoots.withdrawalsRoot,
          [Columns.EXPECTED_TRANSITION_TRACE_ROOT]:
            metadata.expectedRoots.transitionTraceRoot,
          [Columns.EXPECTED_EVENT_TO_STEP_ROOT]:
            metadata.expectedRoots.eventToStepRoot,
          [Columns.EXPECTED_VALIDATION_TRACES_ROOT]:
            metadata.expectedRoots.validationTracesRoot,
          [Columns.EXPECTED_WITHDRAWAL_COUNT]:
            metadata.expectedCounts.withdrawalCount,
          [Columns.EXPECTED_FORCED_TRANSACTION_COUNT]:
            metadata.expectedCounts.forcedTransactionCount,
          [Columns.EXPECTED_L2_TRANSACTION_COUNT]:
            metadata.expectedCounts.l2TransactionCount,
          [Columns.EXPECTED_DEPOSIT_COUNT]:
            metadata.expectedCounts.depositCount,
          [Columns.EXPECTED_TOTAL_EVENT_COUNT]:
            metadata.expectedCounts.totalEventCount,
          [Columns.EXPECTED_TRANSITION_STEP_COUNT]:
            metadata.expectedCounts.transitionStepCount,
          [Columns.EXPECTED_VALIDATION_TRACE_COUNT]:
            metadata.expectedCounts.validationTraceCount,
          [Columns.LEDGER_DELTA_SPENT]: JSON.stringify(
            input.ledgerDelta.spent.map((outref) => outref.toString("hex")),
          ),
          [Columns.LEDGER_DELTA_PRODUCED]: JSON.stringify(
            ledgerDelta.produced.map((entry) => ({
              outref: entry[UtxoColumns.OUTREF].toString("hex"),
              output: entry[UtxoColumns.OUTPUT].toString("hex"),
            })),
          ),
          [Columns.UTXO_PAYLOAD_ENTRY_COUNT]:
            input.utxoPayloadAggregate?.entryCount ?? null,
          [Columns.UTXO_PAYLOAD_ENCODED_TUPLE_BYTES]:
            input.utxoPayloadAggregate?.encodedTupleBytes ?? null,
          [Columns.MPF_OWNER_SCHEMA]: nativeMpfReplay?.schema ?? null,
          [Columns.MPF_OWNER_BINARY_SHA256]:
            nativeMpfReplay?.ownerBinarySha256 ?? null,
          [Columns.MPF_REPLAY_BASE_ROOT]: nativeMpfReplay?.baseRoot ?? null,
          [Columns.MPF_REPLAY_CANDIDATE_ROOT]:
            nativeMpfReplay?.candidateRoot ?? null,
          [Columns.MPF_REPLAY_EVENT_LOG]: nativeMpfReplay?.eventLog ?? null,
          [Columns.MPF_REPLAY_EVENT_LOG_DIGEST]:
            nativeMpfReplay?.eventLogDigest ?? null,
          [Columns.MPF_REPLAY_EVENT_ROOTS]: nativeMpfReplay?.eventRoots ?? null,
          [Columns.MPF_REPLAY_EVENT_COUNT]: nativeMpfReplay?.eventCount ?? null,
          [Columns.STATUS]: Status.PendingSubmission,
          [Columns.OBSERVED_CONFIRMED_AT_MS]: null,
        })}`;
        const permit = candidateHistory;
        const memberHistory = (eventTable: string, eventId: Buffer) =>
          Effect.gen(function* () {
            if (Option.isNone(permit))
              return {
                history_binding_digest: null,
                history_incarnation_id: null,
              };
            const rows = yield* sql<{
              history_binding_digest: Buffer;
              history_incarnation_id: Buffer;
            }>`
            SELECT e.history_binding_digest, e.history_incarnation_id FROM ${sql(eventTable)} e
            JOIN event_history_incarnations i ON i.binding_digest = e.history_binding_digest AND i.incarnation_id = e.history_incarnation_id
            WHERE e.event_id = ${eventId} AND i.event_id = e.event_id AND i.origin_canonical = true
              AND i.binding_digest = ${Buffer.from(permit.value.coverage.bindingDigest, "hex")}
            FOR UPDATE OF e`;
            if (rows.length !== 1)
              return yield* Effect.fail(
                new DatabaseError({
                  table: eventTable,
                  message:
                    "Pending member has no exact canonical history incarnation",
                  cause: eventId.toString("hex"),
                }),
              );
            return rows[0]!;
          });
        const associatedDeposits = yield* Effect.forEach(
          depositMembers,
          (member) =>
            memberHistory(
              DepositsDB.tableName,
              member[MemberColumns.MEMBER_ID],
            ).pipe(
              Effect.map((association) => ({ ...member, ...association })),
            ),
        );
        if (depositMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(depositsTableName)} ${sql.insert(
            associatedDeposits,
          )}`;
        }
        if (forcedTransactionMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(
            forcedTransactionsTableName,
          )} ${sql.insert(forcedTransactionMembers)}`;
        }
        for (const member of withdrawalMembers) {
          const association = yield* memberHistory(
            WithdrawalsDB.tableName,
            member[MemberColumns.MEMBER_ID],
          );
          const values = {
            ...member,
            ...association,
            [WithdrawalMemberColumns.VALIDITY_DETAIL]: sql`CAST(${JSON.stringify(member[WithdrawalMemberColumns.VALIDITY_DETAIL])} AS TEXT)::JSONB`,
          };
          const columns = [
            MemberColumns.HEADER_HASH,
            MemberColumns.MEMBER_ID,
            MemberColumns.ORDINAL,
            MemberColumns.PAYLOAD_CBOR,
            MemberColumns.PAYLOAD_SHA256,
            MemberColumns.SOURCE_TABLE,
            MemberColumns.SOURCE_ID,
            MemberColumns.SOURCE_TIMESTAMP,
            WithdrawalMemberColumns.CLASSIFICATION_REVISION,
            WithdrawalMemberColumns.VALIDITY,
            WithdrawalMemberColumns.VALIDITY_DETAIL,
            WithdrawalMemberColumns.CLASSIFICATION_SHA256,
            "history_binding_digest",
            "history_incarnation_id",
          ] as const;
          yield* sql`INSERT INTO ${sql(withdrawalsTableName)} (${sql.csv(columns.map((column) => sql`${sql(column)}`))}) VALUES (${sql.csv(columns.map((column) => sql`${values[column]}`))})`;
        }
        if (txMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(txsTableName)} ${sql.insert(txMembers)}`;
        }
        if (transitionTraceMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(
            transitionTraceTableName,
          )} ${sql.insert(transitionTraceMembers)}`;
        }
        if (eventToStepMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(eventToStepTableName)} ${sql.insert(
            eventToStepMembers,
          )}`;
        }
        if (validationTraceMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(
            validationTracesTableName,
          )} ${sql.insert(validationTraceMembers)}`;
        }
        if (validationTraceWitnessMembers.length > 0) {
          yield* sql`INSERT INTO ${sql(
            validationTraceWitnessesTableName,
          )} ${sql.insert(validationTraceWitnessMembers)}`;
        }
      }),
    );
  }).pipe(
    Effect.withLogSpan(`preparePendingSubmission ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to prepare pending block finalization",
    ),
  );
