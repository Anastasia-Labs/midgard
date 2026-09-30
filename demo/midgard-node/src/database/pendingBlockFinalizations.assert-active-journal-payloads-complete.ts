import "./pendingBlockFinalizations.retrieve-finalized-missing-da-payloads.js";

import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import {
  Columns,
  depositsTableName,
  eventToStepTableName,
  forcedTransactionsTableName,
  MemberColumns,
  type MemberRecord,
  tableName,
  transitionTraceTableName,
  txsTableName,
  validationTracesTableName,
  validationTraceWitnessesTableName,
  withdrawalsTableName,
} from "./pendingBlockFinalizations.columns.js";
import { retrieveActive } from "./pendingBlockFinalizations.retrieve-record.js";
import { clearTable, DatabaseError } from "./utils/common.js";

export const assertActiveJournalPayloadsComplete: Effect.Effect<
  void,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const active = yield* retrieveActive();
  if (Option.isNone(active)) {
    return;
  }
  const record = active.value;
  const hasIncompletePayload = (members: readonly MemberRecord[]): boolean =>
    members.some(
      (member) =>
        member[MemberColumns.PAYLOAD_CBOR].length <= 0 ||
        member[MemberColumns.PAYLOAD_SHA256].length !== 32,
    );
  if (
    record[Columns.HEADER_CBOR].length <= 0 ||
    [
      record.txMembers,
      record.depositMembers,
      record.forcedTransactionMembers,
      record.withdrawalMembers,
      record.transitionTraceMembers,
      record.eventToStepMembers,
      record.validationTraceMembers,
      record.validationTraceWitnessMembers,
    ].some(hasIncompletePayload) ||
    BigInt(record.validationTraceMembers.length) !==
      record[Columns.EXPECTED_VALIDATION_TRACE_COUNT]
  ) {
    return yield* Effect.fail(
      new DatabaseError({
        table: tableName,
        message:
          "Active pending-finalization journal has incomplete durable payload members",
        cause: `header_hash=${record[Columns.HEADER_HASH].toString("hex")}`,
      }),
    );
  }
}).pipe(Effect.withLogSpan(`assertActiveJournalPayloadsComplete ${tableName}`));

export const clear = Effect.all(
  [
    clearTable(depositsTableName),
    clearTable(forcedTransactionsTableName),
    clearTable(withdrawalsTableName),
    clearTable(txsTableName),
    clearTable(transitionTraceTableName),
    clearTable(eventToStepTableName),
    clearTable(validationTracesTableName),
    clearTable(validationTraceWitnessesTableName),
    clearTable(tableName),
  ],
  { discard: true },
);
