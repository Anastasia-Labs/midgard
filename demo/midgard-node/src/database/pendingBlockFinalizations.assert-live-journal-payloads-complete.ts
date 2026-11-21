import "./pendingBlockFinalizations.retrieve-applied-missing-da-payloads.js";

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
  withdrawalsTableName,
} from "./pendingBlockFinalizations.columns.js";
import { retrieveLive } from "./pendingBlockFinalizations.retrieve-record.js";
import { clearTable, DatabaseError } from "./utils/common.js";

/** The live journal, if any, holds every payload its block needs. */
export const assertLiveJournalPayloadsComplete: Effect.Effect<
  void,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const live = yield* retrieveLive();
  if (Option.isNone(live)) {
    return;
  }
  const record = live.value;
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
    ].some(hasIncompletePayload) ||
    BigInt(record.validationTraceMembers.length) !==
      record[Columns.EXPECTED_VALIDATION_TRACE_COUNT]
  ) {
    return yield* Effect.fail(
      new DatabaseError({
        table: tableName,
        message:
          "Live own-block journal has incomplete durable payload members",
        cause: `header_hash=${record[Columns.HEADER_HASH].toString("hex")}`,
      }),
    );
  }
}).pipe(Effect.withLogSpan(`assertLiveJournalPayloadsComplete ${tableName}`));

export const clear = Effect.all(
  [
    clearTable(depositsTableName),
    clearTable(forcedTransactionsTableName),
    clearTable(withdrawalsTableName),
    clearTable(txsTableName),
    clearTable(transitionTraceTableName),
    clearTable(eventToStepTableName),
    clearTable(validationTracesTableName),
    clearTable(tableName),
  ],
  { discard: true },
);
