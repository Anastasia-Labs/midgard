import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect } from "effect";

import {
  DaPayloadsDB,
  PendingBlockFinalizationsDB,
} from "../../database/index.js";
import { DatabaseError } from "../../database/utils/common.js";
import { NodeConfig } from "../../services/config.js";
import { type Database } from "../../services/database.js";
import { buildDaPayloadInsert } from "./da-payload.js";

export type DaPayloadBackfillSkipped = {
  readonly headerHash: string;
  readonly reason: string;
};

export type DaPayloadBackfillSummary = {
  readonly scanned: number;
  readonly backfilled: readonly string[];
  readonly skipped: readonly DaPayloadBackfillSkipped[];
};

type BackfillDeps<R> = {
  readonly retrieveMissingRecords: (args: {
    readonly headerHash?: Buffer;
    readonly limit?: number;
  }) => Effect.Effect<
    readonly PendingBlockFinalizationsDB.Record[],
    DatabaseError,
    R
  >;
  readonly retrieveJournalStatus?: (
    headerHash: Buffer,
  ) => Effect.Effect<
    PendingBlockFinalizationsDB.OwnBlockStatus | undefined,
    DatabaseError,
    R
  >;
  readonly upsertAvailable: (
    input: DaPayloadsDB.InsertInput,
  ) => Effect.Effect<void, DatabaseError, R>;
};

const defaultDeps: BackfillDeps<Database> = {
  retrieveMissingRecords:
    PendingBlockFinalizationsDB.retrieveAppliedMissingDaPayloads,
  retrieveJournalStatus: PendingBlockFinalizationsDB.retrieveStatusByHeaderHash,
  upsertAvailable: DaPayloadsDB.upsertAvailable,
};

const headerHashHex = (record: PendingBlockFinalizationsDB.Record): string =>
  record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex");

const hasIncompletePayload = (
  members: readonly PendingBlockFinalizationsDB.MemberRecord[],
): boolean =>
  members.some(
    (member) =>
      member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR].length <=
        0 ||
      member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_SHA256]
        .length !== 32,
  );

const journalHasIncompletePayloads = (
  record: PendingBlockFinalizationsDB.Record,
): boolean =>
  record[PendingBlockFinalizationsDB.Columns.HEADER_CBOR].length <= 0 ||
  [
    record.txMembers,
    record.depositMembers,
    record.forcedTransactionMembers,
    record.withdrawalMembers,
    record.transitionTraceMembers,
    record.eventToStepMembers,
    record.validationTraceMembers,
  ].some(hasIncompletePayload) ||
  BigInt(record.withdrawalMembers.length) !==
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWAL_COUNT] ||
  BigInt(record.forcedTransactionMembers.length) !==
    record[
      PendingBlockFinalizationsDB.Columns.EXPECTED_FORCED_TRANSACTION_COUNT
    ] ||
  BigInt(record.txMembers.length) !==
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_L2_TRANSACTION_COUNT] ||
  BigInt(record.depositMembers.length) !==
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSIT_COUNT] ||
  BigInt(record.transitionTraceMembers.length) !==
    record[
      PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSITION_STEP_COUNT
    ] ||
  BigInt(record.eventToStepMembers.length) !==
    record[
      PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSITION_STEP_COUNT
    ] ||
  BigInt(record.validationTraceMembers.length) !==
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACE_COUNT];

export const backfillMissingDaPayloadsFromAppliedJournals = <R = Database>({
  headerHash,
  limit,
  deps = defaultDeps as BackfillDeps<R>,
}: {
  readonly headerHash?: Buffer;
  readonly limit?: number;
  readonly deps?: BackfillDeps<R>;
} = {}): Effect.Effect<
  DaPayloadBackfillSummary,
  DatabaseError,
  R | NodeConfig
> =>
  Effect.gen(function* () {
    const records = yield* deps.retrieveMissingRecords({ headerHash, limit });
    const backfilled: string[] = [];
    const skipped: DaPayloadBackfillSkipped[] = [];
    if (records.length === 0) {
      if (
        headerHash !== undefined &&
        deps.retrieveJournalStatus !== undefined
      ) {
        const status = yield* deps.retrieveJournalStatus(headerHash);
        if (status !== undefined) {
          skipped.push({
            headerHash: headerHash.toString("hex"),
            reason:
              status === PendingBlockFinalizationsDB.OwnBlockStatus.Applied ||
              status === PendingBlockFinalizationsDB.OwnBlockStatus.Folded
                ? "applied journal already has a DA payload or is not missing DA payload backfill"
                : `journal excluded by status: ${status}; its block is backfilled once it is applied`,
          });
        }
      }
      return { scanned: 0, backfilled, skipped };
    }

    for (const record of records) {
      const header = headerHashHex(record);
      if (journalHasIncompletePayloads(record)) {
        skipped.push({
          headerHash: header,
          reason: "journal has incomplete payload members",
        });
        continue;
      }

      const result = yield* Effect.either(
        Effect.gen(function* () {
          const insert = yield* buildDaPayloadInsert({ record });
          yield* deps.upsertAvailable(insert);
        }),
      );
      if (result._tag === "Left") {
        skipped.push({
          headerHash: header,
          reason: formatUnknownError(result.left),
        });
        continue;
      }
      backfilled.push(header);
    }
    return { scanned: records.length, backfilled, skipped };
  });
