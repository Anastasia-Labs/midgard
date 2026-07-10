import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import { DaPayloadsDB, PendingBlockFinalizationsDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { type Database } from "@/services/database.js";
import { buildDaPayloadInsert } from "@/workers/commit-block-header/da-payload.js";

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
  readonly retrieveJournalByHeaderHash?: (
    headerHash: Buffer,
  ) => Effect.Effect<
    Option.Option<PendingBlockFinalizationsDB.Record>,
    DatabaseError,
    R
  >;
  readonly upsertAvailable: (
    input: DaPayloadsDB.InsertInput,
  ) => Effect.Effect<void, DatabaseError, R>;
};

const defaultDeps: BackfillDeps<Database> = {
  retrieveMissingRecords:
    PendingBlockFinalizationsDB.retrieveFinalizedMissingDaPayloads,
  retrieveJournalByHeaderHash: PendingBlockFinalizationsDB.retrieveByHeaderHash,
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
  (record.utxoMembers.length === 0 &&
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT] !==
      EMPTY_MERKLE_TREE_ROOT) ||
  [
    record.txMembers,
    record.depositMembers,
    record.forcedTransactionMembers,
    record.withdrawalMembers,
    record.transitionTraceMembers,
    record.eventToStepMembers,
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
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSITION_STEP_COUNT];

export const backfillMissingDaPayloadsFromFinalizedJournals = <R = Database>({
  headerHash,
  limit,
  deps = defaultDeps as BackfillDeps<R>,
}: {
  readonly headerHash?: Buffer;
  readonly limit?: number;
  readonly deps?: BackfillDeps<R>;
} = {}): Effect.Effect<DaPayloadBackfillSummary, DatabaseError, R> =>
  Effect.gen(function* () {
    const records = yield* deps.retrieveMissingRecords({ headerHash, limit });
    const backfilled: string[] = [];
    const skipped: DaPayloadBackfillSkipped[] = [];
    if (records.length === 0) {
      if (
        headerHash !== undefined &&
        deps.retrieveJournalByHeaderHash !== undefined
      ) {
        const journal = yield* deps.retrieveJournalByHeaderHash(headerHash);
        if (Option.isSome(journal)) {
          const status =
            journal.value[PendingBlockFinalizationsDB.Columns.STATUS];
          skipped.push({
            headerHash: headerHash.toString("hex"),
            reason:
              status === PendingBlockFinalizationsDB.Status.Finalized
                ? "finalized journal already has a DA payload or is not missing DA payload backfill"
                : `journal excluded by status: ${status}; revive and complete local finalization before DA payload backfill`,
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
          const insert = yield* buildDaPayloadInsert({
            record,
          });
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
