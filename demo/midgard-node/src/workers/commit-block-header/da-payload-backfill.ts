import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect } from "effect";

import {
  DaPayloadsDB,
  MempoolLedgerDB,
  PendingBlockFinalizationsDB,
} from "@/database/index.js";
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
  readonly retrieveUtxos: Effect.Effect<
    readonly MempoolLedgerDB.EntryWithTimeStamp[],
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
  retrieveUtxos: MempoolLedgerDB.retrieve,
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
  [record.txMembers, record.depositMembers, record.withdrawalMembers].some(
    hasIncompletePayload,
  );

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
      return { scanned: 0, backfilled, skipped };
    }

    const utxos = yield* deps.retrieveUtxos;
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
          const insert = yield* buildDaPayloadInsert({ record, utxos });
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
