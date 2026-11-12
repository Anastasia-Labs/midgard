/**
 * This node's landed blocks, applied locally by the rebase that takes them
 * in (plan §8.3), the same rebase that applies a foreign block: their
 * events are projected and their delta reaches the working ledger and the
 * native MPF there. What only an own block has is written here, from its
 * journal, in the rebase's transaction: its transactions in `immutable` and
 * `blocks` (what the merge of the block reads), its DA payload and that
 * payload's publication outbox. Its transactions are in a landed block, so
 * a rejection recorded for one while its journal was dead is deleted, with
 * every rejection whose recorded causes are all deleted ones
 * (`TxRejectionsDB.deleteWithTracedRejections`). The journal is not
 * written: it derives landed, then applied, from the landed row.
 *
 * Each write is idempotent by header: a rebase that runs again over the
 * same block writes nothing new.
 */
import { Effect, Option } from "effect";

import { seedDaPayloadPublicationOutboxFromEnv } from "../da/libp2p-producer.js";
import {
  BlocksDB,
  DaPayloadsDB,
  ImmutableDB,
  MempoolDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  TxRejectionsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { Columns as TxColumns } from "../database/utils/tx.js";
import { buildDaPayloadInsert } from "../workers/commit-block-header/da-payload.js";
import type { LandedBlockRow } from "./store.js";

const Journals = PendingBlockFinalizationsDB;
const Member = Journals.MemberColumns;

const failure = (message: string, cause?: unknown) =>
  new DatabaseError({ table: Journals.tableName, message, cause });

/** The journal's transactions in block order: mempool, then processed. */
const blockEntries = (record: PendingBlockFinalizationsDB.Record) =>
  [MempoolDB.tableName, ProcessedMempoolDB.tableName].flatMap((table) =>
    record.txMembers
      .filter((member) => member[Member.SOURCE_TABLE] === table)
      .map(Journals.txMemberToEntry),
  );

/**
 * Writes the local effects of each own row of `rows` (see above). Returns
 * the hashes of the signed commits that landed them (hex).
 */
export const applyOwnBlocks = (rows: readonly LandedBlockRow[]) =>
  Effect.forEach(
    rows.filter((row) => row.kind === "own"),
    (row) =>
      Effect.gen(function* () {
        const found = yield* Journals.retrieveByHeaderHash(
          Buffer.from(row.headerHash, "hex"),
          true,
        );
        if (Option.isNone(found))
          return yield* Effect.fail(
            failure("A landed own block has no journal", row.headerHash),
          );
        const record = found.value;
        const header = record[Journals.Columns.HEADER_HASH];
        const entries = blockEntries(record);
        if (entries.length !== record.txMembers.length)
          return yield* Effect.fail(
            failure(
              "A landed own block's journal names a transaction source other than the mempool tables",
              row.headerHash,
            ),
          );
        yield* ImmutableDB.insertTxsValidatedNative(entries);
        yield* BlocksDB.insert(
          header,
          entries.map((entry) => entry[TxColumns.TX_ID]),
        );
        const payload = yield* buildDaPayloadInsert({ record });
        yield* DaPayloadsDB.upsertAvailable(payload);
        yield* seedDaPayloadPublicationOutboxFromEnv(payload);
        const cleared = yield* TxRejectionsDB.deleteWithTracedRejections(
          entries.map((entry) => entry[TxColumns.TX_ID]),
        );
        if (cleared.length > 0)
          yield* Effect.logWarning(
            `Own block ${row.headerHash} landed: deleted ${cleared.length.toString()} rejections of its transactions and of transactions rejected after them.`,
          );
        const intended = record[Journals.Columns.INTENDED_TX_HASH];
        return intended == null ? [] : [intended.toString("hex")];
      }),
  ).pipe(Effect.map((hashes) => hashes.flat()));
