import { decodeMidgardCekProgramMaterialDaEntry } from "@al-ft/midgard-core/cek-proof";
import { Effect } from "effect";

import {
  CekProgramMaterialDB,
  ForcedTransactionsDB,
  PendingBlockFinalizationsDB,
} from "../../database/index.js";
import { DatabaseError } from "../../database/utils/common.js";
import { journalCekProgramMaterial } from "./da-payload.compute-da-payload-roots.js";

export const pinFinalizedStateScriptRefs = (
  record: PendingBlockFinalizationsDB.Record,
  outputs: readonly Uint8Array[],
) =>
  Effect.try({
    try: () =>
      journalCekProgramMaterial(
        record,
        record.forcedTransactionMembers.map((member) => ({
          key: member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
          ...ForcedTransactionsDB.decodeForcedTransactionJournalMember(
            member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
          ),
        })),
      ).map((entry) =>
        decodeMidgardCekProgramMaterialDaEntry(
          Buffer.from(entry[0], "hex"),
          Buffer.from(entry[1], "hex"),
        ),
      ),
    catch: (cause) =>
      new DatabaseError({
        table: "local_block_finalization",
        message: "Failed to load finalized script material",
        cause,
      }),
  }).pipe(
    Effect.flatMap((material) =>
      CekProgramMaterialDB.pinRetainedStateScriptRefs({
        headerHash: record[PendingBlockFinalizationsDB.Columns.HEADER_HASH],
        outputs,
        material,
      }),
    ),
  );
