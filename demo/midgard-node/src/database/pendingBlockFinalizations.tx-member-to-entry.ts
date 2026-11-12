import {
  MemberColumns,
  type MemberRecord,
} from "./pendingBlockFinalizations.columns.js";
import * as TxTable from "./utils/tx.js";

/** A journal's tx member as the mempool row it was journaled from. */
export const txMemberToEntry = (
  member: MemberRecord,
): TxTable.EntryWithTimeStamp => ({
  [TxTable.Columns.TX_ID]: Buffer.from(member[MemberColumns.MEMBER_ID]),
  [TxTable.Columns.TX]: Buffer.from(member[MemberColumns.PAYLOAD_CBOR]),
  [TxTable.Columns.TIMESTAMPTZ]: member[MemberColumns.SOURCE_TIMESTAMP],
});
