import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { sha256 } from "../sha256.js";
import * as ForcedTransactionsDB from "./forcedTransactions.js";
import {
  Columns,
  MemberColumns,
  type MemberRecord,
  normalizeRow,
  PENDING_BLOCK_FINALIZATION_VERSION,
  PendingBlockFinalizationReplayKind,
  type RawRow,
  type RetainedRootMemberInput,
  type Row,
  Status,
  tableName,
  WithdrawalMemberColumns,
  type WithdrawalMemberRecord,
} from "./pendingBlockFinalizations.columns.js";
import {
  type LedgerDeltaInput,
  type NativeMpfReplayInput,
} from "./pendingBlockFinalizations.parse-ledger-delta.js";
import {
  decodeLedgerDelta,
  decodeNativeMpfReplay,
  parsePendingBlockFinalization,
  pendingBlockFinalizationMetadataFromRow,
} from "./pendingBlockFinalizations.parse-pending-block-finalization.js";
import { DatabaseError } from "./utils/common.js";
import * as WithdrawalsDB from "./withdrawals.js";

export const forcedTransactionMemberEntry = (
  headerHash: Buffer,
  entry: ForcedTransactionsDB.Entry,
  ordinal: number,
): Effect.Effect<MemberRecord, DatabaseError> =>
  Effect.gen(function* () {
    const sourceValueCbor = Buffer.from(
      entry[ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE],
    );
    const canonicalTransactionCbor =
      entry[ForcedTransactionsDB.Columns.NATIVE_TX_CBOR];
    const programMaterialSidecarCbor =
      entry[ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR];
    const programMaterialSidecarSha256 =
      entry[ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256];
    if (
      sourceValueCbor.length === 0 ||
      canonicalTransactionCbor.length === 0 ||
      programMaterialSidecarCbor.length === 0 ||
      programMaterialSidecarSha256.length !== 32 ||
      !sha256(programMaterialSidecarCbor).equals(programMaterialSidecarSha256)
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForcedTransactionsDB.tableName,
          message:
            "Refusing to journal a V1 forced transaction without its exact source value, canonical transaction preimage, and authenticated CEK material sidecar",
          cause: `tx_order_id=${entry[
            ForcedTransactionsDB.Columns.TX_ORDER_ID
          ].toString("hex")}`,
        }),
      );
    }
    const payload = ForcedTransactionsDB.encodeForcedTransactionJournalMember({
      sourceValueCbor,
      canonicalTransactionCbor,
      programMaterialSidecarCbor,
    });
    const memberId = Buffer.from(
      entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
    );
    return {
      [MemberColumns.HEADER_HASH]: headerHash,
      [MemberColumns.MEMBER_ID]: memberId,
      [MemberColumns.ORDINAL]: ordinal,
      [MemberColumns.PAYLOAD_CBOR]: payload,
      [MemberColumns.PAYLOAD_SHA256]: sha256(payload),
      [MemberColumns.SOURCE_TABLE]: ForcedTransactionsDB.tableName,
      [MemberColumns.SOURCE_ID]: memberId,
      [MemberColumns.SOURCE_TIMESTAMP]:
        entry[ForcedTransactionsDB.Columns.INCLUSION_TIME],
    };
  });

export const withdrawalClassificationDigest = (
  member: Omit<
    WithdrawalMemberRecord,
    WithdrawalMemberColumns.CLASSIFICATION_SHA256
  >,
): Buffer =>
  sha256(
    Buffer.from(
      canonicalJson(
        {
          headerHash: member[MemberColumns.HEADER_HASH].toString("hex"),
          eventId: member[MemberColumns.MEMBER_ID].toString("hex"),
          settlementInfoSha256:
            member[MemberColumns.PAYLOAD_SHA256].toString("hex"),
          classificationRevision:
            member[WithdrawalMemberColumns.CLASSIFICATION_REVISION],
          validity: member[WithdrawalMemberColumns.VALIDITY],
          validityDetail: member[WithdrawalMemberColumns.VALIDITY_DETAIL],
        },
        "withdrawal journal classification",
      ),
    ),
  );

export const withdrawalMemberEntry = (
  headerHash: Buffer,
  entry: WithdrawalsDB.Entry,
  ordinal: number,
): Effect.Effect<WithdrawalMemberRecord, DatabaseError> =>
  Effect.gen(function* () {
    const payload = entry[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO];
    const memberId = Buffer.from(entry[WithdrawalsDB.Columns.ID]);
    const validity = entry[WithdrawalsDB.Columns.VALIDITY];
    if (payload === null || validity === null) {
      return yield* Effect.fail(
        new DatabaseError({
          table: WithdrawalsDB.tableName,
          message:
            "Refusing to prepare pending journal for an unclassified withdrawal",
          cause: `event_id=${memberId.toString("hex")}`,
        }),
      );
    }
    const member = {
      [MemberColumns.HEADER_HASH]: headerHash,
      [MemberColumns.MEMBER_ID]: memberId,
      [MemberColumns.ORDINAL]: ordinal,
      [MemberColumns.PAYLOAD_CBOR]: Buffer.from(payload),
      [MemberColumns.PAYLOAD_SHA256]: sha256(Buffer.from(payload)),
      [MemberColumns.SOURCE_TABLE]: WithdrawalsDB.tableName,
      [MemberColumns.SOURCE_ID]: memberId,
      [MemberColumns.SOURCE_TIMESTAMP]:
        entry[WithdrawalsDB.Columns.INCLUSION_TIME],
      [WithdrawalMemberColumns.CLASSIFICATION_REVISION]:
        entry[WithdrawalsDB.Columns.CLASSIFICATION_REVISION],
      [WithdrawalMemberColumns.VALIDITY]: validity,
      [WithdrawalMemberColumns.VALIDITY_DETAIL]:
        entry[WithdrawalsDB.Columns.VALIDITY_DETAIL],
    };
    return {
      ...member,
      [WithdrawalMemberColumns.CLASSIFICATION_SHA256]:
        withdrawalClassificationDigest(member),
    };
  });

export const retainedRootMemberEntry = ({
  headerHash,
  entry,
  ordinal,
  sourceTable,
  blockEndTime,
}: {
  readonly headerHash: Buffer;
  readonly entry: RetainedRootMemberInput;
  readonly ordinal: number;
  readonly sourceTable: string;
  readonly blockEndTime: Date;
}): MemberRecord => {
  const memberId = Buffer.from(entry.keyCbor);
  const payload = Buffer.from(entry.valueCbor);
  return {
    [MemberColumns.HEADER_HASH]: headerHash,
    [MemberColumns.MEMBER_ID]: memberId,
    [MemberColumns.ORDINAL]: ordinal,
    [MemberColumns.PAYLOAD_CBOR]: payload,
    [MemberColumns.PAYLOAD_SHA256]: sha256(payload),
    [MemberColumns.SOURCE_TABLE]: sourceTable,
    [MemberColumns.SOURCE_ID]: memberId,
    [MemberColumns.SOURCE_TIMESTAMP]: blockEndTime,
  };
};

export const retrieveMembers = <Member extends MemberRecord = MemberRecord>(
  sql: SqlClient.SqlClient,
  memberTableName: string,
  headerHash: Buffer,
): Effect.Effect<readonly Member[], never, never> =>
  Effect.gen(function* () {
    return yield* sql<Member>`SELECT * FROM ${sql(memberTableName)}
      WHERE ${sql(MemberColumns.HEADER_HASH)} = ${headerHash}
      ORDER BY ${sql(MemberColumns.ORDINAL)} ASC`;
  }).pipe(Effect.orDie);

export const validateForcedTransactionJournalMembers = (
  members: readonly MemberRecord[],
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError> =>
  Effect.try({
    try: () => {
      for (const member of members) {
        const memberId = member[MemberColumns.MEMBER_ID];
        const payload = member[MemberColumns.PAYLOAD_CBOR];
        const payloadSha256 = member[MemberColumns.PAYLOAD_SHA256];
        if (
          member[MemberColumns.SOURCE_TABLE] !==
            ForcedTransactionsDB.tableName ||
          !member[MemberColumns.SOURCE_ID].equals(memberId) ||
          payloadSha256.length !== 32 ||
          !sha256(payload).equals(payloadSha256)
        ) {
          throw new Error(
            `forced journal member identity or payload digest mismatch: member_id=${memberId.toString("hex")}`,
          );
        }
        ForcedTransactionsDB.decodeForcedTransactionJournalMember(payload);
      }
    },
    catch: (cause) =>
      new DatabaseError({
        table: tableName,
        message:
          "Refusing to load a pending-finalization journal containing a non-canonical ForcedTransactionJournalMemberV1",
        cause: `header_hash=${headerHash.toString("hex")}; ${String(cause)}`,
      }),
  });

export const decodePendingBlockFinalizationRow = (
  row: RawRow,
): Effect.Effect<
  {
    readonly normalizedRow: Row;
    readonly ledgerDelta: LedgerDeltaInput;
    readonly nativeMpfReplay?: NativeMpfReplayInput;
  },
  DatabaseError
> =>
  Effect.try({
    try: () => {
      const persistedFormatVersion: unknown = row[Columns.FORMAT_VERSION];
      if (persistedFormatVersion !== PENDING_BLOCK_FINALIZATION_VERSION) {
        throw new Error(
          `persisted format_version must equal ${PENDING_BLOCK_FINALIZATION_VERSION.toString()}`,
        );
      }
      const normalizedRow = normalizeRow(row);
      validateSignedIntent(
        normalizedRow[Columns.INTENDED_TX_HASH],
        normalizedRow[Columns.SIGNED_TX_CBOR],
      );
      const preparedHash = normalizedRow[Columns.PREPARED_TX_HASH];
      const intendedHash = normalizedRow[Columns.INTENDED_TX_HASH];
      if (
        (preparedHash != null && preparedHash.length !== 32) ||
        (intendedHash != null &&
          (preparedHash == null || !preparedHash.equals(intendedHash)))
      )
        throw new Error(
          "Signed intent does not match prepared transaction body hash",
        );
      const ledgerDelta = decodeLedgerDelta(normalizedRow);
      const nativeMpfReplay = decodeNativeMpfReplay(normalizedRow);
      const pending = parsePendingBlockFinalization({
        version: normalizedRow[Columns.FORMAT_VERSION],
        metadata: pendingBlockFinalizationMetadataFromRow(normalizedRow),
        replay:
          nativeMpfReplay === undefined
            ? {
                kind: normalizedRow[Columns.REPLAY_KIND],
                ledgerDelta,
              }
            : {
                kind: normalizedRow[Columns.REPLAY_KIND],
                ledgerDelta,
                nativeMpfReplay,
              },
      });
      if (
        normalizedRow[Columns.HEADER_HASH].length !== 28 ||
        normalizedRow[Columns.HEADER_CBOR].length === 0 ||
        (normalizedRow[Columns.SUBMITTED_TX_HASH] !== null &&
          normalizedRow[Columns.SUBMITTED_TX_HASH].length !== 32) ||
        normalizedRow[Columns.BLOCK_END_TIME].getTime() <=
          pending.metadata.blockStartTime.getTime() ||
        !Object.values(Status).includes(normalizedRow[Columns.STATUS])
      ) {
        throw new Error(
          "persisted header, status, transaction hash, or block window is invalid",
        );
      }
      return {
        normalizedRow,
        ledgerDelta: pending.replay.ledgerDelta,
        nativeMpfReplay:
          pending.replay.kind ===
          PendingBlockFinalizationReplayKind.LedgerDeltaWithNativeMpf
            ? pending.replay.nativeMpfReplay
            : undefined,
      };
    },
    catch: (cause) =>
      new DatabaseError({
        table: tableName,
        message: "Refusing to load a non-canonical PendingBlockFinalizationV1",
        cause: String(cause),
      }),
  });

export const validateSignedIntent = (
  hash: Buffer | null | undefined,
  cbor: Buffer | null | undefined,
): void => {
  if (hash == null && cbor == null) return;
  if (hash?.length !== 32 || cbor == null || cbor.length === 0)
    throw new Error("Incomplete durable signed transaction intent");
  const tx = CML.Transaction.from_cbor_bytes(cbor);
  const body = tx.body();
  const actual = CML.hash_transaction(body);
  try {
    if (!tx.is_valid() || actual.to_hex() !== hash.toString("hex"))
      throw new Error("Durable signed transaction intent hash mismatch");
  } finally {
    actual.free();
    body.free();
    tx.free();
  }
};
