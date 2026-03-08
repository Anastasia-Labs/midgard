import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import { Database } from "@/services/index.js";
import { DatabaseError, NotFoundError } from "@/database/utils/common.js";
import { LatestLedgerDB, Ledger, UserEvents } from "@/database/index.js";
import { CML, Data } from "@lucid-evolution/lucid";

export const tableName = "withdrawal_order_utxos";

export const insertEntries = (
  entries: UserEvents.Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  UserEvents.insertEntries(tableName, entries);

export const retrieveTimeBoundEntries = (
  startTime: Date,
  endTime: Date,
): Effect.Effect<readonly UserEvents.Entry[], DatabaseError, Database> =>
  UserEvents.retrieveTimeBoundEntries(tableName, startTime, endTime);

export const entryToOutRef = (
  withdrawal: UserEvents.Entry,
): Effect.Effect<Buffer, SDK.CmlDeserializationError | SDK.DataCoercionError> =>
  Effect.gen(function* () {
    const withdrawalInfoHex = SDK.bufferToHex(
      withdrawal[UserEvents.Columns.INFO],
    );
    const withdrawalInfo = yield* Effect.try({
      try: () => Data.from(withdrawalInfoHex, SDK.WithdrawalInfo),
      catch: (e) =>
        new SDK.DataCoercionError({
          message: "Malformed WithdrawalInfo encountered",
          cause: e,
        }),
    });
    const txId = yield* Effect.try({
      try: () =>
        CML.TransactionHash.from_hex(withdrawalInfo.body.l2_outref.txHash.hash),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message:
            "Invalid transaction hash was included in the given WithdrawalInfo",
          cause: e,
        }),
    });
    const outRef = CML.TransactionInput.new(
      txId,
      withdrawalInfo.body.l2_outref.outputIndex,
    );
    return Buffer.from(outRef.to_cbor_bytes());
  });

export type ResolvedWithdrawal = {
  withdrawalEntry: UserEvents.Entry;
  ledgerEntry: Ledger.Entry;
};

/**
 * Grabs corresponding spent ledger entry from the given ledger table. Returns
 * the input entry along with the result for easier conversion to
 * `AddressHistoryDB` entry.
 */
export const resolveEntry = (
  ledgerTableName: string,
  entry: UserEvents.Entry,
): Effect.Effect<
  ResolvedWithdrawal,
  | DatabaseError
  | NotFoundError
  | SDK.CmlDeserializationError
  | SDK.DataCoercionError,
  Database
> =>
  Effect.gen(function* () {
    const outRef = yield* entryToOutRef(entry);
    const ledgerEntry = yield* Ledger.retrieveByOutRef(ledgerTableName, outRef);
    return {
      withdrawalEntry: entry,
      ledgerEntry,
    };
  });

export const entriesToLedgerEntries = (
  entries: UserEvents.Entry[],
): Effect.Effect<
  readonly Ledger.Entry[],
  | DatabaseError
  | NotFoundError
  | SDK.CmlDeserializationError
  | SDK.DataCoercionError,
  Database
> =>
  Effect.all(entries.map(entryToOutRef)).pipe(
    Effect.andThen(LatestLedgerDB.retrieveEntries),
  );
