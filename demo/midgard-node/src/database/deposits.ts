import { Database } from "@/services/database.js";
import { Effect } from "effect";
import { DatabaseError } from "@/database/utils/common.js";
import * as UserEvents from "@/database/utils/user-events.js";

export const tableName = "deposits_utxos";

export const insertEntry = (
  entry: UserEvents.Entry,
): Effect.Effect<void, DatabaseError, Database> =>
  UserEvents.insertEntry(tableName, entry);

export const insertEntries = (
  entries: UserEvents.Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  UserEvents.insertEntries(tableName, entries);

export const retrieveTimeBoundEntries = (
  startTime: Date,
  endTime: Date,
): Effect.Effect<readonly UserEvents.Entry[], DatabaseError, Database> =>
  UserEvents.retrieveTimeBoundEntries(tableName, startTime, endTime);

export const retrieveAllEntries = (): Effect.Effect<
  readonly UserEvents.Entry[],
  DatabaseError,
  Database
> => UserEvents.retrieveAllEntries(tableName);

export const delEntries = (
  ids: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  UserEvents.delEntries(tableName, ids);

export const pruneOlderThan = (
  cutoff: Date,
): Effect.Effect<number, DatabaseError, Database> =>
  UserEvents.pruneOlderThan(tableName, cutoff);
