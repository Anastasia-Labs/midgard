import { Database } from "@/services/database.js";
import { Effect } from "effect";
import { DatabaseError } from "@/database/utils/common.js";
import * as UserEvents from "@/database/utils/user-events.js";

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
