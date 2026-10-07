import pg from "pg";

import {
  openPostgresBackend,
  type PostgresConnection,
} from "./sql/postgres-backend.js";
import {
  createFactStore,
  type FactStore,
  type FactStoreOptions,
} from "./store/fact-store.js";

export type { PostgresConnection } from "./sql/postgres-backend.js";

/** The Postgres fact store, for the node and the committee (§18.1 Q1). */
export const openPostgresFactStore = (
  options: FactStoreOptions & Readonly<{ connection: PostgresConnection }>,
): FactStore =>
  createFactStore(openPostgresBackend(options.connection), options);

/**
 * The channel a committed rewind notifies with its new generation (§7.1
 * step 7), and a reset with the next generation.
 */
export const GENERATION_CHANNEL = "l1_generation";

/**
 * Listens for committed rewinds and resets from another process sharing the
 * database.
 * Returns a function that stops listening and releases the connection. If
 * Postgres drops the connection, listening has stopped: `onConnectionLost`
 * is told (never an uncaught 'error'), and the caller listens again.
 */
export const listenForGenerations = async (
  pool: pg.Pool,
  listener: (generation: number) => void,
  onConnectionLost: (error: Error) => void = () => undefined,
): Promise<() => Promise<void>> => {
  const client = await pool.connect();
  let lost: Error | null = null;
  let stopping = false;
  // A checked-out client has no pool listener: without this, a dropped
  // connection's 'error' would be uncaught and exit the process.
  const onError = (error: Error): void => {
    if (lost !== null) return;
    lost = error;
    if (!stopping) onConnectionLost(error);
  };
  client.on("error", onError);
  const onNotification = (message: pg.Notification): void => {
    if (message.channel !== GENERATION_CHANNEL || message.payload === undefined)
      return;
    const generation = Number(message.payload);
    if (Number.isSafeInteger(generation)) listener(generation);
  };
  client.on("notification", onNotification);
  try {
    await client.query(`LISTEN ${GENERATION_CHANNEL}`);
  } catch (error) {
    client.off("notification", onNotification);
    client.off("error", onError);
    client.release(error instanceof Error ? error : true);
    throw error;
  }
  return async () => {
    stopping = true;
    client.off("notification", onNotification);
    let failed: Error | null = lost;
    if (failed === null)
      try {
        await client.query(`UNLISTEN ${GENERATION_CHANNEL}`);
      } catch (error) {
        failed = error instanceof Error ? error : new Error(String(error));
      }
    // Detached only now: the UNLISTEN itself may meet a dropped connection.
    client.off("error", onError);
    client.release(failed ?? undefined);
  };
};
