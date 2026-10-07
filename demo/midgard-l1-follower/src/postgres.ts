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
 * Returns a function that stops listening and releases the connection.
 */
export const listenForGenerations = async (
  pool: pg.Pool,
  listener: (generation: number) => void,
): Promise<() => Promise<void>> => {
  const client = await pool.connect();
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
    client.release(error instanceof Error ? error : true);
    throw error;
  }
  return async () => {
    client.off("notification", onNotification);
    try {
      await client.query(`UNLISTEN ${GENERATION_CHANNEL}`);
      client.release();
    } catch (error) {
      client.release(error instanceof Error ? error : true);
    }
  };
};
