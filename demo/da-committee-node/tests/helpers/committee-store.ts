import { afterAll, afterEach, expect } from "vitest";

import type { L1SourceState } from "../../src/store.js";
import {
  PostgresCommitteeStore,
  type PostgresCommitteeStoreOptions,
} from "../../src/store/postgres.js";
import {
  DROP_ALL_TIMEOUT_MS,
  type PostgresTestDatabase,
  postgresTestDatabases,
} from "./postgres-database.js";

const databases = postgresTestDatabases("committee_store");
/** Stores a test opened; closed after that test. */
const openStores = new Set<PostgresCommitteeStore>();
/** Stores a `beforeAll` hook opened; closed after the file. */
const fileStores = new Set<PostgresCommitteeStore>();

/** A fresh, empty database for a committee store; dropped after the file. */
export const testStoreDatabase = (): Promise<PostgresTestDatabase> =>
  databases.create();

/**
 * Opens a committee store on `database`, or on a fresh one when it is
 * omitted. Reopening the same database is a restart. A store opened in a
 * test (or `beforeEach`) is closed after that test; one opened in
 * `beforeAll` is closed after the file.
 */
export const openTestCommitteeStore = async (
  database?: PostgresTestDatabase,
  options?: PostgresCommitteeStoreOptions,
): Promise<PostgresCommitteeStore> => {
  const target = database ?? (await databases.create());
  const store = await PostgresCommitteeStore.open(target.url, options);
  (expect.getState().currentTestName === undefined
    ? fileStores
    : openStores
  ).add(store);
  return store;
};

/** Closes a store opened here before the test ends; a second close is a no-op. */
export const closeTestCommitteeStore = async (
  store: PostgresCommitteeStore,
): Promise<void> => {
  if (!openStores.delete(store) && !fileStores.delete(store)) return;
  await store.close();
};

/**
 * A healthy local-node L1 source state. The store refuses a signature or
 * capacity write until one is durable, as a running node saves it first.
 */
export const healthyL1SourceState: L1SourceState = Object.freeze({
  schemaVersion: 1,
  sourceMode: "local_node",
  network: "Preprod",
  authoritySha256: "94".repeat(32),
  status: "healthy",
  observations: [],
  observedAt: "2026-10-07T00:00:00.000Z",
});

/**
 * Makes `store` ready for decision writes, as node startup does. A test whose
 * store a `CommitteeService` later initializes passes that service's source
 * identity.
 */
export const saveHealthyL1SourceState = async (
  store: PostgresCommitteeStore,
  source: Partial<
    Pick<L1SourceState, "sourceMode" | "network" | "authoritySha256">
  > = {},
): Promise<PostgresCommitteeStore> => {
  await store.saveL1SourceState({ ...healthyL1SourceState, ...source });
  return store;
};

const closeAll = async (stores: Set<PostgresCommitteeStore>): Promise<void> => {
  const closing = [...stores];
  stores.clear();
  await Promise.all(closing.map((store) => store.close().catch(() => {})));
};

afterEach(() => closeAll(openStores));
afterAll(async () => {
  await closeAll(openStores);
  await closeAll(fileStores);
  await databases.dropAll();
}, DROP_ALL_TIMEOUT_MS);
