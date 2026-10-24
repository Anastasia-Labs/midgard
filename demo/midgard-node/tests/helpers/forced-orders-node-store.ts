/**
 * The follower store in the node's own test database (plan §5.4: the
 * follower's tables live there), and the node-side pieces a forced-order
 * test drives against it: the ingestion hook and the rows it wrote.
 */
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  type FactStore,
  type FactStoreOptions,
  type FollowerProjection,
  type LedgerOutputs,
  openPostgresFactStore,
  projectionStoreOptions,
  type TxContentSource,
} from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { ForcedTransactionsDB } from "../../src/database/index.js";
import {
  type ForcedOrderConfig,
  forcedOrderIngestionHook,
  type RunDatabase,
} from "../../src/forced-orders/index.js";
import type { FollowerChange } from "../../src/l1-events/driver.js";
import { testDatabaseName } from "../test-env.js";
import { provideDatabaseLayers } from "../utils.js";
import { closeFollowerHost } from "./follower-emulator.host.js";

/** Runs a node database effect; a layer that fails to build is a defect. */
export const runDatabase: RunDatabase = (effect) =>
  Effect.runPromiseExit(
    provideDatabaseLayers(Effect.either(effect)).pipe(
      Effect.orDie,
      Effect.flatMap((result) => result),
    ),
  );

export const db = <A, E>(effect: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(provideDatabaseLayers(effect));

const nodeDatabaseUrl = (): string => {
  const env = process.env;
  return `postgresql://${env.POSTGRES_USER ?? "postgres"}:${env.POSTGRES_PASSWORD ?? "postgres"}@${env.POSTGRES_HOST ?? "127.0.0.1"}:${env.POSTGRES_PORT ?? "5433"}/${testDatabaseName()}`;
};

/**
 * A started follower store with `projections`, in the node's database. It
 * takes the writer lease from the emulator follower host (which resets on
 * its next sync), as one writer replaces another.
 */
export const openNodeFollowerStore = async (
  projections: readonly FollowerProjection[],
  securityParameter: number,
  extra: Partial<FactStoreOptions> = {},
): Promise<FactStore> => {
  await closeFollowerHost();
  const store = openPostgresFactStore({
    ...projectionStoreOptions(
      projections,
      {
        securityParameter,
        trackedSet: {
          addresses: new Set(),
          paymentCredentials: new Set(),
          policies: new Set(),
        },
      },
      "postgres",
    ),
    ...extra,
    connection: { connectionString: nodeDatabaseUrl() },
  });
  const started = await store.start();
  if (started.kind !== "ready") {
    await store.close();
    throw new Error(`store start: ${JSON.stringify(started)}`);
  }
  return store;
};

/** The ingestion hook over `store`, logging into `logs`. */
export const ingestionHook = (
  store: FactStore,
  config: ForcedOrderConfig,
  options: {
    ledger?: LedgerOutputs;
    sources?: TxContentSource[];
    caughtUp?: () => boolean;
    contentSourcesConfigured?: boolean;
  } = {},
) => {
  const logs: string[] = [];
  const hook = forcedOrderIngestionHook({
    store,
    config,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    contentSourcesConfigured: true,
    ...options,
    run: runDatabase,
    log: (line) => logs.push(line),
  });
  return { hook, logs };
};

/** The hook reads the follower's own view; the change it is handed is moot. */
export const UNCHANGED: FollowerChange = {
  kind: "unchanged",
  view: {
    generation: 0,
    point: { slot: 0, hash: Buffer.alloc(32) },
    height: 0,
  },
};

/** The node's `forced_transaction_utxos` rows. */
export const forcedRows = () =>
  db(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<{
        tx_order_l1_tx_hash: Buffer;
        native_tx_cbor: Buffer;
        status: string;
      }>`SELECT tx_order_l1_tx_hash, native_tx_cbor, status
        FROM ${sql(ForcedTransactionsDB.tableName)}`;
    }),
  );
