import { randomUUID } from "node:crypto";
import { createServer } from "node:http";

import { HttpServer } from "@effect/platform";
import { NodeHttpServer } from "@effect/platform-node";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { generateEmulatorAccount } from "@lucid-evolution/lucid";
import {
  ConfigProvider,
  Effect,
  Layer,
  ManagedRuntime,
  Redacted,
} from "effect";

import { buildStateQueueMutationLeaseRouter } from "../../src/commands/listen-router.js";
import {
  MigrationRunner,
  StateQueueMutationLeasesDB,
} from "../../src/database/index.js";
import { NodeConfig } from "../../src/services/config.js";

export type StateQueueMutationLeaseServer = {
  readonly url: string;
  readonly adminApiKey: string;
  readonly databaseName: string;
  readonly inspect: () => Promise<StateQueueMutationLeasesDB.LeaseInspection>;
  readonly close: () => Promise<void>;
};

/**
 * Serve the node's actual authenticated lease routes over loopback HTTP.
 * Each instance owns a new database and runs the normal node migrations.
 * It never changes process.env or attaches to an existing node database.
 */
export const startStateQueueMutationLeaseServer = async (
  options: {
    readonly postgres?: {
      readonly host?: string;
      readonly port?: number;
      readonly username?: string;
      readonly password?: string;
    };
  } = {},
): Promise<StateQueueMutationLeaseServer> => {
  const postgres = {
    host: options.postgres?.host ?? process.env.POSTGRES_HOST ?? "127.0.0.1",
    port: options.postgres?.port ?? Number(process.env.POSTGRES_PORT ?? "5433"),
    username:
      options.postgres?.username ?? process.env.POSTGRES_USER ?? "postgres",
    password: Redacted.make(
      options.postgres?.password ?? process.env.POSTGRES_PASSWORD ?? "postgres",
    ),
    maxConnections: 4,
    applicationName: "midgard-lease-http-test",
  };
  if (!["127.0.0.1", "localhost", "::1"].includes(postgres.host)) {
    throw new Error("The lease HTTP fixture requires loopback Postgres");
  }
  const databaseName = `midgard_lease_${randomUUID().replaceAll("-", "")}`;
  const adminApiKey = randomUUID();
  const maintenance = ManagedRuntime.make(
    PgClient.layer({ ...postgres, database: "postgres" }),
  );
  let databaseCreated = false;
  let closeDatabase = async (): Promise<void> => {};
  let closeHttp = async (): Promise<void> => {};
  let closing: Promise<void> | undefined;
  const close = (): Promise<void> => {
    closing ??= (async () => {
      try {
        await closeHttp();
      } finally {
        try {
          await closeDatabase();
        } finally {
          try {
            if (databaseCreated) {
              await maintenance.runPromise(
                Effect.gen(function* () {
                  const sql = yield* SqlClient.SqlClient;
                  // The identifier is generated above, never caller-supplied.
                  yield* sql.unsafe(`DROP DATABASE "${databaseName}"`);
                }),
              );
            }
          } finally {
            await maintenance.dispose();
          }
        }
      }
    })();
    return closing;
  };

  try {
    await maintenance.runPromise(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql.unsafe(`CREATE DATABASE "${databaseName}"`);
        databaseCreated = true;
        yield* sql.unsafe(
          `ALTER DATABASE "${databaseName}" SET synchronous_commit = on`,
        );
      }),
    );
    const database = ManagedRuntime.make(
      PgClient.layer({ ...postgres, database: databaseName }),
    );
    closeDatabase = () => database.dispose();
    await database.runPromise(
      MigrationRunner.migrate({
        appVersion: "lease-http-integration",
        actor: "lease-http-integration",
      }),
    );
    const sql = await database.runPromise(SqlClient.SqlClient);
    // Decode a complete NodeConfig through the production loader. The HTTP
    // routes use ADMIN_API_KEY only; no wallet or L1 service is instantiated.
    const seed = generateEmulatorAccount({ lovelace: 0n }).seedPhrase;
    const config = await Effect.runPromise(
      NodeConfig.pipe(
        Effect.provide(NodeConfig.layer),
        Effect.withConfigProvider(
          ConfigProvider.fromMap(
            new Map(
              Object.entries({
                L1_PROVIDER: "Kupmios",
                L1_OGMIOS_KEY: "http://127.0.0.1:1",
                L1_KUPO_KEY: "http://127.0.0.1:1",
                NETWORK: "Preprod",
                MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE: "bounded-acceptance-v1",
                L1_OPERATOR_SEED_PHRASE: seed,
                L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: seed,
                L1_REFERENCE_SCRIPT_SEED_PHRASE: seed,
                TESTNET_GENESIS_WALLET_SEED_PHRASE_A: seed,
                TESTNET_GENESIS_WALLET_SEED_PHRASE_B: seed,
                TESTNET_GENESIS_WALLET_SEED_PHRASE_C: seed,
                ADMIN_API_KEY: adminApiKey,
              }),
            ),
          ),
        ),
      ),
    );
    const server = ManagedRuntime.make(
      HttpServer.serve(
        buildStateQueueMutationLeaseRouter().pipe(
          Effect.provideService(SqlClient.SqlClient, sql),
          Effect.provideService(NodeConfig, config),
        ),
      ).pipe(
        Layer.provideMerge(
          NodeHttpServer.layer(createServer, { host: "127.0.0.1", port: 0 }),
        ),
      ),
    );
    closeHttp = () => server.dispose();
    const { address } = await server.runPromise(HttpServer.HttpServer);
    if (address._tag !== "TcpAddress") {
      throw new Error("Lease HTTP fixture did not bind a TCP port");
    }
    return {
      url: `http://127.0.0.1:${address.port.toString()}`,
      adminApiKey,
      databaseName,
      inspect: () => database.runPromise(StateQueueMutationLeasesDB.inspect()),
      close,
    };
  } catch (error) {
    await close();
    throw error;
  }
};
