import type { Reporter, Vitest, TestRunEndReason } from "vitest/node";

import { Effect, Layer, Runtime, Cause } from "effect";
import { SqlClient, SqlError } from "@effect/sql";
import { ConfigError } from "effect/ConfigError";

import { initializeDb } from "../src/database/init";

import { testSqlLayer } from "./database/runner";

const runGlobalEffect = async (
  description: string,
  effectToRun: Effect.Effect<any, any, SqlClient.SqlClient>,
  layer: Layer.Layer<
    SqlClient.SqlClient,
    ConfigError | SqlError.SqlError,
    never
  >,
): Promise<void> => {
  const finalEffect = Effect.provide(effectToRun, layer);

  try {
    await Effect.runPromise(
      finalEffect as Effect.Effect<
        any,
        ConfigError | SqlError.SqlError | Error,
        never
      >,
    );
  } catch (error) {
    console.error(
      `[Global ${description}] Effect failed:`,
      Cause.pretty(error),
    );
    throw error;
  }
};

export default class GlobalSetupReporter implements Reporter {
  async onInit() {
    await runGlobalEffect("[Reporter] Setup", initializeDb(), testSqlLayer);
  }
  async onTestRunEnd() {
    await runGlobalEffect("[Reporter] Teardown", cleanupDb(), testSqlLayer);
  }
}

export const cleanupDb = (): Effect.Effect<
  void,
  SqlError.SqlError,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const tables = [
      "blocks",
      "mempool",
      "mempool_ledger",
      "immutable",
      "confirmed_ledger",
      "latest_ledger",
      "latest_ledger_clone",
    ];
    for (const table of tables) {
      yield* sql`DROP TABLE IF EXISTS ${sql(table)} CASCADE`;
    }
  });
