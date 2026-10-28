/**
 * A node restarted on a database bound to another deployment is refused at
 * its database initialization, before it follows the chain or its driver
 * runs: every application table is left as it was. The production
 * lifecycle starts each generation in `runNode`'s order (the database
 * initialization, then the follower and the driver with the startup
 * preparation).
 */
import "./helpers/follower-emulator-installed.js";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect, it } from "vitest";

import type { NodeConfig } from "../src/services/config.js";
import { openProductionLifecycle } from "./helpers/production-lifecycle.js";
import { provideDatabaseLayers } from "./utils.js";

const run = <A, E>(
  effect: Effect.Effect<A, E, SqlClient.SqlClient | NodeConfig>,
) => Effect.runPromise(provideDatabaseLayers(effect));

/** Each application table's row count and a digest of its rows. */
const tableContents = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const tables = yield* sql<{
    schema: string;
    name: string;
  }>`SELECT schemaname AS schema, tablename AS name FROM pg_tables
    WHERE schemaname <> 'information_schema' AND left(schemaname, 3) <> 'pg_'
    ORDER BY schemaname, tablename`;
  const contents: Record<string, string> = {};
  for (const { schema, name } of tables) {
    const [row] = yield* sql.unsafe<{ rows: string; digest: string | null }>(
      `SELECT count(*)::text AS rows,
        md5(string_agg(t::text, E'\\n' ORDER BY t::text)) AS digest
        FROM "${schema}"."${name}" t`,
    );
    contents[`${schema}.${name}`] = `${row?.rows ?? "?"}:${row?.digest ?? ""}`;
  }
  return contents;
});

it("refuses a restart under another deployment before it writes anything", async () => {
  const h = await openProductionLifecycle();
  const bound = h.fixture.runtimeOverrides!.deploymentIdentity.manifestId;
  expect(bound).toMatch(/^[0-9a-f]{64}$/u);
  const other = bound!.replace(/^./u, (digit) => (digit === "0" ? "1" : "0"));
  let before: Record<string, string> | undefined;
  await expect(
    h.restartRuntime({
      manifestId: other,
      afterStop: async () => {
        before = await run(tableContents);
      },
    }),
  ).rejects.toThrow(
    `This database serves deployment ${bound!}, not the configured deployment ${other}`,
  );
  expect(before).toBeDefined();
  expect(before!["public.node_deployment"]).toMatch(/^1:/u);
  expect(await run(tableContents)).toEqual(before);
});
