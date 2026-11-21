import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import * as NodeDeployment from "../src/database/node-deployment.js";
import type { NodeConfig } from "../src/services/config.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const run = <A, E>(
  effect: Effect.Effect<A, E, SqlClient.SqlClient | NodeConfig>,
) => Effect.runPromise(provideDatabaseLayers(effect));

const recorded = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    deployment_id: string;
  }>`SELECT deployment_id FROM node_deployment`;
  return rows.map((row) => row.deployment_id);
});

const deploymentA = "a1".repeat(32);
const deploymentB = "b2".repeat(32);

beforeEach(() => run(resetApplicationTables));

describe("the node database's deployment record", () => {
  it("binds an empty database to the first deployment and accepts it again", async () => {
    await run(NodeDeployment.record(deploymentA));
    await run(NodeDeployment.record(deploymentA));
    expect(await run(recorded)).toEqual([deploymentA]);
  });

  it("refuses a database that serves another deployment and keeps its record", async () => {
    await run(NodeDeployment.record(deploymentA));
    const refused = await run(Effect.flip(NodeDeployment.record(deploymentB)));
    expect(refused.message).toContain(deploymentA);
    expect(refused.message).toContain(deploymentB);
    expect(await run(recorded)).toEqual([deploymentA]);
  });

  it("refuses a start whose first record lost to another session's", async () => {
    // Another session's record is inserted, not yet committed, when this
    // start records its own: the conflict waits for that commit, which the
    // statement's snapshot does not see.
    let inserted!: () => void;
    let commit!: () => void;
    const insertedOnce = new Promise<void>((resolve) => (inserted = resolve));
    const committed = new Promise<void>((resolve) => (commit = resolve));
    const winner = run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql.withTransaction(
          Effect.gen(function* () {
            yield* sql`INSERT INTO node_deployment (singleton, deployment_id)
              VALUES (true, ${deploymentA})`;
            inserted();
            yield* Effect.promise(() => committed);
          }),
        );
      }),
    );
    await insertedOnce;
    const loser = run(Effect.either(NodeDeployment.record(deploymentB)));
    await vi.waitFor(
      async () =>
        expect(
          await run(
            Effect.gen(function* () {
              const sql = yield* SqlClient.SqlClient;
              const rows = yield* sql<{ waiting: number }>`
                SELECT count(*)::int AS waiting FROM pg_stat_activity
                WHERE datname = current_database() AND wait_event_type = 'Lock'`;
              return rows[0]?.waiting;
            }),
          ),
        ).toBe(1),
      { timeout: 10_000, interval: 20 },
    );
    commit();
    await winner;
    const outcome = await loser;
    expect(outcome._tag).toBe("Left");
    expect(outcome._tag === "Left" ? outcome.left.message : "").toContain(
      `This database serves deployment ${deploymentA}, not the configured deployment ${deploymentB}`,
    );
    expect(await run(recorded)).toEqual([deploymentA]);
  });
});
