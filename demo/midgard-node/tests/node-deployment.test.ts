import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

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
});
