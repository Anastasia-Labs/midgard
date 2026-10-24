/**
 * The deployment this node's database serves (`node_deployment`): the
 * manifest id whose settlement jobs the enqueue trigger keys, recorded by
 * the startup preparation before the driver's first view.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { DatabaseError } from "./utils/common.js";

export const tableName = "node_deployment";

/** Records `deploymentId` (a manifest id) as the deployment this node serves. */
export const record = (deploymentId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO node_deployment (singleton, deployment_id)
      VALUES (true, ${deploymentId})
      ON CONFLICT (singleton) DO UPDATE
        SET deployment_id = EXCLUDED.deployment_id, updated_at = NOW()
        WHERE node_deployment.deployment_id <> EXCLUDED.deployment_id`;
  }).pipe(
    Effect.mapError(
      (cause) =>
        new DatabaseError({
          table: tableName,
          message: "Failed to record the node's deployment",
          cause,
        }),
    ),
  );
