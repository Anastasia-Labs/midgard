/**
 * The deployment this node's database serves (`node_deployment`): the
 * manifest id whose settlement jobs the enqueue trigger keys, recorded by
 * the startup preparation before the driver's first view.
 *
 * The first record binds the database to that deployment. A later start
 * under another manifest id is refused: the database holds another
 * deployment's state, so the node needs a fresh database, never a rebind.
 */
import { SqlClient } from "@effect/sql";
import { Data, Effect } from "effect";

import { DatabaseError } from "./utils/common.js";

export const tableName = "node_deployment";

/** The database already serves another deployment. */
export class NodeDeploymentMismatch extends Data.TaggedError(
  "NodeDeploymentMismatch",
)<{
  readonly recorded: string;
  readonly configured: string;
  readonly message: string;
}> {}

/**
 * Records `deploymentId` (a manifest id) as the deployment this node serves,
 * or fails with `NodeDeploymentMismatch` when the database already serves
 * another one (its record is kept).
 */
export const record = (deploymentId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    // One row either way: the inserted one, or the record already there
    // (the statement's snapshot does not see its own insert).
    const rows = yield* sql<{ deployment_id: string }>`
      WITH inserted AS (
        INSERT INTO node_deployment (singleton, deployment_id)
        VALUES (true, ${deploymentId})
        ON CONFLICT (singleton) DO NOTHING
        RETURNING deployment_id
      )
      SELECT deployment_id FROM inserted
      UNION ALL
      SELECT deployment_id FROM node_deployment WHERE singleton`;
    return rows[0]?.deployment_id;
  }).pipe(
    Effect.mapError(
      (cause) =>
        new DatabaseError({
          table: tableName,
          message: "Failed to record the node's deployment",
          cause,
        }),
    ),
    Effect.flatMap((recorded) =>
      recorded === undefined || recorded === deploymentId
        ? Effect.void
        : Effect.fail(
            new NodeDeploymentMismatch({
              recorded,
              configured: deploymentId,
              message: `This database serves deployment ${recorded}, not the configured deployment ${deploymentId}. Point the node at that deployment's manifest, or at a fresh database for this one.`,
            }),
          ),
    ),
  );
