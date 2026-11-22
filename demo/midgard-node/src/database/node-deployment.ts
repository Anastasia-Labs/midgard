/**
 * The deployment this node's database serves (`node_deployment`): the
 * hub-oracle policy id whose settlement jobs the enqueue trigger keys,
 * recorded by the startup's database initialization, under the instance lock
 * and before any other startup step writes
 * (`listen-startup.initialize-database.ts`).
 *
 * The first record binds the database to that deployment. A later start
 * under another hub-oracle policy id is refused: the database holds another
 * deployment's state, so the node needs a fresh database, never a rebind.
 * A start without a deployment manifest (contracts derived from the
 * blueprint) records nothing and is not checked.
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
 * Records `hubOraclePolicyId` as the deployment this node serves, or fails
 * with `NodeDeploymentMismatch` when the database already serves another one
 * (its record is kept).
 */
export const record = (hubOraclePolicyId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    // One row: the inserted one, or the record already there (the
    // statement's snapshot does not see its own insert). None when another
    // session's insert committed after this statement's snapshot: the
    // conflict waited for it, so a second read sees it.
    const rows = yield* sql<{ hub_oracle_policy_id: string }>`
      WITH inserted AS (
        INSERT INTO node_deployment (singleton, hub_oracle_policy_id)
        VALUES (true, ${hubOraclePolicyId})
        ON CONFLICT (singleton) DO NOTHING
        RETURNING hub_oracle_policy_id
      )
      SELECT hub_oracle_policy_id FROM inserted
      UNION ALL
      SELECT hub_oracle_policy_id FROM node_deployment WHERE singleton`;
    const recorded =
      rows[0]?.hub_oracle_policy_id ??
      (yield* sql<{
        hub_oracle_policy_id: string;
      }>`SELECT hub_oracle_policy_id FROM node_deployment WHERE singleton`)[0]
        ?.hub_oracle_policy_id;
    if (recorded === undefined)
      return yield* Effect.fail(
        new Error("No deployment record after recording one"),
      );
    return recorded;
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
      recorded === hubOraclePolicyId
        ? Effect.void
        : Effect.fail(
            new NodeDeploymentMismatch({
              recorded,
              configured: hubOraclePolicyId,
              message: `This database serves deployment ${recorded}, not the configured deployment ${hubOraclePolicyId}. Point the node at that deployment's manifest, or at a fresh database for this one.`,
            }),
          ),
    ),
  );
