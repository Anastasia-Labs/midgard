/**
 * The node's database initialization at startup (stage
 * `database_initialization`), run under the instance lock and before
 * protocol initialization and the L1 follower: the schema gate
 * (`InitDB.program`), then the deployment record (`node_deployment`), which
 * binds the database to the configured deployment.
 *
 * A database bound to another deployment fails it with
 * `NodeDeploymentMismatch`, which nothing retries: startup fails before any
 * step publishes, follows or writes anything under the configured
 * deployment. A start without a deployment manifest records nothing
 * (`node-deployment.ts`).
 */
import { Effect } from "effect";

import { InitDB } from "../database/index.js";
import * as NodeDeployment from "../database/node-deployment.js";
import { ContractDeploymentIdentity } from "../services/midgard-contracts.js";

export const initializeNodeDatabase = Effect.gen(function* () {
  yield* InitDB.program;
  const { manifestId } = yield* ContractDeploymentIdentity;
  if (manifestId !== undefined) yield* NodeDeployment.record(manifestId);
});
