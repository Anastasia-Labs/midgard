import { type StateQueueTransitionNode } from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import * as DaPayloadTerminalOutcomesDB from "../database/daPayloadTerminalOutcomes.js";
import {
  assertRewoundRemovalsStand,
  parseStateQueueCorrectionObserverState,
  type StateQueueCorrectionObserverStore,
} from "./state-queue-correction-observer.parse-state-queue-correction-observer-state.js";

/**
 * Production observer store. The cursor/admitted set and its terminal-outcome
 * projection commit in one SQL transaction, so a crash cannot leave a newly
 * admitted deletion authority without its durable observer state (or retain a
 * revoked authority after the replacement cursor commits).
 */
export const createDatabaseStateQueueCorrectionObserverStore = ({
  sql,
  deploymentManifest,
}: {
  readonly sql: SqlClient.SqlClient;
  readonly deploymentManifest: unknown;
}): StateQueueCorrectionObserverStore => ({
  load: async () => {
    const authority =
      DaPayloadTerminalOutcomesDB.admitDaPayloadRetentionReleaseAuthority(
        deploymentManifest,
      );
    if (authority === null) {
      throw new Error("Observer database store has no authenticated release");
    }
    const rows = await Effect.runPromise(
      sql<{ readonly state_record: unknown }>`
        SELECT state_record
        FROM state_queue_terminal_observer_states
        WHERE deployment_identity_digest = ${authority.deploymentIdentityDigest}
        LIMIT 1`.pipe(Effect.provideService(SqlClient.SqlClient, sql)),
    );
    const state = rows[0]?.state_record ?? null;
    return typeof state === "string" ? (JSON.parse(state) as unknown) : state;
  },
  save: async (stateInput) => {
    const state = parseStateQueueCorrectionObserverState(stateInput);
    const authority =
      DaPayloadTerminalOutcomesDB.admitDaPayloadRetentionReleaseAuthority(
        deploymentManifest,
      );
    if (
      state === null ||
      authority === null ||
      state.deploymentIdentityDigest !==
        authority.deploymentIdentityDigest.toString("hex") ||
      state.stateQueuePolicyId !== authority.stateQueuePolicyId.toString("hex")
    ) {
      throw new Error(
        "Observer database store refused foreign or non-canonical state",
      );
    }
    const program = sql.withTransaction(
      Effect.gen(function* () {
        const txSql = yield* SqlClient.SqlClient;
        yield* txSql`
          DELETE FROM da_payload_terminal_outcomes
          WHERE deployment_identity_digest = ${authority.deploymentIdentityDigest}`;
        for (const transition of state.admitted) {
          yield* DaPayloadTerminalOutcomesDB.recordAuthenticatedTransition(
            transition,
            deploymentManifest,
          );
        }
        const rows = yield* txSql<{
          readonly deployment_identity_digest: Buffer;
        }>`
          INSERT INTO state_queue_terminal_observer_states (
            deployment_identity_digest,
            state_queue_policy_id,
            state_digest,
            state_record,
            updated_at
          ) VALUES (
            ${authority.deploymentIdentityDigest},
            ${authority.stateQueuePolicyId},
            ${Buffer.from(state.stateDigest, "hex")},
            ${JSON.stringify(state)},
            NOW()
          )
          ON CONFLICT (deployment_identity_digest) DO UPDATE SET
            state_queue_policy_id = EXCLUDED.state_queue_policy_id,
            state_digest = EXCLUDED.state_digest,
            state_record = EXCLUDED.state_record,
            updated_at = NOW()
          WHERE state_queue_terminal_observer_states.state_queue_policy_id = EXCLUDED.state_queue_policy_id
          RETURNING deployment_identity_digest`;
        if (rows.length !== 1) {
          return yield* Effect.fail(
            new Error("Observer database state conflicts with stored policy"),
          );
        }
        // A rewind moved the native ledger off every header it removed and
        // has no forward re-application; a view that stops removing one is
        // refused in the same transaction, so it is never persisted.
        yield* assertRewoundRemovalsStand(state);
      }),
    );
    await Effect.runPromise(
      program.pipe(Effect.provideService(SqlClient.SqlClient, sql)),
    );
  },
});

export const sameQueue = (
  left: readonly StateQueueTransitionNode[],
  right: readonly StateQueueTransitionNode[],
): boolean =>
  left.length === right.length &&
  left.every(
    (node, index) =>
      node.headerHash === right[index]?.headerHash &&
      node.outRef === right[index]?.outRef,
  );
