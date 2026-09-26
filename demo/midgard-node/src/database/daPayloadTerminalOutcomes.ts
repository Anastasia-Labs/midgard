import {
  parseStateQueueAuthenticatedTransition,
  type StateQueueAuthenticatedTransition,
} from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { parseDeploymentManifestValue } from "../deployment-manifest.js";
import { Database } from "../services/database.js";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";

export const tableName = "da_payload_terminal_outcomes";

/**
 * The SQL condition under which the DA payload aliased `payload` is still owed
 * to committee peers and announcements. It is not once its header has an
 * authenticated `removed` outcome under this deployment, or while its journal
 * is abandoned under a named cause (a state-queue removal's transition digest,
 * or the replacement digest of a signed commit that missed its window and was
 * replaced). A replaced journal whose commit later wins its slot after all is
 * revived, which makes its payload owed again. A journal abandoned without a
 * digest may be revived, so its payload stays owed. `deploymentIdentityDigest`
 * is the verified manifest ID; a node running
 * a derived contract bundle has no authenticated outcomes to consult, so only
 * the journal arm applies.
 */
/** The verified manifest ID of the running deployment, if it has one. */
export const deploymentIdentityDigestOf = (identity: {
  readonly manifestId?: string;
}): Buffer | undefined =>
  identity.manifestId === undefined
    ? undefined
    : Buffer.from(identity.manifestId, "hex");

export const owedPayload = (
  sql: SqlClient.SqlClient,
  deploymentIdentityDigest: Buffer | undefined,
) => {
  const removed =
    deploymentIdentityDigest === undefined
      ? sql`FALSE`
      : sql`EXISTS (
          SELECT 1 FROM da_payload_terminal_outcomes outcome
          WHERE outcome.header_hash = payload.header_hash
            AND outcome.terminal_outcome = 'removed'
            AND outcome.deployment_identity_digest = ${deploymentIdentityDigest})`;
  return sql`NOT ${removed} AND NOT EXISTS (
    SELECT 1 FROM pending_block_finalizations journal
    WHERE journal.header_hash = payload.header_hash
      AND journal.status = 'abandoned'
      AND journal.correction_transition_digest IS NOT NULL)`;
};

const HEX_28 = /^[0-9a-f]{56}$/u;

export type DaPayloadRetentionReleaseAuthority = Readonly<{
  deploymentIdentityDigest: Buffer;
  stateQueuePolicyId: Buffer;
  minimumFinalityDepth: bigint;
}>;

/** Admits the deployment identity and finality depth that scope authenticated removal outcomes. */
export const admitDaPayloadRetentionReleaseAuthority = (
  manifestInput: unknown,
): DaPayloadRetentionReleaseAuthority | null => {
  try {
    const manifest = parseDeploymentManifestValue(manifestInput);
    const stateQueueMint = manifest.contracts.stateQueueMint;
    const stateQueuePolicyId = stateQueueMint?.scriptHash;
    if (
      stateQueueMint?.contract.type !== "PlutusV3" ||
      typeof stateQueuePolicyId !== "string" ||
      !HEX_28.test(stateQueuePolicyId) ||
      Reflect.has(manifest, "availabilityChallenges")
    ) {
      return null;
    }
    return Object.freeze({
      deploymentIdentityDigest: Buffer.from(manifest.manifestId, "hex"),
      stateQueuePolicyId: Buffer.from(stateQueuePolicyId, "hex"),
      minimumFinalityDepth: BigInt(manifest.l1Finality.confirmationDepth),
    });
  } catch {
    return null;
  }
};

export const recordAuthenticatedTransition = (
  transitionInput: unknown,
  manifestInput: unknown,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const authority = admitDaPayloadRetentionReleaseAuthority(manifestInput);
    const transition = parseStateQueueAuthenticatedTransition(transitionInput);
    if (
      authority === null ||
      transition === null ||
      transition.deploymentIdentityDigest !==
        authority.deploymentIdentityDigest.toString("hex") ||
      transition.stateQueuePolicyId !==
        authority.stateQueuePolicyId.toString("hex") ||
      BigInt(transition.finalityDepth) < authority.minimumFinalityDepth
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Refusing to persist an unauthenticated or foreign state-queue terminal transition",
          cause: "release/transition provenance mismatch",
        }),
      );
    }
    if (transition.removedHeaderHashes.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Terminal transition must remove exactly one header",
          cause: transition.transitionDigest,
        }),
      );
    }
    const sql = yield* SqlClient.SqlClient;
    const headerHash = Buffer.from(transition.removedHeaderHashes[0]!, "hex");
    const row = {
      header_hash: headerHash,
      terminal_outcome:
        transition.transitionKind === "merge" ? "merged" : "removed",
      transition_kind: transition.transitionKind,
      deployment_identity_digest: authority.deploymentIdentityDigest,
      state_queue_policy_id: authority.stateQueuePolicyId,
      transaction_hash: Buffer.from(transition.transactionHash, "hex"),
      block_hash: Buffer.from(transition.blockHash, "hex"),
      slot: transition.slot,
      block_no: transition.blockNo,
      transaction_index: Number(transition.transactionIndex),
      chain_point_id: Buffer.from(transition.chainPointId, "hex"),
      finality_depth: transition.finalityDepth,
      transition_digest: Buffer.from(transition.transitionDigest, "hex"),
      transition_record: JSON.stringify(
        transition as StateQueueAuthenticatedTransition,
      ),
    } as const;
    const rows = yield* sql<{ readonly header_hash: Buffer }>`
      INSERT INTO ${sql(tableName)} ${sql.insert(row)}
      ON CONFLICT (deployment_identity_digest, header_hash) DO UPDATE SET
        created_at = ${sql(tableName)}.created_at
      WHERE ${sql(tableName)}.terminal_outcome = EXCLUDED.terminal_outcome
        AND ${sql(tableName)}.transition_kind = EXCLUDED.transition_kind
        AND ${sql(tableName)}.deployment_identity_digest = EXCLUDED.deployment_identity_digest
        AND ${sql(tableName)}.state_queue_policy_id = EXCLUDED.state_queue_policy_id
        AND ${sql(tableName)}.transaction_hash = EXCLUDED.transaction_hash
        AND ${sql(tableName)}.block_hash = EXCLUDED.block_hash
        AND ${sql(tableName)}.slot = EXCLUDED.slot
        AND ${sql(tableName)}.block_no = EXCLUDED.block_no
        AND ${sql(tableName)}.transaction_index = EXCLUDED.transaction_index
        AND ${sql(tableName)}.chain_point_id = EXCLUDED.chain_point_id
        AND ${sql(tableName)}.finality_depth = EXCLUDED.finality_depth
        AND ${sql(tableName)}.transition_digest = EXCLUDED.transition_digest
        AND ${sql(tableName)}.transition_record = EXCLUDED.transition_record
      RETURNING header_hash`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Conflicting terminal outcome already exists for header",
          cause: transition.removedHeaderHashes[0],
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`recordAuthenticatedTransitionV1 ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to persist authenticated terminal transition",
    ),
  );

/** Revokes exactly one previously admitted outcome after authenticated rollback. */
export const revokeAuthenticatedTransition = (
  transitionInput: unknown,
  manifestInput: unknown,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const authority = admitDaPayloadRetentionReleaseAuthority(manifestInput);
    const transition = parseStateQueueAuthenticatedTransition(transitionInput);
    if (
      authority === null ||
      transition === null ||
      transition.deploymentIdentityDigest !==
        authority.deploymentIdentityDigest.toString("hex") ||
      transition.stateQueuePolicyId !==
        authority.stateQueuePolicyId.toString("hex") ||
      transition.removedHeaderHashes.length !== 1
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Refusing to revoke a foreign terminal transition",
          cause: "release/transition provenance mismatch",
        }),
      );
    }
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly header_hash: Buffer }>`
      DELETE FROM ${sql(tableName)}
      WHERE deployment_identity_digest = ${authority.deploymentIdentityDigest}
        AND header_hash = ${Buffer.from(transition.removedHeaderHashes[0]!, "hex")}
        AND transition_digest = ${Buffer.from(transition.transitionDigest, "hex")}
        AND transaction_hash = ${Buffer.from(transition.transactionHash, "hex")}
      RETURNING header_hash`;
    if (rows.length > 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Authenticated rollback revoked duplicate terminal outcomes",
          cause: transition.transitionDigest,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`revokeAuthenticatedTransitionV1 ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to revoke authenticated terminal transition",
    ),
  );

export const clear: Effect.Effect<void, DatabaseError, Database> =
  clearTable(tableName);
