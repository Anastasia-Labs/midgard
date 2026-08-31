import {
  parseStateQueueAuthenticatedTransitionV1,
  type StateQueueAuthenticatedTransitionV1,
} from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { parseDeploymentManifestV1Value } from "@/deployment-manifest-v1.js";
import { Database } from "@/services/database.js";

export const tableName = "da_payload_terminal_outcomes";

const HEX_28 = /^[0-9a-f]{56}$/u;

export type DaPayloadRetentionReleaseAuthorityV1 = Readonly<{
  deploymentIdentityDigest: Buffer;
  stateQueuePolicyId: Buffer;
  minimumFinalityDepth: bigint;
  availabilityChallengeCapability: "missing" | "deployed_inactive";
}>;

/**
 * Derives the deletion authority only from the exact, hash-bound release
 * manifest. The V1 manifest schema now carries an `availabilityChallenge`
 * section, but it is release-authenticated Q58 *parameters* (response classes,
 * geometry, bond and fee ceilings) — never a liveness observation:
 * `verifyDeploymentManifestV1Identity` runs
 * `parseDeploymentManifestV1AvailabilityChallenge` before
 * `parseDeploymentManifestV1Value` returns, and that parser rejects any key
 * outside that parameters vocabulary. So a caller still cannot smuggle a
 * challenge-state claim in through this field, and the capability stays
 * `missing`: deliberately not an assertion that no challenge is active.
 * Production deletion remains disabled (`pruneOlderThan` requires
 * `deployed_inactive`) until Q58 supplies an authenticated on-chain inactive
 * observation. The `availabilityChallenges` guard below is retained as
 * defence in depth against a caller-authored plural sibling.
 */
export const admitDaPayloadRetentionReleaseAuthorityV1 = (
  manifestInput: unknown,
): DaPayloadRetentionReleaseAuthorityV1 | null => {
  try {
    const manifest = parseDeploymentManifestV1Value(manifestInput);
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
      availabilityChallengeCapability: "missing",
    });
  } catch {
    return null;
  }
};

export const recordAuthenticatedTransitionV1 = (
  transitionInput: unknown,
  manifestInput: unknown,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const authority = admitDaPayloadRetentionReleaseAuthorityV1(manifestInput);
    const transition =
      parseStateQueueAuthenticatedTransitionV1(transitionInput);
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
        transition as StateQueueAuthenticatedTransitionV1,
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
export const revokeAuthenticatedTransitionV1 = (
  transitionInput: unknown,
  manifestInput: unknown,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const authority = admitDaPayloadRetentionReleaseAuthorityV1(manifestInput);
    const transition =
      parseStateQueueAuthenticatedTransitionV1(transitionInput);
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
