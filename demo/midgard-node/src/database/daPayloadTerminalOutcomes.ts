import {
  type DaAvailabilityRetentionAuthority,
  type DaAvailabilityRetentionEvidence,
  parseDaAvailabilityRetentionEvidence,
  parseStateQueueAuthenticatedTransition,
  type StateQueueAuthenticatedTransition,
} from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { credentialToAddress } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { parseDeploymentManifestValue } from "../deployment-manifest.js";
import { Database } from "../services/database.js";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";

export const tableName = "da_payload_terminal_outcomes";

const HEX_28 = /^[0-9a-f]{56}$/u;

export type DaPayloadRetentionReleaseAuthority = Readonly<{
  deploymentIdentityDigest: Buffer;
  stateQueuePolicyId: Buffer;
  minimumFinalityDepth: bigint;
  availabilityChallengeCapability: "missing" | "deployed_unobserved";
  availabilityPolicyId: Buffer | null;
  stateQueueAddress: string;
}>;

/** Complete hash-bound deployed roles establish capability, never per-header inactivity. */
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
    const availabilityRoles = [
      "availabilityChallengeSpend",
      "availabilityChallengeMint",
      "availabilityChallengeBondWithdraw",
      "availabilityChallengeOpenWithdraw",
      "availabilityChallengeSettleWithdraw",
      "availabilityChallengeCloseWithdraw",
      "availabilityChallengeTimeoutWithdraw",
      "stateQueueUnavailableTimeoutWithdraw",
    ];
    const deployed = availabilityRoles.every((name) => {
      const role = manifest.contracts[name];
      return (
        role?.contract.type === "PlutusV3" &&
        HEX_28.test(role.scriptHash) &&
        role.refScriptUTxO != null
      );
    });
    return Object.freeze({
      deploymentIdentityDigest: Buffer.from(manifest.manifestId, "hex"),
      stateQueuePolicyId: Buffer.from(stateQueuePolicyId, "hex"),
      minimumFinalityDepth: BigInt(manifest.l1Finality.confirmationDepth),
      availabilityChallengeCapability: deployed
        ? "deployed_unobserved"
        : "missing",
      availabilityPolicyId: deployed
        ? Buffer.from(
            manifest.contracts.availabilityChallengeMint!.scriptHash,
            "hex",
          )
        : null,
      stateQueueAddress: credentialToAddress(
        manifest.network === "Mainnet" ? "Mainnet" : "Preprod",
        {
          type: "Script",
          hash: manifest.contracts.stateQueueSpend!.scriptHash,
        },
      ),
    });
  } catch {
    return null;
  }
};

export const availabilityRetentionAuthority = (
  authority: DaPayloadRetentionReleaseAuthority,
): DaAvailabilityRetentionAuthority | null =>
  authority.availabilityPolicyId === null
    ? null
    : {
        deploymentIdentityDigest:
          authority.deploymentIdentityDigest.toString("hex"),
        stateQueuePolicyId: authority.stateQueuePolicyId.toString("hex"),
        stateQueueAddress: authority.stateQueueAddress,
        availabilityPolicyId: authority.availabilityPolicyId.toString("hex"),
        minimumFinalityDepth: authority.minimumFinalityDepth,
      };

export const recordAuthenticatedTransition = (
  transitionInput: unknown,
  manifestInput: unknown,
  availabilityEvidenceInput?: DaAvailabilityRetentionEvidence | null,
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
    const availabilityAuthority = availabilityRetentionAuthority(authority);
    const availabilityEvidence =
      availabilityEvidenceInput == null
        ? null
        : availabilityAuthority === null
          ? null
          : parseDaAvailabilityRetentionEvidence(
              availabilityEvidenceInput,
              transition,
              availabilityAuthority,
            );
    if (availabilityEvidenceInput != null && availabilityEvidence === null)
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Refusing malformed, active, or foreign availability retention evidence",
          cause: transition.transitionDigest,
        }),
      );
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
      availability_terminal_evidence:
        availabilityEvidence === null
          ? null
          : JSON.stringify(availabilityEvidence),
      transition_record: JSON.stringify(
        transition as StateQueueAuthenticatedTransition,
      ),
    } as const;
    const rows = yield* sql<{ readonly header_hash: Buffer }>`
      INSERT INTO ${sql(tableName)} ${sql.insert(row)}
      ON CONFLICT (deployment_identity_digest, header_hash) DO UPDATE SET
        created_at = ${sql(tableName)}.created_at,
        availability_terminal_evidence = COALESCE(${sql(tableName)}.availability_terminal_evidence, EXCLUDED.availability_terminal_evidence)
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
        AND (${sql(tableName)}.availability_terminal_evidence IS NULL OR EXCLUDED.availability_terminal_evidence IS NULL OR ${sql(tableName)}.availability_terminal_evidence = EXCLUDED.availability_terminal_evidence)
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
