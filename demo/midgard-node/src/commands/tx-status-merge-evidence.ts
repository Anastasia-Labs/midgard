import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";

import { admitDaPayloadRetentionReleaseAuthority } from "../database/daPayloadTerminalOutcomes.js";
import { Globals } from "../services/globals.js";
import { ContractDeploymentIdentity } from "../services/midgard-contracts.js";

export type TxStatusMergeEvidence = Readonly<{
  headerHash: string;
  headerStatus: string;
  mergeStatus: "finalized" | "not_finalized";
  confirmedLedgerFinalized: boolean;
}>;

/** Commit finalization is not merge finalization. Read the completed local
 * merge job together with the observer's current canonical merge projection.
 * That projection is replaced atomically on rollback; completed jobs alone
 * survive it. Register the read with the owner so a closed or superseded
 * recovery generation cannot publish its old evidence. Missing retained
 * evidence cannot assert completion, including after legitimate pruning. */
export const readTxStatusMergeEvidence = (txIds: readonly Buffer[]) =>
  Effect.gen(function* () {
    const identity = yield* ContractDeploymentIdentity;
    const authority = admitDaPayloadRetentionReleaseAuthority(
      identity.manifest,
    );
    const globals = yield* Globals;
    const owner = yield* Ref.get(globals.EVENT_HISTORY_OWNER);
    const empty = () => new Map<string, TxStatusMergeEvidence>();
    if (authority === null || owner === undefined || txIds.length === 0)
      return empty();
    const sql = yield* SqlClient.SqlClient;
    const jobDocument = sql`CASE jsonb_typeof(job.payload) WHEN 'string' THEN (job.payload #>> '{}')::jsonb ELSE job.payload END`;
    return yield* owner
      .runProducer((token) =>
        Effect.gen(function* () {
          // One statement observes membership, job, terminal projection and
          // generation together, rather than mixing independently read snapshots.
          const rows = yield* sql<{
            tx_hash: string;
            header_hash: string;
            status: string;
            merged: boolean;
          }>`SELECT encode(member.member_id, 'hex') AS tx_hash,
          encode(member.header_hash, 'hex') AS header_hash, pending.status,
          (pending.status = 'locally_applied' AND EXISTS (
            SELECT 1 FROM local_mutation_jobs job
            JOIN da_payload_terminal_outcomes outcome
              ON outcome.header_hash = pending.header_hash
            WHERE job.job_id = 'confirmed_merge_finalization:' || encode(pending.header_hash, 'hex')
              AND job.kind = 'confirmed_merge_finalization'
              AND job.status = 'completed'
              AND (${jobDocument}) ->> 'headerHash' = encode(pending.header_hash, 'hex')
              AND (${jobDocument}) ->> 'confirmedLedgerSnapshotRoot' = pending.expected_utxos_root
              AND outcome.terminal_outcome = 'merged'
              AND outcome.transition_kind = 'merge'
              AND outcome.deployment_identity_digest = ${authority.deploymentIdentityDigest}
              AND outcome.state_queue_policy_id = ${authority.stateQueuePolicyId}
              AND outcome.finality_depth >= ${authority.minimumFinalityDepth.toString()}::bigint
          ) AND EXISTS (
            SELECT 1 FROM event_history_authority history
            WHERE history.singleton = true
              AND history.deployment_identity = ${authority.deploymentIdentityDigest}
              AND history.deployment_identity = ${Buffer.from(token.deploymentIdentity, "hex")}
              AND history.owner_token = ${token.ownerToken}::uuid
              AND history.generation = ${token.generation}::bigint
              AND history.state = 'ready'
              AND history.lease_until > clock_timestamp()
          )) AS merged
          FROM pending_block_finalization_txs member
          JOIN pending_block_finalizations pending ON pending.header_hash = member.header_hash
          WHERE ${sql.in("member.member_id", [...txIds])} AND pending.status <> 'abandoned'`;
          const grouped = new Map<string, typeof rows>();
          for (const row of rows)
            grouped.set(row.tx_hash, [
              ...(grouped.get(row.tx_hash) ?? []),
              row,
            ]);
          const result = empty();
          for (const [txHash, members] of grouped) {
            if (members.length !== 1) continue;
            const row = members[0]!;
            result.set(txHash, {
              headerHash: row.header_hash,
              headerStatus: row.status,
              mergeStatus: row.merged ? "finalized" : "not_finalized",
              confirmedLedgerFinalized: row.merged,
            });
          }
          return result;
        }),
      )
      .pipe(
        Effect.catchTag("HistoryRecoverySuperseded", () =>
          Effect.succeed(empty()),
        ),
      );
  });
