import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { sqlErrorToDatabaseError } from "./utils/common.js";

export type SettlementKind = "deposit" | "withdrawal";
export type SettlementPhase = "absorb" | "initialize" | "fund" | "conclude";
export type SettlementJob = {
  deployment_id: string;
  kind: SettlementKind;
  event_id: string;
  phase: SettlementPhase | "complete";
  failures: number;
  verified_generation: string;
};
export type SettlementAttempt = {
  deployment_id: string;
  tx_hash: string;
  kind: SettlementKind;
  event_id: string;
  phase: SettlementPhase;
  signed_cbor: string;
  required_outputs: number[];
  fee_inputs: string[];
  recovery: boolean;
  status: "pending" | "confirmed" | "expired";
};
export type SettlementOwner = {
  deploymentId: string;
  walletAddress: string;
  token: string;
};
/** Renew's refusal, as opposed to a database failure. */
export class SettlementOwnershipRefused extends Error {}

export const renew = (owner: SettlementOwner) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql`INSERT INTO settlement_owners
    (deployment_id, wallet_address, owner_token, lease_until)
    VALUES (${owner.deploymentId}, ${owner.walletAddress}, ${owner.token}::uuid, clock_timestamp() + interval '60 seconds')
    ON CONFLICT (deployment_id) DO UPDATE SET owner_token = EXCLUDED.owner_token, lease_until = EXCLUDED.lease_until
    WHERE settlement_owners.wallet_address = EXCLUDED.wallet_address
      AND (settlement_owners.owner_token = EXCLUDED.owner_token OR settlement_owners.lease_until < clock_timestamp())
    RETURNING deployment_id`;
    if (rows.length !== 1)
      return yield* Effect.fail(
        new SettlementOwnershipRefused(
          "Settlement wallet identity changed or another node owns settlement",
        ),
      );
  });

/** The settlement wallet a deployment's ownership row is bound to. Tells a
 * renew refused by another owner's live lease, which expiry resolves, from
 * one refused because the settlement wallet changed, which nothing does. */
export const boundWalletAddress = (deploymentId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ wallet_address: string }>`SELECT wallet_address
    FROM settlement_owners WHERE deployment_id = ${deploymentId}`;
    return rows[0]?.wallet_address;
  });

/** Fences every journal write and submit against worker takeover and history recovery. */
export const assertOwner = (owner: SettlementOwner) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ generation: string }>`SELECT a.generation::text
    FROM settlement_owners s JOIN event_history_authority a ON a.singleton
    WHERE s.deployment_id = ${owner.deploymentId} AND s.wallet_address = ${owner.walletAddress}
      AND s.owner_token = ${owner.token}::uuid AND s.lease_until > clock_timestamp()
      AND a.deployment_identity = ${Buffer.from(owner.deploymentId, "hex")}
      AND a.state = 'ready' AND a.lease_until > clock_timestamp()`;
    if (rows.length !== 1)
      return yield* Effect.fail(
        new Error(
          "Settlement paused: ownership or authenticated history is not ready",
        ),
      );
    return rows[0]!.generation;
  });

export const pending = (deploymentId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<SettlementAttempt>`SELECT * FROM settlement_attempts
    WHERE deployment_id = ${deploymentId} AND status = 'pending'`;
    return rows[0];
  });

/** Count actual canonical blocks in the authenticated follower, never slots. */
export const confirmationDepth = (owner: SettlementOwner, blockHash: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* assertOwner(owner);
    const rows = yield* sql<{
      depth: string;
    }>`SELECT (c.head_height - b.block_height + 1)::text AS depth
    FROM event_history_cursor c JOIN event_history_block_applications b USING (binding_digest)
    WHERE c.manifest_id = ${Buffer.from(owner.deploymentId, "hex")}
      AND b.block_hash = ${Buffer.from(blockHash, "hex")} AND b.canonical`;
    return rows.length === 1 ? Number(rows[0]!.depth) : 0;
  });

export const nextJob = (
  owner: SettlementOwner,
  generation: string,
  preferCompleted = false,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<SettlementJob>`SELECT * FROM (
    (SELECT * FROM settlement_jobs WHERE deployment_id = ${owner.deploymentId}
      AND due_at <= clock_timestamp() AND phase <> 'complete'
      ORDER BY due_at, created_at, kind, event_id LIMIT 1)
    UNION ALL
    (SELECT * FROM settlement_jobs WHERE deployment_id = ${owner.deploymentId}
      AND due_at <= clock_timestamp() AND phase = 'complete'
      AND verified_generation < ${generation}::bigint
      ORDER BY verified_generation, due_at, created_at, kind, event_id LIMIT 1)
    ) candidates ORDER BY CASE WHEN (phase = 'complete') = ${preferCompleted} THEN 0 ELSE 1 END,
      due_at, created_at, kind, event_id LIMIT 1`;
    return rows[0];
  });

export const nextDeferredJob = (owner: SettlementOwner) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      due_at: string;
      last_error: string | null;
    }>`SELECT due_at::text, last_error
    FROM settlement_jobs WHERE deployment_id = ${owner.deploymentId} AND phase <> 'complete'
    ORDER BY due_at, created_at LIMIT 1`;
    return rows[0];
  });

export type SettlementFailingJob = {
  kind: SettlementKind;
  event_id: string;
  phase: SettlementPhase;
  failures: number;
  last_error: string;
  due_at: Date;
};

/** Read-only backlog of the current deployment's settlement jobs: how many
 * are unfinished, and the earliest-due unfinished ones whose last attempt
 * failed, with the worker's error. Jobs of another deployment never count. */
export const inspectBacklog = (failingLimit: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const [counts, failing] = yield* Effect.all(
      [
        sql<{ count: string }>`SELECT COUNT(*)::text AS count
        FROM settlement_jobs j JOIN event_history_authority a ON a.singleton
        WHERE j.deployment_id = encode(a.deployment_identity, 'hex')
          AND j.phase <> 'complete'`,
        sql<SettlementFailingJob>`SELECT j.kind, j.event_id, j.phase, j.failures,
          j.last_error, j.due_at
        FROM settlement_jobs j JOIN event_history_authority a ON a.singleton
        WHERE j.deployment_id = encode(a.deployment_identity, 'hex')
          AND j.phase <> 'complete' AND j.last_error IS NOT NULL
        ORDER BY j.due_at, j.created_at, j.kind, j.event_id
        LIMIT ${failingLimit}`,
      ],
      { concurrency: "unbounded" },
    );
    return {
      unfinishedJobs: BigInt(counts[0]?.count ?? "0"),
      failingJobs: failing,
    };
  });

/** A rollback can restore the fee coin of a receipt still marked confirmed.
 * Find it by the small current wallet set, without scanning receipt history. */
export const restoredFeeReceipt = (
  owner: SettlementOwner,
  inputs: readonly string[],
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<SettlementAttempt>`SELECT * FROM settlement_attempts
    WHERE deployment_id = ${owner.deploymentId} AND status = 'confirmed'
      AND fee_inputs && ARRAY(SELECT jsonb_array_elements_text(CAST(${JSON.stringify(inputs)} AS TEXT)::jsonb))
    ORDER BY created_at, tx_hash LIMIT 1`;
    return rows[0];
  });

export const attempts = (job: SettlementJob) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<SettlementAttempt>`SELECT * FROM settlement_attempts
    WHERE deployment_id = ${job.deployment_id} AND kind = ${job.kind} AND event_id = ${job.event_id}
      AND status = 'confirmed' ORDER BY created_at, tx_hash`;
  });

export const resumeReceipt = (
  owner: SettlementOwner,
  attempt: SettlementAttempt,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* assertOwner(owner);
    yield* sql`UPDATE settlement_attempts SET status = 'pending', recovery = true
    WHERE deployment_id = ${owner.deploymentId} AND tx_hash = ${attempt.tx_hash} AND status = 'confirmed'`;
  });

/** A descendant of a body proven expired cannot land either. This allows
 * interrupted payout chains to rewind without forgetting signed descendants. */
export const expiredParents = (
  owner: SettlementOwner,
  hashes: readonly string[],
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      tx_hash: string;
    }>`SELECT tx_hash FROM settlement_attempts
    WHERE deployment_id = ${owner.deploymentId} AND status = 'expired' AND tx_hash IN ${sql.in(hashes)}`;
    return new Set(rows.map((row) => row.tx_hash));
  });

export const saveAttempt = (
  owner: SettlementOwner,
  attempt: SettlementAttempt,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* assertOwner(owner);
        if (
          (yield* restoredFeeReceipt(owner, attempt.fee_inputs)) !== undefined
        )
          return yield* Effect.fail(
            new Error(
              "Settlement fee input belongs to a receipt that must be reconciled first",
            ),
          );
        // Unique pending index excludes all other jobs until this exact body is reconciled.
        yield* sql`INSERT INTO settlement_attempts
      (deployment_id, kind, event_id, phase, tx_hash, signed_cbor, required_outputs, fee_inputs, status, recovery, hold_slot)
      VALUES (${attempt.deployment_id}, ${attempt.kind}, ${attempt.event_id}, ${attempt.phase},
        ${attempt.tx_hash}, ${attempt.signed_cbor},
        ARRAY(SELECT jsonb_array_elements_text(CAST(${JSON.stringify(attempt.required_outputs)} AS TEXT)::jsonb))::integer[],
        ARRAY(SELECT jsonb_array_elements_text(CAST(${JSON.stringify(attempt.fee_inputs)} AS TEXT)::jsonb)),
        ${attempt.status}, ${attempt.recovery}, (SELECT point_slot FROM event_history_authority WHERE singleton))`;
      }),
    );
  });

/** Keep the block evidence needed by an interrupted confirmation. The partial
 * singleton index makes this independent of settled event history size. */
export const settlementRetentionHoldSlot = (deploymentId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      hold_slot: string;
    }>`SELECT hold_slot::text FROM settlement_attempts
    WHERE deployment_id = ${deploymentId} AND status = 'pending'`;
    return rows[0] === undefined ? undefined : Number(rows[0].hold_slot);
  }).pipe(
    sqlErrorToDatabaseError(
      "settlement_attempts",
      "Failed to read settlement retention hold",
    ),
  );

export const updateJob = (
  owner: SettlementOwner,
  job: Pick<SettlementJob, "kind" | "event_id">,
  phase: SettlementJob["phase"],
  generation: string,
  delayMs = 0,
  error: string | null = null,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* assertOwner(owner);
    yield* sql`UPDATE settlement_jobs SET phase = ${phase}, verified_generation = ${generation}::bigint,
    due_at = clock_timestamp() + ${delayMs} * interval '1 millisecond', last_error = ${error},
    failures = CASE WHEN ${error}::text IS NULL THEN 0 ELSE failures + 1 END
    WHERE deployment_id = ${owner.deploymentId} AND kind = ${job.kind} AND event_id = ${job.event_id}`;
  });

export const finishAttempt = (
  owner: SettlementOwner,
  attempt: SettlementAttempt,
  status: "confirmed" | "expired",
  phase: SettlementJob["phase"],
  generation: string,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* assertOwner(owner);
        yield* sql`UPDATE settlement_attempts SET status = ${status}
      WHERE tx_hash = ${attempt.tx_hash} AND deployment_id = ${owner.deploymentId}`;
        yield* updateJob(
          owner,
          attempt,
          phase,
          attempt.recovery ? "-1" : generation,
        );
      }),
    );
  });
