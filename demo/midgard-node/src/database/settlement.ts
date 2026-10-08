import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

export type SettlementKind = "deposit" | "withdrawal";
export type SettlementPhase = "absorb" | "initialize" | "fund" | "conclude";
export type SettlementJob = {
  deployment_id: string;
  kind: SettlementKind;
  event_id: string;
  phase: SettlementPhase | "complete";
  failures: number;
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
  /**
   * `pending`: signed and journaled; its L1 outcome is the intent journal's
   * derived status (plan §8.2), never stored. `final`: it landed more than k
   * blocks deep, written by the follower's prune step in the step that prunes
   * its journal entry (`settlement.final-hook.ts`). `expired`: proven never
   * to land (`expireAttempt`).
   */
  status: "pending" | "final" | "expired";
};
/** An attempt the tick derives, with its job's stored phase. */
export type OpenSettlementAttempt = SettlementAttempt & {
  job_phase: SettlementJob["phase"];
  /** No later attempt of its job is pending or final. */
  latest: boolean;
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

/**
 * The attempts whose outcome the tick derives, oldest first: every pending
 * attempt, and every final attempt of an unfinished job (one that reached
 * depth > k while the tick did not run, so its job may not have advanced).
 */
export const openAttempts = (deploymentId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<OpenSettlementAttempt>`SELECT a.*, j.phase AS job_phase,
      NOT EXISTS (SELECT 1 FROM settlement_attempts l
        WHERE l.deployment_id = a.deployment_id AND l.kind = a.kind
          AND l.event_id = a.event_id AND l.status <> 'expired'
          AND (l.created_at, l.tx_hash) > (a.created_at, a.tx_hash)) AS latest
    FROM settlement_attempts a JOIN settlement_jobs j USING (deployment_id, kind, event_id)
    WHERE a.deployment_id = ${deploymentId}
      AND (a.status = 'pending' OR (a.status = 'final' AND j.phase <> 'complete'))
    ORDER BY a.created_at, a.tx_hash`;
  });

export const nextJob = (owner: SettlementOwner) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<SettlementJob>`SELECT * FROM settlement_jobs
    WHERE deployment_id = ${owner.deploymentId}
      AND due_at <= clock_timestamp() AND phase <> 'complete'
    ORDER BY due_at, created_at, kind, event_id LIMIT 1`;
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

/**
 * Journals a new attempt in the caller's transaction. The owner row is
 * locked first, so attempts are saved one at a time, and `unsettled` (the
 * derived check that no open attempt reads short of cd) runs under that
 * lock, in the same transaction, before the insert.
 */
export const saveAttempt = <E, R>(
  owner: SettlementOwner,
  attempt: SettlementAttempt,
  unsettled: Effect.Effect<void, E, R>,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* assertOwner(owner);
        yield* sql`SELECT 1 FROM settlement_owners
        WHERE deployment_id = ${owner.deploymentId} FOR UPDATE`;
        yield* unsettled;
        yield* sql`INSERT INTO settlement_attempts
      (deployment_id, kind, event_id, phase, tx_hash, signed_cbor, required_outputs, fee_inputs, status)
      VALUES (${attempt.deployment_id}, ${attempt.kind}, ${attempt.event_id}, ${attempt.phase},
        ${attempt.tx_hash}, ${attempt.signed_cbor},
        ARRAY(SELECT jsonb_array_elements_text(CAST(${JSON.stringify(attempt.required_outputs)} AS TEXT)::jsonb))::integer[],
        ARRAY(SELECT jsonb_array_elements_text(CAST(${JSON.stringify(attempt.fee_inputs)} AS TEXT)::jsonb)),
        ${attempt.status})`;
      }),
    );
  });

export const updateJob = (
  owner: SettlementOwner,
  job: Pick<SettlementJob, "kind" | "event_id">,
  phase: SettlementJob["phase"],
  delayMs = 0,
  error: string | null = null,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* assertOwner(owner);
    yield* sql`UPDATE settlement_jobs SET phase = ${phase},
    due_at = clock_timestamp() + ${delayMs} * interval '1 millisecond', last_error = ${error},
    failures = CASE WHEN ${error}::text IS NULL THEN 0 ELSE failures + 1 END
    WHERE deployment_id = ${owner.deploymentId} AND kind = ${job.kind} AND event_id = ${job.event_id}`;
  });

/** Records an attempt proven never to land, and returns its job to the
 * attempt's phase, so the job rebuilds from current state. */
export const expireAttempt = (
  owner: SettlementOwner,
  attempt: SettlementAttempt,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* assertOwner(owner);
        yield* sql`UPDATE settlement_attempts SET status = 'expired'
      WHERE tx_hash = ${attempt.tx_hash} AND deployment_id = ${owner.deploymentId}`;
        yield* updateJob(owner, attempt, attempt.phase);
      }),
    );
  });
