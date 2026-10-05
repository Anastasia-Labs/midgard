import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { MutationJobsDB } from "../src/database/index.js";
import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import {
  header,
  journalFixture,
} from "./local-mutation-job-abandonment.journal-fixture.js";
import { deterministicFixtureBytes, provideDatabaseLayers } from "./utils.js";

export const DAY_MS = 24 * 60 * 60_000;
export const DEPLOYMENT = Buffer.alloc(32, 7);

/** An authenticated `merged` terminal outcome for `headerHash`, at `blockNo`. */
export const recordMergedOutcome = (headerHash: Buffer, blockNo: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const bytes = (fill: number, length: number) =>
      Buffer.alloc(length, fill + blockNo);
    yield* sql`INSERT INTO da_payload_terminal_outcomes (
        header_hash, terminal_outcome, transition_kind,
        deployment_identity_digest, state_queue_policy_id,
        transaction_hash, block_hash, slot, block_no,
        transaction_index, chain_point_id, finality_depth,
        transition_digest, transition_record
      ) VALUES (
        ${headerHash}, 'merged', 'merge', ${DEPLOYMENT}, ${bytes(1, 28)},
        ${bytes(2, 32)}, ${bytes(3, 32)}, ${blockNo}, ${blockNo}, 0,
        ${bytes(4, 32)}, 3, ${bytes(5, 32)}, ${"{}"}
      )`;
  });

export const run = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const clear = sql`TRUNCATE TABLE pending_block_finalizations,
          state_queue_mutation_leases, event_history_authority,
          local_mutation_jobs, da_payload_terminal_outcomes,
          state_queue_terminal_observer_states, event_history_recovery_plans,
          event_history_cursor
          RESTART IDENTITY CASCADE`;
        yield* clear;
        // Never leave a seeded active lease behind for a later file on
        // this shard to find busy.
        return yield* effect.pipe(Effect.ensuring(Effect.orDie(clear)));
      }),
    ) as Effect.Effect<A, unknown, never>,
  );

export type JournalSpec = {
  readonly label: string;
  readonly status: "finalized" | "abandoned" | "pending_submission";
  readonly endedAgoMs: number;
  /** The journal's confirmed-merge finalization job; a finalized journal's
   * merge has completed unless said otherwise. */
  readonly mergeJob?: "completed" | "running" | "none";
};

export const recordMergeJob = (
  headerHash: Buffer,
  status: "completed" | "running",
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const jobId = MutationJobsDB.confirmedMergeFinalizationJobId(
      headerHash.toString("hex"),
    );
    yield* sql`INSERT INTO local_mutation_jobs (job_id, kind, status, completed_at)
      VALUES (${jobId}, 'confirmed_merge_finalization', ${status},
        ${status === "completed" ? sql`NOW()` : null})
      ON CONFLICT (job_id) DO UPDATE SET status = EXCLUDED.status,
        completed_at = EXCLUDED.completed_at`;
  });

/** One journal per spec, each moved to its status before the next is
 * prepared, ending `endedAgoMs` before now. */
export const journals = (specs: readonly JournalSpec[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    for (const spec of specs) {
      const headerHash = header(spec.label);
      const fixture = journalFixture(headerHash);
      yield* PendingBlockFinalizationsDB.preparePendingSubmission({
        ...fixture,
        metadata: {
          ...fixture.metadata,
          baseTailOutRef: `${spec.label}:base#0`,
          baseTailHeaderHash: header(`${spec.label}:base`),
        },
      });
      yield* sql`UPDATE pending_block_finalizations
        SET status = ${spec.status},
          block_end_time = NOW() - make_interval(secs => ${spec.endedAgoMs / 1000})
        WHERE header_hash = ${headerHash}`;
      const mergeJob =
        spec.mergeJob ?? (spec.status === "finalized" ? "completed" : "none");
      if (mergeJob !== "none") yield* recordMergeJob(headerHash, mergeJob);
    }
  });

export const remainingLabels = (labels: readonly string[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      header_hash: Buffer;
    }>`SELECT header_hash FROM pending_block_finalizations`;
    const present = new Set(rows.map((row) => row.header_hash.toString("hex")));
    return labels.filter((label) => present.has(header(label).toString("hex")));
  });

export const insertLease = (lease: {
  readonly token: string;
  readonly status: "active" | "released" | "failed";
  readonly acquiredAgoMs: number;
  readonly releasedAgoMs: number | null;
}) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO state_queue_mutation_leases
      (token, scope, holder, status, acquired_at, expires_at, released_at)
      VALUES (${lease.token}, 'state_queue', 'test', ${lease.status},
        NOW() - make_interval(secs => ${lease.acquiredAgoMs / 1000}),
        NOW() + INTERVAL '1 hour',
        ${
          lease.releasedAgoMs === null
            ? null
            : sql`NOW() - make_interval(secs => ${lease.releasedAgoMs / 1000})`
        })`;
  });

export const leaseTokens = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    token: string;
  }>`SELECT token FROM state_queue_mutation_leases ORDER BY token`;
  return rows.map((row) => row.token);
});

/** Saves the correction-observer record under `DEPLOYMENT` (replacing any
 * earlier one, as each reconcile does) whose `pending` and `admitted`
 * transitions name `pending` and `admitted` headers as removed and whose
 * cursor queue holds `cursorQueue` after the root, stored string-wrapped when
 * `wrapped` (as JSON.stringify writes it). Its `updated_at` is the save time,
 * so a merge folded before this call counts as observed. */
export const recordObserverState = ({
  pending = [],
  admitted = [],
  cursorQueue = [],
  wrapped = true,
}: {
  readonly pending?: readonly Buffer[];
  readonly admitted?: readonly Buffer[];
  readonly cursorQueue?: readonly Buffer[];
  readonly wrapped?: boolean;
} = {}) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const transition = (headers: readonly Buffer[]) => ({
      kind: "merge",
      removedHeaderHashes: headers.map((hash) => hash.toString("hex")),
    });
    const record = {
      cursorQueue: [
        { headerHash: null, outRef: `${"00".repeat(32)}#0` },
        ...cursorQueue.map((hash, index) => ({
          headerHash: hash.toString("hex"),
          outRef: `${"00".repeat(32)}#${(index + 1).toString()}`,
        })),
      ],
      pending: pending.length === 0 ? [] : [transition(pending)],
      admitted: admitted.length === 0 ? [] : [transition(admitted)],
    };
    const json = JSON.stringify(record);
    // The observer store passes the JSON text as a jsonb parameter, which
    // lands as a jsonb string; a direct text cast lands as an object.
    const stored = wrapped ? sql`${json}` : sql`${json}::text::jsonb`;
    yield* sql`INSERT INTO state_queue_terminal_observer_states (
        deployment_identity_digest, state_queue_policy_id, state_digest,
        state_record
      ) VALUES (${DEPLOYMENT}, ${Buffer.alloc(28, 1)}, ${Buffer.alloc(32, 2)},
        ${stored})
      ON CONFLICT (deployment_identity_digest) DO UPDATE SET
        state_record = EXCLUDED.state_record, updated_at = NOW()`;
    const [shape] = yield* sql<{ kind: string }>`
      SELECT jsonb_typeof(state_record) AS kind
      FROM state_queue_terminal_observer_states`;
    if (shape?.kind !== (wrapped ? "string" : "object"))
      return yield* Effect.die(
        new Error(`observer record stored as ${String(shape?.kind)}`),
      );
  });

/** Gives the journal `label` one deposit member bound to its own
 * event-history incarnation, canonical or orphaned (L1 rolled its origin
 * back). Creates the history cursor the incarnation hangs off on first use. */
export const recordDepositMember = (label: string, canonical: boolean) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const bytes = (tag: string, length = 32) =>
      deterministicFixtureBytes(`history-retention-prune:${tag}`, length);
    const binding = bytes("binding");
    yield* sql`INSERT INTO event_history_cursor (
        binding_digest, manifest_id, origin_receipt, origin_receipt_digest,
        anchor_hash, anchor_slot, anchor_height, anchor_snapshot_digest,
        head_hash, head_slot, head_height, snapshot_digest, revision, addresses
      ) VALUES (${binding}, ${DEPLOYMENT}, 'retention fixture', ${bytes("receipt")},
        ${bytes("anchor")}, 1, 1, ${bytes("snapshot")}, ${bytes("anchor")}, 1, 1,
        ${bytes("snapshot")}, 0, '[]'::jsonb)
      ON CONFLICT (binding_digest) DO NOTHING`;
    const incarnation = bytes(`incarnation:${label}`);
    const eventId = bytes(`event:${label}`, 36);
    yield* sql`INSERT INTO event_history_incarnations (
        binding_digest, incarnation_id, kind, event_id, event_key,
        origin_canonical, incarnation_record, incarnation_digest
      ) VALUES (${binding}, ${incarnation}, 'deposit', ${eventId},
        ${bytes(`key:${label}`)}, ${canonical}, 'retention fixture',
        ${bytes(`digest:${label}`)})`;
    yield* sql`INSERT INTO pending_block_finalization_deposits (
        header_hash, member_id, ordinal, payload_cbor, payload_sha256,
        source_table, source_id, source_time_stamp_tz,
        history_binding_digest, history_incarnation_id
      ) VALUES (${header(label)}, ${eventId}, 0, '\\x00', ${bytes(`payload:${label}`)},
        'deposits_utxos', ${eventId}, NOW(), ${binding}, ${incarnation})`;
  });
