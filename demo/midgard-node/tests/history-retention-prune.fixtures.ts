import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { prepareUnderTestLease } from "./helpers/mutation-lease.js";
import { land, ownLandedRow, retainFold } from "./helpers/own-block-facts.js";
import { header, journalFixture } from "./helpers/own-journal-fixture.js";
import {
  deterministicFixtureBytes,
  provideDatabaseLayers,
  resetApplicationTables,
} from "./utils.js";

export const DAY_MS = 24 * 60 * 60_000;
export const DEPLOYMENT = Buffer.alloc(32, 7);

export const run = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        // Never leave a seeded active lease behind for a later file on
        // this shard to find busy.
        return yield* effect.pipe(
          Effect.ensuring(Effect.orDie(resetApplicationTables)),
        );
      }),
    ) as Effect.Effect<A, unknown, never>,
  );

/** The derived status a spec's journal is given. */
export type JournalStatus = "folded" | "applied" | "landed" | "unlanded";

export type JournalSpec = {
  readonly label: string;
  readonly status: JournalStatus;
  readonly endedAgoMs: number;
};

/**
 * Writes the facts the journal of `headerHash` derives `status` from, on
 * the base its row names: a retained fold, a processed landed row (applied
 * or not), or neither (it has no commit anchor, so it is dead).
 */
export const setJournalStatus = (headerHash: Buffer, status: JournalStatus) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const [row] = yield* sql<{ base_tail_header_hash: Buffer }>`
      SELECT base_tail_header_hash FROM pending_block_finalizations
      WHERE header_hash = ${headerHash}`;
    const header = headerHash.toString("hex");
    const base = row!.base_tail_header_hash.toString("hex");
    yield* sql`DELETE FROM node_confirmed_merges WHERE header_hash = ${headerHash}`;
    yield* sql`DELETE FROM node_landed_blocks WHERE header_hash = ${headerHash}`;
    if (status === "folded") yield* retainFold(header, base);
    if (status === "applied" || status === "landed")
      yield* land(
        ownLandedRow(header, base, { applied: status === "applied" }),
      );
  });

/** One journal per spec, each given its status before the next is
 * prepared, ending `endedAgoMs` before now. */
export const journals = (specs: readonly JournalSpec[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    for (const spec of specs) {
      const headerHash = header(spec.label);
      const fixture = journalFixture(headerHash);
      yield* prepareUnderTestLease({
        ...fixture,
        metadata: {
          ...fixture.metadata,
          baseTailOutRef: `${spec.label}:base#0`,
          baseTailHeaderHash: header(`${spec.label}:base`),
        },
      });
      yield* sql`UPDATE pending_block_finalizations
        SET block_end_time = NOW() - make_interval(secs => ${spec.endedAgoMs / 1000}),
          block_time = NOW() - make_interval(secs => ${spec.endedAgoMs / 1000})
        WHERE header_hash = ${headerHash}`;
      yield* setJournalStatus(headerHash, spec.status);
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

/** Gives the journal `label` one deposit member with its own follower
 * admission identity, canonical (the follower's key set holds it) or orphaned
 * (a follower rewind removed it: L1 rolled its origin back). */
export const recordDepositMember = (label: string, canonical: boolean) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const bytes = (tag: string, length = 32) =>
      deterministicFixtureBytes(`history-retention-prune:${tag}`, length);
    const eventId = bytes(`event:${label}`, 36);
    const key = bytes(`key:${label}`);
    const origin = bytes(`origin:${label}`, 34);
    if (canonical)
      yield* sql`INSERT INTO l1_event_keys (kind, key, origin_outref, first_canonical_slot)
        VALUES ('deposit', ${key}, ${origin}, 1)`;
    yield* sql`INSERT INTO pending_block_finalization_deposits (
        header_hash, member_id, ordinal, payload_cbor, payload_sha256,
        source_table, source_id, source_time_stamp_tz,
        l1_event_key, l1_origin_outref
      ) VALUES (${header(label)}, ${eventId}, 0, '\\x00', ${bytes(`payload:${label}`)},
        'deposits_utxos', ${eventId}, NOW(), ${key}, ${origin})`;
  });
