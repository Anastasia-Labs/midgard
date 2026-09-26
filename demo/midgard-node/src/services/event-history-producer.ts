import { SqlClient } from "@effect/sql";
import { Context, Effect, Option, Ref } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import { DatabaseError } from "../database/utils/common.js";
import type { Database } from "./database.js";
import type { HistoryOwnerCoverage } from "./event-history-owner.js";
import {
  HistoryPreparation,
  HistoryRecoverySuperseded,
} from "./event-history-recovery.js";
import { Globals } from "./globals.js";

export type HistoryProducerPermit = Readonly<{
  token: Authority.Token;
  coverage: HistoryOwnerCoverage;
}>;
export const HistoryProducer = Context.GenericTag<HistoryProducerPermit>(
  "midgard/HistoryProducer",
);
/** Explicit model-fixture capability. No runtime layer provides this. */
export const UnownedHistoryFixture = Context.GenericTag<true>(
  "midgard/UnownedHistoryFixture",
);
const fixtureTransaction = Context.GenericTag<true>(
  "midgard/UnownedHistoryFixtureTransaction",
);

const PRODUCER_REQUIRED = "Current authenticated history producer is required";
const unavailable = (cause: unknown) =>
  new DatabaseError({
    table: Authority.tableName,
    message: PRODUCER_REQUIRED,
    cause,
  });

/** A producer refused only because the history source gate is closed for a
 * recovery the owner is running (or a source signal superseded it). That is
 * the planned state while a rewind, rollback or first start converges, not a
 * failure of the refused work. */
export const isHistoryProducerGateClosed = (error: unknown): boolean =>
  error instanceof DatabaseError &&
  error.table === Authority.tableName &&
  error.message === PRODUCER_REQUIRED &&
  error.cause instanceof HistoryRecoverySuperseded;

/** Under the Ready row lock: the producer's journaled prefix is still the
 * journal's, exactly or as a canonical ancestor of the current head. Within
 * one Ready generation the owner only appends (every rewind first moves the
 * authority to a new recovering generation, which withReady refuses), so
 * later blocks extend the prefix and never invalidate what was read from it.
 * A prefix pruned behind the anchor can no longer be shown and is refused. */
const checkCoverage = (permit: HistoryProducerPermit) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const coverage = permit.coverage;
    const binding = Buffer.from(coverage.bindingDigest, "hex");
    const point = Buffer.from(coverage.point.id, "hex");
    const snapshot = Buffer.from(coverage.snapshotDigest, "hex");
    const rows = yield* sql<{
      revision: string;
      head_hash: Buffer;
      head_slot: string;
      snapshot_digest: Buffer;
      anchor_hash: Buffer;
      anchor_slot: string;
      anchor_snapshot_digest: Buffer;
    }>`
    SELECT revision::text, head_hash, head_slot::text, snapshot_digest,
      anchor_hash, anchor_slot::text, anchor_snapshot_digest
    FROM event_history_cursor WHERE binding_digest = ${binding} AND manifest_id = ${Buffer.from(permit.token.deploymentIdentity, "hex")}`;
    const row = rows[0];
    const changed = Effect.fail(
      unavailable("History producer coverage changed"),
    );
    if (rows.length !== 1 || row === undefined) return yield* changed;
    if (
      row.revision === coverage.checkpointRevision &&
      row.head_hash.equals(point) &&
      Number(row.head_slot) === coverage.point.slot &&
      row.snapshot_digest.equals(snapshot)
    )
      return;
    if (BigInt(row.revision) <= BigInt(coverage.checkpointRevision))
      return yield* changed;
    if (
      row.anchor_hash.equals(point) &&
      Number(row.anchor_slot) === coverage.point.slot &&
      row.anchor_snapshot_digest.equals(snapshot)
    )
      return;
    const ancestor = yield* sql`SELECT 1 FROM event_history_block_applications
      WHERE binding_digest = ${binding} AND canonical AND block_hash = ${point}
        AND block_slot = ${coverage.point.slot} AND after_snapshot_digest = ${snapshot}
        AND application_revision <= ${coverage.checkpointRevision}::bigint`;
    if (ancestor.length !== 1) return yield* changed;
  }).pipe(Effect.mapError(unavailable));

/** Outermost SQL gate. Standalone database fixtures without an acquired owner
 * require an explicit test capability and serialize against first ownership.
 * An acquired owner never permits an unowned writer, even after lease expiry.
 */
export const withHistoryWrite = <A, E, R>(
  work: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | DatabaseError, R | Database> =>
  Effect.gen(function* () {
    const existing = yield* Authority.currentOwnedTransaction;
    if (Option.isSome(existing)) {
      const transaction = yield* Effect.serviceOption(
        SqlClient.TransactionConnection,
      );
      if (Option.isNone(transaction))
        return yield* Effect.fail(
          unavailable("History capability escaped its SQL transaction"),
        );
      return yield* work;
    }
    const preparation = yield* Effect.serviceOption(HistoryPreparation);
    if (Option.isSome(preparation))
      return yield* Authority.withRecovery(
        preparation.value.token,
        preparation.value.assertCurrent.pipe(
          Effect.zipRight(work),
          Effect.tap(() => preparation.value.assertCurrent),
        ),
      ).pipe(Effect.mapError(unavailable));
    const permit = yield* Effect.serviceOption(HistoryProducer);
    if (Option.isSome(permit))
      return yield* Authority.withReady(
        permit.value.token,
        checkCoverage(permit.value).pipe(Effect.zipRight(work)),
      );
    const fixture = yield* Effect.serviceOption(UnownedHistoryFixture);
    if (Option.isNone(fixture))
      return yield* Effect.fail(unavailable("Missing producer permit"));
    const sql = yield* SqlClient.SqlClient;
    const activeFixture = yield* Effect.serviceOption(fixtureTransaction);
    const transaction = yield* Effect.serviceOption(
      SqlClient.TransactionConnection,
    );
    if (Option.isSome(activeFixture) && Option.isSome(transaction))
      return yield* work;
    if (Option.isSome(transaction))
      return yield* Effect.fail(
        unavailable("Fixture gate must own the outermost transaction"),
      );
    return yield* sql
      .withTransaction(
        Effect.gen(function* () {
          // Blocks the INSERT in acquire(), including the first-ever ownership
          // claim, until this explicitly isolated fixture mutation has committed.
          yield* sql`LOCK TABLE event_history_authority IN SHARE ROW EXCLUSIVE MODE`;
          const owner = yield* Authority.retrieve;
          if (Option.isSome(owner))
            return yield* Effect.fail(
              unavailable("Fixture cannot bypass an acquired history owner"),
            );
          return yield* Effect.provideService(work, fixtureTransaction, true);
        }),
      )
      .pipe(Effect.mapError(unavailable));
  });

/** Only canonical source reconciliation may ingest or initially project events.
 * A Ready producer cannot turn a polling result into canonical eligibility. */
export const withHistoryIngestion = <A, E, R>(work: Effect.Effect<A, E, R>) =>
  withHistoryWrite(
    Effect.gen(function* () {
      const transaction = yield* Authority.currentOwnedTransaction;
      if (Option.isSome(transaction)) {
        yield* Authority.requireSourceTransaction;
      } else {
        const fixture = yield* Effect.serviceOption(fixtureTransaction);
        if (Option.isNone(fixture))
          return yield* Effect.fail(
            unavailable("Canonical ingestion context is missing"),
          );
      }
      return yield* work;
    }),
  );

/** Candidate creation needs a checked Ready producer even when a surrounding
 * transaction already owns the authority row. Recovery can repair journals,
 * but cannot create a newly eligible candidate with unbound members.
 */
export const requireCandidateHistory = Effect.gen(function* () {
  const permit = yield* Effect.serviceOption(HistoryProducer);
  const transaction = yield* Authority.currentOwnedTransaction;
  if (Option.isSome(permit)) {
    if (
      Option.isNone(transaction) ||
      transaction.value.state !== "ready" ||
      transaction.value.token.ownerToken !== permit.value.token.ownerToken ||
      transaction.value.token.generation !== permit.value.token.generation ||
      transaction.value.token.deploymentIdentity !==
        permit.value.token.deploymentIdentity
    )
      return yield* Effect.fail(
        unavailable("Candidate requires its Ready producer transaction"),
      );
    yield* checkCoverage(permit.value);
    return permit;
  }
  const fixture = yield* Effect.serviceOption(fixtureTransaction);
  if (Option.isNone(fixture) || Option.isSome(transaction))
    return yield* Effect.fail(
      unavailable("Candidate has no checked producer context"),
    );
  return permit;
});

export const assertHistoryProducer = (
  permit: HistoryProducerPermit | undefined,
) =>
  withHistoryWrite(Effect.void).pipe(
    permit === undefined
      ? (effect) => effect
      : Effect.provideService(HistoryProducer, permit),
  );

/** Register the entire operation, including worker termination and cache deltas.
 * Network calls run outside SQL. Individual writes use withHistoryWrite.
 */
export const runHistoryProducer = <A, E, R>(work: Effect.Effect<A, E, R>) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const owner = yield* Ref.get(globals.EVENT_HISTORY_OWNER);
    if (owner === undefined)
      return yield* Effect.fail(
        unavailable("History owner is not initialized"),
      );
    return yield* owner
      .runProducer((token, assertCurrent, coverage) =>
        assertCurrent.pipe(
          Effect.zipRight(
            Effect.provideService(work, HistoryProducer, { token, coverage }),
          ),
        ),
      )
      .pipe(Effect.mapError(unavailable));
  });
