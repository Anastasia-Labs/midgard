/**
 * The follower-change driver's side of the write gate
 * (`follower-write-gate.ts`): it takes a new epoch for a recompute, drains
 * this process's producers, keeps the recompute pending with why, and
 * publishes the view it applied; its hooks write under its capability.
 */
import type { View } from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Deferred, type Duration, Effect, Ref } from "effect";

import { DatabaseError } from "../database/utils/common.js";
import {
  FOLLOWER_VIEW_STALE,
  FollowerDriverWrite,
  followerWriteHeld,
  followerWriteUnavailable,
} from "./follower-write-gate.js";
import { Globals } from "./globals.globals.js";

/**
 * Starts a driver recompute: this process stops registering producers, then
 * the gate's epoch is bumped and the recompute marked pending in its own
 * transaction (it waits out gated transactions in flight), so every permit
 * taken before is refused. Returns the new epoch, which this process's
 * driver now holds.
 */
export const beginDriverRecompute = (reason: string, detail: string) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Ref.update(globals.FOLLOWER_WRITE_GATE, (local) => {
      local.recomputing = true;
      return local;
    });
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ epoch: string }>`UPDATE node_follower_write_gate
      SET epoch = epoch + 1, pending_reason = ${reason},
        pending_detail = ${detail}, updated_at = NOW()
      WHERE singleton RETURNING epoch::text AS epoch`;
    const epoch = rows[0]?.epoch;
    if (epoch === undefined)
      return yield* Effect.fail(
        followerWriteUnavailable("The follower write gate row is missing"),
      );
    yield* Ref.update(globals.FOLLOWER_WRITE_GATE, (local) => {
      local.epoch = epoch;
      return local;
    });
    return epoch;
  }).pipe(
    Effect.mapError((error) =>
      error instanceof DatabaseError ? error : followerWriteUnavailable(error),
    ),
  );

/**
 * Waits for every producer this process registered before the recompute
 * began, up to `timeout`; `false` if one is still running then.
 */
export const drainFollowerWriters = (timeout: Duration.DurationInput) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const local = yield* Ref.get(globals.FOLLOWER_WRITE_GATE);
    const producers = [...local.producers];
    const drained = yield* Effect.all(producers.map(Deferred.await), {
      concurrency: "unbounded",
      discard: true,
    }).pipe(
      Effect.timeoutTo({
        duration: timeout,
        onSuccess: () => true,
        onTimeout: () => false,
      }),
    );
    return drained;
  });

/** Keeps the recompute pending, with why it cannot finish yet. */
export const holdDriverRecompute = (
  epoch: string,
  reason: string,
  detail: string,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE node_follower_write_gate
      SET pending_reason = ${reason}, pending_detail = ${detail}, updated_at = NOW()
      WHERE singleton AND epoch = ${epoch}::bigint`;
  }).pipe(Effect.mapError(followerWriteUnavailable));

/**
 * Ends the driver's recompute at `epoch`: the gate holds `view` as applied
 * and is open again. Fails if another driver took the gate meanwhile.
 */
export const publishDriverView = (epoch: string, view: View) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql`UPDATE node_follower_write_gate
      SET pending_reason = NULL, pending_detail = NULL,
        applied_generation = ${view.generation}, applied_slot = ${view.point.slot},
        applied_hash = ${Buffer.from(view.point.hash)}, applied_height = ${view.height},
        updated_at = NOW()
      WHERE singleton AND epoch = ${epoch}::bigint RETURNING 1`;
    if (rows.length !== 1)
      return yield* Effect.fail(
        followerWriteHeld(
          FOLLOWER_VIEW_STALE,
          `another driver took the gate before epoch ${epoch} was published`,
        ),
      );
    const globals = yield* Globals;
    yield* Ref.update(globals.FOLLOWER_WRITE_GATE, (local) => {
      if (local.epoch === epoch) local.recomputing = false;
      return local;
    });
  }).pipe(
    Effect.mapError((error) =>
      error instanceof DatabaseError ? error : followerWriteUnavailable(error),
    ),
  );

/**
 * Runs `work` under the driver's capability at `view`, with the epoch this
 * process's driver holds (its hooks' writes, between recomputes).
 */
export const withDriverView =
  (view: View) =>
  <A, E, R>(work: Effect.Effect<A, E, R>) =>
    Effect.flatMap(Globals, (globals) =>
      Effect.flatMap(Ref.get(globals.FOLLOWER_WRITE_GATE), (local) =>
        Effect.provideService(work, FollowerDriverWrite, {
          view,
          epoch: local.epoch,
        }),
      ),
    );
