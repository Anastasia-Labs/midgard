/**
 * The L1 follower's tables as the node's event ingestion reads them (N1),
 * written directly for tests without a followed chain: the cursor and its
 * tip block (one generation, synthetic hashes, height = slot unless given)
 * and the never-reuse key set `l1_event_keys`.
 */
import { createHash } from "node:crypto";

import { encodeOutRef, type View } from "@al-ft/midgard-l1-follower";
import type { ProjectedEvent } from "@al-ft/midgard-l1-follower/events";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { reconcileFollowerEvents } from "../../src/database/follower-events.js";
import { DatabaseError } from "../../src/database/utils/common.js";
import type { IngestionPlan } from "../../src/l1-events/driver.js";
import {
  FollowerWriteFixture,
  withFollowerWrite,
} from "../../src/services/follower-write-gate.js";
import type { CommitHorizonLag } from "../../src/services/history-commit-window.js";

export const FOLLOWER_GENERATION = 1;

/** A synthetic block hash, distinct per slot and per follower generation. */
export const followerBlockHash = (
  slot: number,
  generation: number = FOLLOWER_GENERATION,
): Buffer =>
  createHash("sha256")
    .update(
      generation === FOLLOWER_GENERATION
        ? `test-l1-follower:${slot}`
        : `test-l1-follower:${slot}:${generation}`,
    )
    .digest();

/** The follower's cursor at `slot`, on a tip block of its own at `height`. */
export const writeFollowerTip = (
  slot: number,
  generation: number = FOLLOWER_GENERATION,
  height: number = slot,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const hash = followerBlockHash(slot, generation);
    yield* sql`INSERT INTO l1_blocks (slot, hash, height, parent_hash, qualifying_tx_count)
      VALUES (${slot}, ${hash}, ${height}, NULL, 0) ON CONFLICT DO NOTHING`;
    yield* sql`INSERT INTO l1_follower_cursor
        (id, slot, hash, height, generation, origin_slot, origin_hash, pruned_through_slot)
      VALUES (true, ${slot}, ${hash}, ${height}, ${generation}, 0, ${Buffer.alloc(32)}, 0)
      ON CONFLICT (id) DO UPDATE SET slot = EXCLUDED.slot, hash = EXCLUDED.hash,
        height = EXCLUDED.height, generation = EXCLUDED.generation`;
    const view: View = {
      generation,
      point: { slot, hash },
      height,
    };
    return view;
  });

const outRefOf = (event: ProjectedEvent) =>
  encodeOutRef({
    txHash: Buffer.from(event.admission.outRef.txHash),
    index: event.admission.outRef.index,
  });

/** Admits `events`' keys at their admission outputs (a known key is kept). */
export const admitFollowerKeys = (events: readonly ProjectedEvent[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    for (const event of events)
      yield* sql`INSERT INTO l1_event_keys (kind, key, origin_outref, first_canonical_slot)
        VALUES (${event.kind}, ${Buffer.from(event.key, "hex")}, ${outRefOf(event)},
          ${event.admission.slot}) ON CONFLICT DO NOTHING`;
  });

/**
 * Removes `event`'s key, as a follower rewind past its admission does; with
 * `originOutRef`, only while the key is still that admission's.
 */
export const rewindFollowerKey = (
  event: Pick<ProjectedEvent, "kind" | "key">,
  originOutRef?: Buffer,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM l1_event_keys
      WHERE kind = ${event.kind} AND key = ${Buffer.from(event.key, "hex")}
        ${originOutRef === undefined ? sql`` : sql`AND origin_outref = ${originOutRef}`}`;
  });

/** The follower view at `slot` with `events` admitted, as an ingestion plan. */
export const writeFollowerView = (
  slot: number,
  events: readonly ProjectedEvent[],
  generation: number = FOLLOWER_GENERATION,
  height: number = slot,
) =>
  Effect.gen(function* () {
    const view = yield* writeFollowerTip(slot, generation, height);
    yield* admitFollowerKeys(events);
    return { view, events } satisfies IngestionPlan;
  });

/** POSIX ms of a model slot (the journal tests' 1 s slots from zero). */
export const modelSlotTime = (slot: number) => slot * 1000;

/** The commit horizon lag d, dated by the model clock. */
export const modelHorizonLag = (lagBlocks: number): CommitHorizonLag => ({
  lagBlocks,
  slotToUnixTime: Effect.succeed(modelSlotTime),
});

/**
 * The follower-change driver's ingestion of `events` at a follower view at
 * `slot`, without a running driver (the gate's fixture capability); model
 * slots are 1 s from zero and no deposit is projected.
 */
export const ingestFollowerViewUnowned = (
  slot: number,
  events: readonly ProjectedEvent[] = [],
  generation: number = FOLLOWER_GENERATION,
  height: number = slot,
) =>
  Effect.gen(function* () {
    const plan = yield* writeFollowerView(slot, events, generation, height);
    const outcome = yield* withFollowerWrite(
      reconcileFollowerEvents(plan, {
        network: "Preprod",
        slotToUnixTime: modelSlotTime,
        cutoffMs: 0,
      }),
    ).pipe(Effect.provideService(FollowerWriteFixture, true));
    if (outcome.kind === "stale")
      return yield* Effect.fail(
        new DatabaseError({
          table: "follower_event_ingestion",
          message: "The test follower view moved",
          cause: undefined,
        }),
      );
    return outcome.ingestion;
  });
