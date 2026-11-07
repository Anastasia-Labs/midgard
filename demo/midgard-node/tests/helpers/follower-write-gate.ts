/**
 * The follower write gate's row (`node_follower_write_gate`, plan §8.1) as
 * the follower-change driver leaves it, for tests whose subject reads it
 * (the settlement fence) or writes under a producer's permit: open at an
 * applied view, or held by a pending recompute. A test of the driver's own
 * writes runs a driver instead (`driver-recompute.ts`).
 */
import type { View } from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  DRIVER_RECOMPUTE_PENDING,
  type FollowerDriverPermit,
  type FollowerWritePermit,
  gateViewOf,
} from "../../src/services/follower-write-gate.js";
import { writeFollowerTip } from "./follower-view.js";

/** Opens the gate at `view` with no recompute pending, as a driver's
 * publish leaves it, in a new epoch; a producer's permit at the view. */
export const openFollowerWriteGateAt = (view: View) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const [row] = yield* sql<{ epoch: string }>`UPDATE node_follower_write_gate
      SET epoch = epoch + 1, pending_reason = NULL, pending_detail = NULL,
        applied_generation = ${view.generation}, applied_slot = ${view.point.slot},
        applied_hash = ${Buffer.from(view.point.hash)}, applied_height = ${view.height},
        updated_at = NOW()
      WHERE singleton RETURNING epoch::text AS epoch`;
    return { view: gateViewOf(view), epoch: row!.epoch } as FollowerWritePermit;
  });

/** Opens the gate at a synthetic applied view (one no follower holds). */
export const openFollowerWriteGate = openFollowerWriteGateAt({
  generation: 1,
  point: { slot: 10, hash: new Uint8Array(32).fill(0xb1) },
  height: 1,
} as View);

/** Marks a driver recompute pending, as the driver's next recompute does. */
export const holdFollowerWriteGate = (detail: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE node_follower_write_gate
      SET epoch = epoch + 1, pending_reason = ${DRIVER_RECOMPUTE_PENDING},
        pending_detail = ${detail}, updated_at = NOW()
      WHERE singleton`;
  });

/**
 * Two follower-change drivers' capabilities at one follower view: `stale`
 * took the gate first and `current` took it from it, as a successor's
 * driver does. Only `current` writes; a fixture no longer does either.
 */
export const supersededDriver = Effect.gen(function* () {
  const view = yield* writeFollowerTip(10);
  const driverAt = (permit: FollowerWritePermit): FollowerDriverPermit => ({
    view,
    epoch: permit.epoch,
  });
  const stale = driverAt(yield* openFollowerWriteGateAt(view));
  const current = driverAt(yield* openFollowerWriteGateAt(view));
  return { stale, current };
});
