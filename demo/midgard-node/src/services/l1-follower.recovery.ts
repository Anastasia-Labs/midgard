/**
 * The history owner's share of event ingestion (E-N1-2 ruling 1): the
 * follower-change driver writes the node's event rows, and the owner's
 * reconcile, inside its source transaction, only
 *
 * - in a Ready append, defers to a recovery when the follower orphaned
 *   admissions (their dependents are recovery work);
 * - in a recovery, repairs orphaned admissions and their unpublished
 *   dependents (`requestReconciliation` brings it here, ruling 3), then
 *   ingests the projection at the follower's current view with the deposit
 *   cutoff at min(view time, journal head time), so the recovery's cache
 *   reload sees the repaired and ingested rows together.
 *
 * Both judge orphans only while the follower is caught up: before that a key
 * it lacks may be one it has not reached, and the node is unready by the
 * follower's own reason. The driver asks for a recovery once it is.
 */
import type { Network } from "@lucid-evolution/lucid";
import { Effect, Option, Ref } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import {
  countOrphanedAdmissions,
  reconcileFollowerEvents,
  type ViewTime,
} from "../database/follower-events.js";
import { DatabaseError } from "../database/utils/common.js";
import type {
  HistoryOwnerChange,
  HistoryReconciliationPending,
} from "./event-history-owner.history-owner-change.js";
import { Globals } from "./globals.globals.js";
import { caughtUpFollower } from "./l1-follower.readiness.js";

const table = "follower_event_ingestion";

const refused = (message: string, cause?: unknown) =>
  new DatabaseError({ table, message, cause });

const pending = (reason: string): HistoryReconciliationPending => ({
  status: "pending",
  reason,
});

/**
 * `repair` (the orphan repair) while the follower is caught up; nothing
 * otherwise, as no admission can be judged orphaned yet.
 */
export const repairWhenFollowerCaughtUp = <E, R>(
  repair: Effect.Effect<void, E, R>,
) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    if (caughtUpFollower(yield* Ref.get(globals.L1_FOLLOWER)) !== undefined)
      yield* repair;
  });

/**
 * Recovery ingestion at the caught-up follower's current view, with the
 * deposit cutoff at min(view time, `cutoffSlot` time). Runs after the orphan
 * repair in the same recovery transaction; a moved view or a left orphan
 * fails it, and the owner retries the recovery.
 */
export const ingestAtCaughtUpView = (input: {
  readonly cutoffSlot: number;
  readonly network: Network;
  readonly slotToUnixTime: ViewTime;
}) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const follower = caughtUpFollower(yield* Ref.get(globals.L1_FOLLOWER));
    if (follower === undefined) return;
    const read = yield* Effect.tryPromise({
      try: () => follower.planCurrent(),
      catch: (cause) => refused("The L1 follower's projection failed", cause),
    });
    if (read.kind === "none")
      return yield* Effect.fail(
        refused(`The L1 follower's projection is unreadable: ${read.detail}`),
      );
    const outcome = yield* reconcileFollowerEvents(read.plan, {
      network: input.network,
      slotToUnixTime: input.slotToUnixTime,
      cutoffMs: Math.min(
        input.slotToUnixTime(read.plan.view.point.slot),
        input.slotToUnixTime(input.cutoffSlot),
      ),
    });
    if (outcome.kind === "stale")
      return yield* Effect.fail(
        refused("The L1 follower view moved during recovery"),
      );
    if (outcome.ingestion.orphans > 0)
      return yield* Effect.fail(
        refused(
          `${outcome.ingestion.orphans.toString()} orphaned event admissions remain after repair`,
        ),
      );
  });

/** The history owner's reconcile of `change` (see the module doc). */
export const ingestAtFollowerView = <E, R>(input: {
  readonly change: HistoryOwnerChange;
  /** The orphan repair of this change (`repairUnpublishedHistoryLedger`). */
  readonly repair: Effect.Effect<void, E, R>;
  readonly network: Network;
  readonly slotToUnixTime: ViewTime;
}) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    if (caughtUpFollower(yield* Ref.get(globals.L1_FOLLOWER)) === undefined)
      return undefined;
    const owned = yield* Authority.currentOwnedTransaction;
    if (Option.isSome(owned) && owned.value.state === "ready") {
      const orphans = yield* countOrphanedAdmissions;
      return orphans === 0
        ? undefined
        : pending(
            `L1 follower rewind orphaned ${orphans.toString()} event admissions; a recovery rejects their dependents`,
          );
    }
    yield* input.repair;
    yield* ingestAtCaughtUpView({
      cutoffSlot: input.change.after.head.slot,
      network: input.network,
      slotToUnixTime: input.slotToUnixTime,
    });
    return undefined;
  });
