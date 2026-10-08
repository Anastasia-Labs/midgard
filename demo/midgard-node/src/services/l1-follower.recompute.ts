/**
 * The follower-change driver's recompute (plan §7.3, §8.1, N1, N3): the
 * one path that rebuilds the node's derived state from the follower's view,
 * run by the driver itself (its sink on a first view or a rewind, its
 * landed-block hook when a rebase is due, the own-commit disposition after
 * S6), one at a time.
 *
 * 1. The validation cache retires its epoch, and the write gate takes a new
 *    epoch marked pending (`beginDriverRecompute`): every producer permit
 *    taken before is refused from then on, as a named hold.
 * 2. The producers this process registered drain (bounded; one still
 *    running leaves the recompute pending, retried), and deferred
 *    persistence flushes.
 * 3. Once per process, the startup preparation runs (a failure is the
 *    named hold `startup_preparation_failed`, retried).
 * 4. When the landed-block rebase is due, or orphaned admissions wait for
 *    the working-ledger recompute: under the ledger store lease, the native
 *    MPF moves to the target's root, then step 5 runs. Otherwise the native
 *    owner starts if it is not running, then step 5 runs.
 * 5. Under the cache's recovery locks, in one gated transaction: the events
 *    at the view are ingested, the rebase SQL (`rebaseSql`) runs on the
 *    target, unheld orphans are deleted (`follower-orphan-repair.ts`) and,
 *    when any were, the events are ingested again and the working ledger
 *    recomputed again, so the transactions that spent them are rejected.
 *    The cache reloads; the gate then publishes the view as applied and
 *    opens, unless held orphans remain (`l1_events_orphan_recovery`, the
 *    gate stays pending).
 *
 * Nothing here fails: a recompute that cannot finish returns its hold, the
 * gate stays pending (producers refused by name), and the driver retries
 * on its backoff. A failed rebase also raises its liveness reason.
 */
import { randomUUID } from "node:crypto";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Cause, Duration, Effect, Ref } from "effect";

import {
  countOrphanedAdmissions,
  reconcileFollowerEvents,
} from "../database/follower-events.js";
import {
  countHeldOrphans,
  deleteUnheldOrphans,
} from "../database/follower-orphan-repair.js";
import { MpfEngineStateDB } from "../database/index.js";
import {
  type DriverHold,
  type EventRefusal,
  EVENTS_INGESTION_WAITING,
  EVENTS_ORPHAN_RECOVERY,
  type IngestionPlan,
} from "../l1-events/driver.js";
import {
  failureHold,
  followJournals,
  LANDED_BLOCK_REBASE_PENDING,
  LANDED_BLOCK_REBASE_SOURCE,
  moveNativeRoot,
  rebasePlan,
  rebaseSql,
  type RebaseTarget,
  rebaseTargetOf,
} from "../landed-blocks/index.js";
import { retrieveRows } from "../landed-blocks/store.js";
import { NodeConfig } from "./config.js";
import type { Database } from "./database.js";
import {
  beginDriverRecompute,
  drainFollowerWriters,
  holdDriverRecompute,
  publishDriverView,
} from "./follower-write-gate.driver.js";
import {
  DRIVER_RECOMPUTE_PENDING,
  FOLLOWER_VIEW_STALE,
  FollowerDriverWrite,
  followerWriteHeld,
  followerWriteHoldOf,
  withFollowerWrite,
} from "./follower-write-gate.js";
import { Globals } from "./globals.globals.js";
import type { FollowerPlanRead } from "./l1-follower.readiness.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "./liveness-halt.js";
import { Lucid } from "./lucid.js";
import { MempoolLedgerCache } from "./mempool-ledger-cache.js";
import type { MidgardContracts } from "./midgard-contracts.js";
import { initializeArchitectureGOwner } from "./native-mpf-startup.js";
import { WriteBehind } from "./write-behind.js";

/** The startup preparation failed; the next recompute runs it again. */
export const STARTUP_PREPARATION_FAILED = "startup_preparation_failed";
/** A recompute failed for a reason it could not name otherwise; retried. */
export const DRIVER_RECOMPUTE_FAILED = "l1_driver_recompute_failed";

/** How long a recompute waits for the producers registered before it. */
export const DRIVER_RECOMPUTE_DRAIN = Duration.seconds(60);

/** What one recompute did. */
export type RecomputeOutcome = Readonly<{
  /** The gate published the view and is open. */
  published: boolean;
  /** Why it did not publish (or why the rebase it found due cannot run). */
  hold: DriverHold | undefined;
  inserted: number;
  refused: readonly EventRefusal[];
}>;

const heldOutcome = (hold: DriverHold): RecomputeOutcome => ({
  published: false,
  hold,
  inserted: 0,
  refused: [],
});

export type DriverRecompute = Readonly<{
  /** Runs one recompute at `plan` (or the follower's current view). Never fails. */
  run: (
    reason: string,
    plan?: IngestionPlan,
  ) => Effect.Effect<RecomputeOutcome>;
  /**
   * Runs the recompute when the landed-block rebase is due and can run;
   * returns why it did not finish (a blocked plan's hold). Never fails.
   */
  rebaseIfDue: (reason: string) => Effect.Effect<DriverHold | undefined>;
}>;

type RecomputeContext =
  | Database
  | Globals
  | NodeConfig
  | Lucid
  | MidgardContracts
  | MempoolLedgerCache
  | WriteBehind;

/**
 * The node's recompute. `planCurrent` reads the projection at the
 * follower's current view; `startupPreparation` runs once per process,
 * under the driver's write capability; `initializeNativeOwner` (default on)
 * starts the native MPF owner when it is not running.
 */
export const makeDriverRecompute = <R = never>(options: {
  readonly planCurrent: () => Promise<FollowerPlanRead>;
  readonly startupPreparation?: Effect.Effect<void, unknown, R>;
  readonly initializeNativeOwner?: boolean;
}) =>
  Effect.gen(function* () {
    const runtime = yield* Effect.runtime<R | RecomputeContext>();
    const serial = yield* Effect.makeSemaphore(1);
    let prepared = options.startupPreparation === undefined;
    const initializeNative = options.initializeNativeOwner ?? true;

    const once = (reason: string, given: IngestionPlan | undefined) =>
      Effect.gen(function* () {
        const globals = yield* Globals;
        const config = yield* NodeConfig;
        const lucid = yield* Lucid;
        const cache = yield* MempoolLedgerCache;
        const writeBehind = yield* WriteBehind;
        let plan = given;
        if (plan === undefined) {
          const read = yield* Effect.tryPromise(() => options.planCurrent());
          if (read.kind === "none")
            return heldOutcome({
              reason: EVENTS_INGESTION_WAITING,
              detail: `the follower's projection is not readable: ${read.detail}`,
            });
          plan = read.plan;
        }
        const view = plan.view;
        const recovery = yield* cache.retireCanonicalEpoch;
        const epoch = yield* beginDriverRecompute(
          DRIVER_RECOMPUTE_PENDING,
          reason,
        );
        const keep = (hold: DriverHold) =>
          holdDriverRecompute(epoch, hold.reason, hold.detail).pipe(
            Effect.ignore,
            Effect.as(heldOutcome(hold)),
          );
        const assertCurrent = Effect.flatMap(
          Ref.get(globals.FOLLOWER_WRITE_GATE),
          (local) =>
            local.epoch === epoch
              ? Effect.void
              : Effect.fail(
                  followerWriteHeld(
                    FOLLOWER_VIEW_STALE,
                    `recompute epoch ${epoch} was superseded`,
                  ),
                ),
        );
        if (!(yield* drainFollowerWriters(DRIVER_RECOMPUTE_DRAIN)))
          return yield* keep({
            reason: DRIVER_RECOMPUTE_PENDING,
            detail:
              "a producer registered before the recompute is still running",
          });
        yield* writeBehind.flushNow;
        const asDriver = <A, E, R2>(work: Effect.Effect<A, E, R2>) =>
          Effect.provideService(work, FollowerDriverWrite, { view, epoch });
        if (!prepared) {
          const ran = yield* Effect.either(
            asDriver(options.startupPreparation ?? Effect.void),
          );
          if (ran._tag === "Left")
            return yield* keep({
              reason: STARTUP_PREPARATION_FAILED,
              detail: formatUnknownError(ran.left),
            });
          prepared = true;
        }
        // The rebase target: a due rebase, or the target the orphan repair
        // recomputes the working ledger on.
        const target = yield* asDriver(
          withFollowerWrite(
            Effect.gen(function* () {
              const due = yield* rebasePlan;
              if (due.kind === "ready") return due.target;
              if (due.kind === "blocked") return undefined;
              if ((yield* countOrphanedAdmissions) === 0) return undefined;
              const on = yield* rebaseTargetOf(yield* retrieveRows);
              return on.kind === "ready" ? on.target : undefined;
            }),
          ),
        );
        const ingested = { inserted: 0, refused: [] as EventRefusal[] };
        let heldOrphans = 0;
        const ingest = Effect.gen(function* () {
          const outcome = yield* reconcileFollowerEvents(plan, {
            network: config.NETWORK,
            slotToUnixTime: lucid.api.slotToUnixTime,
            cutoffMs: lucid.api.slotToUnixTime(view.point.slot),
          });
          if (outcome.kind === "stale")
            return yield* Effect.fail(
              followerWriteHeld(
                FOLLOWER_VIEW_STALE,
                "the follower view moved during the recompute",
              ),
            );
          return outcome.ingestion;
        });
        const repair = (on: RebaseTarget | undefined) =>
          Effect.gen(function* () {
            const first = yield* ingest;
            ingested.inserted = first.inserted;
            ingested.refused = [...first.refused];
            if (on === undefined) {
              heldOrphans = first.orphans;
              return undefined;
            }
            const rebased = yield* rebaseSql(on);
            if ((yield* deleteUnheldOrphans) > 0) {
              yield* ingest;
              const again = yield* rebaseTargetOf(yield* retrieveRows);
              if (again.kind !== "ready")
                return yield* Effect.fail(
                  new Error(
                    `the rebase target became unready after the orphan repair: ${again.detail}`,
                  ),
                );
              yield* rebaseSql(again.target);
            }
            // The rebase released its abandoned journals' events to
            // awaiting; the due ones are projected again at once, for the
            // next block to carry.
            yield* ingest;
            heldOrphans = yield* countHeldOrphans;
            return rebased;
          });
        const complete = (on: RebaseTarget | undefined) =>
          recovery.runRecovery(
            asDriver(withFollowerWrite(repair(on))),
            Effect.suspend(() =>
              heldOrphans === 0
                ? publishDriverView(epoch, view)
                : holdDriverRecompute(
                    epoch,
                    EVENTS_ORPHAN_RECOVERY,
                    `${heldOrphans.toString()} orphaned event admissions wait for their block journal's disposition or a landed correction`,
                  ),
            ),
          );
        const attempt = Effect.gen(function* () {
          if (target === undefined) {
            if (
              initializeNative &&
              (yield* Ref.get(globals.NATIVE_MPF_OWNER)) === undefined
            )
              yield* asDriver(
                initializeArchitectureGOwner(globals, config, assertCurrent),
              );
            return { busy: false, outcome: yield* complete(undefined) };
          }
          const leased = yield* MpfEngineStateDB.tryWithLedgerStoreLease(
            `landed-block-rebase:${randomUUID()}`,
            () =>
              Effect.gen(function* () {
                const current = yield* Ref.get(globals.NATIVE_MPF_OWNER);
                if (current === undefined)
                  yield* asDriver(
                    initializeArchitectureGOwner(
                      globals,
                      config,
                      assertCurrent,
                      (owner) =>
                        moveNativeRoot(owner, target, { assertCurrent }),
                    ),
                  );
                else yield* moveNativeRoot(current, target, { assertCurrent });
                yield* assertCurrent;
                return yield* complete(target);
              }),
          );
          return leased._tag === "Busy"
            ? { busy: true, outcome: undefined }
            : { busy: false, outcome: leased.value };
        });
        const result = yield* Effect.either(attempt);
        if (result._tag === "Left") {
          const failure = result.left;
          const refused = followerWriteHoldOf(failure);
          if (refused !== undefined)
            return yield* keep({
              reason: refused.reason,
              detail: refused.detail,
            });
          if (target === undefined)
            return yield* keep({
              reason: DRIVER_RECOMPUTE_FAILED,
              detail: formatUnknownError(failure),
            });
          const { escalateAfterMs, ...hold } = failureHold(failure);
          yield* raiseLivenessIncident(
            globals,
            LANDED_BLOCK_REBASE_SOURCE,
            hold.reason,
            hold.detail,
            escalateAfterMs === undefined ? {} : { escalateAfterMs },
          );
          return yield* keep(hold);
        }
        if (result.right.busy)
          return yield* keep({
            reason: LANDED_BLOCK_REBASE_PENDING,
            detail: "the rebase waits for the ledger store lease",
          });
        yield* clearLivenessIncident(globals, LANDED_BLOCK_REBASE_SOURCE);
        if (result.right.outcome !== undefined)
          yield* followJournals(globals, result.right.outcome);
        if (heldOrphans > 0)
          return heldOutcome({
            reason: EVENTS_ORPHAN_RECOVERY,
            detail: `${heldOrphans.toString()} orphaned event admissions wait for their block journal's disposition or a landed correction`,
          });
        return {
          published: true,
          hold: undefined,
          inserted: ingested.inserted,
          refused: ingested.refused,
        } satisfies RecomputeOutcome;
      });

    const run = (reason: string, plan?: IngestionPlan) =>
      serial
        .withPermits(1)(once(reason, plan))
        .pipe(
          Effect.catchAllCause((cause) =>
            Effect.succeed(
              heldOutcome({
                reason: DRIVER_RECOMPUTE_FAILED,
                detail: Cause.pretty(cause),
              }),
            ),
          ),
          Effect.provide(runtime),
        );

    const rebaseIfDue = (reason: string) =>
      Effect.gen(function* () {
        const due = yield* rebasePlan;
        if (due.kind === "none") return undefined;
        if (due.kind === "blocked")
          return {
            reason: due.reason ?? LANDED_BLOCK_REBASE_PENDING,
            detail: due.detail,
          } satisfies DriverHold;
        return (yield* run(reason)).hold;
      }).pipe(
        Effect.catchAllCause((cause) =>
          Effect.succeed({
            reason: DRIVER_RECOMPUTE_FAILED,
            detail: Cause.pretty(cause),
          } satisfies DriverHold),
        ),
        Effect.provide(runtime),
      );

    return { run, rebaseIfDue } satisfies DriverRecompute;
  });
