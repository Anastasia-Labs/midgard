/**
 * The follower-change driver's recompute: the
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
 *    named hold `startup_preparation_failed`; startup fails on one that is
 *    not transient, `awaitFollowerViewOnStartup`).
 * 4. When the landed-block rebase is due, or orphaned admissions wait for
 *    the working-ledger recompute: under the ledger store lease, the native
 *    MPF moves to the target's root, then step 5 runs. Otherwise the native
 *    owner starts if it is not running, then step 5 runs.
 * 5. Under the cache's recovery locks, in one gated transaction, over the
 *    dead own journals read once in it (the rebase target's, or the
 *    transaction holds `landed_block_rebase_pending` and runs again): the
 *    events at the view are ingested, the rebase SQL (`rebaseSql`) runs on
 *    the target, unheld orphans are deleted (`follower-orphan-repair.ts`) and,
 *    when any were, the events are ingested again and the working ledger
 *    recomputed again, so the transactions that spent them are rejected.
 *    The cache reloads (`l1-follower.recompute-repair.ts`: at the keys the
 *    transaction changed, for the landed-block hook's run that found no
 *    orphan; in full otherwise); the gate then publishes the view as
 *    applied and opens, unless journal or foreign-landed orphans remain
 *    (`l1_events_orphan_recovery`, the gate stays pending).
 *
 * An own-landed orphan (a deposit or withdrawal of a landed own block whose
 * admission left the chain) neither holds the gate nor makes the rebase
 * due: the view is published, and that block holds commits and its own
 * merge until it leaves the landed queue (`poisoned-own-headers.ts`).
 *
 * Nothing here fails: a recompute that cannot finish returns its hold and
 * the gate stays pending (producers refused by name). The driver retries
 * the hold on its backoff, for a bounded time, only when its failure is
 * transient (`isTransientDriverFailure`, or a native restore read that
 * failed: a `transientFailure`); any
 * other failure hold is `notRetried` and waits, named, for the next
 * follower change. A failed rebase also raises its liveness reason.
 */
import { randomUUID } from "node:crypto";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Cause, Duration, Effect, Ref } from "effect";

import { countOrphansAwaitingRecovery } from "../database/follower-orphan-repair.js";
import { MpfEngineStateDB } from "../database/index.js";
import {
  type DriverHold,
  type EventRefusal,
  EVENTS_INGESTION_WAITING,
  EVENTS_ORPHAN_RECOVERY,
  type IngestionPlan,
  notRetried,
  transientFailure,
} from "../l1-events/driver.js";
import {
  LANDED_BLOCK_REBASE_PENDING,
  LANDED_BLOCK_REBASE_SOURCE,
} from "../landed-blocks/holds.js";
import { ownJournalDisposition } from "../landed-blocks/own-journals.js";
import {
  failureHold,
  followJournals,
  moveNativeRoot,
} from "../landed-blocks/rebase.js";
import {
  blockedRebaseHold,
  rebasePlan,
  rebasePlanOf,
  type RebaseTarget,
  rebaseTargetOf,
} from "../landed-blocks/rebase-target.js";
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
  followerWriteQuiet,
  withFollowerWrite,
} from "./follower-write-gate.js";
import { Globals } from "./globals.globals.js";
import {
  failureHold as classifiedHold,
  isL1NodeOutage,
  isTransientDriverFailure,
} from "./l1-follower.failure-hold.js";
import type { FollowerPlanRead } from "./l1-follower.readiness.js";
import {
  followerRepair,
  type RepairFound,
} from "./l1-follower.recompute-repair.js";
import {
  clearLivenessIncident,
  NATIVE_MPF_RESTORE_READ_TRANSIENT,
  raiseLivenessIncident,
} from "./liveness-halt.js";
import { Lucid } from "./lucid.js";
import { MempoolLedgerCache } from "./mempool-ledger-cache.mempool-ledger-cache-service.js";
import type { MidgardContracts } from "./midgard-contracts.js";
import { initializeArchitectureGOwner } from "./native-mpf-startup.js";
import { WriteBehind } from "./write-behind.summarize-write-behind-telemetry.js";

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

    // `keyed`: the cache may reload at the keys the repair changed.
    const once = (
      reason: string,
      given: IngestionPlan | undefined,
      keyed: boolean,
    ) =>
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
            return yield* keep(
              classifiedHold(
                STARTUP_PREPARATION_FAILED,
                formatUnknownError(ran.left),
                ran.left,
              ),
            );
          prepared = true;
        }
        // The rebase target: a due rebase, or the target the orphan repair
        // recomputes the working ledger on (own-landed orphans need none).
        const target = yield* asDriver(
          withFollowerWrite(
            Effect.gen(function* () {
              const journals = yield* ownJournalDisposition;
              const due = yield* rebasePlanOf(
                journals,
                yield* Ref.get(globals.NATIVE_MPF_OWNER),
              );
              if (due.kind === "ready") {
                if (due.drift !== undefined)
                  yield* Effect.logWarning(
                    `landed-block rebase: the native MPF's durable root ${due.drift.durableRoot} is not the target's root ${due.drift.targetRoot}`,
                  );
                return due.target;
              }
              if (due.kind === "blocked") return undefined;
              if ((yield* countOrphansAwaitingRecovery(journals.dead)) === 0)
                return undefined;
              const on = yield* rebaseTargetOf(yield* retrieveRows, journals);
              return on.kind === "ready" ? on.target : undefined;
            }),
          ),
        );
        const found: RepairFound = {
          inserted: 0,
          refused: [],
          heldOrphans: 0,
          heldOrphansDetail: "",
        };
        const repair = followerRepair({
          plan,
          network: config.NETWORK,
          slotToUnixTime: lucid.api.slotToUnixTime,
          keyed,
          found,
        });
        const complete = (on: RebaseTarget | undefined) =>
          recovery.runRecovery(
            asDriver(withFollowerWrite(repair(on))),
            Effect.suspend(() =>
              found.heldOrphans === 0
                ? publishDriverView(epoch, view)
                : holdDriverRecompute(
                    epoch,
                    EVENTS_ORPHAN_RECOVERY,
                    found.heldOrphansDetail,
                  ),
            ),
            (repaired) => repaired.changes,
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
            return yield* keep(
              classifiedHold(
                DRIVER_RECOMPUTE_FAILED,
                formatUnknownError(failure),
                failure,
              ),
            );
          const { escalateAfterMs, ...hold } = failureHold(failure);
          yield* raiseLivenessIncident(
            globals,
            LANDED_BLOCK_REBASE_SOURCE,
            hold.reason,
            hold.detail,
            escalateAfterMs === undefined ? {} : { escalateAfterMs },
          );
          // A native restore read that failed is retried on the backoff
          // (escalated after `NATIVE_MPF_RESTORE_READ_ESCALATION_MS`); a store
          // that lacks the root, a cap it is over, or any other failure that
          // is not transient waits for the next follower change.
          return yield* keep(
            hold.reason === NATIVE_MPF_RESTORE_READ_TRANSIENT ||
              isTransientDriverFailure(failure)
              ? isL1NodeOutage(failure)
                ? hold
                : transientFailure(hold)
              : notRetried(hold),
          );
        }
        if (result.right.busy)
          return yield* keep({
            reason: LANDED_BLOCK_REBASE_PENDING,
            detail: "the rebase waits for the ledger store lease",
          });
        yield* clearLivenessIncident(globals, LANDED_BLOCK_REBASE_SOURCE);
        const rebased = result.right.outcome?.outcome;
        if (rebased !== undefined) yield* followJournals(globals, rebased);
        if (found.heldOrphans > 0)
          return heldOutcome({
            reason: EVENTS_ORPHAN_RECOVERY,
            detail: found.heldOrphansDetail,
          });
        return {
          published: true,
          hold: undefined,
          inserted: found.inserted,
          refused: found.refused,
        } satisfies RecomputeOutcome;
      });

    const runOnce = (
      reason: string,
      plan: IngestionPlan | undefined,
      keyed: boolean,
    ) =>
      serial
        .withPermits(1)(once(reason, plan, keyed))
        .pipe(
          Effect.catchAllCause((cause) =>
            Effect.succeed(
              heldOutcome(
                classifiedHold(
                  DRIVER_RECOMPUTE_FAILED,
                  Cause.pretty(cause),
                  Cause.squash(cause),
                ),
              ),
            ),
          ),
          Effect.provide(runtime),
        );
    const run = (reason: string, plan?: IngestionPlan) =>
      runOnce(reason, plan, false);

    // From a commit's journal insert to the promotion after its worker, the
    // live journal's root is ahead of the native MPF's while the commit's
    // producer runs; the recompute drains producers before it reads the
    // plan. Native root drift alone is due here only with none running.
    const rebaseIfDue = (reason: string) =>
      Effect.gen(function* () {
        const globals = yield* Globals;
        const owner = (yield* followerWriteQuiet)
          ? yield* Ref.get(globals.NATIVE_MPF_OWNER)
          : undefined;
        const due = yield* rebasePlan(owner);
        if (due.kind === "none") return undefined;
        if (
          due.kind === "ready" &&
          due.drift !== undefined &&
          !(yield* followerWriteQuiet)
        )
          return undefined;
        if (due.kind === "blocked") return blockedRebaseHold(due);
        return (yield* runOnce(reason, undefined, true)).hold;
      }).pipe(
        Effect.catchAllCause((cause) =>
          Effect.succeed(
            classifiedHold(
              DRIVER_RECOMPUTE_FAILED,
              Cause.pretty(cause),
              Cause.squash(cause),
            ),
          ),
        ),
        Effect.provide(runtime),
      );

    return { run, rebaseIfDue } satisfies DriverRecompute;
  });
