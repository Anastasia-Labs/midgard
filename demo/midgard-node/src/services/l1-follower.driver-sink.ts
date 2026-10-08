/**
 * The follower-change driver's sink (plan §7.3, §8.1, N1): applies each
 * follower change to the node's event rows under the driver's write
 * capability, or runs the driver's recompute (`l1-follower.recompute.ts`).
 */
import { Data, Effect, Ref, Runtime } from "effect";

import { reconcileFollowerEvents } from "../database/follower-events.js";
import {
  EVENTS_INGESTION_FAILED,
  type FollowerChange,
  type FollowerEventSink,
  type IngestionPlan,
  type SinkResult,
} from "../l1-events/driver.js";
import { NodeConfig } from "./config.js";
import type { Database } from "./database.js";
import { withDriverView } from "./follower-write-gate.driver.js";
import {
  advanceDriverView,
  followerWriteHoldOf,
  readFollowerWriteGate,
  withFollowerWrite,
} from "./follower-write-gate.js";
import { Globals } from "./globals.globals.js";
import { message } from "./l1-follower.network-magic.js";
import {
  DRIVER_RECOMPUTE_FAILED,
  type DriverRecompute,
} from "./l1-follower.recompute.js";
import { Lucid } from "./lucid.js";

/** The ingestion found recompute work; its transaction rolled back. */
class FollowerRecomputeRequired extends Data.TaggedError(
  "FollowerRecomputeRequired",
)<{ readonly reason: string }> {}

const recomputeReason = (change: FollowerChange) =>
  change.kind === "initial"
    ? "the follower-change driver applies its first view"
    : change.kind === "rewind"
      ? "the follower rewound"
      : "the follower-change driver's last recompute is pending";

/**
 * The driver's sink. A first view, a rewind, or a recompute of this
 * process's driver left pending runs the recompute
 * (`l1-follower.recompute.ts`). Otherwise the event rows at the view are
 * written under the driver's capability and the applied view moves
 * forward in the same gated transaction; orphaned admissions, deposits whose
 * spendable ledger row was restored, or a gate another driver took roll the
 * write back and run the recompute.
 */
export const driverSink = (recompute: DriverRecompute) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    const lucid = yield* Lucid;
    const globals = yield* Globals;
    const runtime = yield* Effect.runtime<Globals | Database>();
    const advance = (plan: IngestionPlan) =>
      withDriverView(plan.view)(
        withFollowerWrite(
          Effect.gen(function* () {
            const outcome = yield* reconcileFollowerEvents(plan, {
              network: config.NETWORK,
              slotToUnixTime: lucid.api.slotToUnixTime,
              cutoffMs: lucid.api.slotToUnixTime(plan.view.point.slot),
            });
            if (outcome.kind === "stale") return outcome;
            if (outcome.ingestion.orphans > 0)
              return yield* new FollowerRecomputeRequired({
                reason: `L1 follower rewind orphaned ${outcome.ingestion.orphans.toString()} event admissions; their dependents must be rejected`,
              });
            if (outcome.ingestion.spendableUpserts.length > 0)
              return yield* new FollowerRecomputeRequired({
                reason:
                  "Deposit projection restored spendable ledger rows; the validation cache must reload",
              });
            yield* advanceDriverView;
            return outcome;
          }),
        ),
      );
    const recomputed = async (
      reason: string,
      plan: IngestionPlan,
    ): Promise<SinkResult> => {
      const outcome = await Runtime.runPromise(runtime)(
        recompute.run(reason, plan),
      );
      return outcome.published
        ? {
            kind: "applied",
            inserted: outcome.inserted,
            orphans: 0,
            refused: outcome.refused,
          }
        : {
            kind: "held",
            hold: outcome.hold ?? {
              reason: DRIVER_RECOMPUTE_FAILED,
              detail: "the recompute did not publish the view",
            },
          };
    };
    const sink: FollowerEventSink = {
      apply: async (change, plan) => {
        const local = await Runtime.runPromise(runtime)(
          Ref.get(globals.FOLLOWER_WRITE_GATE),
        );
        if (
          change.kind === "initial" ||
          change.kind === "rewind" ||
          local.epoch === undefined ||
          local.recomputing
        )
          return recomputed(recomputeReason(change), plan);
        const exit = await Runtime.runPromiseExit(runtime)(
          Effect.either(advance(plan)),
        );
        if (exit._tag === "Failure")
          return {
            kind: "held",
            hold: {
              reason: EVENTS_INGESTION_FAILED,
              detail: String(exit.cause),
            },
          };
        const result = exit.value;
        if (result._tag === "Right")
          return result.right.kind === "stale"
            ? {
                kind: "stale",
                detail: "the follower view moved before the write",
              }
            : {
                kind: "applied",
                inserted: result.right.ingestion.inserted,
                orphans: 0,
                refused: result.right.ingestion.refused,
              };
        const error = result.left;
        if (error instanceof FollowerRecomputeRequired)
          return recomputed(error.reason, plan);
        const refused = followerWriteHoldOf(error);
        if (refused === undefined)
          return {
            kind: "held",
            hold: { reason: EVENTS_INGESTION_FAILED, detail: message(error) },
          };
        // Another node process's driver took the gate (a predecessor or
        // successor on the same database): this driver takes it back.
        const gate = await Runtime.runPromise(runtime)(
          Effect.either(readFollowerWriteGate),
        );
        if (gate._tag === "Right" && gate.right.epoch !== local.epoch)
          return recomputed(
            `another driver took the follower write gate (epoch ${gate.right.epoch})`,
            plan,
          );
        return { kind: "held", hold: refused };
      },
    };
    return sink;
  });
