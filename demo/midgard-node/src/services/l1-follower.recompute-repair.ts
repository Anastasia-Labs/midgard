/**
 * The repair transaction of the follower-change driver's recompute
 * (`l1-follower.recompute.ts`, its step 5): over the dead own journals read
 * once in it, the events at the view are ingested, the rebase SQL runs on
 * the target, unheld orphans are deleted and, when any were, the events
 * are ingested again and the working ledger recomputed again.
 *
 * The validation cache then reads again what the transaction changed. A
 * keyed repair (`keyed`) that found no orphan names the working-ledger keys
 * it changed, with their spendable outputs as it left them: each rebase's
 * keys and the deposits whose working-ledger row the ingestion restored.
 * Any other repair names none, and the cache reloads in full.
 */
import type { Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { reconcileFollowerEvents } from "../database/follower-events.js";
import {
  deleteUnheldOrphans,
  describeOrphansAwaitingRecovery,
} from "../database/follower-orphan-repair.js";
import {
  MempoolLedgerDB,
  type PendingBlockFinalizationsDB,
} from "../database/index.js";
import type { EventRefusal, IngestionPlan } from "../l1-events/driver.js";
import { rebaseSql } from "../landed-blocks/rebase.js";
import {
  deadJournalsFor,
  type RebaseTarget,
  rebaseTargetOf,
} from "../landed-blocks/rebase-target.js";
import { retrieveRows } from "../landed-blocks/store.js";
import {
  FOLLOWER_VIEW_STALE,
  followerWriteHeld,
} from "./follower-write-gate.js";
import type { DurableChanges } from "./mempool-ledger-cache.mempool-ledger-cache-service.js";

/** What the repair found, read by the recompute once it ran. */
export type RepairFound = {
  inserted: number;
  refused: EventRefusal[];
  /** Held orphans: they keep the gate pending. */
  heldOrphans: number;
  heldOrphansDetail: string;
};

export const followerRepair =
  (input: {
    readonly plan: IngestionPlan;
    readonly network: Network;
    readonly slotToUnixTime: (slot: number) => number;
    readonly keyed: boolean;
    readonly found: RepairFound;
  }) =>
  (on: RebaseTarget | undefined) =>
    Effect.gen(function* () {
      const { plan, found } = input;
      const keys = new Set<string>();
      let keyed = input.keyed;
      const ingest = (dead: PendingBlockFinalizationsDB.DeadHeaders) =>
        Effect.gen(function* () {
          const outcome = yield* reconcileFollowerEvents(plan, {
            network: input.network,
            slotToUnixTime: input.slotToUnixTime,
            cutoffMs: input.slotToUnixTime(plan.view.point.slot),
            dead,
          });
          if (outcome.kind === "stale")
            return yield* Effect.fail(
              followerWriteHeld(
                FOLLOWER_VIEW_STALE,
                "the follower view moved during the recompute",
              ),
            );
          for (const outRef of outcome.ingestion.spendableRestored)
            keys.add(outRef.toString("hex"));
          if (outcome.ingestion.orphans > 0) keyed = false;
          return outcome.ingestion;
        });
      // Held orphans keep the gate pending. One a foreign landed block
      // holds is in a header that includes an event no longer on L1: the
      // header is fault-provable and a block built on it shares the fault,
      // so the node does not build on it until the header leaves the
      // landed queue. That waits on another actor (a fault proof or an L1
      // rollback), not on a retry here.
      const describeHeld = Effect.gen(function* () {
        if (found.heldOrphans > 0)
          found.heldOrphansDetail = yield* describeOrphansAwaitingRecovery(
            found.heldOrphans,
          );
      });
      const dead = yield* deadJournalsFor(on);
      const first = yield* ingest(dead);
      found.inserted = first.inserted;
      found.refused = [...first.refused];
      if (on === undefined) {
        found.heldOrphans = first.orphans;
        yield* describeHeld;
        return { outcome: undefined, changes: undefined };
      }
      const rebased = yield* rebaseSql(on);
      for (const key of rebased.keys) keys.add(key);
      if ((yield* deleteUnheldOrphans(dead)) > 0) {
        keyed = false;
        yield* ingest(dead);
        const again = yield* rebaseTargetOf(yield* retrieveRows, on.journals);
        if (again.kind !== "ready")
          return yield* Effect.fail(
            new Error(
              `the rebase target became unready after the orphan repair: ${again.detail}`,
            ),
          );
        yield* rebaseSql(again.target);
      }
      // The rebase released its dead journals' events to awaiting; the due
      // ones are projected again at once, for the next block to carry.
      found.heldOrphans = (yield* ingest(dead)).orphans;
      yield* describeHeld;
      return {
        outcome: rebased,
        changes: keyed
          ? ({
              keys,
              spendable: yield* MempoolLedgerDB.spendableAt(keys),
            } satisfies DurableChanges)
          : undefined,
      };
    });
