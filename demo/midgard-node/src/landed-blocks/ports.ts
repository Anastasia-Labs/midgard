/**
 * What landed-block processing needs from the node, as ports, so the same
 * processing runs under the follower-change driver's write capability in
 * production and under the fork simulator in tests.
 */
import type { View } from "@al-ft/midgard-l1-follower";
import { Data, Effect } from "effect";

import type * as Ledger from "../database/utils/ledger.js";
import type { DriverHold } from "../l1-events/driver.js";
import type { LandedStateQueueElement } from "../l1-state-queue/index.js";
import type { Database } from "../services/database.js";
import type { ReplayInput, ReplayOutcome } from "./replay.js";
import type { WithdrawalMembership } from "./store.js";

/** This node's journal of a block it committed, as processing reads it. */
export type OwnJournal = Readonly<{
  status: "active" | "locally_applied" | "abandoned";
  baseTailHeaderHash: string;
  baseUtxosRoot: string;
  expectedUtxosRoot: string;
  spent: readonly Buffer[];
  produced: readonly Ledger.MinimalEntry[];
  depositIds: readonly Buffer[];
  forcedIds: readonly Buffer[];
  /** The withdrawals it included, with the classification it journaled. */
  withdrawals: readonly WithdrawalMembership[];
  txIds: readonly Buffer[];
  /**
   * Revived: its block landed after its journal was abandoned, and it waits
   * for local finalization (`observed_waiting_stability`, abandonment digest
   * kept).
   */
  revived: boolean;
}>;

export type LandedBlockPorts<R> = Readonly<{
  /** Whether the follower is still at `view`; runs inside the write's transaction. */
  confirmView: (view: View) => Effect.Effect<boolean, unknown, R | Database>;
  /**
   * The queue outputs retained at `view`, live or spent since
   * (`stateQueueHistoryIn`); fails `ViewMoved` if the follower left `view`.
   */
  queueHistory: (
    view: View,
  ) => Effect.Effect<readonly LandedStateQueueElement[], unknown, R | Database>;
  /** Runs `work` in one gated transaction that it owns (the outermost one). */
  write: <A, E>(
    work: Effect.Effect<A, E, R | Database>,
  ) => Effect.Effect<A, unknown, R | Database>;
  /** Replays a foreign block on its parent's ledger. */
  replay: (
    input: ReplayInput,
  ) => Effect.Effect<ReplayOutcome, unknown, R | Database>;
  /** This node's journal of `headerHash`, if it committed that block. */
  ownJournal: (
    headerHash: string,
  ) => Effect.Effect<OwnJournal | undefined, unknown, R | Database>;
  /** The configured genesis ledger. */
  genesis: Effect.Effect<
    readonly Ledger.EntryNoTimeStamp[],
    unknown,
    R | Database
  >;
  /**
   * Runs the working-ledger rebase now (the driver's recompute), unless it
   * cannot run yet; returns why it did not finish, as its hold. A held or
   * failed rebase is retried on the driver's backoff.
   */
  rebase: (
    reason: string,
  ) => Effect.Effect<DriverHold | undefined, unknown, R | Database>;
}>;

/** The follower moved off the view a write was computed at. */
export class ViewMoved extends Data.TaggedError("ViewMoved")<
  Record<string, never>
> {}

/**
 * Runs `work` in one write transaction that first checks the follower is
 * still at `view`; fails `ViewMoved` (writing nothing) if it is not.
 */
export const viewChecked = <R, A, E>(
  ports: LandedBlockPorts<R>,
  view: View,
  work: Effect.Effect<A, E, R | Database>,
) =>
  ports.write(
    Effect.gen(function* () {
      if (!(yield* ports.confirmView(view)))
        return yield* Effect.fail(new ViewMoved({}));
      return yield* work;
    }),
  );
