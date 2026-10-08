/**
 * What landed-block processing needs from the node, as ports, so the same
 * processing runs under the node's history producer in production and
 * under the fork simulator in tests.
 */
import type { View } from "@al-ft/midgard-l1-follower";
import type { Effect } from "effect";

import type * as Ledger from "../database/utils/ledger.js";
import type { Database } from "../services/database.js";
import type { ReplayInput, ReplayOutcome } from "./replay.js";

/** This node's journal of a block it committed, as processing reads it. */
export type OwnJournal = Readonly<{
  status: "active" | "finalized" | "abandoned";
  baseTailHeaderHash: string;
  baseUtxosRoot: string;
  expectedUtxosRoot: string;
  spent: readonly Buffer[];
  produced: readonly Ledger.MinimalEntry[];
  depositIds: readonly Buffer[];
  forcedIds: readonly Buffer[];
  txIds: readonly Buffer[];
}>;

export type LandedBlockPorts<R> = Readonly<{
  /** Whether the follower is still at `view`; runs inside the write's transaction. */
  confirmView: (view: View) => Effect.Effect<boolean, unknown, R | Database>;
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
  /** Whether this node finalized its own merged block `headerHash` locally. */
  ownMergeCompleted: (
    headerHash: string,
  ) => Effect.Effect<boolean, unknown, R | Database>;
  /** Folds this node's own merged block into `confirmed_ledger` (its journal's delta). */
  finalizeOwnMerge: (input: {
    readonly headerHash: Buffer;
    readonly headerUtxosRoot: string;
  }) => Effect.Effect<void, unknown, R | Database>;
  /** The configured genesis ledger. */
  genesis: Effect.Effect<
    readonly Ledger.EntryNoTimeStamp[],
    unknown,
    R | Database
  >;
  /**
   * Asks the history owner to run the working-ledger rebase, unless it cannot
   * run yet; then returns why (and asks nothing).
   */
  requestRebase: (
    reason: string,
  ) => Effect.Effect<string | undefined, unknown, R | Database>;
}>;
