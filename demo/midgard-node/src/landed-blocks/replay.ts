/**
 * What processing a foreign landed block needs from a replayer: the
 * block's post-state on its parent's ledger, or why it cannot be had yet
 * (`missing`, `event_unknown`, `forced_order_pending`) or ever (`invalid`:
 * the block does not replay to its header). A replayer throws
 * only for faults it cannot classify; the hook holds on those too.
 */
import type { View } from "@al-ft/midgard-l1-follower";
import type * as SDK from "@al-ft/midgard-sdk";

import type * as Ledger from "../database/utils/ledger.js";
import type { WithdrawalMembership } from "./store.js";

export type ReplayInput = Readonly<{
  headerHash: string;
  header: SDK.Header;
  parentHeaderHash: string;
  parentUtxosRoot: string;
  parentEntries: readonly Ledger.MinimalEntry[];
  /** The follower view the queue was read at. */
  view: View;
}>;

export type Replayed = Readonly<{
  kind: "replayed";
  entries: readonly Ledger.MinimalEntry[];
  root: string;
  depositIds: readonly Buffer[];
  withdrawals: readonly WithdrawalMembership[];
  forcedIds: readonly Buffer[];
  txIds: readonly Buffer[];
}>;

/**
 * - `missing`: the DA payload is not available yet, or the block ends past
 *   what the view can know;
 * - `event_unknown`: the block names an event the follower does not know at
 *   the view;
 * - `forced_order_pending`: a forced order admitted in the block's window
 *   cannot be read back from the facts at the view yet;
 * - `invalid`: the block does not replay to its header.
 */
export type ReplayOutcome =
  | Replayed
  | Readonly<{
      kind: "missing" | "event_unknown" | "forced_order_pending" | "invalid";
      detail: string;
    }>;

export type LandedBlockReplayer = (
  input: ReplayInput,
) => Promise<ReplayOutcome>;
