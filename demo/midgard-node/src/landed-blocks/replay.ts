/**
 * What processing a foreign landed block needs from a replayer: the
 * block's post-state on its parent's ledger, or why it cannot be had yet
 * (`missing`, `da_refetch_pending`, `event_unknown`, `forced_order_pending`,
 * `incomplete`) or
 * ever (`invalid`:
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
 * - `da_refetch_pending`: the retained payload no longer verified, was
 *   deleted, and its refetch has not been served yet;
 * - `event_unknown`: the block names an event the follower does not know at
 *   the view at all (a known one outside the window is `invalid`);
 * - `forced_order_pending`: a forced order admitted in the block's window
 *   cannot be read back from the facts at the view yet;
 * - `incomplete`: the import failed for a reason it cannot pin on the
 *   block (a local computation, or a replay failure it cannot classify);
 * - `invalid`: the block does not replay to its header.
 */
export type ReplayOutcome =
  | Replayed
  | Readonly<{
      kind:
        | "missing"
        | "da_refetch_pending"
        | "event_unknown"
        | "forced_order_pending"
        | "incomplete"
        | "invalid";
      detail: string;
    }>;

export type LandedBlockReplayer = (
  input: ReplayInput,
) => Promise<ReplayOutcome>;
