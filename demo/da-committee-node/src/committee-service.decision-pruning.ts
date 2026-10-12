import type { Obligation, SignedHeader } from "./l1/follower/obligations.js";
import type { CommitteeStore } from "./store.js";

/** Signed decisions deleted per tick at most, so a backlog spreads out. */
export const DECISION_PRUNE_BATCH = 256;

/** A tick's prune of its deletable signed decisions failed. */
export const COMMITTEE_DECISION_PRUNE_FAILED =
  "committee_decision_prune_failed";

export type DecisionPruneInputs = Readonly<{
  obligations: readonly Obligation[];
  signed: readonly SignedHeader[];
  /** Headers live in the queue at the tip. */
  live: ReadonlySet<string>;
  /** Headers in the queue at the latest final block. */
  finalQueue: readonly string[];
  /**
   * Headers whose stored record is still unsettled after this tick: no
   * terminal record (a final exit) is written for them.
   */
  unsettled: ReadonlySet<string>;
  /** The release clock: the latest final block's time, or null. */
  finalBlockTimeMs: number | null;
}>;

/**
 * The signed decisions this tick deletes (plan §11, class B): those
 * `obligations()` reads as `decisionDeletable`, and only once no row of
 * theirs can be read or decided on again.
 *
 * - The header is in no queue the committee still reads: not live at the
 *   tip, nor at the latest final block.
 * - `cannot_land` and `beyond_retention`: no commit of the header is on the
 *   chain, and a final block is past its end time, so none can land.
 * - `final`: the commit is final, and so is the header's exit from the queue
 *   (its stored record is terminal, `terminalRecordOf`). The release clock
 *   must be past the header's end time too: a commit lands only in a block
 *   no later than its end time, so the header can never be observed again.
 *
 * A decision that is deleted is therefore never re-signed. The batch is
 * capped at `DECISION_PRUNE_BATCH`, in header-hash order.
 */
export const decisionsToPrune = (inputs: DecisionPruneInputs): string[] => {
  const endTimes = new Map(
    inputs.signed.map(({ headerHash, endTimeMs }) => [headerHash, endTimeMs]),
  );
  const clock = inputs.finalBlockTimeMs;
  const pastEnd = (headerHash: string): boolean => {
    const endTimeMs = endTimes.get(headerHash);
    return (
      clock !== null && endTimeMs !== undefined && BigInt(clock) > endTimeMs
    );
  };
  const held = new Set([...inputs.live, ...inputs.finalQueue]);
  const out: string[] = [];
  for (const obligation of inputs.obligations) {
    if (out.length >= DECISION_PRUNE_BATCH) break;
    const { headerHash, state } = obligation;
    if (!obligation.decisionDeletable || held.has(headerHash)) continue;
    const exited =
      state === "cannot_land" ||
      state === "beyond_retention" ||
      (state === "final" && !inputs.unsettled.has(headerHash));
    if (exited && pastEnd(headerHash)) out.push(headerHash);
  }
  return out;
};

/**
 * Deletes the signed decisions `decisionsToPrune` selects. Returns the tick
 * errors it adds: none, or `committee_decision_prune_failed` with the
 * failure's message. A failure never fails the tick: its signing and
 * reconcile work stands, the backlog carries forward, and the next tick
 * retries; the reason clears with the next successful prune.
 */
export const pruneDecisions = async (
  store: Pick<CommitteeStore, "pruneSignedDecisions">,
  inputs: DecisionPruneInputs,
): Promise<string[]> => {
  try {
    await store.pruneSignedDecisions(decisionsToPrune(inputs));
    return [];
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    return [`${COMMITTEE_DECISION_PRUNE_FAILED}: ${message}`];
  }
};
