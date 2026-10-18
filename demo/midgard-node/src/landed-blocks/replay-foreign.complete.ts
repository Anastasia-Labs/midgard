/**
 * A foreign block's event sets against the follower's facts at a view
 * (`exact`), and whether those facts hold every event the block's window
 * could name, so that an event the block names and the view lacks is the
 * block's fault (`invalid`) and not a gap in this node's facts (a bounded
 * `event_unknown`).
 *
 * The facts hold every event admitted at or after the follower's origin, and
 * keep a retired one until the prune passes its retirement
 * (`prunedThroughSlot`: facts are complete at every slot above it).
 *
 * - An event's earliest admission is bounded by its validity range: its
 *   inclusion time is the range's upper bound plus the event wait, so an
 *   event in a window starting at `B.start` has its range's lower bound, a
 *   slot's start time, after `B.start - event wait - max event validity
 *   range`, and was admitted after
 *   `T(B) = slotOf(max(0, B.start - event wait - max event validity range))`
 *   (`slotOf` floors). The origin must be at or before `T(B)`.
 * - A deposit or withdrawal in the block's window is retired only once the
 *   confirmed state passed its inclusion time: once the block, or one in its
 *   place, merged, after the block's node output entered the facts at
 *   `entry(B)`. So the prune must still be below `entry(B)`. A seeded node
 *   output has no known entry: never complete.
 * - A forced order's spend is not bound to the block's entry, so the prune
 *   must be at or before `T(B)`: every order admitted in the window, spent or
 *   not, is then still in the facts.
 */
import * as SDK from "@al-ft/midgard-sdk";

import {
  invalid,
  unknownEvent,
  type Verdict,
} from "./replay-foreign.verdict.js";

/** What the replay needs of the follower cursor. */
export type CoverageCursor = Readonly<{
  generation: number;
  origin: Readonly<{ slot: number }>;
  prunedThroughSlot: number;
}>;

/** Complete, or why not. */
export type Coverage =
  | Readonly<{ complete: true }>
  | Readonly<{ complete: false; detail: string }>;

/** `T(B)` for a window starting at `startTime`, if it maps to a slot: the block's events are admitted after it. */
export const earliestAdmissionSlot = (
  startTime: bigint,
  slotOf: (unixTimeMs: number) => number,
): number | null => {
  const earliest =
    startTime -
    BigInt(SDK.EVENT_WAIT_DURATION_MS) -
    SDK.MAX_EVENT_VALIDITY_RANGE_LENGTH_MS;
  try {
    return slotOf(Number(earliest > 0n ? earliest : 0n));
  } catch {
    return null;
  }
};

/** What the coverage is judged for: the block and the view it is read at. */
export type CoverageInput = Readonly<{
  startTime: bigint;
  enteredSlot: number | null;
  generation: number;
}>;

/** The block's deposits and withdrawals, and its forced orders. */
export type EventCoverage = Readonly<{ user: Coverage; txOrder: Coverage }>;

const short = (detail: string): Coverage => ({ complete: false, detail });

/**
 * Whether the facts cover a block's events. For every kind: the cursor in
 * the view's store generation (a reset since the facts were read leaves the
 * cursor describing other facts) and the origin at or before `T(B)`. For
 * deposits and withdrawals, the prune below the block's entry; for forced
 * orders, at or before `T(B)`: an order's spend is not bound to the block's
 * entry, so one admitted in the window may be spent before it.
 */
export const eventCoverage = (
  cursor: CoverageCursor | null,
  input: CoverageInput,
  slotOf: (unixTimeMs: number) => number,
): EventCoverage => {
  const both = (detail: string) => ({
    user: short(detail),
    txOrder: short(detail),
  });
  if (cursor === null) return both("the follower cursor is unreadable");
  if (cursor.generation !== input.generation)
    return both("the follower store changed generation since the view");
  const earliest = earliestAdmissionSlot(input.startTime, slotOf);
  if (earliest === null) return both("the block's start maps to no slot");
  if (cursor.origin.slot > earliest)
    return both(
      `the follower's origin at slot ${cursor.origin.slot.toString()} is after slot ${earliest.toString()}, after which the block's events are admitted`,
    );
  const pruned = cursor.prunedThroughSlot.toString();
  return {
    user:
      input.enteredSlot === null
        ? short("the block's node output predates the follower's origin")
        : cursor.prunedThroughSlot >= input.enteredSlot
          ? short(
              `the facts are pruned through slot ${pruned}, not below the block's entry at slot ${input.enteredSlot.toString()}`,
            )
          : { complete: true },
    txOrder:
      cursor.prunedThroughSlot > earliest
        ? short(
            `the facts are pruned through slot ${pruned}, past slot ${earliest.toString()}, after which the block's orders are admitted`,
          )
        : { complete: true },
  };
};

/** The verdict on an event `kind` `id` a block names and the view does not know. */
export type Absent = (kind: string, id: string) => Verdict;

export const unknownAt: Absent = (kind, id) =>
  unknownEvent(`${kind} ${id} is not known at the view`);

/**
 * An absent event: the block's fault where the facts cover the block's
 * range for its kind (`eventCoverage`), else a gap in this node's facts.
 */
export const absentWhere =
  (coverage: Coverage): Absent =>
  (kind, id) =>
    coverage.complete
      ? invalid(
          `${kind} ${id} is not admitted at the view, whose facts cover the block's range`,
        )
      : unknownEvent(
          `${kind} ${id} is not known at the view: ${coverage.detail}`,
        );

/**
 * `known` are the in-window ids, `seen` every id the view knows: an id
 * outside the window has a fixed inclusion time, so naming it is the
 * block's fault; one the view does not know at all is `absent`'s verdict.
 */
export const exact = (
  known: ReadonlySet<string>,
  seen: ReadonlySet<string>,
  named: readonly SDK.DaPayloadEntry[],
  kind: string,
  absent: Absent = unknownAt,
) => {
  const ids = named.map(([key]) => key);
  if (new Set(ids).size !== ids.length)
    return invalid(`the block names a ${kind} twice`);
  const outside = ids.find((id) => !known.has(id) && seen.has(id));
  if (outside !== undefined)
    return invalid(`${kind} ${outside} is outside the block's window`);
  const unknown = ids.find((id) => !known.has(id));
  if (unknown !== undefined) return absent(kind, unknown);
  const left = [...known].find((id) => !ids.includes(id));
  if (left !== undefined)
    return invalid(`the block leaves out in-window ${kind} ${left}`);
  return undefined;
};
