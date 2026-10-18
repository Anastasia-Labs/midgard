/**
 * A foreign block's event sets against the follower's facts at a view
 * (`exact`), and whether those facts hold every event the block's window
 * could name, so that an event the block names and the view lacks is the
 * block's fault (`invalid`) and not a gap in this node's facts (a bounded
 * `event_unknown`, or `origin_after_frontier` when the follower store's
 * origin is too late for the block).
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
 *   (`slotOf` floors). The origin must be at or before `T(B)`; a later one
 *   is a store whose origin is too late for the block
 *   (`origin_after_frontier`).
 * - A deposit or withdrawal in the block's window is retired only once the
 *   confirmed state passed its inclusion time: once the block, or one in its
 *   place, merged, after the block's node output entered the facts at
 *   `entry(B)`. So the prune must still be below `entry(B)`. A seeded node
 *   output has no known entry, and the origin is after it: never complete,
 *   and the store's origin too late for the block.
 * - A forced order's spend is not bound to the block's entry, so the prune
 *   must be at or before `T(B)`: every order admitted in the window, spent or
 *   not, is then still in the facts.
 * - No event is admitted before the protocol-init spend (the valid tx that
 *   spends the deployment's `hubOracleOneShot`); one in that spend's block
 *   is admitted at its slot. A store holding that spend at the view saw
 *   every event ever admitted, so the block's events are admitted after
 *   `max(T(B), the spend's slot - 1)`, which is at or after the store's
 *   origin.
 */
import type { FactStore, OutRef, View } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";

import {
  invalid,
  originAfterFrontier,
  unknownEvent,
  type Verdict,
} from "./replay-foreign.verdict.js";

/** What the replay needs of the follower cursor. */
export type CoverageCursor = Readonly<{
  generation: number;
  origin: Readonly<{ slot: number }>;
  prunedThroughSlot: number;
}>;

/**
 * Complete, or why not: `lateOrigin` when the follower store's origin is too
 * late for the block (after `T(B)`, or the block's node output seeded at
 * it), which no later read of the same store covers.
 */
export type Coverage =
  | Readonly<{ complete: true }>
  | Readonly<{ complete: false; detail: string; lateOrigin?: true }>;

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
  /** The protocol-init spend's slot, where the view's facts hold it. */
  initSlot?: number;
}>;

/** The block's deposits and withdrawals, and its forced orders. */
export type EventCoverage = Readonly<{ user: Coverage; txOrder: Coverage }>;

const short = (detail: string): Coverage => ({ complete: false, detail });
const late = (detail: string): Coverage => ({
  complete: false,
  detail,
  lateOrigin: true,
});

/**
 * Whether the facts cover a block's events, admitted after `T(B)` (or after
 * `max(T(B), initSlot - 1)` where the facts hold the protocol-init spend).
 * For every kind: the cursor in the view's store generation (a reset since
 * the facts were read leaves the cursor describing other facts) and the
 * origin at or before that slot (else `lateOrigin`, as is a seeded node
 * output for deposits and withdrawals). For deposits and withdrawals, the
 * prune below the block's entry; for forced orders, at or before that slot:
 * an order's spend is not bound to the block's entry, so one admitted in the
 * window may be spent before it.
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
  const tb = earliestAdmissionSlot(input.startTime, slotOf);
  if (tb === null) return both("the block's start maps to no slot");
  const earliest =
    input.initSlot === undefined ? tb : Math.max(tb, input.initSlot - 1);
  if (cursor.origin.slot > earliest) {
    const after = late(
      `the follower's origin at slot ${cursor.origin.slot.toString()} is after slot ${earliest.toString()}, after which the block's events are admitted`,
    );
    return { user: after, txOrder: after };
  }
  const pruned = cursor.prunedThroughSlot.toString();
  return {
    user:
      input.enteredSlot === null
        ? late("the block's node output predates the follower's origin")
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

/**
 * The coverage at `view`, from the follower cursor read after the facts (a
 * prune since only moves it later, and a reset since changes the
 * generation). Only where it falls short, and `oneShot` is known, is the
 * protocol-init fact read (one keyed row), then the cursor again, so a reset
 * since is still seen; the spend counts where it is at or before the view.
 * A read that fails leaves a late origin a bounded wait.
 */
export const coverageAt = async (
  store: Pick<FactStore, "cursor" | "protocolInit">,
  oneShot: OutRef | undefined,
  view: View,
  input: CoverageInput,
  slotOf: (unixTimeMs: number) => number,
): Promise<EventCoverage> => {
  const cursor = () => store.cursor().catch(() => null);
  const coverage = eventCoverage(await cursor(), input, slotOf);
  if (
    oneShot === undefined ||
    (coverage.user.complete && coverage.txOrder.complete)
  )
    return coverage;
  const spend = await store.protocolInit(oneShot).then(
    (found) => ({ found }),
    () => undefined,
  );
  // Unread, a late origin may yet be covered: a bounded wait, not the hold.
  const unread = (kind: Coverage): Coverage =>
    kind.complete || kind.lateOrigin !== true
      ? kind
      : short(`the protocol-init fact is unreadable; ${kind.detail}`);
  if (spend === undefined)
    return { user: unread(coverage.user), txOrder: unread(coverage.txOrder) };
  if (spend.found === null || spend.found.slot > view.point.slot)
    return coverage;
  return eventCoverage(
    await cursor(),
    { ...input, initSlot: spend.found.slot },
    slotOf,
  );
};

/** The verdict on an event `kind` `id` a block names and the view does not know. */
export type Absent = (kind: string, id: string) => Verdict;

export const unknownAt: Absent = (kind, id) =>
  unknownEvent(`${kind} ${id} is not known at the view`);

/**
 * An absent event: the block's fault where the facts cover the block's
 * range for its kind (`eventCoverage`), else a gap in this node's facts: a
 * follower store whose origin is too late for the block
 * (`origin_after_frontier`), or a bounded `event_unknown`.
 */
export const absentWhere =
  (coverage: Coverage): Absent =>
  (kind, id) =>
    coverage.complete
      ? invalid(
          `${kind} ${id} is not admitted at the view, whose facts cover the block's range`,
        )
      : (coverage.lateOrigin === true ? originAfterFrontier : unknownEvent)(
          `${kind} ${id} is not known at the view: ${coverage.detail}`,
        );

/**
 * `known` are the in-window ids, `seen` every id the view knows: an id
 * outside the window has a fixed inclusion time, so naming it is the
 * block's fault; one the view does not know at all is `absent`'s verdict.
 * `undecodable` are the in-window ids ingestion leaves out, with why: never
 * in `known`, so a block need not name them, and naming one is the block's
 * fault.
 */
export const exact = (
  known: ReadonlySet<string>,
  seen: ReadonlySet<string>,
  named: readonly SDK.DaPayloadEntry[],
  kind: string,
  absent: Absent = unknownAt,
  undecodable: ReadonlyMap<string, string> = new Map(),
) => {
  const ids = named.map(([key]) => key);
  if (new Set(ids).size !== ids.length)
    return invalid(`the block names a ${kind} twice`);
  const unread = ids.find((id) => undecodable.has(id));
  if (unread !== undefined)
    return invalid(
      `${kind} ${unread} is not a user event the node reads: ${undecodable.get(unread)}`,
    );
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
