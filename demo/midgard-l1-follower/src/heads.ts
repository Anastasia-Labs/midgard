/**
 * The heads module (plan §9): the one place in a process that knows k, cd,
 * how depth is counted and what "now" means for an L1 validity decision.
 *
 * - `depth(tip, point) = tip.height − point.height + 1`, so the tip block has
 *   depth 1.
 * - Levels: `local` (own, not landed), `landed` (depth ≥ 1), `safe`
 *   (depth ≥ cd; liveness only, never gates a delete, release or retirement),
 *   `final` (depth > k; the durable level) and `merged` (an L2 header whose
 *   merge tx is landed or deeper, reported with that tx's level).
 *
 *   Final is strictly deeper than k. A rollback of up to k blocks is legal
 *   (the node never rolls back more; the fact store raises
 *   `rollback_beyond_k` only beyond k), and a rollback of k blocks removes
 *   depths 1..k. Only depth k + 1 and deeper survives every legal rollback.
 * - `slotNow` = the newest tip slot + the slots elapsed since that tip
 *   arrived, timed on a monotonic clock, held so it never decreases. It is
 *   the only "now" allowed for an L1 validity decision: a wall clock that runs
 *   fast or jumps cannot move it, and a monotonic clock that runs fast only
 *   extrapolates across one gap between blocks.
 *
 * Other modules never compare a depth against `confirmationDepth` or
 * `automaticRecoveryMaxDepth` themselves; the lint
 * `midgard/depth-through-heads` enforces that.
 */
import type { Point } from "./types.js";

/** The level of a tx, block or L2 header (plan §9). */
export type HeadLevel = "local" | "landed" | "safe" | "final";

/** The levels a point on the current chain can have. */
export type ChainLevel = Exclude<HeadLevel, "local">;

/** cd and k, both in blocks. */
export type DepthParameters = Readonly<{
  /** cd: the manifest `confirmationDepth`. Liveness only. */
  confirmationDepth: number;
  /** k: the security parameter, the manifest `automaticRecoveryMaxDepth`. */
  securityParameter: number;
}>;

/** A point with its depth and level, as the API reports a head. */
export type Head = Readonly<{
  point: Point;
  height: number;
  depth: number;
  level: ChainLevel;
}>;

/** Where an L2 header's merge stands: merged once its merge tx has landed. */
export type MergedStatus =
  | Readonly<{ merged: true; level: ChainLevel }>
  | Readonly<{ merged: false; level: "local" | null }>;

/** A tip observation: the slot and height of the chain's latest block. */
export type TipObservation = Readonly<{ slot: number; height: number }>;

export class HeadsParameterError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "HeadsParameterError";
  }
}

const assertCount = (name: string, value: number, minimum: number): void => {
  if (!Number.isSafeInteger(value) || value < minimum)
    throw new HeadsParameterError(
      `${name} must be an integer of at least ${minimum.toString()}, got ${String(value)}`,
    );
};

/** Validates cd and k: cd ≥ 1 and k ≥ cd. */
export const depthParameters = (
  parameters: DepthParameters,
): DepthParameters => {
  assertCount("confirmationDepth", parameters.confirmationDepth, 1);
  assertCount(
    "securityParameter",
    parameters.securityParameter,
    parameters.confirmationDepth,
  );
  return {
    confirmationDepth: parameters.confirmationDepth,
    securityParameter: parameters.securityParameter,
  };
};

/**
 * The depth of the block at `pointHeight` under a tip at `tipHeight`: 1 at
 * the tip, 0 or less for a height above it (not on the chain).
 */
export const depth = (tipHeight: number, pointHeight: number): number =>
  tipHeight - pointHeight + 1;

/** The height of the block at depth `atDepth` under a tip at `tipHeight`. */
export const heightAtDepth = (tipHeight: number, atDepth: number): number =>
  tipHeight - atDepth + 1;

/** The level of a block at depth `atDepth`; null when it is not on the chain. */
export const levelAtDepth = (
  atDepth: number,
  parameters: DepthParameters,
): ChainLevel | null => {
  if (atDepth > parameters.securityParameter) return "final";
  if (atDepth >= parameters.confirmationDepth) return "safe";
  if (atDepth >= 1) return "landed";
  return null;
};

/** Depth ≥ cd. Liveness only: never a reason to delete, release or retire. */
export const isSafe = (
  atDepth: number,
  parameters: Pick<DepthParameters, "confirmationDepth">,
): boolean => atDepth >= parameters.confirmationDepth;

/** Depth > k: beyond every legal rollback, the durable level. */
export const isFinal = (
  atDepth: number,
  parameters: Pick<DepthParameters, "securityParameter">,
): boolean => atDepth > parameters.securityParameter;

/**
 * The level of a tx, block or header that is either on the chain at
 * `atDepth` or absent from it (`null`). An absent item of our own (an own
 * intent or own block) is `local`; an absent foreign item has no level.
 */
export const levelOf = (
  atDepth: number | null,
  parameters: DepthParameters,
  own: boolean,
): HeadLevel | null => {
  const onChain = atDepth === null ? null : levelAtDepth(atDepth, parameters);
  if (onChain !== null) return onChain;
  return own ? "local" : null;
};

/** `merged` is never claimed bare: it carries the merge tx's level. */
export const mergedStatus = (mergeTxLevel: HeadLevel | null): MergedStatus =>
  mergeTxLevel === null || mergeTxLevel === "local"
    ? { merged: false, level: mergeTxLevel }
    : { merged: true, level: mergeTxLevel };

/** A monotonic millisecond clock; never the wall clock. */
export type MonotonicClock = () => number;

const monotonicNow: MonotonicClock = () => performance.now();

/** `slotNow` from tip observations and a monotonic clock (plan §3.6, §9). */
export type SlotClock = Readonly<{
  /** Records the chain's latest block slot, as the follower sees it. */
  observeTipSlot(slot: number): void;
  /** The latest observed tip slot, or null before the first observation. */
  tipSlot(): number | null;
  /**
   * The newest tip slot plus the whole slots elapsed on the monotonic clock
   * since that tip arrived, and never less than a value returned before.
   * Each tip with a slot above every earlier one re-anchors the estimate at
   * its arrival, so a monotonic clock that runs fast or slow against the
   * chain errs only across one gap between blocks, not across the uptime.
   * A tip at or below the newest one (a re-read, a rollback, a stale source)
   * neither re-anchors it nor moves it back. Null until a tip has been
   * observed: no L1 decision may then be taken.
   */
  slotNow(): number | null;
}>;

export type SlotClockOptions = Readonly<{
  /** The current era's slot length, from the era history. */
  slotLengthMs: number;
  /** Defaults to `performance.now()`. */
  monotonicNowMs?: MonotonicClock;
}>;

export const createSlotClock = (options: SlotClockOptions): SlotClock => {
  assertCount("slotLengthMs", options.slotLengthMs, 1);
  const slotLengthMs = options.slotLengthMs;
  const now = options.monotonicNowMs ?? monotonicNow;
  let tip: number | null = null;
  // The newest tip (the highest slot observed) and when it arrived. The
  // monotonic clock extrapolates only from here, so its rate error counts
  // across one gap between blocks.
  let anchor: { slot: number; atMs: number } | null = null;
  // The highest value `slotNow` has returned: a re-anchor below an earlier
  // extrapolation never moves "now" back.
  let highWater: number | null = null;
  return {
    observeTipSlot(slot) {
      assertCount("tip slot", slot, 0);
      tip = slot;
      if (anchor === null || slot > anchor.slot) anchor = { slot, atMs: now() };
    },
    tipSlot: () => tip,
    slotNow() {
      if (anchor === null) return null;
      const elapsedSlots = Math.max(
        0,
        Math.floor((now() - anchor.atMs) / slotLengthMs),
      );
      highWater = Math.max(highWater ?? 0, anchor.slot + elapsedSlots);
      return highWater;
    },
  };
};

/** One per process: cd, k, the latest tip and `slotNow`. */
export type Heads = DepthParameters &
  Readonly<{
    observeTip(tip: TipObservation): void;
    tip(): TipObservation | null;
    slotNow(): number | null;
    /** Depth of the block at `pointHeight`; null before the first tip. */
    depthOf(pointHeight: number): number | null;
    /** Level of the block at `pointHeight`; null when not on the chain. */
    levelOf(pointHeight: number): ChainLevel | null;
    /** The head record for a stored point, or null when not on the chain. */
    head(point: Point, pointHeight: number): Head | null;
  }>;

export type HeadsOptions = DepthParameters & SlotClockOptions;

export const createHeads = (options: HeadsOptions): Heads => {
  const parameters = depthParameters(options);
  const clock = createSlotClock(options);
  let tip: TipObservation | null = null;
  const depthOf = (pointHeight: number): number | null =>
    tip === null ? null : depth(tip.height, pointHeight);
  const chainLevel = (pointHeight: number): ChainLevel | null => {
    const atDepth = depthOf(pointHeight);
    return atDepth === null ? null : levelAtDepth(atDepth, parameters);
  };
  return {
    ...parameters,
    observeTip(observed) {
      assertCount("tip height", observed.height, 0);
      clock.observeTipSlot(observed.slot);
      tip = { slot: observed.slot, height: observed.height };
    },
    tip: () => tip,
    slotNow: clock.slotNow,
    depthOf,
    levelOf: chainLevel,
    head(point, pointHeight) {
      const atDepth = depthOf(pointHeight);
      const level = chainLevel(pointHeight);
      return atDepth === null || level === null
        ? null
        : { point, height: pointHeight, depth: atDepth, level };
    },
  };
};
