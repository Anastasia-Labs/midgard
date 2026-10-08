/**
 * The node's heads (plan §9): its L1 "now" and its tip source.
 *
 * `l1SlotNow(api)` is the only "now" for an L1 validity decision in the node
 * (plan §3.6): max(tip slot, last tip slot + elapsed / slotLength), from
 * `@al-ft/midgard-l1-follower/heads`, with the elapsed time read from a
 * monotonic clock. A wall clock that runs fast or jumps cannot move it. The
 * wall clock may still pick `valid_to` for a new tx, never declare anything
 * due, expired or mature.
 *
 * The tip is the L1 follower's covered tip (N1): the cursor of the follower
 * store in the node database (`l1_follower_cursor`), the same tip `depth()`
 * counts from. Every node process that opens the node database (the main
 * thread, its worker threads, a CLI command) installs one reader of it
 * (`installL1FollowerTipReader`), and each Lucid client the node builds
 * registers it as its source (`registerL1TipSource`). An emulator client has
 * its own exact chain slot and needs no source.
 *
 * `l1BlockBelowCoveredTip(store, d)` is the heads source for "the block d
 * below the covered tip": the follower block at depth d + 1 under the
 * store's cursor (the covered tip has depth 1). U3 caps the commit end time
 * at `slot(tip − d) + W − 1` from it (`laggedEligibilityCap` in
 * `services/history-commit-window.ts`).
 */
import { SUBMIT_SLOT_LENGTH_MS } from "@al-ft/midgard-core/ogmios-slot";
import type {
  Cursor,
  Point as L1Point,
  StoredBlock,
} from "@al-ft/midgard-l1-follower";
import {
  createSlotClock,
  depth,
  heightAtDepth,
  type MonotonicClock,
  type SlotClock,
} from "@al-ft/midgard-l1-follower/heads";
import {
  isEmulatorProvider,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Data, Effect } from "effect";

import { slotToUnixTimeForLucidOrEmulatorFallback } from "./lucid-time.js";

/** No tip has been observed yet, so no L1 decision may be taken: retry. */
export class L1SlotUnknownError extends Data.TaggedError("L1SlotUnknownError")<{
  readonly message: string;
  readonly cause?: unknown;
}> {}

/** A tip read is reused for this long before `l1SlotNow` reads again. */
export const L1_TIP_REFRESH_MS = 1_000;

/** Reads the tip slot: the follower's covered tip in production. */
export type L1TipRead = () => Effect.Effect<number, unknown>;

type TipSource = {
  readonly clock: SlotClock;
  readonly read: L1TipRead;
  readonly monotonicNowMs: MonotonicClock;
  lastReadAtMs: number | undefined;
};

// One source per Lucid client, so separate clients (and separate tests) never
// share a "now".
const sources = new WeakMap<LucidEvolution, TipSource>();

const monotonicNow: MonotonicClock = () => performance.now();

let followerTipReader: L1TipRead | undefined;

/**
 * Installs this process's reader of the follower's covered tip (one per
 * process; the node database layer installs it). A later install replaces
 * an earlier one.
 */
export const installL1FollowerTipReader = (read: L1TipRead): void => {
  followerTipReader = read;
};

/**
 * The follower's covered tip slot through this process's reader. Fails while
 * none is installed or the follower store has no cursor yet; `l1SlotNow`
 * then keeps its last estimate, or stays unknown.
 */
export const readL1FollowerTipSlot: L1TipRead = () =>
  followerTipReader === undefined
    ? Effect.fail(
        new L1SlotUnknownError({
          message:
            "L1 slot unknown: this process has no reader of the follower's covered tip",
        }),
      )
    : followerTipReader();

/**
 * Registers one tip source for live Lucid clients that share an L1 view. A
 * read through `l1SlotNow`, or one reported with `observeL1Tip`, moves the
 * "now" of every client registered with it.
 */
export const registerL1TipSource = (
  apis: readonly LucidEvolution[],
  read: L1TipRead,
  options: {
    readonly slotLengthMs?: number;
    readonly monotonicNowMs?: MonotonicClock;
  } = {},
): void => {
  const monotonicNowMs = options.monotonicNowMs ?? monotonicNow;
  const source: TipSource = {
    clock: createSlotClock({
      slotLengthMs: options.slotLengthMs ?? SUBMIT_SLOT_LENGTH_MS,
      monotonicNowMs,
    }),
    read,
    monotonicNowMs,
    lastReadAtMs: undefined,
  };
  for (const api of apis) sources.set(api, source);
};

/** Records a tip slot read outside `l1SlotNow` for this client. */
export const observeL1Tip = (api: LucidEvolution, tipSlot: number): void => {
  const source = sources.get(api);
  if (source === undefined) return;
  source.clock.observeTipSlot(tipSlot);
  source.lastReadAtMs = source.monotonicNowMs();
};

/**
 * The client's L1 `slotNow`. It reads the tip again when the last read is
 * older than `L1_TIP_REFRESH_MS`; a failed read keeps the estimate from the
 * last good one. Fails only while no tip has ever been read. An emulator
 * client without a registered source answers with its own chain slot.
 */
export const l1SlotNow = (
  api: LucidEvolution,
): Effect.Effect<number, L1SlotUnknownError> =>
  Effect.gen(function* () {
    const source = sources.get(api);
    if (source === undefined) {
      if (isEmulatorProvider(api.config().provider)) return api.currentSlot();
      return yield* Effect.fail(
        new L1SlotUnknownError({
          message: "L1 slot unknown: this Lucid client has no tip source",
        }),
      );
    }
    let readFailure: unknown;
    if (
      source.lastReadAtMs === undefined ||
      source.monotonicNowMs() - source.lastReadAtMs >= L1_TIP_REFRESH_MS
    ) {
      const read = yield* Effect.either(source.read());
      if (read._tag === "Right") observeL1Tip(api, read.right);
      else readFailure = read.left;
    }
    const slot = source.clock.slotNow();
    if (slot === null)
      return yield* Effect.fail(
        new L1SlotUnknownError({
          message: "L1 slot unknown: no tip has been read yet",
          cause: readFailure,
        }),
      );
    return slot;
  });

/** The POSIX time (ms) at the start of `l1SlotNow`, for ms-domain bounds. */
export const l1NowUnixTimeMs = (
  api: LucidEvolution,
): Effect.Effect<number, L1SlotUnknownError> =>
  Effect.map(l1SlotNow(api), (slot) =>
    slotToUnixTimeForLucidOrEmulatorFallback(api, slot),
  );

/** The block d below the follower's covered tip, or why there is none. */
export type L1BlockBelowCoveredTip =
  | Readonly<{
      kind: "block";
      point: L1Point;
      height: number;
      /** `depth(tip, point)`: always d + 1. */
      depth: number;
      /** The covered tip (the store's cursor) the block was read under. */
      tip: Readonly<{ point: L1Point; height: number }>;
    }>
  | Readonly<{
      kind: "unavailable";
      /**
       * `not_initialized`: the store has no cursor yet. `outside_history`:
       * that height lies below the follower's origin or its pruned history.
       * Both are transient for a caller: it holds its horizon and retries.
       */
      reason: "not_initialized" | "outside_history";
      detail: string;
    }>;

/** The follower reads the lag needs: its cursor and a block by height. A
 * `FactStore` is one; a caller with only SQL passes the same rows. */
export type CoveredTipHeads = Readonly<{
  cursor(): Promise<Pick<Cursor, "point" | "height"> | null>;
  blockAtHeight(
    height: number,
  ): Promise<Pick<StoredBlock, "slot" | "hash" | "height"> | null>;
}>;

/**
 * The block d below the covered tip (the follower cursor), read through the
 * heads module's `depth()`: the block at depth d + 1. d = 0 is the covered
 * tip itself.
 */
export const l1BlockBelowCoveredTip = async (
  store: CoveredTipHeads,
  lagBlocks: number,
): Promise<L1BlockBelowCoveredTip> => {
  if (!Number.isSafeInteger(lagBlocks) || lagBlocks < 0)
    throw new RangeError(
      `the lag d must be a non-negative integer, got ${String(lagBlocks)}`,
    );
  const cursor = await store.cursor();
  if (cursor === null)
    return {
      kind: "unavailable",
      reason: "not_initialized",
      detail: "the follower store has no covered tip yet",
    };
  const height = heightAtDepth(cursor.height, lagBlocks + 1);
  const block = height < 0 ? null : await store.blockAtHeight(height);
  if (block === null)
    return {
      kind: "unavailable",
      reason: "outside_history",
      detail: `no follower block at height ${height.toString()} (${lagBlocks.toString()} below the covered tip at ${cursor.height.toString()}): below the origin or pruned`,
    };
  return {
    kind: "block",
    point: { slot: block.slot, hash: block.hash },
    height: block.height,
    depth: depth(cursor.height, block.height),
    tip: { point: cursor.point, height: cursor.height },
  };
};
