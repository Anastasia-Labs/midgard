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
 * The tip source is the local Ogmios ledger tip (`queryNetwork/tip`) until the
 * node reads the follower (N1), which replaces the source and these Ogmios
 * reads. Each Lucid client the node builds registers its source here
 * (`registerL1TipSource`); every snapshot read through it is observed. An
 * emulator client has its own exact chain slot and needs no source.
 *
 * `l1BlockBelowCoveredTip(store, d)` is the heads source for "the block d
 * below the covered tip": the follower block at depth d + 1 under the
 * store's cursor (the covered tip has depth 1). U3 caps the history commit
 * end time at `slot(tip − d) + W − 1` from it once N1 deletes the census
 * tables.
 */
import {
  type LocalOgmiosShelleyGenesisSlotOptions,
  type LocalOgmiosSubmitSlotOptions,
  queryLocalOgmiosShelleyGenesisSlotConfig,
  queryLocalOgmiosSubmitSlotSnapshot,
  type ShelleyGenesisSlotEvidence,
  SUBMIT_SLOT_LENGTH_MS,
  type SubmitSlotSnapshot,
} from "@al-ft/midgard-core/ogmios-slot";
import type { FactStore, Point as L1Point } from "@al-ft/midgard-l1-follower";
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

type TipSource = {
  readonly clock: SlotClock;
  readonly read: () => Effect.Effect<SubmitSlotSnapshot, unknown>;
  readonly monotonicNowMs: MonotonicClock;
  lastReadAtMs: number | undefined;
};

// One source per Lucid client, so separate clients (and separate tests) never
// share a "now".
const sources = new WeakMap<LucidEvolution, TipSource>();

const monotonicNow: MonotonicClock = () => performance.now();

/** The ledger tip a snapshot carries (its `currentSlot` runs on wall time). */
export const snapshotTipSlot = (snapshot: SubmitSlotSnapshot): number =>
  snapshot.ledgerTipSlot ?? snapshot.currentSlot;

/**
 * Registers one tip source for live Lucid clients that share an L1 view. A
 * read through `l1SlotNow`, or one reported with `observeL1Tip`, moves the
 * "now" of every client registered with it.
 */
export const registerL1TipSource = (
  apis: readonly LucidEvolution[],
  read: () => Effect.Effect<SubmitSlotSnapshot, unknown>,
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

/** Records a tip read made outside `l1SlotNow` for this client. */
export const observeL1Tip = (
  api: LucidEvolution,
  snapshot: SubmitSlotSnapshot,
): void => {
  const source = sources.get(api);
  if (source === undefined) return;
  source.clock.observeTipSlot(snapshotTipSlot(snapshot));
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

export const fetchLocalOgmiosSubmitSlotSnapshot = (
  options: LocalOgmiosSubmitSlotOptions,
): Effect.Effect<SubmitSlotSnapshot, Error> =>
  Effect.tryPromise({
    try: (effectSignal) =>
      queryLocalOgmiosSubmitSlotSnapshot({
        ...options,
        signal:
          options.signal === undefined
            ? effectSignal
            : AbortSignal.any([options.signal, effectSignal]),
      }),
    catch: (cause) =>
      cause instanceof Error
        ? cause
        : new Error("Failed to fetch local Ogmios submit slot", { cause }),
  });

export const fetchLocalOgmiosShelleyGenesisSlotConfig = (
  options: LocalOgmiosShelleyGenesisSlotOptions,
): Effect.Effect<ShelleyGenesisSlotEvidence, Error> =>
  Effect.tryPromise({
    try: (effectSignal) =>
      queryLocalOgmiosShelleyGenesisSlotConfig({
        ...options,
        signal:
          options.signal === undefined
            ? effectSignal
            : AbortSignal.any([options.signal, effectSignal]),
      }),
    catch: (cause) =>
      cause instanceof Error
        ? cause
        : new Error("Failed to fetch local Ogmios Shelley genesis", {
            cause,
          }),
  });

export const makeLocalOgmiosSubmitSlotSnapshotProvider = (
  options: Omit<LocalOgmiosSubmitSlotOptions, "nowMs">,
): (() => Effect.Effect<SubmitSlotSnapshot, unknown>) => {
  return () =>
    fetchLocalOgmiosSubmitSlotSnapshot({ ...options, nowMs: Date.now() });
};

export const localOgmiosSubmitSlotEvidence = (
  snapshot: SubmitSlotSnapshot,
): string => {
  const health = snapshot.health;
  return [
    `submitSlot=${snapshot.currentSlot.toString()}`,
    `slotSource=${snapshot.source}`,
    `observedAtMs=${snapshot.observedAtMs.toString()}`,
    ...(health?.connectionStatus === undefined
      ? []
      : [`connectionStatus=${health.connectionStatus}`]),
    ...(health?.networkSynchronization === undefined
      ? []
      : [`networkSynchronization=${health.networkSynchronization.toString()}`]),
    ...(health?.lastKnownTipSlot === undefined
      ? []
      : [`lastKnownTipSlot=${health.lastKnownTipSlot.toString()}`]),
    ...(health?.lastTipUpdate === undefined
      ? []
      : [`lastTipUpdate=${health.lastTipUpdate}`]),
  ].join(",");
};

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

/**
 * The block d below the covered tip (the follower cursor), read through the
 * heads module's `depth()`: the block at depth d + 1. d = 0 is the covered
 * tip itself.
 */
export const l1BlockBelowCoveredTip = async (
  store: Pick<FactStore, "cursor" | "blockAtHeight">,
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
