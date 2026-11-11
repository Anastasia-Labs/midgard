import { setTimeout as pause } from "node:timers/promises";

import { isWatcherReplaySourcePending } from "midgard-watcher/replayed-ledger";

/** How long a journey replay read waits on a follower that does not move. */
export const JOURNEY_REPLAY_STALL_MS = 90_000;

/** The journey replay follower's state as a read waits on it. */
export type JourneyReplayFollowerView = Readonly<{
  isClosed(): boolean;
  /** Whether the follower holds no readiness reason. */
  ready(): boolean;
  /** The follower's cursor, or null before its first status. */
  cursor(): Readonly<{ height: number; generation: number }> | null;
  /** The follower's readiness reasons, for the failure message. */
  pending(): Promise<unknown>;
}>;

const cursorKey = (cursor: ReturnType<JourneyReplayFollowerView["cursor"]>) =>
  cursor === null
    ? null
    : `${cursor.generation.toString()}:${cursor.height.toString()}`;

/**
 * Runs `read` once the journey's replay follower is ready, and again after a
 * failure that only waits on its L1 or DA source, as the watcher's own replay
 * does. The follower starts at the stack's L1 origin and walks every block
 * since, which on a public network takes far longer than one read, so while
 * it catches up the wait is bounded by progress, not wall clock: it fails
 * once the cursor has not moved for `stallMs`. Once it is ready, cursor
 * movement is only the live chain and says nothing of the read, so reads that
 * keep waiting fail `stallMs` after the first one did, unless the follower
 * falls back to catching up. The journey's limit bounds the total.
 */
export const readWhenJourneyReplayed = async <T>(
  follower: JourneyReplayFollowerView,
  read: () => Promise<T>,
  options: Readonly<{ stallMs?: number; pollMs?: number }> = {},
): Promise<T> => {
  const stallMs = options.stallMs ?? JOURNEY_REPLAY_STALL_MS;
  const pollMs = options.pollMs ?? 250;
  let progress = cursorKey(follower.cursor());
  let deadline = performance.now() + stallMs;
  let waitingSince: number | null = null;
  for (;;) {
    if (follower.isClosed()) throw new Error("Journey replay host is closed");
    if (follower.ready()) {
      try {
        return await read();
      } catch (error) {
        if (!isWatcherReplaySourcePending(error)) throw error;
        waitingSince ??= performance.now();
        if (performance.now() - waitingSince >= stallMs) throw error;
      }
      // A fall back to catching up starts a fresh progress allowance.
      progress = cursorKey(follower.cursor());
      deadline = performance.now() + stallMs;
    } else {
      waitingSince = null;
      const current = cursorKey(follower.cursor());
      if (current !== progress) {
        progress = current;
        deadline = performance.now() + stallMs;
      }
      if (performance.now() >= deadline)
        throw new Error(
          `Journey replay follower is pending: ${JSON.stringify(await follower.pending())}`,
        );
    }
    await pause(pollMs);
  }
};

/** The follower as the warm-up drives it: also the node tip it last heard. */
export type JourneyReplayWarmFollowerView = JourneyReplayFollowerView &
  Readonly<{ tip(): Readonly<{ height: number }> | null }>;

/** Where a warm-up left the follower, by block height. */
export type JourneyReplayWarmth = Readonly<{
  /** The first cursor this process saw, or null if it saw none. */
  fromHeight: number | null;
  cursorHeight: number | null;
  tipHeight: number | null;
}>;

/**
 * Drives the journey's replay follower until it is ready at the node's tip,
 * then runs `advance` once, as a read would (`readWhenJourneyReplayed`), so
 * its only bound is that read's progress rule: it fails once the cursor has
 * not moved for `stallMs`, however long the whole walk takes.
 */
export const warmWhenJourneyReplayed = async (
  follower: JourneyReplayWarmFollowerView,
  advance: () => Promise<void>,
  options: Readonly<{ stallMs?: number; pollMs?: number }> = {},
): Promise<JourneyReplayWarmth> => {
  let fromHeight: number | null = null;
  await readWhenJourneyReplayed(
    {
      ...follower,
      cursor: () => {
        const cursor = follower.cursor();
        fromHeight ??= cursor?.height ?? null;
        return cursor;
      },
    },
    advance,
    options,
  );
  return {
    fromHeight,
    cursorHeight: follower.cursor()?.height ?? null,
    tipHeight: follower.tip()?.height ?? null,
  };
};
