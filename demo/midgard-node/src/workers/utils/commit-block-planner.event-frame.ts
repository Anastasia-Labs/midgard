import { Data, Effect } from "effect";

import type {
  CommitEventCounts,
  CommitEventInstant,
  CommitEventWindow,
} from "./commit-block-planner.event-window.js";

type SelectedWindow = Extract<CommitEventWindow, { endTimeMs: number }>;
type Measurement = Readonly<{
  innerBytesUpperBound: number;
  materialDigest: string;
}>;
export class CommitEventFrameError extends Data.TaggedError(
  "CommitEventFrameError",
)<{ readonly message: string }> {}

/**
 * Rebuilds only complete event prefixes from one base and one commit clock.
 * Withdrawal effects can make frame sizes nonmonotonic, so a binary search
 * cannot prove that a fitting prefix was not skipped. The largest fitting
 * prefix wins. If none of the conservative estimates fit, rebuild the smallest
 * measured candidate for the exact pre-submit check and verify its identity.
 */
export const stepDownCommitEventWindowToDaFrame = <P, E, R>({
  initialWindow,
  groups,
  maxInnerBytes,
  run,
  measure,
  rebase,
}: {
  readonly initialWindow: SelectedWindow;
  readonly groups: readonly CommitEventInstant[];
  readonly maxInnerBytes: number;
  readonly run: (window: SelectedWindow) => Effect.Effect<P, E, R>;
  readonly measure: (built: P) => Measurement | undefined;
  readonly rebase: Effect.Effect<void, E, R>;
}) =>
  Effect.gen(function* () {
    let window = initialWindow;
    let result = yield* run(window);
    let measurement = measure(result);
    let passes = 1;
    if (
      measurement === undefined ||
      measurement.innerBytesUpperBound <= maxInnerBytes
    )
      return { result, window, passes };
    const prefixes: CommitEventCounts[] = [];
    let counts: CommitEventCounts = {
      deposits: 0,
      forcedTransactions: 0,
      withdrawals: 0,
    };
    for (const group of groups.slice(0, initialWindow.includedInstantCount)) {
      counts = {
        deposits: counts.deposits + group.deposits,
        forcedTransactions:
          counts.forcedTransactions + group.forcedTransactions,
        withdrawals: counts.withdrawals + group.withdrawals,
      };
      prefixes.push(counts);
    }
    let best = { window, measurement };
    for (
      let index = initialWindow.includedInstantCount - 1;
      index >= 0;
      index -= 1
    ) {
      const endTimeMs = groups[index]!.inclusionTimeMs;
      if (endTimeMs === initialWindow.endTimeMs) continue;
      window = {
        mode: "catch_up",
        endTimeMs,
        counts: prefixes[index]!,
        includedInstantCount: index + 1,
      };
      yield* rebase;
      result = yield* run(window);
      passes += 1;
      measurement = measure(result);
      if (measurement === undefined)
        return yield* Effect.fail(
          new CommitEventFrameError({
            message: "event prefix has no complete DA measurement",
          }),
        );
      if (measurement.innerBytesUpperBound <= maxInnerBytes)
        return { result, window, passes };
      if (
        measurement.innerBytesUpperBound < best.measurement.innerBytesUpperBound
      )
        best = { window, measurement };
    }
    if (best.window.endTimeMs !== window.endTimeMs) {
      window = best.window;
      yield* rebase;
      result = yield* run(window);
      passes += 1;
      measurement = measure(result);
      if (
        measurement?.innerBytesUpperBound !==
          best.measurement.innerBytesUpperBound ||
        measurement.materialDigest !== best.measurement.materialDigest
      )
        return yield* Effect.fail(
          new CommitEventFrameError({
            message: "event prefix changed while rebuilding",
          }),
        );
    }
    return { result, window, passes };
  });
