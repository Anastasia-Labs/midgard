import { Duration, Effect } from "effect";

import {
  itemRowCount,
  sliceItem,
  type WriteBehindItem,
} from "./write-behind.summarize-write-behind-telemetry.js";

/** Selects up to the row cap for each target table in one transaction. */
export const takeWriteBehindProjectionBatch = (
  items: readonly WriteBehindItem[],
  maxRowsPerProjection: number,
): {
  readonly batch: readonly WriteBehindItem[];
  readonly remaining: readonly WriteBehindItem[];
} => {
  let deltaCapacity = Math.max(1, maxRowsPerProjection);
  let addressCapacity = Math.max(1, maxRowsPerProjection);
  const batch: WriteBehindItem[] = [];
  const remaining: WriteBehindItem[] = [];
  for (const item of items) {
    const capacity =
      item.kind === "tx_deltas" ? deltaCapacity : addressCapacity;
    if (capacity === 0) {
      remaining.push(item);
      continue;
    }
    const rows = itemRowCount(item);
    const selectedRows = Math.min(rows, capacity);
    batch.push(sliceItem(item, 0, selectedRows));
    if (selectedRows < rows) {
      remaining.push(sliceItem(item, selectedRows));
    }
    if (item.kind === "tx_deltas") {
      deltaCapacity -= selectedRows;
    } else {
      addressCapacity -= selectedRows;
    }
  }
  return { batch, remaining };
};

/**
 * Keeps queue-overflow rows owned by the producer until their inline write
 * succeeds. The iterative retry is deliberate: returning the persistence
 * error would strand derived rows after the authoritative accept transaction
 * has already committed, while recursive retries could grow the JS stack.
 */
export const persistWriteBehindInlineOverflowWithRetry = <E, R>(
  persist: Effect.Effect<void, E, R>,
  retryDelayMs: number,
): Effect.Effect<void, never, R> =>
  Effect.gen(function* () {
    const delayMs = Math.max(1, Math.floor(retryDelayMs));
    let attempt = 0;
    while (true) {
      const result = yield* Effect.either(persist);
      if (result._tag === "Right") return;
      attempt += 1;
      if (attempt === 1 || attempt % 10 === 0) {
        yield* Effect.logWarning(
          `Write-behind inline overflow persistence failed; retaining producer backpressure and retrying (attempt=${attempt.toString()}): ${String(result.left)}`,
        );
      }
      yield* Effect.sleep(Duration.millis(delayMs));
    }
  });
