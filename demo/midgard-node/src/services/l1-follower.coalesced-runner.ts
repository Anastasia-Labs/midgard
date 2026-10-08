import {
  type DriverHold,
  EVENTS_INGESTION_FAILED,
} from "../l1-events/driver.js";

/** Retry delays for a held driver: capped exponential. */
const RETRY_INITIAL_MS = 500;
const RETRY_MAX_MS = 30_000;

/**
 * Runs `run` once per trigger, coalesced: at most one run at a time and one
 * queued; a run that ends with holds is retried on a capped backoff until a
 * run clears them or `signal` aborts.
 */
export const coalescedRunner = (
  run: () => Promise<readonly DriverHold[]>,
  signal: AbortSignal,
): (() => void) => {
  let running = false;
  let queued = false;
  let retryMs = RETRY_INITIAL_MS;
  let timer: ReturnType<typeof setTimeout> | undefined;
  const trigger = (): void => {
    if (signal.aborted) return;
    if (timer !== undefined) {
      clearTimeout(timer);
      timer = undefined;
    }
    if (running) {
      queued = true;
      return;
    }
    running = true;
    void run()
      .catch(() => [{ reason: EVENTS_INGESTION_FAILED, detail: "driver run" }])
      .then((holds) => {
        running = false;
        if (queued) {
          queued = false;
          trigger();
          return;
        }
        if (holds.length === 0 || signal.aborted) {
          retryMs = RETRY_INITIAL_MS;
          return;
        }
        timer = setTimeout(trigger, retryMs);
        timer.unref?.();
        retryMs = Math.min(retryMs * 2, RETRY_MAX_MS);
      });
  };
  signal.addEventListener("abort", () => {
    if (timer !== undefined) clearTimeout(timer);
  });
  return trigger;
};
