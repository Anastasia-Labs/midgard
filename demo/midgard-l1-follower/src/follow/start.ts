import type { FactStore, StartResult } from "../store/fact-store.js";
import type { StoreLocked } from "../types.js";

const sleep = (ms: number, signal?: AbortSignal): Promise<void> =>
  new Promise((resolve) => {
    const timer = setTimeout(resolve, ms);
    signal?.addEventListener(
      "abort",
      () => {
        clearTimeout(timer);
        resolve();
      },
      { once: true },
    );
  });

/**
 * Starts the store, waiting out `store_locked` (another process holds the
 * writer lease) with backoff instead of exiting. Returns the first other
 * result (`ready`, or an intervention), or `undefined` once `signal` aborts.
 * `onLocked` hears each `store_locked` before the wait.
 */
export const startWhenFree = async (
  store: FactStore,
  options: Readonly<{
    signal?: AbortSignal;
    backoffMs?: Readonly<{ initial: number; max: number }>;
    log?: (line: string) => void;
    onLocked?: (locked: StoreLocked) => void | Promise<void>;
  }> = {},
): Promise<Exclude<StartResult, StoreLocked> | undefined> => {
  const backoff = options.backoffMs ?? { initial: 500, max: 30_000 };
  for (
    let delay = backoff.initial;
    ;
    delay = Math.min(delay * 2, backoff.max)
  ) {
    if (options.signal?.aborted === true) return undefined;
    const started = await store.start();
    if (started.kind !== "store_locked") return started;
    options.log?.(
      `store locked, starting again in ${delay} ms: ${started.detail}`,
    );
    await options.onLocked?.(started);
    await sleep(delay, options.signal);
  }
};
