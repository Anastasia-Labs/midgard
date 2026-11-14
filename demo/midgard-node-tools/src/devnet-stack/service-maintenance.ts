import {
  eventRecorder,
  type ServiceSpec,
  sleep,
  superviseServices,
  type SupervisorPaths,
  type SupervisorPolicy,
} from "./supervisor.js";

/**
 * Work the supervisor runs in its own process, for its whole life, beside its
 * services: not a child process, so it has no command, liveness URL or
 * restart policy of its own.
 */
export type InProcessMaintainer = {
  readonly name: string;
  /** Runs until `signal` aborts; may reject, which only restarts it. */
  readonly run: (signal: AbortSignal) => Promise<void>;
};
/** How long a maintainer that failed waits before it runs again. */
export const MAINTAINER_RESTART_MS = 60_000;
/** Resolves once `signal` aborts, leaving no listener behind if it already has. */
const abortedOf = (signal: AbortSignal) =>
  new Promise<void>((resolve) => {
    if (signal.aborted) return resolve();
    signal.addEventListener("abort", () => resolve(), { once: true });
  });
/**
 * Runs `maintainer` until `signal` aborts and never rejects. A failure before
 * the abort (an L1 read that timed out, a run record it could not read) is
 * logged when it differs from the last one and the maintainer runs again
 * after `restartMs`; whatever it throws once aborted (its sleeps reject with
 * "stopped") is the shutdown, not a fault. It returns as soon as `signal`
 * aborts, even while the maintainer is inside work that does not watch the
 * signal.
 */
export const keepMaintaining = async (
  maintainer: InProcessMaintainer,
  signal: AbortSignal,
  options: {
    readonly restartMs: number;
    readonly record: (event: Record<string, unknown>) => void;
  },
): Promise<void> => {
  const aborted = abortedOf(signal);
  let last: string | undefined;
  while (!signal.aborted) {
    const attempt = Promise.resolve()
      .then(() => maintainer.run(signal))
      .then(
        () => "returned before the supervisor stopped",
        (error: unknown) =>
          error instanceof Error ? error.message : String(error),
      );
    // `aborted` settles first on the abort (its listener is the oldest), so
    // a message is a failure from before it.
    const message = await Promise.race([attempt, aborted]);
    if (message === undefined) break;
    if (message !== last)
      try {
        options.record({
          event: "maintainer-error",
          maintainer: maintainer.name,
          error: message,
          restartMs: options.restartMs,
        });
      } catch {
        // The event log is evidence; a write that fails never stops the work.
      }
    last = message;
    await sleep(options.restartMs, signal);
  }
};
/**
 * superviseServices with in-process maintainers beside it. A maintainer can
 * never take the services down: each runs under keepMaintaining, which never
 * rejects, so only the services decide how this ends. When they end (on
 * `signal`, or a fault of the supervisor itself) every maintainer is stopped
 * and this returns or rethrows at once, so a maintainer never keeps a
 * supervisor whose services are gone.
 */
export const superviseWithMaintainers = async (
  services: readonly ServiceSpec[],
  paths: SupervisorPaths,
  signal: AbortSignal,
  maintainers: readonly InProcessMaintainer[],
  options: {
    readonly policy?: SupervisorPolicy;
    readonly restartMs?: number;
  } = {},
): Promise<void> => {
  const stop = new AbortController();
  const forward = () => stop.abort();
  if (signal.aborted) stop.abort();
  else signal.addEventListener("abort", forward, { once: true });
  const record = eventRecorder(paths);
  const kept = maintainers.map((maintainer) =>
    keepMaintaining(maintainer, stop.signal, {
      restartMs: options.restartMs ?? MAINTAINER_RESTART_MS,
      record,
    }),
  );
  try {
    await superviseServices(services, paths, signal, options.policy);
  } finally {
    signal.removeEventListener("abort", forward);
    stop.abort();
    await Promise.all(kept);
  }
};
