/**
 * The committee's one exit after startup (owner ruling 2026-10-09): a
 * transient failure that outlived its bound. The store's instance lock that
 * could not be taken again past its reacquire budget, and the L1 follower
 * whose transient store failures outlived its budget (`exhausted`), each
 * report here; the process logs one named line, shuts down what it can
 * within a short deadline, and exits non-zero, its supervisor's restart
 * being the backoff. Every other post-start failure keeps the process up and
 * unready under a named reason.
 */

/** The log event of a transient failure that outlived its bound. */
export const COMMITTEE_TRANSIENT_BUDGET_EXHAUSTED =
  "committee_transient_budget_exhausted";

/** How long the shutdown may take before the process exits anyway. */
export const EXHAUSTED_SHUTDOWN_DEADLINE_MS = 10_000;

export type TransientExhaustion = Readonly<{
  /** What ran out: `store_instance_lock` or `l1_follower`. */
  source: string;
  /** Its named reason. */
  reason: string;
  detail: string;
}>;

/**
 * Exits the process non-zero on the first exhaustion reported; later ones
 * are ignored. `shutdown` is read when the exhaustion is reported, so it is
 * whatever the process had built by then.
 */
export const transientExhaustionExit = (deps: {
  readonly write: (line: string) => void;
  readonly shutdown: () => (() => Promise<void>) | undefined;
  readonly exit: (code: number) => void;
  readonly deadlineMs?: number;
}): ((exhaustion: TransientExhaustion) => void) => {
  let reported = false;
  return (exhaustion) => {
    if (reported) return;
    reported = true;
    deps.write(
      `${JSON.stringify({ event: COMMITTEE_TRANSIENT_BUDGET_EXHAUSTED, ...exhaustion, outcome: "a transient failure outlived its bound; the process exits non-zero" })}\n`,
    );
    const shutdown = deps.shutdown();
    let deadline: ReturnType<typeof setTimeout> | undefined;
    void Promise.race([
      Promise.resolve()
        .then(() => shutdown?.())
        .catch(() => undefined),
      new Promise<void>((resolve) => {
        deadline = setTimeout(
          resolve,
          deps.deadlineMs ?? EXHAUSTED_SHUTDOWN_DEADLINE_MS,
        );
      }),
    ]).then(() => {
      clearTimeout(deadline);
      deps.exit(1);
    });
  };
};
