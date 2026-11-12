/**
 * The exit deadline every role shares (owner ruling 2026-10-09, plan
 * §7.5): once a process has decided to exit non-zero, its shutdown has
 * `ROLE_EXIT_DEADLINE_MS` to finish, and past that the process exits
 * anyway, so a hung finalizer cannot keep a failed process up.
 */

/** How long a shutdown on the way to an exit may take. */
export const ROLE_EXIT_DEADLINE_MS = 10_000;

/**
 * Arms the deadline: `exit(code)` runs `deadlineMs` from now unless the
 * returned disarm runs first. Disarm says whether it stopped the deadline
 * (false once it has fired), so the caller exits only once. The timer never
 * keeps the process up by itself.
 */
export const armExitDeadline = (input: {
  readonly exit: (code: number) => void;
  readonly code: number;
  readonly deadlineMs?: number;
}): (() => boolean) => {
  let fired = false;
  const timer = setTimeout(() => {
    fired = true;
    input.exit(input.code);
  }, input.deadlineMs ?? ROLE_EXIT_DEADLINE_MS);
  timer.unref();
  return () => {
    clearTimeout(timer);
    return !fired;
  };
};
