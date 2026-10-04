import type { ChildProcessWithoutNullStreams } from "node:child_process";

/** Install before consuming output: ChildProcess close joins exit and stdio. */
export const watcherNativeChildDrain = (
  child: ChildProcessWithoutNullStreams,
) => {
  const closed = new Promise<void>((resolve) =>
    child.once("close", () => resolve()),
  );
  let termination: Promise<void> | undefined;
  return {
    closed,
    terminate: (): Promise<void> => {
      termination ??= (async () => {
        if (!child.killed) child.kill("SIGTERM");
        const timer = setTimeout(() => child.kill("SIGKILL"), 5_000);
        try {
          await closed;
        } finally {
          clearTimeout(timer);
        }
      })();
      return termination;
    },
  };
};
