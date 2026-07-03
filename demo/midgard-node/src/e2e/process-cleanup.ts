export type ChildProcessCleanupResult = {
  readonly attempted: boolean;
  readonly pid: number | null;
  readonly target: "process_group" | "process" | "none";
  readonly signal: NodeJS.Signals;
  readonly success: boolean;
  readonly error: string | null;
};

const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

export const shouldSpawnDetachedProcessGroup = (): boolean =>
  process.platform !== "win32";

export const terminateChildProcessGroup = ({
  pid,
  signal = "SIGTERM",
}: {
  readonly pid: number | null;
  readonly signal?: NodeJS.Signals;
}): ChildProcessCleanupResult => {
  if (pid === null) {
    return {
      attempted: false,
      pid,
      target: "none",
      signal,
      success: false,
      error: "child pid is unavailable",
    };
  }
  if (shouldSpawnDetachedProcessGroup()) {
    try {
      process.kill(-pid, signal);
      return {
        attempted: true,
        pid,
        target: "process_group",
        signal,
        success: true,
        error: null,
      };
    } catch (groupError) {
      try {
        process.kill(pid, signal);
        return {
          attempted: true,
          pid,
          target: "process",
          signal,
          success: true,
          error: `process group kill failed: ${errorMessage(groupError)}`,
        };
      } catch (processError) {
        return {
          attempted: true,
          pid,
          target: "process_group",
          signal,
          success: false,
          error: `process group kill failed: ${errorMessage(
            groupError,
          )}; direct child kill failed: ${errorMessage(processError)}`,
        };
      }
    }
  }
  try {
    process.kill(pid, signal);
    return {
      attempted: true,
      pid,
      target: "process",
      signal,
      success: true,
      error: null,
    };
  } catch (error) {
    return {
      attempted: true,
      pid,
      target: "process",
      signal,
      success: false,
      error: errorMessage(error),
    };
  }
};
