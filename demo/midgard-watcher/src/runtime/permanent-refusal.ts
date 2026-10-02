/**
 * Exit code for a refusal that no restart can clear: the configuration or the
 * verified deployment identity does not match what this release accepts. It is
 * sysexits' EX_CONFIG, so a supervisor can stop restarting the watcher on it
 * while it keeps restarting on exit code 70.
 */
export const WATCHER_PERMANENT_REFUSAL_EXIT_CODE = 78;

/** A configuration or deployment-identity refusal made by comparing bytes. */
export class WatcherPermanentRefusalError extends Error {
  override readonly name = "WatcherPermanentRefusalError";

  constructor(stage: string, cause: unknown) {
    super(
      `watcher ${stage} refused: ${cause instanceof Error ? cause.message : String(cause)}`,
      { cause },
    );
  }
}

const hasSystemErrorCode = (error: unknown): boolean => {
  let current = error;
  for (let depth = 0; depth < 8 && current instanceof Error; depth += 1) {
    const code = (current as { code?: unknown }).code;
    if (typeof code === "string" && /^E[A-Z0-9]+$/.test(code)) return true;
    current = current.cause;
  }
  return false;
};

/**
 * Runs a stage whose refusals are permanent. A file-system or other system
 * error (ENOENT, EIO, EMFILE, ...) is left as it is: the file may not be
 * written yet, so a restart may clear it.
 */
export const refusePermanently = async <T>(
  stage: string,
  action: () => T | Promise<T>,
): Promise<T> => {
  try {
    return await action();
  } catch (error) {
    throw hasSystemErrorCode(error)
      ? error
      : new WatcherPermanentRefusalError(stage, error);
  }
};

/** True when a permanent refusal is anywhere on the error's cause chain. */
export const isWatcherPermanentRefusal = (error: unknown): boolean => {
  let current = error;
  for (let depth = 0; depth < 8 && current instanceof Error; depth += 1) {
    if (current instanceof WatcherPermanentRefusalError) return true;
    current = current.cause;
  }
  return false;
};
