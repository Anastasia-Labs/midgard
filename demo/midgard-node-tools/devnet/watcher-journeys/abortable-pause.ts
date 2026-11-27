import { setTimeout as pause } from "node:timers/promises";

/**
 * Pauses `ms`, ending early once `signal` aborts. Before the pause, and when
 * the abort ends it, it throws the abort's reason, so a wait that pauses
 * here stops at its next pause with what stopped it.
 */
export const pauseUnlessAborted = async (
  ms: number,
  signal: AbortSignal,
): Promise<void> => {
  signal.throwIfAborted();
  try {
    await pause(ms, undefined, { signal });
  } catch (cause) {
    signal.throwIfAborted();
    throw cause;
  }
};
