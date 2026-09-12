export type WatcherStartupProgress = Readonly<{
  stage: string;
  outcome: "started" | "pending" | "completed" | "failed";
  elapsedMs: number;
  observedAt: string;
  error?: string;
}>;

/** Startup diagnostics remain available before the operations server binds. */
export const createWatcherStartupProgress =
  (report: ((progress: WatcherStartupProgress) => void) | undefined) =>
  async <T>(stage: string, action: () => Promise<T>): Promise<T> => {
    if (report === undefined) return await action();
    const startedAt = performance.now();
    const emit = (
      outcome: WatcherStartupProgress["outcome"],
      error?: unknown,
    ) =>
      report({
        stage,
        outcome,
        elapsedMs: performance.now() - startedAt,
        observedAt: new Date().toISOString(),
        ...(outcome === "failed"
          ? { error: error instanceof Error ? error.message : String(error) }
          : {}),
      });
    emit("started");
    const timer = setInterval(() => emit("pending"), 30_000);
    timer.unref();
    try {
      const result = await action();
      emit("completed");
      return result;
    } catch (error) {
      emit("failed", error);
      throw error;
    } finally {
      clearInterval(timer);
    }
  };
