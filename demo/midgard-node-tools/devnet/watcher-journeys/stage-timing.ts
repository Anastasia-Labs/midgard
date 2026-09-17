import { appendFile } from "node:fs/promises";
import { join } from "node:path";
import { inspect } from "node:util";

/** Preserve duration and outcome even when a stage fails before HTTP diagnostics exist. */
export const measureJourneyStage = async <T>(
  directory: string,
  name: string,
  action: () => Promise<T>,
): Promise<T> => {
  const startedAt = new Date().toISOString();
  const start = performance.now();
  console.info(`Live watcher journey: ${name}`);
  const record = async (outcome: "completed" | "failed", error?: unknown) => {
    const seconds = (performance.now() - start) / 1000;
    const entry = {
      stage: name,
      outcome,
      startedAt,
      completedAt: new Date().toISOString(),
      seconds,
      ...(error === undefined
        ? {}
        : {
            error:
              error instanceof Error
                ? { message: error.message, stack: error.stack }
                : inspect(error),
          }),
    };
    await appendFile(
      join(directory, "timings.ndjson"),
      `${JSON.stringify(entry)}\n`,
    );
    console.info(
      `Live watcher journey: ${name} ${outcome} in ${seconds.toFixed(1)}s`,
    );
  };
  let value: T;
  try {
    value = await action();
  } catch (cause) {
    try {
      await record("failed", cause);
    } catch (recordFailure) {
      throw new AggregateError(
        [cause, recordFailure],
        "Journey stage failed and its timing could not be recorded",
      );
    }
    throw cause;
  }
  await record("completed");
  return value;
};
