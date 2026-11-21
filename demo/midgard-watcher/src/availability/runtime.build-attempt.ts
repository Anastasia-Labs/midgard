import type { buildWatcherAvailabilityOperation } from "./runtime.build-operation.js";
import type { watcherAvailabilityExecutionLimits } from "./runtime.execution-limits.js";
import { buildWatcherAvailabilityAttempt } from "./runtime.protocol-parameter-refresh.js";
import type { createWatcherAvailabilityReadAttempt } from "./runtime.read-attempt.js";

type Operation = Awaited<ReturnType<typeof buildWatcherAvailabilityOperation>>;

/** Initial selection and every rebuild share the same isolated attempt. */
export const watcherAvailabilityAttemptOperation = (input: {
  attempt: ReturnType<typeof createWatcherAvailabilityReadAttempt>;
  operation: Operation;
  execution: ReturnType<typeof watcherAvailabilityExecutionLimits>;
  assertCurrent(): void;
  reselect(): Promise<Operation>;
}) => ({
  ...input.operation,
  preparationScope: input.attempt.scope,
  ...(input.attempt.scope.deadlineEpochMs === undefined
    ? {}
    : { unsignedDeadlineMs: input.attempt.scope.deadlineEpochMs }),
  build: async () => {
    const lucid = await input.attempt.lucid();
    const built = await buildWatcherAvailabilityAttempt({
      lucid,
      scope: input.attempt.scope,
      assertCurrent: input.assertCurrent,
      build: async () => {
        const selected = await input.reselect();
        if (
          selected.action !== input.operation.action ||
          selected.completesWorkflow !== input.operation.completesWorkflow
        )
          throw new Error("Refreshed availability transition changed");
        return await selected.build();
      },
    });
    input.execution.capture(lucid, input.attempt.scope);
    return built;
  },
});
