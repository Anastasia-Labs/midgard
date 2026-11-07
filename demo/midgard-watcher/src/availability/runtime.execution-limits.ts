import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

/** Signed validation reads the parameters of the successful unsigned build,
 * rather than a mutable Lucid instance that may later finish retired work. */
export const watcherAvailabilityExecutionLimits = (
  context: SDK.DaAvailabilityOperationContext,
  parameters: SDK.DaAvailabilityParameters,
  assertCurrent: () => void,
) => {
  let limits = context.transactionLimits;
  return {
    context: {
      ...context,
      get transactionLimits() {
        return limits;
      },
      observe: async (...args: Parameters<typeof context.observe>) => {
        const observed = await context.observe(...args);
        // Construction validates its winning snapshot. A later signed replay
        // validates the fresh snapshot captured by its canonical observer.
        limits = context.transactionLimits;
        return observed;
      },
    },
    capture: (
      lucid: LucidEvolution,
      scope: SDK.DaAvailabilityReadScope,
    ): void => {
      assertCurrent();
      scope.assertCurrent();
      const captured = SDK.daAvailabilityOperationLimits(lucid, parameters);
      assertCurrent();
      scope.assertCurrent();
      limits = captured;
    },
  };
};
