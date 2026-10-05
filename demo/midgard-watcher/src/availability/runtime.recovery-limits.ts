import type { AvailabilityOperationIntent } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import type { createWatcherAvailabilityReadAttempt } from "./runtime.read-attempt.js";

/** A signed intent gets current limits from its own bounded canonical read,
 * including after restart. No unsigned attempt or mutable base provider owns it. */
export const watcherAvailabilityRecoveryLimits = (input: {
  initial: SDK.DaAvailabilityOperationLimits;
  parameters: SDK.DaAvailabilityParameters;
  observation: WatcherAuthenticatedStateQueueObservation;
  attempt(
    scope?: SDK.DaAvailabilityReadScope,
  ): ReturnType<typeof createWatcherAvailabilityReadAttempt>;
  assertCurrent(): void;
}) => {
  let limits = input.initial;
  let submission:
    | Readonly<{ scope: SDK.DaAvailabilityReadScope; signedCbor: string }>
    | undefined;
  return {
    current: () => limits,
    observe: async (
      intent: AvailabilityOperationIntent,
      scope?: SDK.DaAvailabilityReadScope,
    ): Promise<SDK.DaAvailabilityOperationObservation> => {
      submission = undefined;
      const reader = input.attempt(scope);
      const read = () =>
        reader.read(() => reader.intake.operation(input.observation, intent));
      const observed = await read();
      // Unknown inclusion/input evidence cannot grant submit authority. Positive
      // TTL expiry remains the SDK's independent canonical retirement decision.
      if (
        observed.status !== "unspent" ||
        observed.currentSlot >= intent.validUntilSlot
      )
        return observed;
      const lucid = await reader.lucid();
      input.assertCurrent();
      reader.scope.assertCurrent();
      const captured = SDK.daAvailabilityOperationLimits(
        lucid,
        input.parameters,
      );
      // Re-pin/re-authenticate the same canonical point after parameter I/O.
      // Losing evidence or native ownership here must not publish cached success.
      const current = await read();
      input.assertCurrent();
      reader.scope.assertCurrent();
      if (
        current.status === "unspent" &&
        current.currentSlot < intent.validUntilSlot
      ) {
        limits = captured;
        submission = { scope: reader.scope, signedCbor: intent.signedCbor };
      }
      return current;
    },
    assertSubmission: (signedCbor: string): void => {
      input.assertCurrent();
      if (submission === undefined || submission.signedCbor !== signedCbor)
        throw new Error(
          "Availability rebroadcast lacks current canonical read authority",
        );
      submission.scope.assertCurrent();
    },
  };
};

/** Wire only signed recovery around the caller's existing authority ports.
 * The thunk preserves initial limits/state creation before context construction. */
export const watcherAvailabilityRecoveryContext = (
  input: Omit<
    Parameters<typeof watcherAvailabilityRecoveryLimits>[0],
    "initial"
  > & {
    lucid: LucidEvolution;
  },
) => {
  const recovery = watcherAvailabilityRecoveryLimits({
    initial: SDK.daAvailabilityOperationLimits(input.lucid, input.parameters),
    parameters: input.parameters,
    observation: input.observation,
    attempt: input.attempt,
    assertCurrent: input.assertCurrent,
  });
  return (
    ports: () => Omit<
      SDK.DaAvailabilityOperationContext,
      "transactionLimits" | "observe"
    >,
  ): SDK.DaAvailabilityOperationContext => {
    const context = ports();
    return {
      ...context,
      get transactionLimits() {
        return recovery.current();
      },
      observe: recovery.observe,
      submit: (cbor) => {
        recovery.assertSubmission(cbor);
        return context.submit(cbor);
      },
    };
  };
};
