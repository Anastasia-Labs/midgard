import type * as SDK from "@al-ft/midgard-sdk";
import { Effect, Schedule } from "effect";

import { DatabaseError } from "@/database/utils/common.js";
import { Database } from "@/services/index.js";

export type UserEventFetchBounds = Pick<
  SDK.UserEventFetchConfig,
  "inclusionTimeLowerBound" | "inclusionTimeUpperBound"
>;

export type UserEventReconcileResult = {
  readonly reconciledCount: number;
  readonly completedAt: Date;
};

export const persistVisibleUserEventUTxOs = <
  Utxo,
  Entry,
  EntryError,
  EntryRequirements,
>({
  visibleUtxos,
  toEntry,
  insertEntries,
  emptyLogMessage,
  foundLogMessage,
}: {
  readonly visibleUtxos: readonly Utxo[];
  readonly toEntry: (
    utxo: Utxo,
  ) => Effect.Effect<Entry, EntryError, EntryRequirements>;
  readonly insertEntries: (
    entries: readonly Entry[],
  ) => Effect.Effect<void, DatabaseError, Database>;
  readonly emptyLogMessage: string;
  readonly foundLogMessage: (count: number) => string;
}): Effect.Effect<
  UserEventReconcileResult,
  EntryError | DatabaseError,
  EntryRequirements | Database
> =>
  Effect.gen(function* () {
    if (visibleUtxos.length <= 0) {
      yield* Effect.logDebug(emptyLogMessage);
      return {
        reconciledCount: 0,
        completedAt: new Date(),
      } as const;
    }

    yield* Effect.logInfo(foundLogMessage(visibleUtxos.length));

    const entries = yield* Effect.forEach(visibleUtxos, toEntry);
    yield* insertEntries(entries);
    return {
      reconciledCount: entries.length,
      completedAt: new Date(),
    } as const;
  });

export const logReconciledVisibleUserEvents = ({
  reconciledCount,
  message,
}: {
  readonly reconciledCount: number;
  readonly message: (count: number) => string;
}): Effect.Effect<void> =>
  reconciledCount <= 0 ? Effect.void : Effect.logInfo(message(reconciledCount));

export const runCommitTimeUserEventIngestionBarrier = <Error, Requirements>({
  inclusionTimeUpperBound,
  inclusionTimeUpperBoundOffsetMs,
  startLogMessage,
  completedLogMessage,
  reconcile,
}: {
  readonly inclusionTimeUpperBound: Date;
  readonly inclusionTimeUpperBoundOffsetMs: number;
  readonly startLogMessage: (inclusionTimeUpperBound: Date) => string;
  readonly completedLogMessage: (input: {
    readonly reconciledCount: number;
    readonly completedAt: Date;
    readonly inclusionTimeUpperBound: Date;
  }) => string;
  readonly reconcile: (
    bounds: UserEventFetchBounds,
  ) => Effect.Effect<UserEventReconcileResult, Error, Requirements>;
}): Effect.Effect<Date, Error, Requirements> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(startLogMessage(inclusionTimeUpperBound));
    const { reconciledCount, completedAt } = yield* reconcile({
      inclusionTimeUpperBound: BigInt(
        inclusionTimeUpperBound.getTime() + inclusionTimeUpperBoundOffsetMs,
      ),
    });
    yield* Effect.logInfo(
      completedLogMessage({
        reconciledCount,
        completedAt,
        inclusionTimeUpperBound,
      }),
    );
    return inclusionTimeUpperBound;
  });

export const repeatVisibleUserEventIngestionFiber = <
  ActionError,
  ActionRequirements,
>({
  schedule,
  startLogMessage,
  spanName,
  action,
}: {
  readonly schedule: Schedule.Schedule<number>;
  readonly startLogMessage: string;
  readonly spanName: string;
  readonly action: Effect.Effect<void, ActionError, ActionRequirements>;
}): Effect.Effect<void, never, ActionRequirements> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(startLogMessage);
    const repeatableAction = action.pipe(
      Effect.withSpan(spanName),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(repeatableAction, schedule);
  });
