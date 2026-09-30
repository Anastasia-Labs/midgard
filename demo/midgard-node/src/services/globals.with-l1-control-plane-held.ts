import { Duration, Effect, Metric, Option, Ref } from "effect";

import { Globals } from "./globals.globals.js";
import {
  DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
  l1ControlPlaneAcquisitionCounter,
  l1ControlPlaneHoldTimer,
  l1ControlPlaneTimeoutCounter,
  L1ControlPlaneTimeoutError,
  l1ControlPlaneWaitTimer,
  type MempoolLedgerDelta,
} from "./globals.next-l1-provider-health-evidence.js";

export const withL1ControlPlaneIfAvailable = <A, E, R>(
  globals: Globals,
  options: {
    readonly scope: string;
    readonly maxHoldMs?: number;
  },
  effect: Effect.Effect<A, E, R>,
) =>
  globals.L1_CONTROL_PLANE.withPermitsIfAvailable(1)(
    withL1ControlPlaneHeld(options, effect),
  );

const withL1ControlPlaneHeld = <A, E, R>(
  options: {
    readonly scope: string;
    readonly maxHoldMs?: number;
  },
  effect: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | Error, R> => {
  const maxHoldMs = options.maxHoldMs ?? DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS;
  const holdTimer = Metric.tagged(
    l1ControlPlaneHoldTimer,
    "scope",
    options.scope,
  );
  const acquisitionCounter = Metric.tagged(
    l1ControlPlaneAcquisitionCounter,
    "scope",
    options.scope,
  );
  const timeoutCounter = Metric.tagged(
    l1ControlPlaneTimeoutCounter,
    "scope",
    options.scope,
  );
  return Effect.gen(function* () {
    yield* Metric.increment(acquisitionCounter);
    const holdStartedAtMs = Date.now();
    return yield* effect.pipe(
      Effect.timeoutFail({
        duration: Duration.millis(maxHoldMs),
        onTimeout: () =>
          new L1ControlPlaneTimeoutError(options.scope, maxHoldMs),
      }),
      Effect.tapError((error) =>
        error instanceof L1ControlPlaneTimeoutError
          ? Metric.increment(timeoutCounter)
          : Effect.void,
      ),
      Effect.ensuring(
        holdTimer(
          Effect.succeed(Duration.millis(Date.now() - holdStartedAtMs)),
        ),
      ),
    );
  });
};

export const withL1ControlPlaneWaitTimeout = <A, E, R>(
  globals: Globals,
  options: {
    readonly scope: string;
    readonly waitTimeoutMs: number;
    readonly maxHoldMs?: number;
  },
  effect: Effect.Effect<A, E, R>,
): Effect.Effect<Option.Option<A>, E | Error, R> =>
  Effect.uninterruptibleMask((restore) =>
    Effect.gen(function* () {
      const waitStartedAtMs = Date.now();
      const acquired = yield* restore(
        globals.L1_CONTROL_PLANE.take(1).pipe(
          Effect.timeoutOption(Duration.millis(options.waitTimeoutMs)),
        ),
      );
      if (Option.isNone(acquired)) {
        return Option.none<A>();
      }
      yield* Metric.tagged(
        l1ControlPlaneWaitTimer,
        "scope",
        options.scope,
      )(Effect.succeed(Duration.millis(Date.now() - waitStartedAtMs)));
      return yield* restore(withL1ControlPlaneHeld(options, effect)).pipe(
        Effect.map(Option.some),
        Effect.ensuring(globals.L1_CONTROL_PLANE.release(1)),
      );
    }),
  );

export const publishMempoolLedgerDelta = (
  globals: Globals,
  delta: Omit<MempoolLedgerDelta, "version">,
  maxEntries: number,
): Effect.Effect<number> =>
  Ref.modify(globals.MEMPOOL_LEDGER_DELTA_LOG, (state) => {
    const version = state.version + 1;
    const entry: MempoolLedgerDelta = {
      version,
      full: delta.full,
      upserts: delta.upserts.map(([outRefHex, output]) => [
        outRefHex,
        Buffer.from(output),
      ]),
      deletes: [...delta.deletes],
    };
    return [
      version,
      {
        version,
        entries: [...state.entries, entry].slice(-Math.max(1, maxEntries)),
      },
    ];
  });
