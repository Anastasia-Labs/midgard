import { Duration, Effect, Option, Ref } from "effect";

import { Globals } from "./globals.globals.js";
import { runRegisteredL1ControlPlaneHold } from "./globals.l1-control-plane.js";
import type { MempoolLedgerStale } from "./globals.next-l1-provider-health-evidence.js";

/** Runs `effect` only when the permit is free, as a registered hold: the
 * wedge and hold-timeout readiness reasons see it like any other scope. */
export const withL1ControlPlaneIfAvailable = <A, E, R>(
  globals: Globals,
  options: {
    readonly scope: string;
    readonly maxHoldMs?: number;
  },
  effect: Effect.Effect<A, E, R>,
) =>
  Effect.suspend(() => {
    const waitStartedAtMs = Date.now();
    return globals.L1_CONTROL_PLANE.withPermitsIfAvailable(1)(
      runRegisteredL1ControlPlaneHold(
        globals,
        options,
        waitStartedAtMs,
        effect,
      ),
    );
  });

/** Waits at most `waitTimeoutMs` for the permit, then holds it as a
 * registered hold: the wedge and hold-timeout readiness reasons see it. */
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
      return yield* restore(
        runRegisteredL1ControlPlaneHold(
          globals,
          options,
          waitStartedAtMs,
          effect,
        ),
      ).pipe(
        Effect.map(Option.some),
        Effect.ensuring(globals.L1_CONTROL_PLANE.release(1)),
      );
    }),
  );

/**
 * Marks `mempool_ledger` rows the validation cache must read again: the hex
 * outrefs `keys`, or every row (`"all"`). Called once the write committed.
 */
export const markMempoolLedgerStale = (
  globals: Globals,
  keys: Iterable<string> | "all",
): Effect.Effect<void> =>
  Ref.update(
    globals.MEMPOOL_LEDGER_STALE,
    (stale): MempoolLedgerStale =>
      keys === "all"
        ? { all: true, keys: stale.keys }
        : { all: stale.all, keys: new Set([...stale.keys, ...keys]) },
  );
