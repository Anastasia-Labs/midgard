import { Effect, Exit, Metric, Ref, Schedule } from "effect";
import { describe, expect, it } from "vitest";

import {
  NATIVE_MPF_OWNER_RECOVERY_PENDING,
  NATIVE_MPF_OWNER_RESTART_EXHAUSTED,
  NATIVE_MPF_OWNER_SUPERVISOR_SOURCE,
  nativeMpfOwnerRestartsInWindowGauge,
  nativeMpfOwnerSupervisorFiber,
} from "../src/fibers/native-mpf-owner-supervisor.js";
import { Globals } from "../src/services/globals.js";
import type { NativeMpfOwnerService } from "../src/services/mpf-native-owner/protocol.js";
import type { NativeOwnerRestartHealth } from "../src/services/mpf-native-owner/service.restart-policy.js";

const health = (
  overrides: Partial<NativeOwnerRestartHealth> = {},
): NativeOwnerRestartHealth => ({
  restartsInWindow: 0,
  failedRestartsInWindow: 0,
  restartLimit: 3,
  restartWindowMs: 3_600_000,
  exhausted: false,
  ...overrides,
});

const owner = (
  terminalFailure: () => Error | undefined,
  restartHealth: () => NativeOwnerRestartHealth = () => health(),
) => ({ terminalFailure, restartHealth }) as unknown as NativeMpfOwnerService;

/** One tick per owner; the schedule installs the next owner between ticks, as
 * a recovery flow replaces the live one. Returns the exit, the reason raised
 * after each tick (read where the schedule steps, as it ends too) and the
 * restart gauge after the last one. */
const supervise = (owners: readonly (NativeMpfOwnerService | undefined)[]) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const globals = yield* Globals;
      const raised: (string | undefined)[] = [];
      const reason = Effect.map(Ref.get(globals.LIVENESS_REASONS), (reasons) =>
        reasons.get(NATIVE_MPF_OWNER_SUPERVISOR_SOURCE),
      );
      yield* Ref.set(globals.NATIVE_MPF_OWNER, owners[0]);
      const exit = yield* Effect.exit(
        nativeMpfOwnerSupervisorFiber(
          Schedule.recurs(owners.length - 1).pipe(
            Schedule.tapOutput((tick) =>
              Effect.zipRight(
                Effect.flatMap(reason, (current) =>
                  Effect.sync(() => raised.push(current)),
                ),
                Ref.set(globals.NATIVE_MPF_OWNER, owners[tick + 1]),
              ),
            ),
          ),
        ),
      );
      const gauge = (yield* Metric.value(nativeMpfOwnerRestartsInWindowGauge))
        .value;
      return { exit, raised, gauge };
    }).pipe(Effect.provide(Globals.Default)),
  );

describe("native MPF owner supervisor", () => {
  it("raises nothing while the live owner, or none, is healthy", async () => {
    let checks = 0;
    const healthy = () => {
      checks += 1;
      return undefined;
    };
    const { exit, raised } = await supervise([
      undefined,
      owner(healthy),
      owner(healthy),
    ]);
    expect(Exit.isSuccess(exit)).toBe(true);
    expect(checks).toBe(2);
    expect(raised).toEqual([undefined, undefined, undefined]);
  });

  it("surfaces an owner's refusal instead of stopping the node, and clears it once the owner serves again", async () => {
    const exhausted = new Error("Native MPF owner restart limit exhausted");
    const pending = new Error("Native MPF canonical recovery is not installed");
    const { exit, raised } = await supervise([
      owner(() => undefined),
      owner(
        () => exhausted,
        () => health({ exhausted: true, failedRestartsInWindow: 3 }),
      ),
      owner(() => pending),
      owner(() => undefined),
      undefined,
    ]);
    expect(Exit.isSuccess(exit)).toBe(true);
    expect(raised).toEqual([
      undefined,
      NATIVE_MPF_OWNER_RESTART_EXHAUSTED,
      NATIVE_MPF_OWNER_RECOVERY_PENDING,
      undefined,
      undefined,
    ]);
  });

  it("publishes the live owner's restarts inside its window", async () => {
    const { gauge } = await supervise([
      owner(
        () => undefined,
        () => health({ restartsInWindow: 5 }),
      ),
    ]);
    expect(gauge).toBe(5);
  });
});
