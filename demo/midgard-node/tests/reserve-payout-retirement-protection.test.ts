import { SUBMIT_SLOT_LENGTH_MS } from "@al-ft/midgard-core/ogmios-slot";
import * as SDK from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import { Clock, Duration, Effect, Fiber, TestClock } from "effect";
import { describe, expect } from "vitest";

import { retryAfterRetirementProtection } from "../src/commands/reserve-payout.js";

const PROTECTION_DURATION_MS = 120_000n;

const protectedFailure = (protectedUntilMs: bigint) =>
  new SDK.ReservePayoutTxError({
    message: "Failed to resolve authenticated retirement state",
    cause: new SDK.HistoryRetirementProtectedError(
      protectedUntilMs,
      0,
      PROTECTION_DURATION_MS,
    ),
  });

/** A retirement build whose attempts fail or succeed in the given order. */
const scripted = (
  outcomes: readonly (SDK.ReservePayoutTxError | Error | "tx")[],
) => {
  let calls = 0;
  const attempt = Effect.suspend(() => {
    const outcome = outcomes[calls++]!;
    return outcome === "tx"
      ? Effect.succeed("tx")
      : Effect.fail(outcome as SDK.ReservePayoutTxError);
  });
  return { attempt, calls: () => calls };
};

/** Protection `aheadMs` past the test clock is awaited for exactly one slot
 * beyond it, and only then rebuilt. */
const waitsThenRebuilds = (aheadMs: bigint) =>
  Effect.gen(function* () {
    const until = BigInt(yield* Clock.currentTimeMillis) + aheadMs;
    const script = scripted([protectedFailure(until), "tx"]);
    const retirement = yield* Effect.fork(
      retryAfterRetirementProtection(script.attempt),
    );
    yield* Effect.yieldNow();
    yield* TestClock.adjust(
      Duration.millis(Number(aheadMs) + SUBMIT_SLOT_LENGTH_MS - 1),
    );
    expect(script.calls()).toBe(1);
    expect((yield* Fiber.poll(retirement))._tag).toBe("None");
    yield* TestClock.adjust("1 millis");
    expect(yield* Fiber.join(retirement)).toBe("tx");
    expect(script.calls()).toBe(2);
  });

describe("retirement protection wait", () => {
  // Right after another retirement, an honest bound sits past a full validity
  // window by part of the list's protection duration.
  it.effect(
    "waits past a bound inside the honest window plus duration, then rebuilds",
    () =>
      waitsThenRebuilds(
        SDK.MAX_VALIDITY_RANGE_LENGTH_MS + PROTECTION_DURATION_MS / 2n,
      ),
  );

  it.effect("waits past a bound exactly at the honest limit", () =>
    waitsThenRebuilds(
      SDK.MAX_VALIDITY_RANGE_LENGTH_MS + PROTECTION_DURATION_MS,
    ),
  );

  it.effect(
    "re-arms a timer that fires before the clock reaches the bound",
    () =>
      Effect.gen(function* () {
        // Node timers scheduled from a stale event-loop time fire early.
        let nowMs = 0;
        const earlyFiring: Clock.Clock = {
          [Clock.ClockTypeId]: Clock.ClockTypeId,
          unsafeCurrentTimeMillis: () => nowMs,
          currentTimeMillis: Effect.sync(() => nowMs),
          unsafeCurrentTimeNanos: () => BigInt(nowMs) * 1_000_000n,
          currentTimeNanos: Effect.sync(() => BigInt(nowMs) * 1_000_000n),
          sleep: (duration) =>
            Effect.sync(() => {
              nowMs += Math.max(1, Duration.toMillis(duration) - 500);
            }),
        };
        const until = 10_000n;
        let rebuiltAtMs: number | undefined;
        const attempt = Effect.suspend(() => {
          if (nowMs === 0) return Effect.fail(protectedFailure(until));
          rebuiltAtMs = nowMs;
          return Effect.succeed("tx");
        });
        expect(
          yield* Effect.withClock(
            retryAfterRetirementProtection(attempt),
            earlyFiring,
          ),
        ).toBe("tx");
        expect(rebuiltAtMs).toBeGreaterThanOrEqual(
          Number(until) + SUBMIT_SLOT_LENGTH_MS,
        );
      }),
  );

  it.effect(
    "refuses at once, naming protected_until, past the honest protection bound",
    () =>
      Effect.gen(function* () {
        const until =
          BigInt(yield* Clock.currentTimeMillis) +
          SDK.MAX_VALIDITY_RANGE_LENGTH_MS +
          PROTECTION_DURATION_MS +
          1n;
        const script = scripted([protectedFailure(until), "tx"]);
        // Settled with the test clock never advanced: no wait was started.
        const retirement = yield* Effect.fork(
          Effect.flip(retryAfterRetirementProtection(script.attempt)),
        );
        yield* Effect.yieldNow();
        const settled = yield* Fiber.poll(retirement);
        expect(settled._tag).toBe("Some");
        expect(String(yield* Fiber.join(retirement))).toContain(
          `protected_until=${until.toString()}`,
        );
        expect(script.calls()).toBe(1);
      }),
  );

  it.effect(
    "waits once only and reports the bound that still protects the rebuild",
    () =>
      Effect.gen(function* () {
        const first = BigInt(yield* Clock.currentTimeMillis) + 1_000n;
        const second = first + 600_000n;
        const script = scripted([
          protectedFailure(first),
          protectedFailure(second),
          "tx",
        ]);
        const retirement = yield* Effect.fork(
          Effect.flip(retryAfterRetirementProtection(script.attempt)),
        );
        yield* Effect.yieldNow();
        yield* TestClock.adjust(Duration.millis(1_000 + SUBMIT_SLOT_LENGTH_MS));
        const failure = yield* Fiber.join(retirement);
        expect(String(failure)).toContain(
          `protected_until=${second.toString()}`,
        );
        expect(script.calls()).toBe(2);
      }),
  );

  it.effect("passes any other build failure through without waiting", () =>
    Effect.gen(function* () {
      const other = new SDK.ReservePayoutTxError({
        message: "Failed to resolve authenticated retirement state",
        cause: new Error("Event is no longer an authenticated live Order"),
      });
      const script = scripted([other, "tx"]);
      const failure = yield* Effect.flip(
        retryAfterRetirementProtection(script.attempt),
      );
      expect(failure).toBe(other);
      expect(script.calls()).toBe(1);
    }),
  );
});
