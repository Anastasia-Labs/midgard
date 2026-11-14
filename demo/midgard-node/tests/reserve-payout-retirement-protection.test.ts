import { SUBMIT_SLOT_VALIDITY_BUFFER } from "@al-ft/midgard-core/ogmios-slot";
import * as SDK from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import { Emulator, Lucid } from "@lucid-evolution/lucid";
import { Effect, TestClock } from "effect";
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

/**
 * An emulator chain whose tip is the command's observed ledger tip, with the
 * test clock set to its time: the wall clock and the tip agree at the start,
 * and only the tip moves while the command waits.
 */
const chain = Effect.gen(function* () {
  const emulator = new Emulator([]);
  const lucid = yield* Effect.promise(() => Lucid(emulator, "Custom"));
  yield* TestClock.setTime(emulator.now());
  const tipSlot = () => lucid.unixTimeToSlot(emulator.now());
  return { emulator, lucid, tipSlot };
});

/** A retirement build whose attempts fail or succeed in the given order,
 * recording the tip slot each attempt was built at. */
const scripted = (
  tipSlot: () => number,
  outcomes: readonly (SDK.ReservePayoutTxError | Error | "tx")[],
) => {
  const builtAt: number[] = [];
  const attempt = Effect.suspend(() => {
    const outcome = outcomes[builtAt.length]!;
    builtAt.push(tipSlot());
    return outcome === "tx"
      ? Effect.succeed("tx")
      : Effect.fail(outcome as SDK.ReservePayoutTxError);
  });
  return { attempt, builtAt };
};

/** Protection `aheadMs` past the tip is awaited until the tip passes it by
 * the submit margin, and only then rebuilt. */
const waitsForTipThenRebuilds = (aheadMs: bigint) =>
  Effect.gen(function* () {
    const { emulator, lucid, tipSlot } = yield* chain;
    const startSlot = tipSlot();
    const until = BigInt(emulator.now()) + aheadMs;
    const script = scripted(tipSlot, [protectedFailure(until), "tx"]);
    expect(yield* retryAfterRetirementProtection(lucid, script.attempt)).toBe(
      "tx",
    );
    expect(script.builtAt).toEqual([
      startSlot,
      lucid.unixTimeToSlot(Number(until)) + SUBMIT_SLOT_VALIDITY_BUFFER,
    ]);
  });

describe("retirement protection wait", () => {
  // Right after another retirement, an honest bound sits past a full validity
  // window by part of the list's protection duration.
  it.effect(
    "waits for the tip to pass a bound inside the honest window plus duration, then rebuilds",
    () =>
      waitsForTipThenRebuilds(
        SDK.MAX_VALIDITY_RANGE_LENGTH_MS + PROTECTION_DURATION_MS / 2n,
      ),
  );

  it.effect(
    "waits for the tip to pass a bound exactly at the honest limit",
    () =>
      waitsForTipThenRebuilds(
        SDK.MAX_VALIDITY_RANGE_LENGTH_MS + PROTECTION_DURATION_MS,
      ),
  );

  it.effect(
    "refuses at once, naming protected_until, past the honest protection bound",
    () =>
      Effect.gen(function* () {
        const { emulator, lucid, tipSlot } = yield* chain;
        const startSlot = tipSlot();
        const until =
          BigInt(emulator.now()) +
          SDK.MAX_VALIDITY_RANGE_LENGTH_MS +
          PROTECTION_DURATION_MS +
          1n;
        const script = scripted(tipSlot, [protectedFailure(until), "tx"]);
        const failure = yield* Effect.flip(
          retryAfterRetirementProtection(lucid, script.attempt),
        );
        expect(String(failure)).toContain(
          `protected_until=${until.toString()}`,
        );
        // No wait was started: the tip never moved.
        expect(script.builtAt).toEqual([startSlot]);
        expect(tipSlot()).toBe(startSlot);
      }),
  );

  it.effect(
    "waits once only and reports the bound that still protects the rebuild",
    () =>
      Effect.gen(function* () {
        const { emulator, lucid, tipSlot } = yield* chain;
        const first = BigInt(emulator.now()) + 1_000n;
        const second = first + 600_000n;
        const script = scripted(tipSlot, [
          protectedFailure(first),
          protectedFailure(second),
          "tx",
        ]);
        const failure = yield* Effect.flip(
          retryAfterRetirementProtection(lucid, script.attempt),
        );
        expect(String(failure)).toContain(
          `protected_until=${second.toString()}`,
        );
        expect(script.builtAt).toHaveLength(2);
      }),
  );

  it.effect("passes any other build failure through without waiting", () =>
    Effect.gen(function* () {
      const { lucid, tipSlot } = yield* chain;
      const startSlot = tipSlot();
      const other = new SDK.ReservePayoutTxError({
        message: "Failed to resolve authenticated retirement state",
        cause: new Error("Event is no longer an authenticated live Order"),
      });
      const script = scripted(tipSlot, [other, "tx"]);
      const failure = yield* Effect.flip(
        retryAfterRetirementProtection(lucid, script.attempt),
      );
      expect(failure).toBe(other);
      expect(script.builtAt).toEqual([startSlot]);
    }),
  );
});
