import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  makeManifestStrikeGate,
  type ManifestGateDecision,
  type ManifestVerification,
  OPERATOR_WATCHDOG_MANIFEST_MISMATCH,
  OPERATOR_WATCHDOG_MANIFEST_MISMATCH_RECHECK_MS,
  OPERATOR_WATCHDOG_MANIFEST_RETRY_BASE_MS,
  OPERATOR_WATCHDOG_MANIFEST_RETRY_MAX_MS,
  OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED,
} from "../src/fibers/operator-watchdog.manifest-gate.js";

const SOURCE = "operator_watchdog_manifest";

type Outcome = "error" | "defect" | "mismatch" | "match";

/** A gate whose verification returns `outcomes` in order (the last repeats),
 * with the number of verifications it ran and the reason it raised. */
const gateOver = (outcomes: readonly Outcome[]) => {
  const globals = {
    LIVENESS_REASONS: Ref.unsafeMake<ReadonlyMap<string, string>>(new Map()),
  };
  let verifications = 0;
  const verify = Effect.suspend(
    (): Effect.Effect<ManifestVerification, Error> => {
      const outcome = outcomes[Math.min(verifications, outcomes.length - 1)]!;
      verifications += 1;
      switch (outcome) {
        case "error":
          return Effect.fail(new Error("manifest file unreadable"));
        case "defect":
          return Effect.die(new Error("config read crashed"));
        case "mismatch":
          return Effect.succeed({
            ok: false,
            mismatches: ["state_queue policy differs"],
          });
        case "match":
          return Effect.succeed({ ok: true, mismatches: [] });
      }
    },
  );
  const gate = Effect.runSync(makeManifestStrikeGate(globals, verify));
  return {
    beforeStrike: (nowMs: number): ManifestGateDecision =>
      Effect.runSync(gate.beforeStrike(nowMs)),
    verifications: () => verifications,
    raised: () => Effect.runSync(Ref.get(globals.LIVENESS_REASONS)).get(SOURCE),
  };
};

describe("operator watchdog manifest strike gate", () => {
  it("retries a verification error with backoff, strikes nobody meanwhile, then allows strikes after one good verification", async () => {
    const gate = gateOver(["error", "error", "defect", "match"]);
    const strikes: number[] = [];
    const attempt = (nowMs: number) => {
      const decision = gate.beforeStrike(nowMs);
      if (decision.ok) strikes.push(nowMs);
      return decision;
    };
    const base = OPERATOR_WATCHDOG_MANIFEST_RETRY_BASE_MS;
    expect(attempt(0)).toEqual({
      ok: false,
      reason: "manifest_unverified",
      untilMs: base,
    });
    // Before the retry is due nothing is verified again.
    expect(attempt(base - 1)).toMatchObject({ ok: false });
    expect(gate.verifications()).toBe(1);
    expect(attempt(base)).toMatchObject({ ok: false, untilMs: 3 * base });
    expect(gate.raised()).toBeUndefined();
    // The third failure in a row (a defect of the verification) surfaces it.
    expect(attempt(3 * base)).toMatchObject({ ok: false, untilMs: 7 * base });
    expect(gate.raised()).toBe(OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED);
    expect(strikes).toEqual([]);

    expect(attempt(7 * base)).toEqual({ ok: true });
    expect(gate.raised()).toBeUndefined();
    expect(attempt(7 * base + 1)).toEqual({ ok: true });
    // Verified once; later strikes need no verification.
    expect(gate.verifications()).toBe(4);
    expect(strikes).toEqual([7 * base, 7 * base + 1]);
  });

  it("bounds the retry delay", () => {
    const gate = gateOver(["error"]);
    let nowMs = 0;
    let longest = 0;
    for (let failure = 0; failure < 20; failure += 1) {
      const decision = gate.beforeStrike(nowMs);
      if (decision.ok) throw new Error("must not strike");
      longest = Math.max(longest, decision.untilMs - nowMs);
      nowMs = decision.untilMs;
    }
    expect(longest).toBe(OPERATOR_WATCHDOG_MANIFEST_RETRY_MAX_MS);
    expect(gate.verifications()).toBe(20);
  });

  it("refuses every strike on a confirmed mismatch, surfaces it, and re-reads the manifest only at the slow cadence", () => {
    const gate = gateOver(["mismatch"]);
    const recheck = OPERATOR_WATCHDOG_MANIFEST_MISMATCH_RECHECK_MS;
    expect(gate.beforeStrike(0)).toEqual({
      ok: false,
      reason: "manifest_mismatch",
      untilMs: recheck,
    });
    expect(gate.raised()).toBe(OPERATOR_WATCHDOG_MANIFEST_MISMATCH);
    for (const nowMs of [1, recheck / 2, recheck - 1])
      expect(gate.beforeStrike(nowMs)).toMatchObject({
        ok: false,
        reason: "manifest_mismatch",
      });
    expect(gate.verifications()).toBe(1);
    // Still mismatched at the re-read: still refused, still surfaced.
    expect(gate.beforeStrike(recheck)).toMatchObject({
      ok: false,
      untilMs: 2 * recheck,
    });
    expect(gate.verifications()).toBe(2);
    expect(gate.raised()).toBe(OPERATOR_WATCHDOG_MANIFEST_MISMATCH);
  });

  it("keeps a mismatch refused through a verification error at its re-read, and allows strikes once the manifest matches", () => {
    const gate = gateOver(["mismatch", "error", "match"]);
    const recheck = OPERATOR_WATCHDOG_MANIFEST_MISMATCH_RECHECK_MS;
    gate.beforeStrike(0);
    expect(gate.beforeStrike(recheck)).toMatchObject({
      ok: false,
      reason: "manifest_mismatch",
    });
    expect(gate.raised()).toBe(OPERATOR_WATCHDOG_MANIFEST_MISMATCH);
    expect(gate.beforeStrike(recheck + recheck)).toEqual({ ok: true });
    expect(gate.raised()).toBeUndefined();
  });
});
