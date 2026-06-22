import "./utils.js";

import {
  assertReferenceScriptAuthMinimumRemaining,
  ReferenceScriptAuthDeadlineError,
  type ReferenceScriptAuthMintingPolicy,
  referenceScriptAuthRemainingMs,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

const policy = (expiresAtUnixTime?: number): ReferenceScriptAuthMintingPolicy =>
  ({
    policyId: "00".repeat(28),
    mintingScript: { type: "Native", script: "8200" },
    mintingScriptCBOR: "8200",
    ...(expiresAtUnixTime === undefined ? {} : { expiresAtUnixTime }),
  }) as ReferenceScriptAuthMintingPolicy;

describe("reference-script auth deadline guard", () => {
  it("computes remaining validity from the policy expiry", () => {
    expect(referenceScriptAuthRemainingMs(policy(10_000), 4_000)).toEqual(
      6_000,
    );
    expect(referenceScriptAuthRemainingMs(policy(undefined), 4_000)).toEqual(
      undefined,
    );
  });

  it("passes when remaining validity is above the guard threshold", () => {
    expect(() =>
      assertReferenceScriptAuthMinimumRemaining({
        policy: policy(10_000),
        nowMs: 1_000,
        minRemainingMs: 5_000,
        scopeName: "node-runtime",
        targetNames: ["scheduler minting"],
      }),
    ).not.toThrow();
  });

  it("fails at or below the guard threshold with target diagnostics", () => {
    expect(() =>
      assertReferenceScriptAuthMinimumRemaining({
        policy: policy(10_000),
        nowMs: 5_000,
        minRemainingMs: 5_000,
        scopeName: "node-runtime",
        targetNames: ["scheduler minting", "state-queue minting"],
      }),
    ).toThrow(ReferenceScriptAuthDeadlineError);

    try {
      assertReferenceScriptAuthMinimumRemaining({
        policy: policy(undefined),
        nowMs: 5_000,
        minRemainingMs: 5_000,
        scopeName: "node-runtime",
        targetNames: ["scheduler minting"],
      });
      throw new Error("expected guard to fail");
    } catch (cause) {
      expect(cause).toBeInstanceOf(ReferenceScriptAuthDeadlineError);
      expect(String(cause)).toContain("expires_at_unix_time=missing");
      expect(String(cause)).toContain("targets=scheduler minting");
    }
  });
});
