/**
 * Commit and scheduler-refresh validity windows follow the selected deployment
 * profile's `timing.max_validity_range_ms`. Every shipped profile uses 480 s,
 * so the SDK's profile-derived constants are replaced with a shorter range to
 * tell a profile read apart from a hardcoded copy.
 */
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  COMMIT_VALIDITY_BACKDATE_MS,
  commitValidityEndTimeCapMs,
  resolveCommitValidityInterval,
} from "../src/workers/utils/commit-end-time.js";
import { resolveSchedulerRefreshValidityWindow } from "../src/workers/utils/scheduler-refresh.js";

const { PROFILE_MAX_VALIDITY_RANGE_MS } = vi.hoisted(() => ({
  PROFILE_MAX_VALIDITY_RANGE_MS: 300_000,
}));

vi.mock("@al-ft/midgard-sdk", async (importOriginal) => ({
  ...(await importOriginal<typeof import("@al-ft/midgard-sdk")>()),
  COMMIT_MAX_VALIDITY_RANGE_MS: PROFILE_MAX_VALIDITY_RANGE_MS,
  MAX_VALIDITY_RANGE_LENGTH_MS: BigInt(PROFILE_MAX_VALIDITY_RANGE_MS),
}));

const secondSlotLucid = {
  slotToUnixTime: (slot: number) => slot * 1_000,
  unixTimeToSlot: (unixTime: number) => Math.floor(unixTime / 1_000),
} as unknown as LucidEvolution;

describe("validity windows read the selected profile's range", () => {
  it("bounds the commit end by the profile range less the backdate", () => {
    const currentSlot = 5_000_000;
    expect(COMMIT_MINIMUM_FUTURE_BUFFER_MS).toBe(
      PROFILE_MAX_VALIDITY_RANGE_MS - COMMIT_VALIDITY_BACKDATE_MS - 1_000,
    );
    expect(commitValidityEndTimeCapMs(secondSlotLucid, currentSlot)).toBe(
      currentSlot * 1_000 + COMMIT_MINIMUM_FUTURE_BUFFER_MS,
    );
  });

  it("moves the commit lower bound forward to stay within the profile range", () => {
    const currentSlot = 5_000_000;
    const interval = (validToMs: number) =>
      resolveCommitValidityInterval({
        lucid: secondSlotLucid,
        submitSlotSnapshot: {
          source: "test",
          currentSlot,
          observedAtMs: currentSlot * 1_000,
          slotLengthMs: 1_000,
        },
        validToMs,
      });
    // A validTo 400 s ahead is more than the range past the backdated slot
    // start, so the lower bound follows the range instead.
    const slotAligned = interval(currentSlot * 1_000 + 400_000);
    expect(slotAligned.validFromMs).toBe(
      slotAligned.validToMs - PROFILE_MAX_VALIDITY_RANGE_MS,
    );
    // Mid-slot, the range bound floors to its slot and would exceed the range,
    // so the lower bound moves to the next slot.
    const midSlot = interval(currentSlot * 1_000 + 400_500);
    expect(midSlot.validFromMs).toBe(
      currentSlot * 1_000 + 400_500 - PROFILE_MAX_VALIDITY_RANGE_MS + 500,
    );
  });

  it("spans a scheduler refresh over exactly the profile range", () => {
    const currentSlot = 5_000_000;
    const window = resolveSchedulerRefreshValidityWindow(secondSlotLucid, 0n, {
      currentSlot,
      currentSlotStartMs: currentSlot * 1_000,
      observedAtMs: currentSlot * 1_000,
    });
    expect(window.validTo - window.validFrom).toBe(
      BigInt(PROFILE_MAX_VALIDITY_RANGE_MS),
    );
  });
});
