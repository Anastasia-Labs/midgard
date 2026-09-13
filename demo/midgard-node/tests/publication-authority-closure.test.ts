import { afterEach, expect, it, vi } from "vitest";

import { waitForPublicationAuthorityExpiry } from "./helpers/published-workflow-deployment.js";

afterEach(() => vi.useRealTimers());

it("requires canonical expiry despite clock changes and waits that return without chain progress", async () => {
  vi.useFakeTimers();
  vi.setSystemTime(new Date("2026-09-11T01:00:00Z"));
  const canonicalSlots = [100, 100, 100, 150, 151];
  const waits: number[] = [];
  let observations = 0;
  const closedAtSlot = await waitForPublicationAuthorityExpiry({
    expiresAtSlot: 150,
    synchronize: async () => canonicalSlots[observations++]!,
    awaitSlot: async (slots) => {
      waits.push(slots);
      // A host clock can jump beyond expiry, then backwards, while the
      // canonical tip remains unchanged. Neither event proves closure.
      vi.setSystemTime(
        new Date(
          waits.length % 2 === 1
            ? "2026-09-12T01:00:00Z"
            : "2026-09-10T01:00:00Z",
        ),
      );
    },
  });
  expect(observations).toBe(5);
  expect(closedAtSlot).toBe(151);
  expect(waits).toEqual([30, 30, 30, 1]);
});

it("uses the canonical tip when a stale local slot would require an unnecessary wait", async () => {
  vi.useFakeTimers();
  vi.setSystemTime(new Date(0));
  const awaitSlot = vi.fn();
  await waitForPublicationAuthorityExpiry({
    expiresAtSlot: 150,
    synchronize: async () => 151,
    awaitSlot,
  });
  expect(awaitSlot).not.toHaveBeenCalled();
});

it("does not close authority when the canonical synchronization barrier fails after a wait", async () => {
  let observations = 0;
  const awaitSlot = vi.fn();
  await expect(
    waitForPublicationAuthorityExpiry({
      expiresAtSlot: 150,
      synchronize: async () => {
        if (observations++ === 0) return 150;
        throw new Error("Canonical checkpoint unavailable");
      },
      awaitSlot,
    }),
  ).rejects.toThrow("Canonical checkpoint unavailable");
  expect(awaitSlot).toHaveBeenCalledExactlyOnceWith(1);
});
