import { setTimeout as delay } from "node:timers/promises";

import { expect, vi } from "vitest";

import { setup } from "./user-event-runtime.setup.js";

export const waitForNativeQueries = async (
  context: Awaited<ReturnType<typeof setup>>,
  blockHash: string,
  count: number,
) => {
  await vi.waitFor(
    async () => {
      expect(
        context.capturesOf(await context.fixture.readNativeQueries(), blockHash)
          .length,
      ).toBeGreaterThanOrEqual(count);
    },
    { timeout: 15_000, interval: 50 },
  );
  // Both query processes have emitted their receipts; let capture cleanup
  // finish before advancing only the parent's monotonic clock.
  await delay(200);
};
