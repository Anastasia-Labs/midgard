import type { LucidEvolution } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import {
  COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  resolveHistoryCommitBlockTime,
} from "../src/workers/utils/commit-end-time.js";
export const registerCatchUpCommitClockCase = (
  makeLucid: () => Promise<LucidEvolution>,
  HISTORY_BUFFER_MS: number,
) => {
  it("keeps the submission clock current while a six-hour-old coverage end remains independently selectable", async () => {
    const lucid = await makeLucid();
    const currentSlot = lucid.currentSlot();
    const slotStartMs = lucid.slotToUnixTime(currentSlot);
    const fit = resolveHistoryCommitBlockTime({
      lucid,
      currentSlot,
      latestEndTime: slotStartMs - 6 * 60 * 60_000,
      nowMs: slotStartMs + 250,
      minimumFutureBufferMs: HISTORY_BUFFER_MS,
    });
    expect(fit.status).toBe("fits");
    expect(fit.resolvedEndTime - 1).toBe(
      slotStartMs + COMMIT_MINIMUM_FUTURE_BUFFER_MS - 1,
    );
  });
  it("preserves normal L2 progress when the safe preprod horizon clears the submission floor", async () => {
    const lucid = await makeLucid();
    const currentSlot = lucid.currentSlot();
    const slotStartMs = lucid.slotToUnixTime(currentSlot);
    const coverageHorizonMs = slotStartMs + 299_000 - 1;
    const fit = resolveHistoryCommitBlockTime({
      lucid,
      currentSlot,
      latestEndTime: slotStartMs - 60_000,
      nowMs: slotStartMs + 250,
      minimumFutureBufferMs: 120_000,
      coverageHorizonMs,
    });
    expect(fit.status).toBe("fits");
    expect(fit.resolvedEndTime - 1).toBe(coverageHorizonMs);
  });
  it("uses an independent current clock when old coverage cannot clear the submission floor", async () => {
    const lucid = await makeLucid();
    const currentSlot = lucid.currentSlot();
    const slotStartMs = lucid.slotToUnixTime(currentSlot);
    const fit = resolveHistoryCommitBlockTime({
      lucid,
      currentSlot,
      latestEndTime: slotStartMs - 6 * 60 * 60_000,
      nowMs: slotStartMs + 250,
      minimumFutureBufferMs: 120_000,
      coverageHorizonMs: slotStartMs - 5 * 60 * 60_000,
    });
    expect(fit.status).toBe("fits");
    expect(fit.resolvedEndTime - 1).toBe(
      slotStartMs + COMMIT_MINIMUM_FUTURE_BUFFER_MS - 1,
    );
  });
};
