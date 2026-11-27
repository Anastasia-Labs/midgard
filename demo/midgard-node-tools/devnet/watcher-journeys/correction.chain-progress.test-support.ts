import { expect, vi } from "vitest";

import {
  type JourneyChainCorrection,
  verifyJourneyCorrection,
} from "./correction.js";
import type { JourneyWatcherStartStatus } from "./correction.watcher-start-deadline.js";

type CorrectionInput = Parameters<typeof verifyJourneyCorrection>[0];

export const target = {
  category: "validationTraceDispute",
  headerHash: "34".repeat(28),
} as const;

/** A correction as the chain holds it, at `confirmationDepth`. */
export const chainCorrection = (
  confirmationDepth: number,
): JourneyChainCorrection => ({
  ...target,
  proofToken: {
    unit: `${"ab".repeat(28)}0100${target.headerHash}`,
    outRef: `${"11".repeat(32)}#0`,
    createdByTxHash: "11".repeat(32),
  },
  correction: {
    removalTxHash: "22".repeat(32),
    removedStateQueueOutRef: `${"33".repeat(32)}#0`,
  },
  economics: {
    operatorBondInputOutRef: `${"44".repeat(32)}#0`,
    operatorBondInputLovelace: "5000000",
    slashTxHash: "55".repeat(32),
    slashedLovelace: "2000000",
    proverRewardOutputOutRef: `${"55".repeat(32)}#0`,
    proverRewardLovelace: "3000000",
  },
  observedAt: { slot: 1234, blockHash: "66".repeat(32), confirmationDepth },
});

/** One read of the chain and the watcher at a wall-clock instant. */
export type ChainObservation = Readonly<{
  at: number;
  /** The transactions of ours landed so far. */
  landed: readonly string[];
  watcher?: JourneyWatcherStartStatus;
  correction?: JourneyChainCorrection;
}>;

/**
 * Drive the journey's correction wait through `observations`, each one poll;
 * the run ends after the last observation unless the wait returns or fails
 * first. The independent stage is not run: these observations carry no
 * transaction bytes. The error names how many observations the wait read
 * before it failed; the result also counts the watcher status reads.
 */
export const pollChainCorrection = async (
  observations: readonly ChainObservation[],
  { actionDepth = 1 }: { actionDepth?: number } = {},
) => {
  const now = vi.spyOn(Date, "now").mockReturnValue(observations[0]!.at);
  let current = observations[0]!;
  let read = 0;
  let statusReads = 0;
  try {
    const corrected = await verifyJourneyCorrection({
      context: { deployment: {}, provider: {} } as CorrectionInput["context"],
      transactions: {} as CorrectionInput["transactions"],
      chain: {
        landed: async () => new Set(current.landed),
        correction: async () => current.correction,
      },
      watcherStatus: async () => {
        statusReads += 1;
        return current.watcher ?? {};
      },
      proverAddress: "addr_test1unused",
      ...target,
      predecessorHeaderHash: "78".repeat(28),
      actionDepth,
      operatorVkey: "90".repeat(28),
      requireLive: () => undefined,
      stage: async () => undefined as never,
      poll: async (_name, action, timeoutMs) => {
        expect(timeoutMs).toBe(Number.POSITIVE_INFINITY);
        for (const observation of observations) {
          now.mockReturnValue(observation.at);
          current = observation;
          read += 1;
          const result = await action();
          if (result !== undefined) return result;
        }
        throw new Error("observations ended");
      },
    });
    return { ...corrected, read, statusReads };
  } catch (error) {
    throw new Error(
      `at observation ${read.toString()} of ${observations.length.toString()}: ${(error as Error).message}`,
    );
  } finally {
    now.mockRestore();
  }
};

/** A watcher `/v1/status` carrying these L1 holds and no supervisor yet. */
export const holding = (
  ...holds: readonly Readonly<{ reason: string; headerHash?: string }>[]
): JourneyWatcherStartStatus => ({
  l1Readiness: holds.map(({ reason, headerHash = target.headerHash }) => ({
    reason,
    detail: `${target.category}/${headerHash}: ${reason} detail`,
  })),
});
