import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import type { PruneFloor } from "@al-ft/midgard-l1-follower";

/**
 * Bootstrap retains the origin until the replay keeper has fsynced a checked
 * ledger. Afterward retain every possible admission after its coverage end:
 * due = admission upper + wait, and upper - lower <= admission range. The
 * extra slot covers an admission straddling the slot boundary. No persisted
 * floor is trusted on restart before the checkpoint is root-checked again.
 */
export const createWatcherReplayRetention = (
  input: Readonly<{
    slotConfig: L1SlotConfig;
    eventWaitMs: bigint;
    maximumAdmissionRangeMs: bigint;
    maximumProtectionWaitMs: bigint;
  }>,
) => {
  if (
    input.eventWaitMs < 0n ||
    input.maximumAdmissionRangeMs < 0n ||
    input.maximumProtectionWaitMs < 0n ||
    !Number.isSafeInteger(input.slotConfig.slotLength) ||
    input.slotConfig.slotLength <= 0
  )
    throw new Error("replay retention requires valid admission timing");
  let coverageEnd: bigint | undefined;
  const floor: PruneFloor = {
    name: "watcher_replayed_ledger",
    floor: async ({ tx }) => {
      const cursor = (
        await tx.query("SELECT origin_slot FROM l1_follower_cursor")
      )[0];
      if (cursor === undefined) return 0;
      const origin = Number(cursor.origin_slot);
      if (coverageEnd === undefined) return origin;
      const admissionTime =
        coverageEnd -
        input.eventWaitMs -
        input.maximumAdmissionRangeMs -
        input.maximumProtectionWaitMs;
      const delta = admissionTime - BigInt(input.slotConfig.zeroTime);
      const length = BigInt(input.slotConfig.slotLength);
      const quotient =
        delta >= 0n ? delta / length : (delta - length + 1n) / length;
      const slot = BigInt(input.slotConfig.zeroSlot) + quotient - 1n;
      if (slot < BigInt(origin)) return origin;
      const candidate = Number(slot);
      if (!Number.isSafeInteger(candidate))
        throw new Error(
          "replay retention slot is outside the safe integer range",
        );
      return candidate;
    },
  };
  return Object.freeze({
    floor,
    checkpointDurable: (endTime: bigint): void => {
      if (coverageEnd === undefined || endTime > coverageEnd)
        coverageEnd = endTime;
    },
    reset: (): void => {
      coverageEnd = undefined;
    },
  });
};
export type WatcherReplayRetention = ReturnType<
  typeof createWatcherReplayRetention
>;
