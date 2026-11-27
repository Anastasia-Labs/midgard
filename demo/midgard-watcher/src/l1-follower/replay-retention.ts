import {
  type L1SlotConfig,
  l1SlotToPosixTimeMs,
} from "@al-ft/midgard-core/l1-slot-config";
import type { PruneFloor } from "@al-ft/midgard-l1-follower";

/**
 * Bootstrap retains the origin until the replay keeper has fsynced a checked
 * ledger. Afterward retain every possible admission after its coverage end:
 * due = admission upper + wait, and upper - lower <= admission range. The
 * extra slot covers an admission straddling the slot boundary. No persisted
 * floor is trusted on restart before the checkpoint is root-checked again.
 *
 * The floor also keeps a node row of every block inside the ledger window
 * (`replay-source.window.ts`), merged ones included. Such a block has
 * `block_time > tip - ledger_retention`, and its last node row closes no
 * earlier than its merge, whose validity starts at `block_time + maturity`
 * (state_queue.ak's merge rule), so at a slot no earlier than
 * slot(tip - ledger_retention + maturity). Prune deletes rows closed at or
 * below the boundary, so one slot below that keeps them; the floor is never
 * above the admission rule. Where the window equals maturity no block inside
 * it has matured, so none has merged, its open row is never pruned, and the
 * admission rule alone applies.
 */
export const createWatcherReplayRetention = (
  input: Readonly<{
    slotConfig: L1SlotConfig;
    eventWaitMs: bigint;
    maximumAdmissionRangeMs: bigint;
    maximumProtectionWaitMs: bigint;
    /** The deployment's off-chain `ledger_retention_ms`. */
    ledgerRetentionMs: bigint;
    /** The deployment's `block_maturity_ms`. */
    maturityMs: bigint;
  }>,
) => {
  if (
    input.eventWaitMs < 0n ||
    input.maximumAdmissionRangeMs < 0n ||
    input.maximumProtectionWaitMs < 0n ||
    input.maturityMs < 0n ||
    input.ledgerRetentionMs < input.maturityMs ||
    !Number.isSafeInteger(input.slotConfig.slotLength) ||
    input.slotConfig.slotLength <= 0
  )
    throw new Error("replay retention requires valid admission timing");
  let coverageEnd: bigint | undefined;
  /** The last slot whose start is at or before `timeMs`. */
  const slotAt = (timeMs: bigint): bigint => {
    const delta = timeMs - BigInt(input.slotConfig.zeroTime);
    const length = BigInt(input.slotConfig.slotLength);
    const quotient =
      delta >= 0n ? delta / length : (delta - length + 1n) / length;
    return BigInt(input.slotConfig.zeroSlot) + quotient;
  };
  const floor: PruneFloor = {
    name: "watcher_replayed_ledger",
    floor: async ({ tx }) => {
      const cursor = (
        await tx.query("SELECT slot, origin_slot FROM l1_follower_cursor")
      )[0];
      if (cursor === undefined) return 0;
      const origin = Number(cursor.origin_slot);
      if (coverageEnd === undefined) return origin;
      const admission =
        slotAt(
          coverageEnd -
            input.eventWaitMs -
            input.maximumAdmissionRangeMs -
            input.maximumProtectionWaitMs,
        ) - 1n;
      const window =
        input.ledgerRetentionMs === input.maturityMs
          ? admission
          : slotAt(
              l1SlotToPosixTimeMs(
                input.slotConfig,
                BigInt(String(cursor.slot)),
              ) -
                input.ledgerRetentionMs +
                input.maturityMs,
            ) - 1n;
      const slot = window < admission ? window : admission;
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
    slotConfig: input.slotConfig,
    ledgerRetentionMs: input.ledgerRetentionMs,
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
