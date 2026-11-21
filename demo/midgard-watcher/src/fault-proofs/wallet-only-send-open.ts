import {
  type L1SlotConfig,
  l1SlotToPosixTimeMs,
} from "@al-ft/midgard-core/l1-slot-config";
import { WALLET_ONLY_VALIDITY_MS } from "@al-ft/midgard-fault-proofs/wallet-only-validity";
import type { WalletOnlySend } from "@al-ft/midgard-fault-proofs/workflow/wallet-only-send-gate";

/**
 * Whether the restart wait is over: the clock and the time of the last
 * block the follower applied are both at or past `notBeforeMs`. With no
 * applied block the wait is not over, so a follower still catching up never
 * opens it early.
 */
export const isWalletOnlySendOpen = ({
  notBeforeMs,
  nowMs,
  appliedL1TimeMs,
}: {
  readonly notBeforeMs: number;
  readonly nowMs: number;
  readonly appliedL1TimeMs: number | null;
}): boolean =>
  appliedL1TimeMs !== null &&
  nowMs >= notBeforeMs &&
  appliedL1TimeMs >= notBeforeMs;

/**
 * The L1 chain the restart wait reads: the deployment's slot mapping (the
 * one a wallet-only transaction's upper bound is built with, which startup
 * checks the watcher's network against) and the slot of the follower's last
 * applied block (its cursor, not the node's tip), null before the first.
 */
export type WatcherL1Clock = Readonly<{
  slotConfig: L1SlotConfig;
  appliedSlot: () => number | null | Promise<number | null>;
}>;

/**
 * The process's restart wait and latest wallet-only bound, both on
 * `nowMs`. The instant the wait opens at is fixed here, one wallet-only
 * validity window past `nowMs()` at the call, so the caller builds it once
 * per process. A failed read of the applied slot reads as no applied block:
 * the wait holds. The latest bound is the deployment slot one window past
 * `nowMs()` at the check.
 */
export const createWalletOnlySend = ({
  nowMs,
  slotConfig,
  appliedSlot,
}: WatcherL1Clock & { readonly nowMs: () => number }): WalletOnlySend => {
  const notBeforeMs = nowMs() + WALLET_ONLY_VALIDITY_MS;
  return Object.freeze({
    open: async () => {
      const slot = await Promise.resolve()
        .then(appliedSlot)
        .catch(() => null);
      return isWalletOnlySendOpen({
        notBeforeMs,
        nowMs: nowMs(),
        appliedL1TimeMs:
          slot === null
            ? null
            : Number(l1SlotToPosixTimeMs(slotConfig, BigInt(slot))),
      });
    },
    latestValidToSlot: () =>
      Math.floor(
        (nowMs() + WALLET_ONLY_VALIDITY_MS - slotConfig.zeroTime) /
          slotConfig.slotLength,
      ) + slotConfig.zeroSlot,
  });
};
