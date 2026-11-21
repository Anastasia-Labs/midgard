import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import { WALLET_ONLY_VALIDITY_MS } from "../../src/wallet-only-validity.js";
import {
  latestValidToSlotWithoutFundingReservation,
  type WalletOnlySend,
} from "../../src/workflow/wallet-only-send-gate.js";

/**
 * The restart wait already over, for tests whose subject is not the wait and
 * whose runs bind no funding reservation: no input of theirs is a wallet coin
 * (`spendsOnlyWalletCoins`), so no wallet-only bound is checked, and its bound
 * clock throws if one ever is.
 */
export const WALLET_ONLY_SEND_ALWAYS_OPEN: WalletOnlySend = Object.freeze({
  open: () => true,
  latestValidToSlot: latestValidToSlotWithoutFundingReservation,
});

/** The restart wait already over, with the latest bound on the clock of the
 * test's chain (`lucid` on its emulator), the one its wallet builds bounds
 * on, for runs that bind a funding reservation. */
export const walletOnlySendAlwaysOpenOn = (
  lucid: Pick<
    LucidEvolution,
    "currentSlot" | "slotToUnixTime" | "unixTimeToSlot"
  >,
): WalletOnlySend =>
  Object.freeze({
    open: () => true,
    latestValidToSlot: () =>
      lucid.unixTimeToSlot(
        lucid.slotToUnixTime(lucid.currentSlot()) + WALLET_ONLY_VALIDITY_MS,
      ),
  });

/** The restart wait already over, on Preview's slot clock at the wall clock,
 * for runs that bind a funding reservation over fixture transactions on no
 * chain: a fixture bound is a low slot, long before one window past now. */
export const WALLET_ONLY_SEND_ALWAYS_OPEN_ON_PREVIEW: WalletOnlySend =
  Object.freeze({
    open: () => true,
    latestValidToSlot: () => {
      const { zeroTime, zeroSlot, slotLength } =
        NAMED_NETWORK_L1_SLOT_CONFIGS.Preview;
      return (
        Math.floor(
          (Date.now() + WALLET_ONLY_VALIDITY_MS - zeroTime) / slotLength,
        ) + zeroSlot
      );
    },
  });
