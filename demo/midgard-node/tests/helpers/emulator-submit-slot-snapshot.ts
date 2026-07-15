import { SLOT_CONFIG_NETWORK } from "@lucid-evolution/lucid";

import type { SubmitSlotSnapshot } from "@/local-ledger-slot.js";

const EMULATOR_SLOT_LENGTH_MS = 1_000;

export const captureCustomSlotConfigRestore = (): (() => void) => {
  const previous = SLOT_CONFIG_NETWORK.Custom;
  return () => {
    SLOT_CONFIG_NETWORK.Custom = previous;
  };
};

export const setEmulatorCustomSlotConfig = ({
  zeroTimeMs,
  zeroSlot,
}: {
  readonly zeroTimeMs: number;
  readonly zeroSlot: number;
}): void => {
  if (
    !Number.isSafeInteger(zeroTimeMs) ||
    zeroTimeMs < 0 ||
    !Number.isSafeInteger(zeroSlot) ||
    zeroSlot < 0
  ) {
    throw new Error(
      `Invalid emulator Custom slot configuration: zero_time_ms=${zeroTimeMs.toString()},zero_slot=${zeroSlot.toString()}`,
    );
  }
  SLOT_CONFIG_NETWORK.Custom = {
    zeroTime: zeroTimeMs,
    zeroSlot,
    slotLength: EMULATOR_SLOT_LENGTH_MS,
  };
};

export const deriveEmulatorSubmitSlotSnapshot = ({
  currentSlot,
  observedAtMs,
}: {
  readonly currentSlot: number;
  readonly observedAtMs: number;
}): SubmitSlotSnapshot => {
  if (
    !Number.isSafeInteger(currentSlot) ||
    currentSlot < 0 ||
    !Number.isSafeInteger(observedAtMs) ||
    observedAtMs < 0
  ) {
    throw new Error(
      `Invalid emulator submit-slot snapshot: current_slot=${currentSlot.toString()},observed_at_ms=${observedAtMs.toString()}`,
    );
  }
  return {
    source: "emulator",
    currentSlot,
    observedAtMs,
    slotLengthMs: EMULATOR_SLOT_LENGTH_MS,
  };
};
