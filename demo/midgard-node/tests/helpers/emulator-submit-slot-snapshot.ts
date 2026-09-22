import type { SubmitSlotSnapshot } from "../../src/local-ledger-slot.js";

const EMULATOR_SLOT_LENGTH_MS = 1_000;

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
