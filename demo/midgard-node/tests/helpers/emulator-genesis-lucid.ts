import { type Emulator, Lucid } from "@lucid-evolution/lucid";

/**
 * A Lucid client over `emulator` with the emulator's genesis slot mapping.
 * By default an emulator client's first slot is the slot it was created at,
 * so a lower bound backdated from the tip of a chain it joined moments ago
 * is out of its range: the bound would clamp to the tip, and local script
 * evaluation refuses an earlier one as too far in the past. The mapping is
 * the same line either way; only its first slot moves back to genesis.
 */
export const emulatorGenesisLucid = (emulator: Emulator) =>
  Lucid(emulator, "Custom", {
    slotConfig: {
      zeroTime: emulator.now() - emulator.slot * 1000,
      zeroSlot: 0,
      slotLength: 1000,
    },
  });
