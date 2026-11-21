import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
/**
 * Builds a Lucid instance for commit-end-time tests, its chain `chainAgeSlots`
 * old: a lower bound backdated from the tip never reaches before genesis.
 */
export const makeLucid = async (chainAgeSlots = 0) => {
  const operator = generateEmulatorAccount({ lovelace: 50_000_000n });
  const emulator = new Emulator([operator]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);
  if (chainAgeSlots > 0) emulator.awaitSlot(chainAgeSlots);
  return lucid;
};

export const currentSlotStartMs = (
  lucid: Awaited<ReturnType<typeof makeLucid>>,
) => lucid.slotToUnixTime(lucid.currentSlot());
