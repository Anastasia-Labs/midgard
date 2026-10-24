import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
/**
 * Builds a Lucid instance for commit-end-time tests.
 */
export const makeLucid = async () => {
  const operator = generateEmulatorAccount({ lovelace: 50_000_000n });
  const emulator = new Emulator([operator]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);
  return lucid;
};

export const currentSlotStartMs = (
  lucid: Awaited<ReturnType<typeof makeLucid>>,
) => lucid.slotToUnixTime(lucid.currentSlot());
