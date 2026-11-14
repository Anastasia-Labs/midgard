import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";

/**
 * The emulator's own refusal of a second spend of an input a pending
 * transaction already holds: the ledger answer a submission sees while an
 * earlier attempt over the same inputs is in flight.
 */
export const emulatorDoubleSpendRefusal = async (): Promise<unknown> => {
  const account = generateEmulatorAccount({ lovelace: 100_000_000n });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const [input] = await lucid.wallet().getUtxos();
  const pay = async (lovelace: bigint) =>
    (
      await (
        await lucid
          .newTx()
          .collectFrom([input!])
          .pay.ToAddress(account.address, { lovelace })
          .complete({ coinSelection: false })
      ).sign
        .withWallet()
        .complete()
    ).toCBOR();
  const [first, second] = [await pay(2_000_000n), await pay(3_000_000n)];
  await lucid.wallet().submitTx(first);
  return await lucid
    .wallet()
    .submitTx(second)
    .then(
      () => {
        throw new Error("The emulator accepted a double spend.");
      },
      (error: unknown) => error,
    );
};
