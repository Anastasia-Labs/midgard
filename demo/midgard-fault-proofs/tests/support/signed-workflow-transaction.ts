import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";

import type { SignedWorkflowTransaction } from "../../src/workflow/signed-transaction-reconciliation.js";

/** Exact signed bytes of one wallet spend, as a workflow records its intent.
 * The wallet spends the emulator's genesis output `coin`, so fixtures with
 * different coins never spend the same outRef. */
export const signedWorkflowTransactionFixture = async ({
  ttl = 6000,
  coin = 0,
}: { readonly ttl?: number | null; readonly coin?: number } = {}) => {
  const accounts = Array.from({ length: coin + 1 }, () =>
    generateEmulatorAccount({ lovelace: 1_000_000_000n }),
  );
  const account = accounts[coin]!;
  const emulator = new Emulator(accounts);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const planned = lucid
    .newTx()
    .pay.ToAddress(account.address, { lovelace: 5_000_000n });
  if (ttl !== null) planned.validTo(lucid.slotToUnixTime(ttl));
  const signed = await (await planned.complete({ localUPLCEval: true })).sign
    .withWallet()
    .complete();
  const input: SignedWorkflowTransaction = {
    transactionHash: signed.toHash(),
    signedTransactionCborHex: signed.toTransaction().to_cbor_hex(),
  };
  return { input, signed };
};
