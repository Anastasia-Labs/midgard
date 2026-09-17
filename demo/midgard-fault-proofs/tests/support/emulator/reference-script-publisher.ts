import {
  createReferenceScriptAuthPolicy,
  type MidgardValidators,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

/** The deployment funding wallet, independent of every fault-proof signer. */
export type ReferenceScriptPublisher = Readonly<{
  lucid: LucidEvolution;
  reservedInputs: readonly UTxO[];
}>;

export type ReferenceScriptPublishingContracts = MidgardValidators & {
  readonly referenceScriptPublisher?: ReferenceScriptPublisher;
};

/** Reserve the contract nonce before the funding wallet publishes references. */
export const createReferenceScriptPublisher = async (
  lucid: LucidEvolution,
  now: number,
) => {
  const nonceLovelace = 10_000_000n;
  const address = await lucid.wallet().address();
  const unsigned = await lucid
    .newTx()
    .pay.ToAddress(address, { lovelace: nonceLovelace })
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  const nonceCandidates = (await lucid.wallet().getUtxos()).filter(
    (utxo) =>
      utxo.txHash === txHash &&
      utxo.assets.lovelace === nonceLovelace &&
      Object.keys(utxo.assets).length === 1,
  );
  if (nonceCandidates.length !== 1) {
    throw new Error("Expected exactly one reserved deployment nonce output");
  }
  const nonceUtxo = nonceCandidates[0]!;
  return {
    nonceUtxo,
    referenceScriptAuth: await createReferenceScriptAuthPolicy(lucid, now),
    referenceScriptPublisher: {
      lucid,
      reservedInputs: [nonceUtxo],
    } satisfies ReferenceScriptPublisher,
  };
};
