import {
  createReferenceScriptAuthPolicy,
  type MidgardValidators,
} from "@al-ft/midgard-sdk";
import {
  type Emulator,
  type EmulatorAccount,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";

/**
 * The reference-script wallet: it holds the authentication policy key, pays
 * every authenticated publication, and is a different wallet from the funder
 * (the operator) and every fault-proof signer.
 */
export type ReferenceScriptPublisher = Readonly<{
  lucid: LucidEvolution;
}>;

export type ReferenceScriptPublishingContracts = MidgardValidators & {
  readonly referenceScriptPublisher?: ReferenceScriptPublisher;
};

/** The reference-script wallet's genesis account, unless the caller fixes one. */
export const referenceScriptPublisherAccount = (
  account?: EmulatorAccount,
): EmulatorAccount =>
  account ?? generateEmulatorAccount({ lovelace: 40_000_000_000n });

/**
 * Reserve the contract nonce in the funder (operator) wallet, and bind the
 * reference-script authentication policy to the separate `publisher` genesis
 * account, which then pays every authenticated publication.
 *
 * The nonce stays with the funder because deployment setup spends it from
 * there; the publisher wallet holds no reserved input. The policy's timelock
 * is anchored at the emulator time this is called at, as before the nonce.
 * Publication transactions carry no Plutus script, so the publisher's Lucid
 * needs no custom evaluator. The auth policy expires `timelockMs` after that
 * time, by default the SDK's.
 */
export const createReferenceScriptPublisher = async (
  funderLucid: LucidEvolution,
  publisher: EmulatorAccount,
  emulator: Emulator,
  timelockMs?: number,
) => {
  const now = emulator.now();
  const publisherLucid = await Lucid(emulator, "Custom");
  publisherLucid.selectWallet.fromSeed(publisher.seedPhrase);
  const funderAddress = await funderLucid.wallet().address();
  if (funderAddress === (await publisherLucid.wallet().address())) {
    throw new Error(
      "The reference-script publisher must be a different wallet from the funder",
    );
  }
  const nonceLovelace = 10_000_000n;
  const unsigned = await funderLucid
    .newTx()
    .pay.ToAddress(funderAddress, { lovelace: nonceLovelace })
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await funderLucid.awaitTx(txHash);
  const nonceCandidates = (await funderLucid.wallet().getUtxos()).filter(
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
    referenceScriptAuth: await createReferenceScriptAuthPolicy(
      publisherLucid,
      now,
      timelockMs,
    ),
    referenceScriptPublisher: {
      lucid: publisherLucid,
    } satisfies ReferenceScriptPublisher,
  };
};
