/**
 * The prover wallet of an installed-application run, as its operator moves
 * funds in and out of it between watcher turns.
 */
import {
  getAddressDetails,
  mintingPolicyToId,
  scriptFromNative,
} from "@lucid-evolution/lucid";

import type { InstalledTurnContext } from "./script-decode-installed-run.js";

type Harness = InstalledTurnContext["harness"];

const ADA = 1_000_000n;

/**
 * Leaves the prover wallet a collateral output and `fundingLovelace` of
 * plain funding, everything else going to the funder: the operator's wallet
 * as it is when it runs low.
 */
export const drainProver = async (
  harness: Harness,
  fundingLovelace: bigint,
) => {
  harness.emulator.awaitBlock();
  const lucid = harness.proverLucid;
  lucid.clearUTxOOverride();
  const plain = (await lucid.wallet().getUtxos()).filter(
    (utxo) =>
      utxo.datum == null && utxo.datumHash == null && utxo.scriptRef == null,
  );
  const drained = await lucid
    .newTx()
    .collectFrom(plain)
    .pay.ToAddress(harness.proverSigner.address, { lovelace: 2_000n * ADA })
    .pay.ToAddress(harness.proverSigner.address, { lovelace: fundingLovelace })
    .complete({
      presetWalletInputs: plain,
      coinSelection: false,
      changeAddress: await harness.funderLucid.wallet().address(),
    });
  await (await drained.sign.withWallet().complete()).submit();
  harness.emulator.awaitBlock();
};

/** The funder pays the prover wallet `lovelace`. */
export const topUpProver = async (harness: Harness, lovelace: bigint) => {
  const topUp = await harness.funderLucid
    .newTx()
    .pay.ToAddress(harness.proverSigner.address, { lovelace })
    .complete();
  await (await topUp.sign.withWallet().complete()).submit();
  harness.emulator.awaitBlock();
};

/**
 * The funder pays the prover wallet one coin of `lovelace` and a token;
 * returns the coin's out-ref.
 */
export const sendTokenCoin = async (harness: Harness, lovelace: bigint) => {
  const funder = harness.funderLucid;
  const policy = scriptFromNative({
    type: "sig",
    keyHash: getAddressDetails(await funder.wallet().address())
      .paymentCredential!.hash,
  });
  const unit = `${mintingPolicyToId(policy)}cafe`;
  const sent = await funder
    .newTx()
    .mintAssets({ [unit]: 1n })
    .attach.MintingPolicy(policy)
    .pay.ToAddress(harness.proverSigner.address, { lovelace, [unit]: 1n })
    .complete();
  const txHash = await (await sent.sign.withWallet().complete()).submit();
  harness.emulator.awaitBlock();
  const [coin] = (
    await harness.proverLucid.utxosAt(harness.proverSigner.address)
  ).filter((utxo) => utxo.txHash === txHash && utxo.assets[unit] === 1n);
  return { txHash, outputIndex: coin!.outputIndex };
};
