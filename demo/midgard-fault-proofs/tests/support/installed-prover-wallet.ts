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
 * The most collateral any of the watcher's transactions may post on the
 * emulator deployment (its slash penalty's collateral): the plain ada the
 * prover wallet must hold in its largest coins before the watcher reserves
 * any of it.
 */
export const COLLATERAL_LINE = 750n * ADA;

/** Leaves the prover wallet one plain coin of each of `keep`, everything
 * else going to the funder. */
const drainProverTo = async (harness: Harness, keep: readonly bigint[]) => {
  harness.emulator.awaitBlock();
  const lucid = harness.proverLucid;
  lucid.clearUTxOOverride();
  // Every coin but a reference-script one; a refund's tag datum is money.
  const plain = (await lucid.wallet().getUtxos()).filter(
    (utxo) => utxo.scriptRef == null,
  );
  let draining = lucid.newTx().collectFrom(plain);
  for (const lovelace of keep)
    draining = draining.pay.ToAddress(harness.proverSigner.address, {
      lovelace,
    });
  const drained = await draining.complete({
    presetWalletInputs: plain,
    coinSelection: false,
    changeAddress: await harness.funderLucid.wallet().address(),
  });
  await (await drained.sign.withWallet().complete()).submit();
  harness.emulator.awaitBlock();
};

/**
 * Leaves the prover wallet a 2,000 ada coin and `fundingLovelace` of plain
 * funding, everything else going to the funder: a wallet low on funds, still
 * above the collateral line, whose large coin any transaction may spend.
 */
export const drainProver = async (harness: Harness, fundingLovelace: bigint) =>
  await drainProverTo(harness, [2_000n * ADA, fundingLovelace]);

/**
 * Leaves the prover wallet `lovelace` of plain ada in one coin, everything
 * else going to the funder. Below {@link COLLATERAL_LINE} the watcher
 * reserves nothing from it until a top-up.
 */
export const drainProverToOneCoin = async (
  harness: Harness,
  lovelace: bigint,
) => await drainProverTo(harness, [lovelace]);

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
