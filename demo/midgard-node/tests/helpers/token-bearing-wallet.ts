/**
 * Operator wallets whose outputs carry a foreign token, for the funding rows:
 * coin selection over the whole operator wallet view (plan §8.5) must fund
 * from them and return the tokens in change.
 */
import {
  generateEmulatorAccount,
  type LucidEvolution,
  mintingPolicyToId,
  paymentCredentialOf,
  scriptFromNative,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

export const lovelaceIn = (utxos: readonly UTxO[]): bigint =>
  utxos.reduce((total, utxo) => total + (utxo.assets.lovelace ?? 0n), 0n);

export const amountOf = (utxos: readonly UTxO[], unit: string): bigint =>
  utxos.reduce((total, utxo) => total + (utxo.assets[unit] ?? 0n), 0n);

export const isPlain = (utxo: UTxO): boolean =>
  Object.keys(utxo.assets).every((unit) => unit === "lovelace") &&
  utxo.scriptRef == null &&
  utxo.datum == null &&
  utxo.datumHash == null;

/**
 * Re-spends the whole wallet into `outputs` outputs that each carry a
 * foreign token (minted under a policy of the wallet's key, as the DA
 * scaffold's attestation token is) and share `keep` lovelace, the remainder
 * going to a sink. With `plainLovelace`, one plain-ADA output of that amount
 * is kept as well; without it the wallet is left without a single plain-ADA
 * output. Returns the token unit.
 */
export const makeTokenBearingWallet = async (
  lucid: LucidEvolution,
  {
    outputs = 3,
    keep,
    plainLovelace,
  }: {
    readonly outputs?: number;
    readonly keep?: bigint;
    readonly plainLovelace?: bigint;
  },
): Promise<string> => {
  const address = await lucid.wallet().address();
  const policy = scriptFromNative({
    type: "sig",
    keyHash: paymentCredentialOf(address)!.hash,
  });
  const unit = toUnit(mintingPolicyToId(policy), "00");
  const utxos = await lucid.utxosAt(address);
  const spendable =
    keep ?? lovelaceIn(utxos) - (plainLovelace ?? 0n) - 20_000_000n;
  let tx = lucid
    .newTx()
    .collectFrom(utxos)
    .mintAssets({ [unit]: BigInt(outputs) })
    .attach.MintingPolicy(policy);
  for (let index = 0; index < outputs; index += 1)
    tx = tx.pay.ToAddress(address, {
      lovelace: spendable / BigInt(outputs),
      [unit]: 1n,
    });
  if (plainLovelace !== undefined)
    tx = tx.pay.ToAddress(address, { lovelace: plainLovelace });
  const unsigned = await tx.complete({
    coinSelection: false,
    changeAddress: generateEmulatorAccount({ lovelace: 0n }).address,
    presetWalletInputs: utxos,
  });
  await lucid.awaitTx(
    await (await unsigned.sign.withWallet().complete()).submit(),
  );
  const after = await lucid.utxosAt(address);
  const plainOutputs = plainLovelace === undefined ? 0 : 1;
  expect(after.length).toBe(outputs + plainOutputs);
  expect(after.filter(isPlain).length).toBe(plainOutputs);
  return unit;
};
