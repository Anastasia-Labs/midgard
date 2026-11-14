import {
  calculateMinLovelaceFromUTxO,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import { tokenChangeMinLovelace } from "./operators/funding-preflight.js";
import { utxoOutRefKey } from "./reference-scripts.fetch-reference-script-utxos-program.js";

/**
 * A stake registration found too little to fund it among the outputs it may
 * spend: the funding wallet's outputs less reference-script and datum-bearing
 * ones, and less any the caller reserves.
 */
export class RewardRegistrationUnfunded extends Error {
  override readonly name = "RewardRegistrationUnfunded";
}

/**
 * Builds one transaction registering every reward address in
 * `rewardAddresses`, funded from `funding.utxos` and returning the change to
 * `funding.address`.
 *
 * Lucid's coin selection (0.8.3) leaves certificate deposits out of what it
 * must cover, so the inputs are selected here: the largest outputs first,
 * until they cover the deposits, a fee allowance for a transaction of the
 * maximum size and the change. The change returns every selected token, so
 * it needs their minimum ADA rather than a plain output's. A fragmented
 * wallet is not collected whole. Reference-script and datum-bearing outputs
 * are never spent; an output the caller reserves is left out of `funding`.
 * Revisit this if a later Lucid counts the deposits.
 *
 * Throws `RewardRegistrationUnfunded` when the outputs it may spend cannot
 * cover the registrations.
 */
export const buildRewardRegistrationsTx = async (
  lucid: LucidEvolution,
  funding: { readonly address: string; readonly utxos: readonly UTxO[] },
  rewardAddresses: readonly string[],
  label: string,
): Promise<TxSignBuilder> => {
  const inputs = funding.utxos.filter(
    (utxo) =>
      utxo.scriptRef == null && utxo.datum == null && utxo.datumHash == null,
  );
  if (inputs.length === 0)
    throw new RewardRegistrationUnfunded(
      `No output can fund ${label} once reference-script, datum-bearing and reserved outputs are left out`,
    );
  const parameters = lucid.config().protocolParameters;
  if (parameters === undefined)
    throw new Error(`Funding ${label} requires protocol parameters`);
  const plainChange = calculateMinLovelaceFromUTxO(
    parameters.coinsPerUtxoByte,
    {
      txHash: "00".repeat(32),
      outputIndex: 0,
      address: funding.address,
      assets: { lovelace: 0xffffffffffffffffn },
    },
  );
  const depositsAndFee =
    BigInt(rewardAddresses.length) * parameters.keyDeposit +
    BigInt(parameters.minFeeA) * BigInt(parameters.maxTxSize) +
    BigInt(parameters.minFeeB);
  const budgetFor = (selection: readonly UTxO[]) => {
    const tokenChange = tokenChangeMinLovelace(
      lucid,
      funding.address,
      selection,
    );
    return (
      depositsAndFee + (tokenChange > plainChange ? tokenChange : plainChange)
    );
  };
  const selected: UTxO[] = [];
  let total = 0n;
  for (const input of inputs.sort((left, right) => {
    const delta = (right.assets.lovelace ?? 0n) - (left.assets.lovelace ?? 0n);
    return delta === 0n
      ? left.txHash.localeCompare(right.txHash) ||
          left.outputIndex - right.outputIndex
      : delta > 0n
        ? 1
        : -1;
  })) {
    selected.push(input);
    total += input.assets.lovelace ?? 0n;
    if (total >= budgetFor(selected)) break;
  }
  if (total < budgetFor(selected))
    throw new RewardRegistrationUnfunded(
      `Too little funding for ${label}: ${total.toString()} lovelace may be spent, ${budgetFor(selected).toString()} needed for the deposits, the fee and the change`,
    );
  const completed = await rewardAddresses
    .reduce(
      (builder, rewardAddress) => builder.register.Stake(rewardAddress),
      lucid.newTx().collectFrom(selected),
    )
    .complete({
      coinSelection: false,
      presetWalletInputs: selected,
      localUPLCEval: true,
    });
  const allowed = new Set(selected.map(utxoOutRefKey));
  const consumed = completed.toTransaction().body().inputs();
  for (let index = 0; index < consumed.len(); index += 1) {
    const input = consumed.get(index);
    if (!allowed.has(`${input.transaction_id().to_hex()}#${input.index()}`))
      throw new Error(`The build of ${label} spent an input it did not select`);
  }
  return completed;
};
