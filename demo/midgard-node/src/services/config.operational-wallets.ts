/**
 * Config load refuses a wallet topology in which two of the node's wallet
 * roles are one wallet. The operator, merge, settlement (when set) and
 * reference-script roles each keep a local wallet view of their own; one
 * wallet under two roles would let one role spend an output the other role's
 * view still counts as its own.
 *
 * Wallets are compared by what the seed phrases derive, the payment key hash
 * and the address, never by the phrase text: two spellings that derive one
 * key are one wallet.
 *
 * The reference-script deploy address may be the reference-script wallet
 * (the default, for reclaimable test deployments) or a non-spendable address
 * (production), but never the operator, merge or settlement wallet.
 */
import type { Network } from "@lucid-evolution/lucid";
import { paymentCredentialOf } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { seedPhraseAddress } from "./config.node-config-dep.js";

/** The seed settings of the node's wallet roles. */
export type OperationalWalletSeeds = Readonly<{
  L1_OPERATOR_SEED_PHRASE: string;
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: string;
  /** Empty when the settlement worker has no wallet configured. */
  L1_SETTLEMENT_SEED_PHRASE: string;
  L1_REFERENCE_SCRIPT_SEED_PHRASE: string;
}>;

type WalletIdentity = Readonly<{
  setting: string;
  address: string;
  paymentKeyHash: string;
}>;

const identityOf = (
  setting: string,
  seedPhrase: string,
  network: Network,
): WalletIdentity => {
  let address: string;
  try {
    address = seedPhraseAddress(seedPhrase, network);
  } catch (cause) {
    throw new Error(
      `${setting} is not a valid wallet seed phrase: ${String(cause)}`,
    );
  }
  return {
    setting,
    address,
    paymentKeyHash: paymentCredentialOf(address).hash,
  };
};

const sameWallet = (left: WalletIdentity, right: WalletIdentity): boolean =>
  left.paymentKeyHash === right.paymentKeyHash ||
  left.address === right.address;

/** The derived identities of every configured wallet role. */
const operationalWalletIdentities = (
  seeds: OperationalWalletSeeds,
  network: Network,
): WalletIdentity[] =>
  (
    [
      "L1_OPERATOR_SEED_PHRASE",
      "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
      "L1_SETTLEMENT_SEED_PHRASE",
      "L1_REFERENCE_SCRIPT_SEED_PHRASE",
    ] as const
  ).flatMap((setting) => {
    const seedPhrase = seeds[setting].trim();
    return seedPhrase === "" ? [] : [identityOf(setting, seedPhrase, network)];
  });

/**
 * Refuses any two wallet roles that derive one wallet, and a deploy address
 * that is the operator, merge or settlement wallet.
 */
export const assertDistinctOperationalWallets = (input: {
  readonly seeds: OperationalWalletSeeds;
  readonly referenceScriptDeployAddress: string;
  readonly network: Network;
}): void => {
  const wallets = operationalWalletIdentities(input.seeds, input.network);
  for (const [index, left] of wallets.entries())
    for (const right of wallets.slice(index + 1))
      if (sameWallet(left, right))
        throw new Error(
          `${left.setting} and ${right.setting} derive the same wallet (payment key hash ${left.paymentKeyHash}); each wallet role needs a wallet of its own`,
        );
  const deployAddress = input.referenceScriptDeployAddress.trim();
  let deployCredentialHash: string | undefined;
  try {
    deployCredentialHash = paymentCredentialOf(deployAddress).hash;
  } catch {
    deployCredentialHash = undefined;
  }
  const deployWallet = wallets.find(
    (wallet) =>
      wallet.setting !== "L1_REFERENCE_SCRIPT_SEED_PHRASE" &&
      (wallet.address === deployAddress ||
        wallet.paymentKeyHash === deployCredentialHash),
  );
  if (deployWallet !== undefined)
    throw new Error(
      `L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS is the ${deployWallet.setting} wallet; set it to the reference-script wallet or to a non-spendable address`,
    );
};

/** `assertDistinctOperationalWallets` as a config-load step. */
export const refuseSharedOperationalWallets = (
  input: OperationalWalletSeeds & {
    readonly L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: string;
    readonly NETWORK: Network;
  },
): Effect.Effect<void, Error> =>
  Effect.try({
    try: () =>
      assertDistinctOperationalWallets({
        seeds: input,
        referenceScriptDeployAddress: input.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
        network: input.NETWORK,
      }),
    catch: (cause) => cause as Error,
  });
