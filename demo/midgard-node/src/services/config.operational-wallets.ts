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
import { createHash } from "node:crypto";

import type { Network } from "@lucid-evolution/lucid";
import { paymentCredentialOf, walletFromSeed } from "@lucid-evolution/lucid";
import { Config } from "effect";

/**
 * Wallet derivation runs bip39 PBKDF2 in wasm, tens of milliseconds a call,
 * and every NodeConfig load derives up to four wallets. Hosts that rebuild
 * the config layer per effect (commands, tests) paid that on every run. The
 * derived address is a pure function of (network, phrase), so it is memoized
 * under a SHA-256 of that pair: the cache holds no seed material, only public
 * addresses. A failed derivation is not cached and throws again every load.
 */
const seedAddresses = new Map<string, string>();
const SEED_ADDRESS_CACHE_LIMIT = 64;

export const seedPhraseAddress = (
  seedPhrase: string,
  network: Network,
): string => {
  const key = createHash("sha256")
    .update(network)
    .update("\0")
    .update(seedPhrase)
    .digest("hex");
  const cached = seedAddresses.get(key);
  if (cached !== undefined) return cached;
  const { address } = walletFromSeed(seedPhrase, { network });
  if (seedAddresses.size >= SEED_ADDRESS_CACHE_LIMIT) seedAddresses.clear();
  seedAddresses.set(key, address);
  return address;
};

const validateSeedPhrase = (name: string, value: string): string => {
  const trimmed = value.trim();
  if (trimmed.length === 0) {
    throw new Error(
      `${name} is required: set a wallet seed phrase in .env (see the "Required settings" block at the top of .env.example).`,
    );
  }
  try {
    // Address prefix depends on the network, mnemonic validity does not, so a
    // fixed network is enough to reject a malformed phrase at config load.
    seedPhraseAddress(trimmed, "Preprod");
  } catch (cause) {
    throw new Error(
      `${name} is not a valid wallet seed phrase: ${String(cause)}`,
    );
  }
  return trimmed;
};

/**
 * Reads a wallet seed phrase that the node cannot start without. Fails config
 * load with the variable named and the fix stated, instead of letting the
 * wallet derivation surface a bare bip39 "invalid mnemonic" defect for an
 * empty `.env` value.
 */
export const requiredSeedPhrase = (name: string) =>
  Config.string(name).pipe(
    Config.mapAttempt((value) => validateSeedPhrase(name, value)),
  );

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
