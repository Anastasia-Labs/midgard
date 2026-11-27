/**
 * A command that pays from a wallet refuses the running node's operational
 * wallets as payer: the operator's main and merge wallets, the reference
 * script wallet, and the deployment's reference-script deploy address. One
 * check for every command (it began as the availability command's actor
 * check): a payer is refused when its payment credential is one of theirs,
 * and a payer seed read from an environment variable is refused when that
 * variable is one of the node's own seed settings.
 *
 * The deploy flow (nonce, reference scripts, init, operator registration)
 * pays from the operator's wallets on purpose and does not take this check.
 */
import type * as LE from "@lucid-evolution/lucid";
import { paymentCredentialOf, walletFromSeed } from "@lucid-evolution/lucid";

/** The node's operational seed settings, by wallet role. */
export const OPERATIONAL_WALLET_SEED_ENV = {
  "operator-main": "L1_OPERATOR_SEED_PHRASE",
  "operator-merge": "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
  "reference-scripts": "L1_REFERENCE_SCRIPT_SEED_PHRASE",
} as const;

export type OperationalWallet = Readonly<{ role: string; address: string }>;

/** A command was asked to pay from one of the node's operational wallets. */
export class OperationalWalletPayerRefusedError extends Error {
  override readonly name = "OperationalWalletPayerRefusedError";
  readonly reason = "payer_is_operational_wallet";
  constructor(
    readonly command: string,
    readonly roles: readonly string[],
    detail: string,
  ) {
    super(
      `${command} refuses the node's operational wallet as payer (${roles.join(",")}): ${detail}; pay from a dedicated wallet`,
    );
  }
}

/** The operational wallets whose seeds `env` carries, on `network`. */
export const operationalWalletsFromEnv = (
  env: NodeJS.ProcessEnv,
  network: LE.Network,
): OperationalWallet[] =>
  Object.entries(OPERATIONAL_WALLET_SEED_ENV).flatMap(([role, name]) => {
    const seed = env[name]?.trim();
    return seed
      ? [{ role, address: walletFromSeed(seed, { network }).address }]
      : [];
  });

/** Refuses a payer seed read from one of the node's own seed settings. */
export const assertPayerSeedEnvIsDedicated = (
  command: string,
  seedEnvName: string,
): void => {
  const role = Object.entries(OPERATIONAL_WALLET_SEED_ENV).find(
    ([, name]) => name === seedEnvName,
  )?.[0];
  if (role !== undefined)
    throw new OperationalWalletPayerRefusedError(
      command,
      [role],
      `the payer seed comes from ${seedEnvName}, the node's own seed setting`,
    );
};

const paymentHashOf = (address: string): string =>
  paymentCredentialOf(address).hash;

/** Refuses `payerAddress` when its payment credential is an operational one. */
export const assertPayerIsNotOperationalWallet = (input: {
  readonly command: string;
  readonly payerAddress: string;
  readonly operational: readonly OperationalWallet[];
}): void => {
  const payer = paymentHashOf(input.payerAddress);
  const roles = input.operational
    .filter(({ address }) => paymentHashOf(address) === payer)
    .map(({ role }) => role);
  if (roles.length > 0)
    throw new OperationalWalletPayerRefusedError(
      input.command,
      roles,
      `payer ${input.payerAddress} shares their payment credential`,
    );
};

/**
 * The whole check for a command paying from a seed read from
 * `walletSeedEnv`: the variable is not one of the node's seed settings, and
 * the payer is none of the wallets those settings hold nor the deployment's
 * reference-script deploy address. Runs before the command opens any L1.
 */
export const assertCommandPayerIsDedicated = (input: {
  readonly command: string;
  readonly walletSeedEnv: string;
  readonly payerAddress: string;
  readonly referenceScriptDeployAddress: string;
  readonly network: LE.Network;
  readonly env: NodeJS.ProcessEnv;
}): void => {
  assertPayerSeedEnvIsDedicated(input.command, input.walletSeedEnv);
  assertPayerIsNotOperationalWallet({
    command: input.command,
    payerAddress: input.payerAddress,
    operational: [
      ...operationalWalletsFromEnv(input.env, input.network),
      {
        role: "reference-script-deploy",
        address: input.referenceScriptDeployAddress,
      },
    ],
  });
};
