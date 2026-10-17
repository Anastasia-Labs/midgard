import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Lucid,
  type LucidEvolution,
  type Network,
  toUnit,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";

import type { L1Access, L1ViewPoint } from "../l1-access.js";
import {
  authenticatedManifestReference,
  availabilityParametersFromManifest,
  manifestReferenceScriptAuthPolicy,
  mintingValidatorOf,
  spendingValidatorOf,
} from "./availability-challenge-deployment.js";
import { readDeploymentManifestFile } from "./contract-deployment-info.js";
import {
  daBondChainSubmit,
  type DaBondContext,
  type DaBondStatus,
  daBondStatusCommand,
  parseLovelace,
} from "./da-bond.da-bond-context.js";
import {
  daBondAssembleCommand,
  type DaBondChainOptions,
  daBondNetwork,
  DaBondSlotMappingError,
  daBondWithdrawBuildCommand,
} from "./da-bond.da-bond-withdraw-build-command.js";
import {
  daBondTopUpCommand,
  type DaBondWithdrawBuildOptions,
  type DaBondWithdrawStep,
} from "./da-bond.refusal-message.js";
import {
  daBondSigningKeyFromSecret,
  readDaBondSecretEnv,
} from "./da-bond-files.js";
import { withCommandL1Access } from "./l1-command-access.js";
import { assertCommandPayerIsDedicated } from "./operational-wallet-refusal.js";

/**
 * The part of a tool L1 access (`l1-command-access.ts`) the da-bond commands
 * read: any of the tool adapters, each with its ledger tip.
 */
export type DaBondL1Access = Pick<
  L1Access,
  "provider" | "endpoint" | "slotConfig"
> &
  Readonly<{ ledgerTip: () => Promise<L1ViewPoint> }>;

/**
 * Lucid on the deployment's network, with the slot mapping the access gives
 * (the local node's ledger: its system start and era history), exactly as
 * the node takes it (`services/lucid.ts`). Never a configured `zeroTime`: a wrong one shifts
 * every validity interval, and the pool's `unlock_at` is anchored at one. A
 * failed read stops the command before Lucid is built.
 */
export const daBondLucid = async (input: {
  readonly access: DaBondL1Access;
  readonly network: Network;
}): Promise<LucidEvolution> => {
  let slotConfig;
  try {
    slotConfig = await input.access.slotConfig();
  } catch (cause) {
    throw new DaBondSlotMappingError(
      `Refusing the deployment: the slot mapping from the L1 access at ${input.access.endpoint} failed: ${cause instanceof Error ? cause.message : String(cause)}`,
      { cause },
    );
  }
  return Lucid(input.access.provider, input.network, {
    evaluator: createScalusEvaluator(),
    slotConfig,
  });
};

/**
 * The time of the access's ledger tip slot: the clock the node checks
 * validity bounds against.
 */
export const daBondLedgerTimeMs = async (
  lucid: LucidEvolution,
  access: Pick<DaBondL1Access, "ledgerTip">,
): Promise<number> => lucid.slotToUnixTime((await access.ledgerTip()).slot);

/**
 * `da-bond top-up` never pays from the node's operational wallets
 * (`operational-wallet-refusal.ts`): the funding secret's variable is not
 * one of the node's seed settings, and the wallet it selects (the payment
 * key's enterprise address, or the mnemonic's base address as
 * `daBondTopUpCommand` selects it) shares no payment credential with them
 * or with the deployment's reference-script deploy address.
 */
export const assertDaBondPayerIsDedicated = (input: {
  readonly walletSeedEnv: string;
  readonly walletSecret: string;
  readonly referenceScriptDeployAddress: string;
  readonly network: Network;
  readonly env: NodeJS.ProcessEnv;
}): void => {
  const secret = input.walletSecret.trim();
  const payerAddress =
    secret.startsWith("ed25519_sk1") || secret.startsWith("ed25519e_sk1")
      ? credentialToAddress(input.network, {
          type: "Key",
          hash: CML.PrivateKey.from_bech32(secret).to_public().hash().to_hex(),
        })
      : walletFromSeed(secret, { network: input.network }).address;
  assertCommandPayerIsDedicated({
    command: "da-bond top-up",
    walletSeedEnv: input.walletSeedEnv,
    payerAddress,
    referenceScriptDeployAddress: input.referenceScriptDeployAddress,
    network: input.network,
    env: input.env,
  });
};

const readVerifiedManifest = (path: string) => {
  const manifest = readDeploymentManifestFile(path);
  verifyFinalizedDeploymentManifest(manifest);
  return manifest;
};

const daBondContextFrom = async (
  manifest: ReturnType<typeof readVerifiedManifest>,
  access: DaBondL1Access,
): Promise<DaBondContext> => {
  const lucid = await daBondLucid({
    access,
    network: daBondNetwork(manifest.network),
  });
  const network = lucid.config().network;
  if (network === undefined || network !== manifest.network) {
    throw new Error("da-bond network differs from the verified deployment");
  }
  const authPolicy = manifestReferenceScriptAuthPolicy(manifest);
  const spending = await authenticatedManifestReference(
    lucid,
    manifest,
    authPolicy,
    "daBondPoolSpend",
    "da-bond-pool spending",
  );
  const minting = await authenticatedManifestReference(
    lucid,
    manifest,
    authPolicy,
    "daBondPoolMint",
    "da-bond-pool minting",
  );
  const governorSpend = manifest.contracts.daParamsGovernorSpend?.scriptHash;
  const governorMint = manifest.contracts.daParamsGovernorMint?.scriptHash;
  if (governorSpend === undefined || governorMint === undefined) {
    throw new Error("Deployment omits the DA params governor");
  }
  const ledgerTimeMs = await daBondLedgerTimeMs(lucid, access);
  return {
    lucid,
    network,
    manifestId: manifest.manifestId,
    poolValidator: {
      ...spendingValidatorOf(network, spending.scriptRef),
      ...mintingValidatorOf(minting.scriptRef),
    },
    poolSpendingReference: spending,
    parameters: availabilityParametersFromManifest(manifest),
    daParamsGovernor: {
      address: credentialToAddress(network, {
        type: "Script",
        hash: governorSpend,
      }),
      unit: toUnit(governorMint, SDK.DA_PARAMS_ASSET_NAME),
    },
    withdrawDelayMs: BigInt(
      manifest.deploymentProfile.timing.da_bond_withdraw_delay_ms,
    ),
    now: () => ledgerTimeMs,
    submit: daBondChainSubmit(access.provider, lucid),
  };
};

/**
 * The production context: a verified finalized manifest, the node's L1
 * access, and only the references these commands read, each authenticated:
 * the pool's reference scripts (manifest role outputs carrying their
 * reference-script-auth token) and the DA params governor's address and NFT.
 */
export const loadDaBondContext = (
  options: DaBondChainOptions,
  access: DaBondL1Access,
): Promise<DaBondContext> =>
  daBondContextFrom(readVerifiedManifest(options.manifest), access);

/**
 * Runs `use` with the production context over the tool L1 access `--l1`
 * selects, on the manifest's network (`l1-command-access.ts`), closing it
 * afterwards.
 */
const withDaBondContext = async <T>(
  options: DaBondChainOptions,
  env: NodeJS.ProcessEnv,
  use: (ctx: DaBondContext) => Promise<T>,
  /** A refusal checked on the verified manifest, before any L1 read. */
  guard?: (
    manifest: ReturnType<typeof readVerifiedManifest>,
    network: Network,
  ) => void,
): Promise<T> => {
  const manifest = readVerifiedManifest(options.manifest);
  const network = daBondNetwork(manifest.network);
  guard?.(manifest, network);
  return withCommandL1Access({ network, env }, async (access) =>
    use(await daBondContextFrom(manifest, access)),
  );
};

/** `da-bond top-up` from the CLI: the funding secret comes from one env var. */
export const runDaBondTopUp = async (
  options: DaBondChainOptions & { amount: string; walletSeedEnv: string },
  env: NodeJS.ProcessEnv = process.env,
) => {
  const walletSecret = readDaBondSecretEnv(
    env,
    options.walletSeedEnv,
    "--wallet-seed-env",
  );
  daBondSigningKeyFromSecret(walletSecret);
  parseLovelace(options.amount, "--amount");
  return withDaBondContext(
    options,
    env,
    (ctx) => daBondTopUpCommand(ctx, { amount: options.amount, walletSecret }),
    (manifest, network) =>
      assertDaBondPayerIsDedicated({
        walletSeedEnv: options.walletSeedEnv,
        walletSecret,
        referenceScriptDeployAddress: manifest.referenceScriptDeployAddress,
        network,
        env,
      }),
  );
};

/** `da-bond status` from the CLI. */
export const runDaBondStatus = async (
  options: DaBondChainOptions,
  env: NodeJS.ProcessEnv = process.env,
): Promise<DaBondStatus> =>
  withDaBondContext(options, env, daBondStatusCommand);

/** `da-bond withdraw begin|cancel|complete --build-unsigned` from the CLI. */
export const runDaBondWithdrawBuild = async (
  step: DaBondWithdrawStep,
  options: DaBondChainOptions & DaBondWithdrawBuildOptions,
  env: NodeJS.ProcessEnv = process.env,
) =>
  withDaBondContext(options, env, (ctx) =>
    daBondWithdrawBuildCommand(ctx, step, options),
  );

/** `da-bond assemble` from the CLI. */
export const runDaBondAssemble = async (
  options: DaBondChainOptions,
  unsignedPath: string,
  witnessPaths: readonly string[],
  env: NodeJS.ProcessEnv = process.env,
) =>
  withDaBondContext(options, env, (ctx) =>
    daBondAssembleCommand(ctx, unsignedPath, witnessPaths),
  );
