import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  normalizeOgmiosHttpUrl,
  parseOgmiosTipSlot,
} from "@al-ft/midgard-core/ogmios-slot";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Lucid,
  type LucidEvolution,
  type Network,
  type Provider,
  type SlotConfig,
  toUnit,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";

import {
  fetchLocalOgmiosShelleyGenesisSlotConfig,
  fetchLocalOgmiosSubmitSlotSnapshot,
} from "../l1-heads.js";
import { customSlotConfigFromShelleyGenesis } from "../lucid-time.js";
import {
  makeNodeKupmios,
  nativeLedgerSettingsFromEnv,
} from "../services/native-ledger.js";
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
  DaBondCustomSlotMappingError,
  daBondNetwork,
  daBondWithdrawBuildCommand,
  runOrThrow,
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
import { resolveKupmiosUrls } from "./l1-utxos.js";

/**
 * The `Custom` slot mapping, derived exactly as the node derives it
 * (`services/lucid.ts`): a live submit-slot snapshot and the Shelley genesis
 * from the local Ogmios, checked against each other. Never a configured
 * `zeroTime`: a wrong one shifts every validity interval, and the pool's
 * `unlock_at` is anchored at one.
 */
const daBondCustomSlotConfig = async (
  ogmiosUrl: string,
): Promise<SlotConfig> => {
  const stage = async <A>(label: string, run: () => Promise<A>) => {
    try {
      return await run();
    } catch (cause) {
      throw new DaBondCustomSlotMappingError(
        `Refusing the Custom deployment: ${label} from the local Ogmios at ${ogmiosUrl} failed: ${cause instanceof Error ? cause.message : String(cause)}`,
        { cause },
      );
    }
  };
  const snapshot = await stage("the submit-slot snapshot", () =>
    runOrThrow(fetchLocalOgmiosSubmitSlotSnapshot({ ogmiosUrl })),
  );
  const genesis = await stage("the Shelley genesis query", () =>
    runOrThrow(fetchLocalOgmiosShelleyGenesisSlotConfig({ ogmiosUrl })),
  );
  return stage("the slot mapping check", async () =>
    customSlotConfigFromShelleyGenesis(genesis, snapshot),
  );
};

/**
 * Lucid on the deployment's network. Mainnet, Preprod and Preview keep
 * Lucid's built-in slot mapping and never query Ogmios for it; `Custom` has
 * none built in and takes `daBondCustomSlotConfig`'s, or the command stops
 * before Lucid is built.
 */
export const daBondLucid = async (input: {
  readonly provider: Provider;
  readonly network: Network;
  readonly ogmiosUrl: string;
}): Promise<LucidEvolution> => {
  const slotConfig =
    input.network === "Custom"
      ? await daBondCustomSlotConfig(input.ogmiosUrl)
      : undefined;
  return Lucid(input.provider, input.network, {
    evaluator: createScalusEvaluator(),
    ...(slotConfig === undefined ? {} : { slotConfig }),
  });
};

/**
 * The time of the local node's tip slot, read from its Ogmios: the clock the
 * node checks validity bounds against.
 */
export const daBondLedgerTimeMs = async (
  lucid: LucidEvolution,
  ogmiosUrl: string,
  fetchImpl: typeof fetch = fetch,
): Promise<number> => {
  const response = await fetchImpl(normalizeOgmiosHttpUrl(ogmiosUrl), {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      jsonrpc: "2.0",
      method: "queryNetwork/tip",
      id: "midgard-da-bond-ledger-time",
    }),
    signal: AbortSignal.timeout(10_000),
  });
  if (!response.ok) {
    throw new Error(
      `The local Ogmios tip query at ${ogmiosUrl} failed: HTTP ${response.status.toString()}`,
    );
  }
  return lucid.slotToUnixTime(parseOgmiosTipSlot(await response.json()));
};

/**
 * The production context: a verified finalized manifest, local Kupmios, and
 * only the references these commands read, each authenticated: the pool's
 * reference scripts (manifest role outputs carrying their
 * reference-script-auth token) and the DA params governor's address and NFT.
 */
export const loadDaBondContext = async (
  options: DaBondChainOptions,
  env: NodeJS.ProcessEnv = process.env,
): Promise<DaBondContext> => {
  const manifest = readDeploymentManifestFile(options.manifest);
  verifyFinalizedDeploymentManifest(manifest);
  const manifestNetwork = daBondNetwork(manifest.network);
  const connection = resolveKupmiosUrls({
    kupoUrl: options.kupoUrl,
    ogmiosUrl: options.ogmiosUrl,
    env,
  });
  const provider = makeNodeKupmios({
    kupoUrl: connection.kupoUrl,
    ogmiosUrl: connection.ogmiosUrl,
    network: manifestNetwork,
    nativeLedger: nativeLedgerSettingsFromEnv(env),
  });
  const lucid = await daBondLucid({
    provider,
    network: manifestNetwork,
    ogmiosUrl: connection.ogmiosUrl,
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
  const ledgerTimeMs = await daBondLedgerTimeMs(lucid, connection.ogmiosUrl);
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
    submit: daBondChainSubmit(provider, lucid),
  };
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
  return daBondTopUpCommand(await loadDaBondContext(options, env), {
    amount: options.amount,
    walletSecret,
  });
};

/** `da-bond status` from the CLI. */
export const runDaBondStatus = async (
  options: DaBondChainOptions,
  env: NodeJS.ProcessEnv = process.env,
): Promise<DaBondStatus> =>
  daBondStatusCommand(await loadDaBondContext(options, env));

/** `da-bond withdraw begin|cancel|complete --build-unsigned` from the CLI. */
export const runDaBondWithdrawBuild = async (
  step: DaBondWithdrawStep,
  options: DaBondChainOptions & DaBondWithdrawBuildOptions,
  env: NodeJS.ProcessEnv = process.env,
) =>
  daBondWithdrawBuildCommand(
    await loadDaBondContext(options, env),
    step,
    options,
  );

/** `da-bond assemble` from the CLI. */
export const runDaBondAssemble = async (
  options: DaBondChainOptions,
  unsignedPath: string,
  witnessPaths: readonly string[],
  env: NodeJS.ProcessEnv = process.env,
) =>
  daBondAssembleCommand(
    await loadDaBondContext(options, env),
    unsignedPath,
    witnessPaths,
  );
