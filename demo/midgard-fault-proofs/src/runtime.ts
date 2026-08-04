import { normalizeHex } from "@al-ft/midgard-core/hex";
import {
  compareOutRefs,
  outRefLabel,
  type OutRefLike,
  outRefsEqual,
  parseOutRefLabel,
} from "@al-ft/midgard-core/out-ref";
import {
  buildDaHashPreimageFaultProofContracts,
  buildDoubleSpendFaultProofContracts,
  buildInputNoIdxFaultProofContracts,
  buildInvalidRangeFaultProofContracts,
  buildNonExistentInputFaultProofContracts,
  buildTransitionTraceFaultProofContracts,
  buildValidationTraceDisputeFaultProofContracts,
  buildZeroInputFaultProofContracts,
  type DaHashPreimageFaultProofContracts,
  type DoubleSpendFaultProofContracts,
  type FraudProofCatalogueCategoryDeploymentInfo,
  type FraudProofCatalogueCategoryName,
  type InputNoIdxFaultProofContracts,
  type InvalidRangeFaultProofContracts,
  MerkleRoot,
  type NonExistentInputFaultProofContracts,
  parseFaultProofBlueprint,
  Proof,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  type TransitionTraceFaultProofContracts,
  type ValidationTraceDisputeFaultProofContracts,
  type ZeroInputFaultProofContracts,
} from "@al-ft/midgard-sdk";
import {
  Blockfrost,
  CML,
  credentialToAddress,
  Data,
  getAddressDetails,
  Kupmios,
  Lucid,
  type LucidEvolution,
  type Network,
  type OutRef,
  type PrivateKey,
  type Script,
  type UTxO,
  validatorToScriptHash,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  assertFraudProofCatalogueCategoryReady,
  type ContractDeploymentInfo,
  parseContractDeploymentInfo,
  parseContractDeploymentReferenceScriptAuthPolicyId,
} from "./inspect-contracts.js";
import { aikenSerialisedPlutusDataCbor } from "./plutus-data-cbor.js";

const DEFAULT_WALLET_SEED_ENV = "USER_WALLET";

type LucidDataSchema = Parameters<typeof Data.to>[1];

export const DEFAULT_CONFIRMATION_POLL_MS = 5_000;

export type ProviderKind = "Blockfrost" | "Kupmios";

export type SubmitProviderConfig = {
  readonly network: Network;
  readonly provider?: ProviderKind;
  readonly blockfrostApiUrl?: string;
  readonly blockfrostKey?: string;
  readonly kupoUrl?: string;
  readonly ogmiosUrl?: string;
};

export type ProverSignerConfig = {
  readonly network: Network;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
};

export type ResolvedProverSigner = {
  readonly source: "direct-seed-phrase" | string;
  readonly address: string;
  readonly paymentKeyHash: string;
  readonly selectWallet: (lucid: LucidEvolution) => void;
};

export type ParsedOutRef = OutRefLike;

export { compareOutRefs, outRefLabel, outRefsEqual };
export { readJsonFile } from "./json-file.js";

const normalizeNonEmpty = (value: string | undefined): string | undefined => {
  const trimmed = value?.trim() ?? "";
  return trimmed.length === 0 ? undefined : trimmed;
};

const parseProviderKind = (
  value: string | undefined,
  env: NodeJS.ProcessEnv = process.env,
): ProviderKind => {
  const resolved =
    normalizeNonEmpty(value) ?? normalizeNonEmpty(env.L1_PROVIDER);
  if (resolved === "Blockfrost" || resolved === "Kupmios") {
    return resolved;
  }
  if (resolved === undefined) {
    return "Blockfrost";
  }
  throw new Error('--provider must be either "Blockfrost" or "Kupmios".');
};

const requireConfigValue = (
  direct: string | undefined,
  envName: string,
  label: string,
  env: NodeJS.ProcessEnv,
): string => {
  const resolved = normalizeNonEmpty(direct) ?? normalizeNonEmpty(env[envName]);
  if (resolved === undefined) {
    throw new Error(
      `${label} is required; pass it directly or set ${envName}.`,
    );
  }
  return resolved;
};

export const makeLucidForSubmit = async (
  config: SubmitProviderConfig,
  env: NodeJS.ProcessEnv = process.env,
): Promise<LucidEvolution> => {
  const provider = parseProviderKind(config.provider, env);
  if (provider === "Blockfrost") {
    return await Lucid(
      new Blockfrost(
        requireConfigValue(
          config.blockfrostApiUrl,
          "L1_BLOCKFROST_API_URL",
          "--blockfrost-api-url",
          env,
        ),
        requireConfigValue(
          config.blockfrostKey,
          "L1_BLOCKFROST_KEY",
          "--blockfrost-key",
          env,
        ),
      ),
      config.network,
    );
  }

  return await Lucid(
    new Kupmios(
      requireConfigValue(config.kupoUrl, "L1_KUPO_KEY", "--kupo-url", env),
      requireConfigValue(
        config.ogmiosUrl,
        "L1_OGMIOS_KEY",
        "--ogmios-url",
        env,
      ),
    ),
    config.network,
  );
};

const paymentKeyHashFromAddress = (address: string): string => {
  const paymentCredential = getAddressDetails(address).paymentCredential;
  if (paymentCredential === undefined || paymentCredential.type !== "Key") {
    throw new Error("Prover wallet address must contain a payment key hash.");
  }
  return paymentCredential.hash;
};

const resolveSeedSigner = (
  seedPhrase: string,
  source: string,
  network: Network,
): ResolvedProverSigner => {
  const wallet = walletFromSeed(seedPhrase, { network });
  return {
    source,
    address: wallet.address,
    paymentKeyHash: paymentKeyHashFromAddress(wallet.address),
    selectWallet: (lucid) => lucid.selectWallet.fromSeed(seedPhrase),
  };
};

const resolvePrivateKeySigner = (
  privateKey: string,
  source: string,
  network: Network,
): ResolvedProverSigner => {
  const parsedPrivateKey = CML.PrivateKey.from_bech32(privateKey);
  const paymentKeyHash = parsedPrivateKey.to_public().hash().to_hex();
  return {
    source,
    address: credentialToAddress(network, {
      type: "Key",
      hash: paymentKeyHash,
    }),
    paymentKeyHash,
    selectWallet: (lucid) =>
      lucid.selectWallet.fromPrivateKey(privateKey as PrivateKey),
  };
};

export const resolveProverSigner = (
  config: ProverSignerConfig,
  env: NodeJS.ProcessEnv = process.env,
): ResolvedProverSigner => {
  const directSeed = normalizeNonEmpty(config.walletSeedPhrase);
  const directPrivateKey = normalizeNonEmpty(config.walletPrivateKey);
  const privateKeyEnvName = normalizeNonEmpty(config.walletPrivateKeyEnv);
  const privateKeyFromEnv =
    privateKeyEnvName === undefined
      ? undefined
      : normalizeNonEmpty(env[privateKeyEnvName]);

  if (
    directSeed !== undefined &&
    (directPrivateKey !== undefined || privateKeyFromEnv !== undefined)
  ) {
    throw new Error(
      "Provide either a wallet seed phrase or a wallet private key, not both.",
    );
  }
  if (directPrivateKey !== undefined && privateKeyFromEnv !== undefined) {
    throw new Error(
      "Provide the wallet private key either directly or through an env var, not both.",
    );
  }
  if (directSeed !== undefined) {
    return resolveSeedSigner(directSeed, "direct-seed-phrase", config.network);
  }
  if (directPrivateKey !== undefined) {
    return resolvePrivateKeySigner(
      directPrivateKey,
      "direct-private-key",
      config.network,
    );
  }
  if (privateKeyFromEnv !== undefined && privateKeyEnvName !== undefined) {
    return resolvePrivateKeySigner(
      privateKeyFromEnv,
      privateKeyEnvName,
      config.network,
    );
  }

  const normalizedSeedEnvName = normalizeNonEmpty(
    config.walletSeedPhraseEnv ?? DEFAULT_WALLET_SEED_ENV,
  );
  if (normalizedSeedEnvName === undefined) {
    throw new Error("Wallet seed phrase env var name must not be empty.");
  }
  const seedFromEnv = normalizeNonEmpty(env[normalizedSeedEnvName]);
  if (seedFromEnv !== undefined) {
    return resolveSeedSigner(
      seedFromEnv,
      normalizedSeedEnvName,
      config.network,
    );
  }

  throw new Error(
    `No prover signer configured; pass --wallet-seed-phrase, set ${normalizedSeedEnvName}, or pass --wallet-private-key/--wallet-private-key-env.`,
  );
};

export const parseOutRef = (value: string, label: string): ParsedOutRef => {
  try {
    return parseOutRefLabel(value);
  } catch {
    throw new Error(`${label} must use the format <txHash>#<outputIndex>.`);
  }
};

export const compareUtxoOutRefs = compareOutRefs;

export const requireDeploymentScriptHash = (
  deploymentInfo: ContractDeploymentInfo,
  name: string,
): string => {
  const entry = deploymentInfo[name];
  if (entry === undefined) {
    throw new Error(`Deployment info is missing "${name}"`);
  }
  return entry.scriptHash;
};

export const requireMatchingScriptHash = ({
  label,
  deployed,
  derived,
}: {
  readonly label: string;
  readonly deployed: string;
  readonly derived: string;
}): void => {
  if (deployed !== derived) {
    throw new Error(
      `${label} mismatch: deployment=${deployed}, derived=${derived}.`,
    );
  }
};

export type ResolvedDoubleSpendDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly doubleSpendCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: DoubleSpendFaultProofContracts;
};

export type ResolvedInvalidRangeDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly invalidRangeCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: InvalidRangeFaultProofContracts;
};

export type ResolvedNonExistentInputDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly nonExistentInputCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: NonExistentInputFaultProofContracts;
};

export type ResolvedTransitionTraceDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly transitionTraceCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: TransitionTraceFaultProofContracts;
};

export type ResolvedValidationTraceDisputeDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly referenceScriptAuthPolicyId: string;
  readonly validationTraceDisputeCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: ValidationTraceDisputeFaultProofContracts;
};

export type ResolvedDaHashPreimageDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly daHashPreimageCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: DaHashPreimageFaultProofContracts;
};

export type ResolvedInputNoIdxDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly nonExistentInputNoIndexCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: InputNoIdxFaultProofContracts;
};

export type ResolvedZeroInputDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly zeroInputCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: ZeroInputFaultProofContracts;
};

type SupportedFaultProofCategoryName = Extract<
  FraudProofCatalogueCategoryName,
  | "doubleSpend"
  | "nonExistentInput"
  | "nonExistentInputNoIndex"
  | "invalidRange"
  | "transitionTrace"
  | "zeroInput"
  | "validationTraceDispute"
  | "daHashPreimage"
>;

const FRAUD_PROOF_DEPLOYMENT_ENTRY_BY_CATEGORY = {
  doubleSpend: "fraudProofDoubleSpend",
  nonExistentInput: "fraudProofNonExistentInput",
  nonExistentInputNoIndex: "fraudProofNonExistentInputNoIndex",
  invalidRange: "fraudProofInvalidRange",
  transitionTrace: "fraudProofTransitionTrace",
  zeroInput: "fraudProofZeroInput",
  validationTraceDispute: "validationTraceDispute",
  daHashPreimage: "fraudProofDaHashPreimage",
} as const satisfies Record<SupportedFaultProofCategoryName, string>;

const categoryLabel = (
  categoryName: SupportedFaultProofCategoryName,
): string => {
  switch (categoryName) {
    case "doubleSpend":
      return "double-spend";
    case "nonExistentInput":
      return "non-existent-input";
    case "nonExistentInputNoIndex":
      return "input-no-idx";
    case "invalidRange":
      return "invalid-range";
    case "transitionTrace":
      return "transition-trace";
    case "zeroInput":
      return "zero-input";
    case "validationTraceDispute":
      return "validation-trace-dispute";
    case "daHashPreimage":
      return "da-hash-preimage";
  }
};

const resolveFaultProofDeploymentContracts = async ({
  blueprint,
  deploymentInfo,
  network,
  categoryName,
  requireStateQueueMint = false,
  requireFraudProofSpend = false,
}: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly categoryName: SupportedFaultProofCategoryName;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<{
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly category: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts:
    | DoubleSpendFaultProofContracts
    | NonExistentInputFaultProofContracts
    | InvalidRangeFaultProofContracts
    | TransitionTraceFaultProofContracts
    | ZeroInputFaultProofContracts
    | ValidationTraceDisputeFaultProofContracts
    | DaHashPreimageFaultProofContracts
    | InputNoIdxFaultProofContracts;
}> => {
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);
  const catalogue =
    parsedDeploymentInfo.fraudProofCatalogueMint?.fraudProofCatalogue;
  if (catalogue === undefined) {
    throw new Error(
      "Deployment info is missing fraudProofCatalogueMint.fraudProofCatalogue.",
    );
  }
  const category = (
    catalogue.categories as Readonly<
      Partial<
        Record<
          SupportedFaultProofCategoryName,
          FraudProofCatalogueCategoryDeploymentInfo
        >
      >
    >
  )[categoryName];
  if (category === undefined) {
    throw new Error(
      `Deployment info is missing fraudProofCatalogueMint.fraudProofCatalogue.categories.${categoryName}.`,
    );
  }

  const stateQueuePolicyId = requireStateQueueMint
    ? requireDeploymentScriptHash(parsedDeploymentInfo, "stateQueueMint")
    : undefined;
  const fraudProofCataloguePolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofCatalogueMint",
  );
  const hubOraclePolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "hubOracleMint",
  );
  const deployedFraudProofPolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofMint",
  );
  const deployedFraudProofSpendHash = requireFraudProofSpend
    ? requireDeploymentScriptHash(parsedDeploymentInfo, "fraudProofSpend")
    : undefined;
  const deployedFirstStepHash = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    FRAUD_PROOF_DEPLOYMENT_ENTRY_BY_CATEGORY[categoryName],
  );

  const parsedBlueprint = parseFaultProofBlueprint(blueprint);
  let contracts:
    | DoubleSpendFaultProofContracts
    | NonExistentInputFaultProofContracts
    | InvalidRangeFaultProofContracts
    | TransitionTraceFaultProofContracts
    | ZeroInputFaultProofContracts
    | ValidationTraceDisputeFaultProofContracts
    | DaHashPreimageFaultProofContracts
    | InputNoIdxFaultProofContracts;
  let derivedFirstStepHash: string;
  if (categoryName === "doubleSpend") {
    const doubleSpendContracts = await Effect.runPromise(
      buildDoubleSpendFaultProofContracts({
        blueprint: parsedBlueprint,
        network,
        hubOraclePolicyId,
        fraudProofCataloguePolicyId,
      }),
    );
    contracts = doubleSpendContracts;
    derivedFirstStepHash =
      doubleSpendContracts.doubleSpend.firstStep.spendingScriptHash;
  } else if (categoryName === "nonExistentInput") {
    const nonExistentInputContracts = await Effect.runPromise(
      buildNonExistentInputFaultProofContracts({
        blueprint: parsedBlueprint,
        network,
        hubOraclePolicyId,
        fraudProofCataloguePolicyId,
      }),
    );
    contracts = nonExistentInputContracts;
    derivedFirstStepHash =
      nonExistentInputContracts.nonExistentInput.firstStep.spendingScriptHash;
  } else if (categoryName === "nonExistentInputNoIndex") {
    const inputNoIdxContracts = await Effect.runPromise(
      buildInputNoIdxFaultProofContracts({
        blueprint: parsedBlueprint,
        network,
        hubOraclePolicyId,
        fraudProofCataloguePolicyId,
      }),
    );
    contracts = inputNoIdxContracts;
    derivedFirstStepHash =
      inputNoIdxContracts.nonExistentInputNoIndex.firstStep.spendingScriptHash;
  } else if (categoryName === "invalidRange") {
    const invalidRangeContracts = await Effect.runPromise(
      buildInvalidRangeFaultProofContracts({
        blueprint: parsedBlueprint,
        network,
        hubOraclePolicyId,
        fraudProofCataloguePolicyId,
      }),
    );
    contracts = invalidRangeContracts;
    derivedFirstStepHash =
      invalidRangeContracts.invalidRange.firstStep.spendingScriptHash;
  } else if (categoryName === "transitionTrace") {
    const transitionTraceContracts = await Effect.runPromise(
      buildTransitionTraceFaultProofContracts({
        blueprint: parsedBlueprint,
        network,
        hubOraclePolicyId,
        fraudProofCataloguePolicyId,
      }),
    );
    contracts = transitionTraceContracts;
    derivedFirstStepHash =
      transitionTraceContracts.transitionTrace.firstStep.spendingScriptHash;
  } else if (categoryName === "validationTraceDispute") {
    const validationTraceDisputeContracts = await Effect.runPromise(
      buildValidationTraceDisputeFaultProofContracts({
        blueprint: parsedBlueprint,
        network,
        hubOraclePolicyId,
        fraudProofCataloguePolicyId,
      }),
    );
    contracts = validationTraceDisputeContracts;
    derivedFirstStepHash =
      validationTraceDisputeContracts.validationTraceDispute.firstStep
        .spendingScriptHash;
  } else if (categoryName === "daHashPreimage") {
    const daHashPreimageContracts = await Effect.runPromise(
      buildDaHashPreimageFaultProofContracts({
        blueprint: parsedBlueprint,
        network,
        hubOraclePolicyId,
        fraudProofCataloguePolicyId,
      }),
    );
    contracts = daHashPreimageContracts;
    derivedFirstStepHash =
      daHashPreimageContracts.daHashPreimage.firstStep.spendingScriptHash;
  } else {
    const zeroInputContracts = await Effect.runPromise(
      buildZeroInputFaultProofContracts({
        blueprint: parsedBlueprint,
        network,
        hubOraclePolicyId,
        fraudProofCataloguePolicyId,
      }),
    );
    contracts = zeroInputContracts;
    derivedFirstStepHash =
      zeroInputContracts.zeroInput.firstStep.spendingScriptHash;
  }
  requireMatchingScriptHash({
    label: "fraudProofMint policy",
    deployed: deployedFraudProofPolicyId,
    derived: contracts.fraudProof.policyId,
  });
  if (deployedFraudProofSpendHash !== undefined) {
    requireMatchingScriptHash({
      label: "fraudProofSpend script",
      deployed: deployedFraudProofSpendHash,
      derived: contracts.fraudProof.spendingScriptHash,
    });
  }
  requireMatchingScriptHash({
    label: `${FRAUD_PROOF_DEPLOYMENT_ENTRY_BY_CATEGORY[categoryName]} step-01 script`,
    deployed: deployedFirstStepHash,
    derived: derivedFirstStepHash,
  });
  const readyCategory = await assertFraudProofCatalogueCategoryReady({
    catalogue,
    categoryName,
    expectedFirstStepHash: derivedFirstStepHash,
    deploymentMatchesFirstStep: true,
  });

  return {
    deploymentInfo: parsedDeploymentInfo,
    category: readyCategory,
    stateQueuePolicyId,
    fraudProofCataloguePolicyId,
    hubOraclePolicyId,
    contracts,
  };
};

export const resolveDoubleSpendDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedDoubleSpendDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "doubleSpend",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    doubleSpendCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as DoubleSpendFaultProofContracts,
  };
};

export const resolveNonExistentInputDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedNonExistentInputDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "nonExistentInput",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    nonExistentInputCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as NonExistentInputFaultProofContracts,
  };
};

export const resolveInputNoIdxDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedInputNoIdxDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "nonExistentInputNoIndex",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    nonExistentInputNoIndexCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as InputNoIdxFaultProofContracts,
  };
};

export const resolveInvalidRangeDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedInvalidRangeDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "invalidRange",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    invalidRangeCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as InvalidRangeFaultProofContracts,
  };
};

export const resolveTransitionTraceDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedTransitionTraceDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "transitionTrace",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    transitionTraceCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as TransitionTraceFaultProofContracts,
  };
};

export const resolveValidationTraceDisputeDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedValidationTraceDisputeDeploymentContracts> => {
  const referenceScriptAuthPolicyId =
    parseContractDeploymentReferenceScriptAuthPolicyId(
      params.deploymentInfo,
      "V1 validation-trace dispute",
    );
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "validationTraceDispute",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    referenceScriptAuthPolicyId,
    validationTraceDisputeCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as ValidationTraceDisputeFaultProofContracts,
  };
};

export const resolveZeroInputDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedZeroInputDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "zeroInput",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    zeroInputCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as ZeroInputFaultProofContracts,
  };
};

export const resolveDaHashPreimageDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedDaHashPreimageDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "daHashPreimage",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    daHashPreimageCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as DaHashPreimageFaultProofContracts,
  };
};

export const faultProofCategoryLabel = categoryLabel;

export const requireSingletonUtxo = async ({
  lucid,
  address,
  unit,
  label,
}: {
  readonly lucid: LucidEvolution;
  readonly address: string;
  readonly unit: string;
  readonly label: string;
}): Promise<UTxO> => {
  const utxos = await lucid.utxosAtWithUnit(address, unit);
  const matches = utxos.filter((utxo) => (utxo.assets[unit] ?? 0n) === 1n);
  if (matches.length !== 1) {
    throw new Error(
      `Expected exactly one ${label} UTxO with unit ${unit}, found ${matches.length.toString()}.`,
    );
  }
  return matches[0]!;
};

export const fetchUtxoByOutRef = async ({
  lucid,
  outRef,
  label,
}: {
  readonly lucid: LucidEvolution;
  readonly outRef: ParsedOutRef;
  readonly label: string;
}): Promise<UTxO> => {
  const outRefs: OutRef[] = [
    { txHash: outRef.txHash, outputIndex: outRef.outputIndex },
  ];
  const utxos = await lucid.utxosByOutRef(outRefs);
  if (utxos.length !== 1) {
    throw new Error(
      `Expected exactly one ${label} at ${outRefLabel(outRef)}, found ${utxos.length.toString()}.`,
    );
  }
  return utxos[0]!;
};

export const resolveFraudulentHeaderHash = ({
  stateQueuePolicyId,
  fraudulentBlockUtxo,
  configuredHeaderHash,
}: {
  readonly stateQueuePolicyId: string;
  readonly fraudulentBlockUtxo: UTxO;
  readonly configuredHeaderHash?: string;
}): string => {
  const prefix = `${stateQueuePolicyId}${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}`;
  const candidates = Object.entries(fraudulentBlockUtxo.assets)
    .filter(
      ([unit, amount]) =>
        amount === 1n &&
        unit.startsWith(prefix) &&
        unit.length === prefix.length + 56,
    )
    .map(([unit]) => unit.slice(prefix.length));
  if (candidates.length !== 1) {
    throw new Error(
      `Expected fraudulent block UTxO ${outRefLabel(fraudulentBlockUtxo)} to carry exactly one state-queue block token for policy ${stateQueuePolicyId}, found ${candidates.length.toString()}.`,
    );
  }
  const derived = candidates[0]!;
  const configured =
    configuredHeaderHash === undefined
      ? undefined
      : normalizeHex(configuredHeaderHash, {
          fieldName: "--fraudulent-header-hash",
          byteLength: 28,
        });
  if (configured !== undefined && configured !== derived) {
    throw new Error(
      `--fraudulent-header-hash mismatch: provided=${configured}, derived=${derived}.`,
    );
  }
  return derived;
};

export const phasMembershipRewardAddress = (
  network: Network,
  script: Script,
): string => {
  const networkId = network === "Mainnet" ? 1 : 0;
  const credential = CML.Credential.new_script(
    CML.ScriptHash.from_hex(validatorToScriptHash(script)),
  );
  return CML.RewardAddress.new(networkId, credential).to_address().to_bech32();
};

export const getCompiledScript = (
  blueprint: unknown,
  title: string,
): string => {
  const parsed = parseFaultProofBlueprint(blueprint);
  const found = parsed.validators.find(
    (validator) => validator.title === title,
  );
  if (found === undefined) {
    throw new Error(`Validator with title "${title}" not found in blueprint.`);
  }
  return found.compiledCode;
};

export const encodePhasMembershipProofRedeemer = ({
  root,
  keyCbor,
  valueCbor,
  membershipProofCbor,
}: {
  readonly root: string;
  readonly keyCbor: string;
  readonly valueCbor: string;
  readonly membershipProofCbor: string;
}): string => {
  const proof = Data.from(membershipProofCbor, Proof);
  const rootData = Data.from(Data.to(root, MerkleRoot));
  const keyData = Data.from(
    Data.to(
      aikenSerialisedPlutusDataCbor(keyCbor),
      Data.Bytes() as unknown as LucidDataSchema,
    ),
  );
  const valueData = Data.from(
    Data.to(
      aikenSerialisedPlutusDataCbor(valueCbor),
      Data.Bytes() as unknown as LucidDataSchema,
    ),
  );
  const proofData = Data.from(
    Data.to(proof, Proof as unknown as LucidDataSchema),
  );
  return Data.to(
    [rootData, keyData, valueData, proofData],
    Data.Array(Data.Any()) as unknown as LucidDataSchema,
  );
};

/**
 * Non-membership (exclusion) redeemer for the `pexcludes.exclusion.withdraw`
 * validator: `[root, key, proof]` (no value, unlike the phas membership
 * counterpart). `keyBytes` are the trie's native key bytes — the ledger trie
 * keyed by Cardano `TransactionInput` CBOR, or the transactions trie keyed by
 * the raw 32-byte native tx id.
 */
export const encodeRawPexcludesProofRedeemer = ({
  root,
  keyBytes,
  nonMembershipProofCbor,
}: {
  readonly root: string;
  readonly keyBytes: string;
  readonly nonMembershipProofCbor: string;
}): string => {
  const proof = Data.from(nonMembershipProofCbor, Proof);
  const rootData = Data.from(Data.to(root, MerkleRoot));
  const keyData = Data.from(
    Data.to(keyBytes, Data.Bytes() as unknown as LucidDataSchema),
  );
  const proofData = Data.from(
    Data.to(proof, Proof as unknown as LucidDataSchema),
  );
  return Data.to(
    [rootData, keyData, proofData],
    Data.Array(Data.Any()) as unknown as LucidDataSchema,
  );
};

export const encodeRawPhasMembershipProofRedeemer = ({
  root,
  keyBytes,
  valueBytes,
  membershipProofCbor,
}: {
  readonly root: string;
  readonly keyBytes: string;
  readonly valueBytes: string;
  readonly membershipProofCbor: string;
}): string => {
  const proof = Data.from(membershipProofCbor, Proof);
  const rootData = Data.from(Data.to(root, MerkleRoot));
  const keyData = Data.from(
    Data.to(keyBytes, Data.Bytes() as unknown as LucidDataSchema),
  );
  const valueData = Data.from(
    Data.to(valueBytes, Data.Bytes() as unknown as LucidDataSchema),
  );
  const proofData = Data.from(
    Data.to(proof, Proof as unknown as LucidDataSchema),
  );
  return Data.to(
    [rootData, keyData, valueData, proofData],
    Data.Array(Data.Any()) as unknown as LucidDataSchema,
  );
};
