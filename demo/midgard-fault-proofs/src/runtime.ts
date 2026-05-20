import {
  Blockfrost,
  CML,
  Data,
  Kupmios,
  Lucid,
  type LucidEvolution,
  type Network,
  type OutRef,
  type PrivateKey,
  type Script,
  type UTxO,
  credentialToAddress,
  getAddressDetails,
  validatorToScriptHash,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import {
  buildDoubleSpendFaultProofContracts,
  type DoubleSpendFaultProofContracts,
  type FraudProofCatalogueCategoryDeploymentInfo,
  MerkleRoot,
  Proof,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import {
  parseContractDeploymentInfo,
  type ContractDeploymentInfo,
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

export type ParsedOutRef = {
  readonly txHash: string;
  readonly outputIndex: number;
};

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

  const seedEnvName = config.walletSeedPhraseEnv ?? DEFAULT_WALLET_SEED_ENV;
  const normalizedSeedEnvName = seedEnvName.trim();
  if (normalizedSeedEnvName.length === 0) {
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
  const parts = value.trim().toLowerCase().split("#");
  if (parts.length !== 2) {
    throw new Error(`${label} must use the format <txHash>#<outputIndex>.`);
  }
  const [txHash, outputIndexRaw] = parts;
  if (txHash === undefined || !/^[0-9a-f]{64}$/.test(txHash)) {
    throw new Error(`${label}.txHash must be a 32-byte hex string.`);
  }
  if (outputIndexRaw === undefined || !/^\d+$/.test(outputIndexRaw)) {
    throw new Error(`${label}.outputIndex must be a non-negative integer.`);
  }
  const outputIndex = Number(outputIndexRaw);
  if (!Number.isSafeInteger(outputIndex) || outputIndex < 0) {
    throw new Error(`${label}.outputIndex exceeds the safe integer range.`);
  }
  return { txHash, outputIndex };
};

export const outRefLabel = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

export const parsedOutRefFromUtxo = (utxo: UTxO): ParsedOutRef => ({
  txHash: utxo.txHash.toLowerCase(),
  outputIndex: utxo.outputIndex,
});

export const compareOutRefs = (
  left: ParsedOutRef,
  right: ParsedOutRef,
): number => {
  const txHashOrder = Buffer.from(left.txHash, "hex").compare(
    Buffer.from(right.txHash, "hex"),
  );
  if (txHashOrder !== 0) {
    return txHashOrder;
  }
  return left.outputIndex - right.outputIndex;
};

export const referenceInputIndex = (
  sortedReferenceInputs: readonly UTxO[],
  target: UTxO,
): bigint => {
  const index = sortedReferenceInputs.findIndex(
    (utxo) =>
      utxo.txHash === target.txHash && utxo.outputIndex === target.outputIndex,
  );
  if (index < 0) {
    throw new Error(`Reference input not found: ${outRefLabel(target)}`);
  }
  return BigInt(index);
};

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

const requireMatchingDeploymentScriptHash = ({
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

export const resolveDoubleSpendDeploymentContracts = async ({
  blueprint,
  deploymentInfo,
  network,
  requireStateQueueMint = false,
  requireFraudProofSpend = false,
}: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedDoubleSpendDeploymentContracts> => {
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);
  const catalogue =
    parsedDeploymentInfo.fraudProofCatalogueMint?.fraudProofCatalogue;
  const doubleSpendCategory = catalogue?.categories.doubleSpend;
  if (doubleSpendCategory === undefined) {
    throw new Error(
      "Deployment info is missing fraudProofCatalogueMint.fraudProofCatalogue.categories.doubleSpend.",
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
  const deployedDoubleSpendHash = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofDoubleSpend",
  );

  const contracts = await Effect.runPromise(
    buildDoubleSpendFaultProofContracts({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId,
      fraudProofCataloguePolicyId,
    }),
  );
  requireMatchingDeploymentScriptHash({
    label: "fraudProofMint policy",
    deployed: deployedFraudProofPolicyId,
    derived: contracts.fraudProof.policyId,
  });
  if (deployedFraudProofSpendHash !== undefined) {
    requireMatchingDeploymentScriptHash({
      label: "fraudProofSpend script",
      deployed: deployedFraudProofSpendHash,
      derived: contracts.fraudProof.spendingScriptHash,
    });
  }
  requireMatchingDeploymentScriptHash({
    label: "fraudProofDoubleSpend step-01 script",
    deployed: deployedDoubleSpendHash,
    derived: contracts.doubleSpend.firstStep.spendingScriptHash,
  });

  return {
    deploymentInfo: parsedDeploymentInfo,
    doubleSpendCategory,
    stateQueuePolicyId,
    fraudProofCataloguePolicyId,
    hubOraclePolicyId,
    contracts,
  };
};

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
      `Expected exactly one ${label} at ${outRef.txHash}#${outRef.outputIndex.toString()}, found ${utxos.length.toString()}.`,
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
  if (
    configuredHeaderHash !== undefined &&
    configuredHeaderHash.toLowerCase() !== derived
  ) {
    throw new Error(
      `--fraudulent-header-hash mismatch: provided=${configuredHeaderHash}, derived=${derived}.`,
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
