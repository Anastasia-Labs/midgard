import { readFile } from "node:fs/promises";
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
  scriptHashToCredential,
  toUnit,
  validatorToScriptHash,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import {
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  FraudProofComputationThreadRedeemer,
  FraudProofComputationThreadStepDatum,
  HUB_ORACLE_ASSET_NAME,
  MerkleRoot,
  Proof,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  buildDoubleSpendFaultProofContracts,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import {
  type ContractDeploymentInfo,
  inspectContracts,
  parseContractDeploymentInfo,
} from "./inspect-contracts.js";
import { aikenSerialisedPlutusDataCbor } from "./plutus-data-cbor.js";
import { Effect } from "effect";

const DEFAULT_WALLET_SEED_ENV = "USER_WALLET";
const PHAS_MEMBERSHIP_WITHDRAW_TITLE = "phas.membership.withdraw";
const FIRST_STEP_OUTPUT_INDEX = 0n;
const PHAS_WITHDRAW_REDEEMER_INDEX = 0n;
const DEFAULT_CONFIRMATION_POLL_MS = 5_000;

type LucidDataSchema = Parameters<typeof Data.to>[1];

export type ProviderKind = "Blockfrost" | "Kupmios";

export type SubmitInitCliConfig = {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly network: Network;
  readonly provider?: ProviderKind;
  readonly blockfrostApiUrl?: string;
  readonly blockfrostKey?: string;
  readonly kupoUrl?: string;
  readonly ogmiosUrl?: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly fraudulentBlockOutRef: string;
  readonly fraudulentHeaderHash?: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitProviderConfig = Pick<
  SubmitInitCliConfig,
  | "network"
  | "provider"
  | "blockfrostApiUrl"
  | "blockfrostKey"
  | "kupoUrl"
  | "ogmiosUrl"
>;

export type ResolvedProverSigner = {
  readonly source: "direct-seed-phrase" | string;
  readonly address: string;
  readonly paymentKeyHash: string;
  readonly selectWallet: (lucid: LucidEvolution) => void;
};

export type SubmitInitResult = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly fraudulentBlockOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly firstStepAddress: string;
  readonly firstStepOutputIndex: number;
  readonly fraudCategoryId: string;
  readonly fraudCategory: string;
  readonly fraudProofCatalogueRoot: string;
  readonly awaitedConfirmation: boolean;
};

export type ParsedOutRef = {
  readonly txHash: string;
  readonly outputIndex: number;
};

export const readJsonFile = async (path: string): Promise<unknown> => {
  const raw = await readFile(path, "utf8");
  return JSON.parse(raw) as unknown;
};

const normalizeNonEmpty = (
  value: string | undefined,
): string | undefined => {
  const trimmed = value?.trim() ?? "";
  return trimmed.length === 0 ? undefined : trimmed;
};

const parseProviderKind = (
  value: string | undefined,
  env: NodeJS.ProcessEnv = process.env,
): ProviderKind => {
  const resolved = normalizeNonEmpty(value) ?? normalizeNonEmpty(env.L1_PROVIDER);
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
    throw new Error(`${label} is required; pass it directly or set ${envName}.`);
  }
  return resolved;
};

export const makeLucidForSubmitInit = async (
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
  config: Pick<
    SubmitInitCliConfig,
    | "network"
    | "walletSeedPhrase"
    | "walletSeedPhraseEnv"
    | "walletPrivateKey"
    | "walletPrivateKeyEnv"
  >,
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
    return resolveSeedSigner(seedFromEnv, normalizedSeedEnvName, config.network);
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

export const compareOutRefs = (
  left: ParsedOutRef,
  right: ParsedOutRef,
): number => {
  const txHashOrder = left.txHash.localeCompare(right.txHash);
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

const requireFraudProofCatalogue =
  (deploymentInfo: ContractDeploymentInfo) => {
    const catalogue = deploymentInfo.fraudProofCatalogueMint?.fraudProofCatalogue;
    if (catalogue === undefined) {
      throw new Error(
        "Deployment info is missing fraudProofCatalogueMint.fraudProofCatalogue.",
      );
    }
    return catalogue;
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

const fetchFraudulentBlockUtxo = async (
  lucid: LucidEvolution,
  outRef: ParsedOutRef,
): Promise<UTxO> => {
  return await fetchUtxoByOutRef({
    lucid,
    outRef,
    label: "fraudulent block UTxO",
  });
};

const normalizeHeaderHash = (value: string | undefined): string | undefined => {
  const normalized = normalizeNonEmpty(value)?.toLowerCase();
  if (normalized === undefined) {
    return undefined;
  }
  if (!/^[0-9a-f]{56}$/.test(normalized)) {
    throw new Error("--fraudulent-header-hash must be a 28-byte hex string.");
  }
  return normalized;
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

export const getCompiledScript = (blueprint: unknown, title: string): string => {
  const parsed = parseFaultProofBlueprint(blueprint);
  const found = parsed.validators.find((validator) => validator.title === title);
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

const encodePhasMembershipRedeemer = ({
  root,
  categoryId,
  categoryScriptHash,
  membershipProofCbor,
}: {
  readonly root: string;
  readonly categoryId: string;
  readonly categoryScriptHash: string;
  readonly membershipProofCbor: string;
}): string =>
  encodePhasMembershipProofRedeemer({
    root,
    keyCbor: Data.to(
      categoryId,
      Data.Bytes({ minLength: 4, maxLength: 4 }) as unknown as LucidDataSchema,
    ),
    valueCbor: Data.to(
      categoryScriptHash,
      Data.Bytes({ minLength: 28, maxLength: 28 }) as unknown as LucidDataSchema,
    ),
    membershipProofCbor,
  });

export const submitInit = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  fraudulentBlockOutRef,
  fraudulentHeaderHash,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly fraudulentBlockOutRef: string;
  readonly fraudulentHeaderHash?: string;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitInitResult> => {
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);
  const inspection = await Effect.runPromise(
    inspectContracts({ blueprint, deploymentInfo, network }),
  );
  if (!inspection.fraudProofCatalogue.initReady) {
    throw new Error(
      "Fraud-proof catalogue is not ready for double-spend Init; run inspect-contracts and redeploy/export deployment info with the real double-spend first step.",
    );
  }

  const catalogue = requireFraudProofCatalogue(parsedDeploymentInfo);
  const doubleSpendCategory = catalogue.categories.doubleSpend;
  const stateQueuePolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "stateQueueMint",
  );
  const fraudProofCataloguePolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofCatalogueMint",
  );
  const fraudProofCatalogueSpendHash = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofCatalogueSpend",
  );
  const hubOraclePolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "hubOracleMint",
  );

  const contracts = await Effect.runPromise(
    buildDoubleSpendFaultProofContracts({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId,
      fraudProofCataloguePolicyId,
    }),
  );
  const parsedFraudulentBlockOutRef = parseOutRef(
    fraudulentBlockOutRef,
    "--fraudulent-block-out-ref",
  );
  const [catalogueUtxo, hubOracleUtxo, fraudulentBlockUtxo] =
    await Promise.all([
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(fraudProofCatalogueSpendHash),
        ),
        unit: toUnit(
          fraudProofCataloguePolicyId,
          FRAUD_PROOF_CATALOGUE_ASSET_NAME,
        ),
        label: "fraud-proof catalogue",
      }),
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(hubOraclePolicyId),
        ),
        unit: toUnit(hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
        label: "hub oracle",
      }),
      fetchFraudulentBlockUtxo(lucid, parsedFraudulentBlockOutRef),
    ]);
  const resolvedHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId,
    fraudulentBlockUtxo,
    configuredHeaderHash: normalizeHeaderHash(fraudulentHeaderHash),
  });
  const computationThreadAssetName = `${doubleSpendCategory.categoryId}${resolvedHeaderHash}`;
  const computationThreadUnit = toUnit(
    contracts.computationThread.policyId,
    computationThreadAssetName,
  );
  const sortedReferenceInputs = [
    catalogueUtxo,
    hubOracleUtxo,
    fraudulentBlockUtxo,
  ].sort(compareOutRefs);
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };

  signer.selectWallet(lucid);
  const tx = lucid
    .newTx()
    .readFrom(sortedReferenceInputs)
    .withdraw(
      phasMembershipRewardAddress(network, phasMembershipScript),
      0n,
      encodePhasMembershipRedeemer({
        root: catalogue.root,
        categoryId: doubleSpendCategory.categoryId,
        categoryScriptHash: doubleSpendCategory.scriptHash,
        membershipProofCbor: doubleSpendCategory.membershipProofCbor,
      }),
    )
    .mintAssets(
      { [computationThreadUnit]: 1n },
      Data.to(
        {
          Init: {
            first_step_output_index: FIRST_STEP_OUTPUT_INDEX,
            fraud_category_id: doubleSpendCategory.categoryId,
            fraud_category: inspection.doubleSpend.categoryFirstStepHash,
            fraud_category_membership_proof: Data.from(
              doubleSpendCategory.membershipProofCbor,
              Proof,
            ),
            fraud_proof_catalogue_ref_input_index: referenceInputIndex(
              sortedReferenceInputs,
              catalogueUtxo,
            ),
            inclusion_proof_script_redeemer_index:
              PHAS_WITHDRAW_REDEEMER_INDEX,
            hub_oracle_ref_input_index: referenceInputIndex(
              sortedReferenceInputs,
              hubOracleUtxo,
            ),
            fraudulent_block_ref_input_index: referenceInputIndex(
              sortedReferenceInputs,
              fraudulentBlockUtxo,
            ),
          },
        },
        FraudProofComputationThreadRedeemer,
      ),
    )
    .pay.ToContract(
      contracts.doubleSpend.firstStep.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to(
          {
            fraud_prover: signer.paymentKeyHash,
            data: null,
          },
          FraudProofComputationThreadStepDatum,
        ),
      },
      { [computationThreadUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .attach.WithdrawalValidator(phasMembershipScript);

  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    fraudulentBlockOutRef,
    fraudulentHeaderHash: resolvedHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName,
    computationThreadUnit,
    firstStepAddress: contracts.doubleSpend.firstStep.spendingScriptAddress,
    firstStepOutputIndex: Number(FIRST_STEP_OUTPUT_INDEX),
    fraudCategoryId: doubleSpendCategory.categoryId,
    fraudCategory: inspection.doubleSpend.categoryFirstStepHash,
    fraudProofCatalogueRoot: catalogue.root,
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitInitFromFiles = async (
  config: SubmitInitCliConfig,
): Promise<SubmitInitResult> => {
  const [blueprint, deploymentInfo, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    makeLucidForSubmitInit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitInit({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    fraudulentBlockOutRef: config.fraudulentBlockOutRef,
    fraudulentHeaderHash: config.fraudulentHeaderHash,
    awaitConfirmation: config.awaitConfirmation,
  });
};

export const __submitInitTest = {
  parseOutRef,
  resolveFraudulentHeaderHash,
  encodePhasMembershipRedeemer,
};
