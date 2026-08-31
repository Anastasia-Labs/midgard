import { normalizeHex } from "@al-ft/midgard-core/hex";
import {
  compareOutRefs,
  outRefLabel,
  type OutRefLike,
  outRefsEqual,
  parseOutRefLabel,
} from "@al-ft/midgard-core/out-ref";
import {
  buildCanonicalDecodabilityFaultProofContracts,
  buildCommittedFieldShapeFaultProofContracts,
  buildCrossBlockDuplicateEventFaultProofContracts,
  buildDaHashPreimageFaultProofContracts,
  buildDoubleSpendFaultProofContracts,
  buildDoubleWithdrawFaultProofContracts,
  buildFabricatedDepositFaultProofContracts,
  buildFabricatedWithdrawalFaultProofContracts,
  buildInputNoIdxFaultProofContracts,
  buildInputSetUniquenessFaultProofContracts,
  buildInvalidRangeFaultProofContracts,
  buildInvalidSignatureFaultProofContracts,
  buildL2TxMistagFaultProofContracts,
  buildMinAdaFaultProofContracts,
  buildMinFeeFaultProofContracts,
  buildMintAuthorizationFaultProofContracts,
  buildMissingNativeScriptTxFaultProofContracts,
  buildMissingNativeScriptUtxoFaultProofContracts,
  buildMissingSignatureFaultProofContracts,
  buildNativeScriptDecodingFaultProofContracts,
  buildNativeScriptInvalidFaultProofContracts,
  buildNetworkIdFaultProofContracts,
  buildNonExistentInputFaultProofContracts,
  buildNoReferenceInputFaultProofContracts,
  buildReferenceInputNoIdxFaultProofContracts,
  buildTransitionTraceFaultProofContracts,
  buildValidationTraceDisputeFaultProofContracts,
  buildValueNotPreservedFaultProofContracts,
  buildWithdrawalMistagFaultProofContracts,
  buildWithdrawnInputFaultProofContracts,
  buildWithdrawnReferenceInputFaultProofContracts,
  buildZeroInputFaultProofContracts,
  type DaHashPreimageFaultProofContracts,
  type DoubleSpendFaultProofContracts,
  type FaultProofContracts,
  type FraudProofCatalogueCategoryDeploymentInfo,
  type FraudProofCatalogueCategoryName,
  type InputNoIdxFaultProofContracts,
  type InvalidRangeFaultProofContracts,
  type InvalidSignatureFaultProofContracts,
  MerkleRoot,
  type NonExistentInputFaultProofContracts,
  type NoReferenceInputFaultProofContracts,
  parseFaultProofBlueprint,
  Proof,
  type ReferenceInputNoIdxFaultProofContracts,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  type TransitionTraceFaultProofContracts,
  type ValidationTraceDisputeFaultProofContracts,
  type ZeroInputFaultProofContracts,
} from "@al-ft/midgard-sdk";
import {
  applyParamsToScript,
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
  const wallet = walletFromSeed(seedPhrase, {
    addressType: "Enterprise",
    network,
  });
  return {
    source,
    address: wallet.address,
    paymentKeyHash: paymentKeyHashFromAddress(wallet.address),
    selectWallet: (lucid) =>
      lucid.selectWallet.fromSeed(seedPhrase, { addressType: "Enterprise" }),
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

export const requireDeploymentReferenceScriptOutRef = (
  deploymentInfo: ContractDeploymentInfo,
  name: string,
): OutRefLike => {
  const entry = deploymentInfo[name];
  if (entry === undefined) {
    throw new Error(`Deployment info is missing "${name}"`);
  }
  if (entry.refScriptUTxO == null) {
    throw new Error(
      `Deployment info entry "${name}" is missing refScriptUTxO; publish the canonical reference script and regenerate deployment info before using this fraud-proof category.`,
    );
  }
  return entry.refScriptUTxO;
};

export const requireDeploymentReferenceScript = async ({
  lucid,
  deploymentInfo,
  name,
}: {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly name: string;
}): Promise<UTxO> => {
  const expectedScriptHash = requireDeploymentScriptHash(deploymentInfo, name);
  const utxo = await fetchUtxoByOutRef({
    lucid,
    outRef: requireDeploymentReferenceScriptOutRef(deploymentInfo, name),
    label: `${name} reference-script UTxO`,
  });
  if (utxo.scriptRef == null) {
    throw new Error(
      `${name} reference-script UTxO ${outRefLabel(utxo)} does not carry a reference script.`,
    );
  }
  requireMatchingScriptHash({
    label: `${name} reference script`,
    deployed: expectedScriptHash,
    derived: validatorToScriptHash(utxo.scriptRef),
  });
  return utxo;
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

/**
 * Validates a published step reference-script UTxO fail-closed and returns it
 * for use as a transaction reference input.
 *
 * A registered fraud-proof family sources its step spending validators from
 * reference scripts (owner ruling: always reference scripts, never inline
 * attach), so every step submitter accepts an optional `referenceScriptUtxo`.
 * The UTxO must carry a reference script, and that script must hash to the
 * step's own deployed spending-script hash — a divergence would read a witness
 * that does not authorize the spend.
 */
export const requireFaultProofStepReferenceScriptV1 = ({
  utxo,
  expectedScriptHash,
  label,
}: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly label: string;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw new Error(
      `${label} reference UTxO ${outRefLabel(utxo)} carries no reference script.`,
    );
  }
  requireMatchingScriptHash({
    label: `${label} reference script`,
    deployed: expectedScriptHash,
    derived: validatorToScriptHash(utxo.scriptRef),
  });
  return utxo;
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
  readonly cekProgramMaterialScriptHash: string;
  readonly cekProgramMaterialAddress: string;
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

export type ResolvedNoReferenceInputDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly noReferenceInputCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: NoReferenceInputFaultProofContracts;
};

export type ResolvedReferenceInputNoIdxDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly referenceInputNoIdxCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: ReferenceInputNoIdxFaultProofContracts;
};

export type ResolvedInvalidSignatureDeploymentContracts = {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly invalidSignatureCategory: FraudProofCatalogueCategoryDeploymentInfo;
  readonly stateQueuePolicyId: string | undefined;
  readonly fraudProofCataloguePolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly contracts: InvalidSignatureFaultProofContracts;
};

export type SupportedFaultProofCategoryName = FraudProofCatalogueCategoryName;

/**
 * Canonical manifest entries in the same order as each SDK chain's `steps`.
 *
 * The older families still have only their historical first-step deployment
 * entry. Every category registered by the current append wave names every
 * step, so production resolution cannot silently fall back to an inline
 * validator for a later transition.
 */
export const FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY = {
  doubleSpend: ["fraudProofDoubleSpend"],
  nonExistentInput: ["fraudProofNonExistentInput"],
  nonExistentInputNoIndex: ["fraudProofNonExistentInputNoIndex"],
  invalidRange: ["fraudProofInvalidRange"],
  transitionTrace: [
    "fraudProofTransitionTrace",
    "fraudProofTransitionTraceControl",
    "fraudProofTransitionTraceSource",
    "fraudProofTransitionTraceWithdrawal",
    "fraudProofTransitionTraceForced",
    "fraudProofTransitionTraceAcceptedTransaction",
    "fraudProofTransitionTraceDeposit",
    "fraudProofTransitionTraceL1Event",
    "fraudProofTransitionTraceDuplicate",
  ],
  zeroInput: ["fraudProofZeroInput"],
  validationTraceDispute: ["validationTraceDispute"],
  daHashPreimage: ["fraudProofDaHashPreimage"],
  noReferenceInput: ["fraudProofNoReferenceInput"],
  referenceInputNoIdx: ["fraudProofReferenceInputNoIdx"],
  invalidSignature: ["fraudProofInvalidSignature"],
  fabricatedDeposit: [
    "fraudProofFabricatedDeposit",
    "fraudProofFabricatedDepositStep02",
    "fraudProofFabricatedDepositStep03",
    "fraudProofFabricatedDepositStep04",
  ],
  fabricatedWithdrawal: [
    "fraudProofFabricatedWithdrawal",
    "fraudProofFabricatedWithdrawalStep02",
    "fraudProofFabricatedWithdrawalStep03",
    "fraudProofFabricatedWithdrawalStep04",
  ],
  nativeScriptDecoding: [
    "fraudProofNativeScriptDecoding",
    "fraudProofNativeScriptDecodingStep02",
    "fraudProofNativeScriptDecodingStep03OpenSubject",
    "fraudProofNativeScriptDecodingStep03BindDescriptor",
    "fraudProofNativeScriptDecodingStep03AdvanceOrClose",
    "fraudProofNativeScriptDecodingStep04",
  ],
  missingSignature: [
    "fraudProofMissingSignature",
    "fraudProofMissingSignatureStep02",
    "fraudProofMissingSignatureStep03",
    "fraudProofMissingSignatureStep04",
  ],
  missingNativeScriptTx: [
    "fraudProofMissingNativeScriptTx",
    "fraudProofMissingNativeScriptTxStep02",
    "fraudProofMissingNativeScriptTxStep03",
    "fraudProofMissingNativeScriptTxStep04",
    "fraudProofMissingNativeScriptTxStep05",
    "fraudProofMissingNativeScriptTxStep06",
    "fraudProofMissingNativeScriptTxStep07",
    "fraudProofMissingNativeScriptTxStep08",
  ],
  withdrawnReferenceInput: [
    "fraudProofWithdrawnReferenceInput",
    "fraudProofWithdrawnReferenceInputStep02",
    "fraudProofWithdrawnReferenceInputStep03",
  ],
  canonicalDecodability: [
    "fraudProofCanonicalDecodability",
    "fraudProofCanonicalDecodabilityStep02",
  ],
  committedFieldShape: [
    "fraudProofCommittedFieldShape",
    "fraudProofCommittedFieldShapeStep02",
  ],
  minFee: ["fraudProofMinFee", "fraudProofMinFeeStep02"],
  withdrawalMistag: [
    "fraudProofWithdrawalMistag",
    "fraudProofWithdrawalMistagStep02",
    "fraudProofWithdrawalMistagStep03",
    "fraudProofWithdrawalMistagStep04",
    "fraudProofWithdrawalMistagStep05",
  ],
  doubleWithdraw: [
    "fraudProofDoubleWithdraw",
    "fraudProofDoubleWithdrawStep02",
  ],
  crossBlockDuplicateEvent: [
    "fraudProofCrossBlockDuplicateEvent",
    "fraudProofCrossBlockDuplicateEventStep02",
  ],
  l2TxMistag: ["fraudProofL2TxMistag", "fraudProofL2TxMistagStep02"],
  withdrawnInput: [
    "fraudProofWithdrawnInput",
    "fraudProofWithdrawnInputStep02",
    "fraudProofWithdrawnInputStep03",
  ],
  valueNotPreserved: [
    "fraudProofValueNotPreserved",
    "fraudProofValueNotPreservedStep02",
    "fraudProofValueNotPreservedStep03",
    "fraudProofValueNotPreservedStep04",
  ],
  inputSetUniqueness: [
    "fraudProofInputSetUniqueness",
    "fraudProofInputSetUniquenessStep02",
  ],
  mintAuthorization: [
    "fraudProofMintAuthorization",
    "fraudProofMintAuthorizationStep02",
    "fraudProofMintAuthorizationStep03",
    "fraudProofMintAuthorizationStep04",
    "fraudProofMintAuthorizationStep05",
  ],
  networkId: ["fraudProofNetworkId", "fraudProofNetworkIdStep02"],
  missingNativeScriptUtxo: [
    "fraudProofMissingNativeScriptUtxo",
    "fraudProofMissingNativeScriptUtxoStep02",
    "fraudProofMissingNativeScriptUtxoStep03",
    "fraudProofMissingNativeScriptUtxoStep04",
    "fraudProofMissingNativeScriptUtxoStep05",
    "fraudProofMissingNativeScriptUtxoStep06",
    "fraudProofMissingNativeScriptUtxoStep07",
  ],
  nativeScriptInvalid: [
    "fraudProofNativeScriptInvalid",
    "fraudProofNativeScriptInvalidStep02",
    "fraudProofNativeScriptInvalidStep03",
    "fraudProofNativeScriptInvalidStep04",
    "fraudProofNativeScriptInvalidStep05",
  ],
  minAda: [
    "fraudProofMinAda",
    "fraudProofMinAdaStep02",
    "fraudProofMinAdaStep03",
    "fraudProofMinAdaStep04",
    "fraudProofMinAdaStep05",
  ],
} as const satisfies Record<
  SupportedFaultProofCategoryName,
  readonly [string, ...string[]]
>;

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
    case "noReferenceInput":
      return "no-reference-input";
    case "referenceInputNoIdx":
      return "reference-input-no-idx";
    case "invalidSignature":
      return "invalid-signature";
    case "fabricatedDeposit":
      return "fabricated-deposit";
    case "fabricatedWithdrawal":
      return "fabricated-withdrawal";
    case "nativeScriptDecoding":
      return "native-script-decoding";
    case "missingSignature":
      return "missing-signature";
    case "missingNativeScriptTx":
      return "missing-native-script-tx";
    case "withdrawnReferenceInput":
      return "withdrawn-reference-input";
    case "canonicalDecodability":
      return "canonical-decodability";
    case "committedFieldShape":
      return "committed-field-shape";
    case "minFee":
      return "min-fee";
    case "withdrawalMistag":
      return "withdrawal-mistag";
    case "doubleWithdraw":
      return "double-withdraw";
    case "crossBlockDuplicateEvent":
      return "cross-block-duplicate-event";
    case "l2TxMistag":
      return "l2-tx-mistag";
    case "withdrawnInput":
      return "withdrawn-input";
    case "valueNotPreserved":
      return "value-not-preserved";
    case "inputSetUniqueness":
      return "input-set-uniqueness";
    case "mintAuthorization":
      return "mint-authorization";
    case "networkId":
      return "network-id";
    case "missingNativeScriptUtxo":
      return "missing-native-script-utxo";
    case "nativeScriptInvalid":
      return "native-script-invalid";
    case "minAda":
      return "min-ada";
  }
};

type OneCategoryFaultProofContracts = Pick<
  FaultProofContracts,
  "computationThread" | "fraudProof"
> &
  Partial<{
    readonly [CategoryName in SupportedFaultProofCategoryName]: FaultProofContracts[CategoryName];
  }>;

const buildOneCategoryFaultProofContracts = async ({
  blueprint,
  network,
  hubOraclePolicyId,
  fraudProofCataloguePolicyId,
  categoryName,
}: {
  readonly blueprint: ReturnType<typeof parseFaultProofBlueprint>;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly fraudProofCataloguePolicyId: string;
  readonly categoryName: SupportedFaultProofCategoryName;
}): Promise<OneCategoryFaultProofContracts> => {
  const params = {
    blueprint,
    network,
    hubOraclePolicyId,
    fraudProofCataloguePolicyId,
  };
  switch (categoryName) {
    case "doubleSpend":
      return await Effect.runPromise(
        buildDoubleSpendFaultProofContracts(params),
      );
    case "nonExistentInput":
      return await Effect.runPromise(
        buildNonExistentInputFaultProofContracts(params),
      );
    case "nonExistentInputNoIndex":
      return await Effect.runPromise(
        buildInputNoIdxFaultProofContracts(params),
      );
    case "invalidRange":
      return await Effect.runPromise(
        buildInvalidRangeFaultProofContracts(params),
      );
    case "transitionTrace":
      return await Effect.runPromise(
        buildTransitionTraceFaultProofContracts(params),
      );
    case "zeroInput":
      return await Effect.runPromise(buildZeroInputFaultProofContracts(params));
    case "validationTraceDispute":
      return await Effect.runPromise(
        buildValidationTraceDisputeFaultProofContracts(params),
      );
    case "daHashPreimage":
      return await Effect.runPromise(
        buildDaHashPreimageFaultProofContracts(params),
      );
    case "noReferenceInput":
      return await Effect.runPromise(
        buildNoReferenceInputFaultProofContracts(params),
      );
    case "referenceInputNoIdx":
      return await Effect.runPromise(
        buildReferenceInputNoIdxFaultProofContracts(params),
      );
    case "invalidSignature":
      return await Effect.runPromise(
        buildInvalidSignatureFaultProofContracts(params),
      );
    case "fabricatedDeposit":
      return await Effect.runPromise(
        buildFabricatedDepositFaultProofContracts(params),
      );
    case "fabricatedWithdrawal":
      return await Effect.runPromise(
        buildFabricatedWithdrawalFaultProofContracts(params),
      );
    case "nativeScriptDecoding":
      return await Effect.runPromise(
        buildNativeScriptDecodingFaultProofContracts(params),
      );
    case "missingSignature":
      return await Effect.runPromise(
        buildMissingSignatureFaultProofContracts(params),
      );
    case "missingNativeScriptTx":
      return await Effect.runPromise(
        buildMissingNativeScriptTxFaultProofContracts(params),
      );
    case "withdrawnReferenceInput":
      return await Effect.runPromise(
        buildWithdrawnReferenceInputFaultProofContracts(params),
      );
    case "canonicalDecodability":
      return await Effect.runPromise(
        buildCanonicalDecodabilityFaultProofContracts(params),
      );
    case "committedFieldShape":
      return await Effect.runPromise(
        buildCommittedFieldShapeFaultProofContracts(params),
      );
    case "minFee":
      return await Effect.runPromise(buildMinFeeFaultProofContracts(params));
    case "withdrawalMistag":
      return await Effect.runPromise(
        buildWithdrawalMistagFaultProofContracts(params),
      );
    case "doubleWithdraw":
      return await Effect.runPromise(
        buildDoubleWithdrawFaultProofContracts(params),
      );
    case "crossBlockDuplicateEvent":
      return await Effect.runPromise(
        buildCrossBlockDuplicateEventFaultProofContracts(params),
      );
    case "l2TxMistag":
      return await Effect.runPromise(
        buildL2TxMistagFaultProofContracts(params),
      );
    case "withdrawnInput":
      return await Effect.runPromise(
        buildWithdrawnInputFaultProofContracts(params),
      );
    case "valueNotPreserved":
      return await Effect.runPromise(
        buildValueNotPreservedFaultProofContracts(params),
      );
    case "inputSetUniqueness":
      return await Effect.runPromise(
        buildInputSetUniquenessFaultProofContracts(params),
      );
    case "mintAuthorization":
      return await Effect.runPromise(
        buildMintAuthorizationFaultProofContracts(params),
      );
    case "networkId":
      return await Effect.runPromise(buildNetworkIdFaultProofContracts(params));
    case "missingNativeScriptUtxo":
      return await Effect.runPromise(
        buildMissingNativeScriptUtxoFaultProofContracts(params),
      );
    case "nativeScriptInvalid":
      return await Effect.runPromise(
        buildNativeScriptInvalidFaultProofContracts(params),
      );
    case "minAda":
      return await Effect.runPromise(buildMinAdaFaultProofContracts(params));
  }
};

export const resolveFaultProofDeploymentContracts = async ({
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
  readonly contracts: OneCategoryFaultProofContracts;
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
  const parsedBlueprint = parseFaultProofBlueprint(blueprint);
  const contracts = await buildOneCategoryFaultProofContracts({
    blueprint: parsedBlueprint,
    network,
    hubOraclePolicyId,
    fraudProofCataloguePolicyId,
    categoryName,
  });
  const categoryContracts = contracts[categoryName];
  if (categoryContracts === undefined) {
    throw new Error(
      `${categoryLabel(categoryName)} builder did not return its category chain.`,
    );
  }
  const derivedFirstStepHash = categoryContracts.firstStep.spendingScriptHash;
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
  const deploymentEntries =
    FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY[categoryName];
  if (deploymentEntries.length > categoryContracts.steps.length) {
    throw new Error(
      `${categoryLabel(categoryName)} manifest declares ${deploymentEntries.length.toString()} step entries, but the compiled chain has only ${categoryContracts.steps.length.toString()} steps.`,
    );
  }
  for (const [stepIndex, deploymentEntry] of deploymentEntries.entries()) {
    const derivedStep = categoryContracts.steps[stepIndex];
    if (derivedStep === undefined) {
      throw new Error(
        `${categoryLabel(categoryName)} is missing compiled step ${(stepIndex + 1).toString()}.`,
      );
    }
    const deployedStepHash =
      stepIndex === 0
        ? requireDeploymentScriptHash(parsedDeploymentInfo, deploymentEntry)
        : parsedDeploymentInfo[deploymentEntry]?.scriptHash;
    if (deployedStepHash === undefined) {
      continue;
    }
    requireMatchingScriptHash({
      label: `${deploymentEntry} step-${(stepIndex + 1).toString().padStart(2, "0")} script`,
      deployed: deployedStepHash,
      derived: derivedStep.spendingScriptHash,
    });
  }
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
  const contracts =
    resolved.contracts as ValidationTraceDisputeFaultProofContracts;
  const deployedCekProgramMaterialScriptHash = requireDeploymentScriptHash(
    resolved.deploymentInfo,
    "cekProgramMaterialSpend",
  );
  requireMatchingScriptHash({
    label: "cekProgramMaterialSpend script",
    deployed: deployedCekProgramMaterialScriptHash,
    derived:
      contracts.validationTraceDispute.cekProgramMaterial.spendingScriptHash,
  });
  const cekProgramMaterialAddress =
    contracts.validationTraceDispute.cekProgramMaterial.spendingScriptAddress;
  const addressCredential = getAddressDetails(
    cekProgramMaterialAddress,
  ).paymentCredential;
  if (
    addressCredential?.type !== "Script" ||
    addressCredential.hash !== deployedCekProgramMaterialScriptHash
  ) {
    throw new Error(
      `Derived CEK program-material address ${cekProgramMaterialAddress} is not locked by deployed script ${deployedCekProgramMaterialScriptHash}.`,
    );
  }
  return {
    deploymentInfo: resolved.deploymentInfo,
    referenceScriptAuthPolicyId,
    cekProgramMaterialScriptHash: deployedCekProgramMaterialScriptHash,
    cekProgramMaterialAddress,
    validationTraceDisputeCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts,
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

export const resolveNoReferenceInputDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedNoReferenceInputDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "noReferenceInput",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    noReferenceInputCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as NoReferenceInputFaultProofContracts,
  };
};

export const resolveReferenceInputNoIdxDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedReferenceInputNoIdxDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "referenceInputNoIdx",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    referenceInputNoIdxCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as ReferenceInputNoIdxFaultProofContracts,
  };
};

export const resolveInvalidSignatureDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedInvalidSignatureDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "invalidSignature",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    invalidSignatureCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as InvalidSignatureFaultProofContracts,
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

/**
 * The bare-load door (#610). Every caller of this function deploys the returned
 * `compiledCode` as-is, applying nothing — so it is only sound while the
 * validator declares no parameters.
 *
 * A declared parameter deployed unapplied is the #605 under-application shape:
 * the remaining `validator main(...)` parameters stay as lambdas, the ledger's
 * single Plutus V3 script-context application reduces to a lambda VALUE instead
 * of running the validator body, evaluation terminates without error, and the
 * ledger reads "no error" as SUCCESS. The deployment is an unconditional
 * always-succeeds script standing where an authenticated one should be, and
 * nothing downstream can tell the difference. Before this check an arity
 * mismatch surfaced only as an opaque `→ undefined` evaluation failure several
 * hundred milliseconds into a submission, if at all.
 *
 * `parseFaultProofBlueprint` normalises an absent `parameters` key to the empty
 * list, so ABSENT MEANS ZERO here — never "unknown, skip the check".
 */
type ParsedFaultProofBlueprintValidator = ReturnType<
  typeof parseFaultProofBlueprint
>["validators"][number];

const requireUniqueBlueprintValidator = (
  blueprint: unknown,
  title: string,
): ParsedFaultProofBlueprintValidator => {
  const parsed = parseFaultProofBlueprint(blueprint);
  const matches = parsed.validators.filter(
    (validator) => validator.title === title,
  );
  if (matches.length === 0) {
    throw new Error(`Validator with title "${title}" not found in blueprint.`);
  }
  if (matches.length > 1) {
    throw new Error(
      `Blueprint must contain exactly one validator with title "${title}"; found ${matches.length.toString()}.`,
    );
  }
  const found = matches[0];
  if (found === undefined) {
    throw new Error(
      `Blueprint uniqueness check for validator "${title}" succeeded without a matching validator.`,
    );
  }
  return found;
};

export const getCompiledScript = (
  blueprint: unknown,
  title: string,
): string => {
  const found = requireUniqueBlueprintValidator(blueprint, title);
  const declaredParameters = found.parameters;
  if (declaredParameters.length !== 0) {
    throw new Error(
      `${title} declares ${declaredParameters.length} parameter(s) but this loader deploys compiledCode bare — declared: ${declaredParameters
        .map((parameter) => parameter.title)
        .join(
          ", ",
        )}. An unapplied declared parameter deploys an always-succeeds script; route this title through an arity-checking parameter-applying helper instead of widening this zero-arity door (#610).`,
    );
  }
  return found.compiledCode;
};

/**
 * Apply one blueprint validator only after proving the supplied parameter list
 * has exactly the declared arity. This is the parameterized counterpart to
 * {@link getCompiledScript}; neither under- nor over-application is permitted.
 */
export const applyBlueprintParamsExact = ({
  blueprint,
  title,
  params,
}: {
  readonly blueprint: unknown;
  readonly title: string;
  readonly params: readonly Data[];
}): string => {
  const found = requireUniqueBlueprintValidator(blueprint, title);
  if (found.parameters.length !== params.length) {
    throw new Error(
      `${title} declares ${found.parameters.length.toString()} parameter(s), but ${params.length.toString()} were supplied. Apply exactly the declared parameters; under-application deploys an always-succeeds script (#609).`,
    );
  }
  return applyParamsToScript<Data[]>(found.compiledCode, [...params]);
};

/**
 * Measure an unapplied blueprint body without deploying it. The caller must
 * pin the expected declared arity so an accidental parameter-shape change
 * fails alongside the byte-size measurement.
 */
export const measureBlueprintValidatorBytes = ({
  blueprint,
  title,
  expectedDeclaredParameterCount,
}: {
  readonly blueprint: unknown;
  readonly title: string;
  readonly expectedDeclaredParameterCount: number;
}): number => {
  const found = requireUniqueBlueprintValidator(blueprint, title);
  if (found.parameters.length !== expectedDeclaredParameterCount) {
    throw new Error(
      `${title} declares ${found.parameters.length.toString()} parameter(s), not the measured invariant ${expectedDeclaredParameterCount.toString()}.`,
    );
  }
  return found.compiledCode.length / 2;
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
