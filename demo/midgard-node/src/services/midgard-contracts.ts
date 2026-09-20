import { createHash } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  MIDGARD_CONSENSUS_PROFILE,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  assertDeploymentMarkerMatches,
  type DeploymentManifestL1Finality,
  type DeploymentMarker,
  makeDeploymentMarker,
  parseDeploymentManifestAvailabilityChallenge,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { normalizeOutRef } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  MintingPolicy,
  mintingPolicyToId,
  Network,
  type Script,
  scriptHashToCredential,
  SpendingValidator,
  validatorToAddress,
  validatorToScriptHash,
  WithdrawalValidator,
} from "@lucid-evolution/lucid";
import { Effect, Layer } from "effect";

import { parseDeploymentManifestValue } from "../deployment-manifest.js";
import {
  defaultDeploymentRunStatePath,
  loadDeploymentRunState,
} from "../e2e/run-state.js";
import {
  contractDeploymentInfoPathOverride,
  daAvailabilityChallengeEnvironmentInput,
  realBlueprintPathOverride,
} from "../environment.js";
import { AlwaysSucceedsContract } from "./always-succeeds.js";
import { NodeConfig, type NodeConfigDep } from "./config.js";

/**
 * Contract-loading service for Midgard validators.
 *
 * This module can either expose the always-succeeds bundle for test flows or
 * derive the real script set from a blueprint, applying protocol parameters
 * where required.
 */
type Blueprint = SDK.FaultProofBlueprint;

export type ContractDeploymentIdentityValue = {
  readonly kind: "manifest" | "derived";
  readonly manifestId?: string;
  readonly deploymentMarker?: DeploymentMarker;
  readonly path?: string;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly l1Finality?: DeploymentManifestL1Finality;
  /** Exact parser-admitted manifest; absent for derived/dev contract bundles. */
  readonly manifest?: DeploymentManifest;
};

type MidgardContractRuntimeValue = {
  readonly contracts: SDK.MidgardValidators;
  readonly identity: ContractDeploymentIdentityValue;
};

export const availabilityParametersFromManifest = (
  value: unknown,
): SDK.DaAvailabilityParameters => {
  const parsed = parseDeploymentManifestAvailabilityChallenge(value);
  return SDK.daAvailabilityParameters({
    responseGeometry: SDK.availabilityResponseGeometry(parsed.responseGeometry),
    daBondLovelace: BigInt(parsed.daBondLovelace),
    challengerBondLovelace: BigInt(parsed.challengerBondLovelace),
    maxOpenFeeLovelace: BigInt(parsed.maxOpenFeeLovelace),
    maxPublicationFeeLovelace: BigInt(parsed.maxPublicationFeeLovelace),
    maxSettlementFeeLovelace: BigInt(parsed.maxSettlementFeeLovelace),
    maxCloseFeeLovelace: BigInt(parsed.maxCloseFeeLovelace),
    maxTimeoutFeeLovelace: BigInt(parsed.maxTimeoutFeeLovelace),
  });
};

export const availabilityParametersFromExplicitEnvironment =
  (): SDK.DaAvailabilityParameters =>
    availabilityParametersFromManifest(
      daAvailabilityChallengeEnvironmentInput(
        (name) =>
          `${name} must be set to an explicit positive integer before deriving Q58 scripts without a finalized manifest`,
      ),
    );

const moduleDir = path.dirname(fileURLToPath(import.meta.url));
const DEFAULT_REAL_BLUEPRINT_CANDIDATES = [
  path.resolve(moduleDir, "../../../../onchain/aiken/plutus.json"),
  path.resolve(moduleDir, "../../../onchain/aiken/plutus.json"),
  path.resolve(process.cwd(), "../../onchain/aiken/plutus.json"),
  path.resolve(process.cwd(), "onchain/aiken/plutus.json"),
] as const;
/**
 * Cached real blueprint loaded from either `MIDGARD_REAL_BLUEPRINT_PATH` or
 * the canonical onchain Aiken build output.
 */
let cachedRealBlueprint:
  | {
      readonly path: string;
      readonly blueprint: Blueprint;
    }
  | undefined;

const parseBlueprint = (raw: string, sourcePath: string): Blueprint => {
  try {
    return SDK.parseFaultProofBlueprint(JSON.parse(raw) as unknown);
  } catch (cause) {
    throw new Error(`Invalid blueprint at "${sourcePath}"`, { cause });
  }
};

const resolveDefaultRealBlueprintPath = (): string => {
  for (const candidate of new Set(DEFAULT_REAL_BLUEPRINT_CANDIDATES)) {
    if (existsSync(candidate)) {
      return candidate;
    }
  }

  throw new Error(
    `Failed to locate canonical real blueprint. Looked in: ${DEFAULT_REAL_BLUEPRINT_CANDIDATES.join(", ")}`,
  );
};

const resolveConfiguredRealBlueprintPath = (): string =>
  realBlueprintPathOverride() ?? resolveDefaultRealBlueprintPath();

export const loadRealBlueprintSha256 = (): Effect.Effect<string, Error> =>
  Effect.try({
    try: () => {
      const blueprintPath = resolveConfiguredRealBlueprintPath();
      const raw = readFileSync(blueprintPath);
      parseBlueprint(raw.toString("utf8"), blueprintPath);
      return createHash("sha256").update(raw).digest("hex");
    },
    catch: (cause) =>
      new Error(
        `Failed to hash canonical real blueprint: ${formatUnknownError(cause)}`,
      ),
  });

/**
 * Loads the real-contract blueprint, optionally honoring an override path from
 * the environment.
 */
const loadRealBlueprint = (): Effect.Effect<Blueprint, Error> =>
  Effect.try({
    try: () => {
      const blueprintPath = resolveConfiguredRealBlueprintPath();

      if (cachedRealBlueprint?.path === blueprintPath) {
        return cachedRealBlueprint.blueprint;
      }

      const blueprint = parseBlueprint(
        readFileSync(blueprintPath, "utf8"),
        blueprintPath,
      );

      cachedRealBlueprint = {
        path: blueprintPath,
        blueprint,
      };
      return blueprint;
    },
    catch: (cause) =>
      new Error(`Failed to load real blueprint: ${formatUnknownError(cause)}`),
  });

const loadReferenceScriptAuthValidator = (): Effect.Effect<
  SDK.MintingValidator,
  Error
> =>
  Effect.tryPromise({
    try: async () => {
      const runStatePath = defaultDeploymentRunStatePath();
      const runState = await loadDeploymentRunState(runStatePath);
      if (runState === null) {
        throw new Error(`Deployment run state does not exist: ${runStatePath}`);
      }
      const referenceScriptAuthPolicy =
        runState.identity.referenceScriptAuthPolicy;
      const policyId =
        typeof referenceScriptAuthPolicy?.policyId === "string"
          ? referenceScriptAuthPolicy.policyId
          : "";
      const cborHex =
        referenceScriptAuthPolicy?.nativeScript?.type === "Native" &&
        typeof referenceScriptAuthPolicy.nativeScript.cborHex === "string"
          ? referenceScriptAuthPolicy.nativeScript.cborHex
          : "";
      if (!/^[0-9a-fA-F]{56}$/.test(policyId)) {
        throw new Error(
          `Deployment run state at "${runStatePath}" does not contain a valid identity.referenceScriptAuthPolicy.policyId`,
        );
      }
      if (!/^[0-9a-fA-F]+$/.test(cborHex)) {
        throw new Error(
          `Deployment run state at "${runStatePath}" does not contain a valid identity.referenceScriptAuthPolicy.nativeScript.cborHex`,
        );
      }
      const mintingScript: MintingPolicy = {
        type: "Native",
        script: cborHex,
      };
      const derivedPolicyId = mintingPolicyToId(mintingScript);
      if (derivedPolicyId !== policyId.toLowerCase()) {
        throw new Error(
          `referenceScriptAuthPolicy policy id mismatch: configured=${policyId}, derived=${derivedPolicyId}`,
        );
      }
      return {
        mintingScriptCBOR: cborHex,
        mintingScript,
        policyId: derivedPolicyId,
      };
    },
    catch: (cause) =>
      new Error(
        `Failed to load reference-script auth policy id from deployment run state: ${formatUnknownError(
          cause,
        )}`,
      ),
  });

export const parseRuntimeDeploymentManifest = (
  raw: unknown,
): DeploymentManifest => parseDeploymentManifestValue(raw);

export const readRuntimeDeploymentManifestFile = (
  deploymentInfoPath: string,
  required: boolean,
):
  | {
      readonly path: string;
      readonly manifest: DeploymentManifest;
    }
  | undefined => {
  if (!existsSync(deploymentInfoPath)) {
    if (required) {
      throw new Error(
        `Configured deployment manifest does not exist: ${deploymentInfoPath}`,
      );
    }
    return undefined;
  }
  const parsed = JSON.parse(
    readFileSync(deploymentInfoPath, "utf8"),
  ) as unknown;
  return {
    path: deploymentInfoPath,
    manifest: parseRuntimeDeploymentManifest(parsed),
  };
};

const readConfiguredDeploymentManifest = () => {
  const configuredPath = contractDeploymentInfoPathOverride();
  if (configuredPath === undefined) {
    return undefined;
  }
  return readRuntimeDeploymentManifestFile(path.resolve(configuredPath), true);
};

const requireManifestString = (
  value: unknown,
  field: string,
  sourcePath: string,
): string => {
  if (typeof value === "string" && value.length > 0) {
    return value;
  }
  throw new Error(`Deployment manifest at "${sourcePath}" is missing ${field}`);
};

const requireManifestInteger = (
  value: unknown,
  field: string,
  sourcePath: string,
): number => {
  if (typeof value === "number" && Number.isSafeInteger(value) && value >= 0) {
    return value;
  }
  throw new Error(
    `Deployment manifest at "${sourcePath}" has invalid ${field}`,
  );
};

const isManifestScriptType = (value: string): value is Script["type"] =>
  value === "Native" ||
  value === "PlutusV1" ||
  value === "PlutusV2" ||
  value === "PlutusV3";

const requireManifestScriptType = (
  value: unknown,
  field: string,
  sourcePath: string,
): Script["type"] => {
  const scriptType = requireManifestString(value, field, sourcePath);
  if (isManifestScriptType(scriptType)) {
    return scriptType;
  }
  throw new Error(
    `Deployment manifest at "${sourcePath}" has invalid ${field}: ${scriptType}`,
  );
};

export const assertDeploymentManifestMatchesConfig = (
  manifest: DeploymentManifest,
  sourcePath: string,
  nodeConfig: Pick<
    NodeConfigDep,
    | "NETWORK"
    | "L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS"
    | "HUB_ORACLE_ONE_SHOT_TX_HASH"
    | "HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX"
    | "MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE"
    | "OPERATOR_REQUIRED_BOND_LOVELACE"
    | "OPERATOR_SLASHING_PENALTY_LOVELACE"
  >,
): void => {
  const mismatches: string[] = [];
  const manifestNetwork = requireManifestString(
    manifest.network,
    "network",
    sourcePath,
  );
  const manifestReferenceScriptAddress = requireManifestString(
    manifest.referenceScriptDeployAddress,
    "referenceScriptDeployAddress",
    sourcePath,
  );
  const manifestOneShotTxHash = requireManifestString(
    manifest.hubOracleOneShot?.txHash,
    "hubOracleOneShot.txHash",
    sourcePath,
  ).toLowerCase();
  const manifestOneShotOutputIndex = requireManifestInteger(
    manifest.hubOracleOneShot?.outputIndex,
    "hubOracleOneShot.outputIndex",
    sourcePath,
  );

  if (manifestNetwork !== nodeConfig.NETWORK) {
    mismatches.push(
      `network manifest=${manifestNetwork} config=${nodeConfig.NETWORK}`,
    );
  }
  if (
    manifestReferenceScriptAddress !==
    nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS
  ) {
    mismatches.push(
      `referenceScriptDeployAddress manifest=${manifestReferenceScriptAddress} config=${nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS}`,
    );
  }
  if (manifestOneShotTxHash !== nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH) {
    mismatches.push(
      `hubOracleOneShot.txHash manifest=${manifestOneShotTxHash} config=${nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH}`,
    );
  }
  if (
    manifestOneShotOutputIndex !== nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX
  ) {
    mismatches.push(
      `hubOracleOneShot.outputIndex manifest=${manifestOneShotOutputIndex.toString()} config=${nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX.toString()}`,
    );
  }
  if (
    manifest.economics.profile !==
    nodeConfig.MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE
  ) {
    mismatches.push(
      `economics.profile manifest=${manifest.economics.profile} config=${nodeConfig.MIDGARD_DEPLOYMENT_ECONOMICS_PROFILE}`,
    );
  }
  if (
    BigInt(manifest.economics.requiredBondLovelace) !==
    nodeConfig.OPERATOR_REQUIRED_BOND_LOVELACE
  ) {
    mismatches.push(
      `economics.requiredBondLovelace manifest=${manifest.economics.requiredBondLovelace.toString()} config=${nodeConfig.OPERATOR_REQUIRED_BOND_LOVELACE.toString()}`,
    );
  }
  if (
    BigInt(manifest.economics.slashingPenaltyLovelace) !==
    nodeConfig.OPERATOR_SLASHING_PENALTY_LOVELACE
  ) {
    mismatches.push(
      `economics.slashingPenaltyLovelace manifest=${manifest.economics.slashingPenaltyLovelace.toString()} config=${nodeConfig.OPERATOR_SLASHING_PENALTY_LOVELACE.toString()}`,
    );
  }
  if (mismatches.length > 0) {
    throw new Error(
      `Deployment manifest at "${sourcePath}" does not match node config: ${mismatches.join(
        "; ",
      )}`,
    );
  }
};

const manifestScript = (
  manifest: DeploymentManifest,
  sourcePath: string,
  name: string,
): {
  readonly script: Script;
  readonly scriptHash: string;
  readonly cborHex: string;
} => {
  const entry = manifest.contracts?.[name];
  if (entry === undefined) {
    throw new Error(
      `Deployment manifest at "${sourcePath}" is missing contracts.${name}`,
    );
  }
  const type = requireManifestScriptType(
    entry.contract?.type,
    `contracts.${name}.contract.type`,
    sourcePath,
  );
  const cborHex = requireManifestString(
    entry.contract?.cborHex,
    `contracts.${name}.contract.cborHex`,
    sourcePath,
  );
  const scriptHash = requireManifestString(
    entry.scriptHash,
    `contracts.${name}.scriptHash`,
    sourcePath,
  ).toLowerCase();
  if (!/^[0-9a-fA-F]+$/.test(cborHex)) {
    throw new Error(
      `Deployment manifest at "${sourcePath}" has non-hex contracts.${name}.contract.cborHex`,
    );
  }
  return {
    script: {
      type,
      script: cborHex,
    },
    scriptHash,
    cborHex,
  };
};

const assertManifestScriptHash = (
  sourcePath: string,
  name: string,
  expected: string,
  actual: string,
): void => {
  if (expected !== actual) {
    throw new Error(
      `Deployment manifest at "${sourcePath}" has invalid contracts.${name}.scriptHash: expected=${expected}, derived=${actual}`,
    );
  }
};

const mintingValidatorFromManifest = (
  manifest: DeploymentManifest,
  sourcePath: string,
  name: string,
): SDK.MintingValidator => {
  const entry = manifestScript(manifest, sourcePath, name);
  const mintingScript = entry.script as MintingPolicy;
  const policyId = mintingPolicyToId(mintingScript);
  assertManifestScriptHash(sourcePath, name, entry.scriptHash, policyId);
  return {
    mintingScriptCBOR: entry.cborHex,
    mintingScript,
    policyId,
  };
};

const spendingValidatorFromManifest = (
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
  name: string,
): SDK.SpendingValidator => {
  const entry = manifestScript(manifest, sourcePath, name);
  const spendingScript = entry.script as SpendingValidator;
  const spendingScriptHash = validatorToScriptHash(spendingScript);
  assertManifestScriptHash(
    sourcePath,
    name,
    entry.scriptHash,
    spendingScriptHash,
  );
  return {
    spendingScriptCBOR: entry.cborHex,
    spendingScript,
    spendingScriptHash,
    spendingScriptAddress: validatorToAddress(network, spendingScript),
  };
};

const withdrawalValidatorFromManifest = (
  manifest: DeploymentManifest,
  sourcePath: string,
  name: string,
): SDK.WithdrawalValidator => {
  const entry = manifestScript(manifest, sourcePath, name);
  const withdrawalScript = entry.script as WithdrawalValidator;
  const withdrawalScriptHash = validatorToScriptHash(withdrawalScript);
  assertManifestScriptHash(
    sourcePath,
    name,
    entry.scriptHash,
    withdrawalScriptHash,
  );
  return {
    withdrawalScriptCBOR: entry.cborHex,
    withdrawalScript,
    withdrawalScriptHash,
  };
};

const authenticatedValidatorFromManifest = (
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
  spendName: string,
  mintName: string,
): SDK.AuthenticatedValidator => ({
  ...spendingValidatorFromManifest(network, manifest, sourcePath, spendName),
  ...mintingValidatorFromManifest(manifest, sourcePath, mintName),
});

// This tuple is a compile-time registry used to derive the exact category union.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
const REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES = [
  "fabricatedDeposit",
  "fabricatedWithdrawal",
  "nativeScriptDecoding",
  "missingSignature",
  "missingNativeScriptTx",
  "withdrawnReferenceInput",
  "canonicalDecodability",
  "committedFieldShape",
  "minFee",
  "withdrawalMistag",
  "doubleWithdraw",
  "crossBlockDuplicateEvent",
  "l2TxMistag",
  "withdrawnInput",
  "valueNotPreserved",
  "inputSetUniqueness",
  "mintAuthorization",
  "networkId",
  "missingNativeScriptUtxo",
  "nativeScriptInvalid",
  "minAda",
  "fieldPreimageLengthMismatch",
  "fieldItemWidthIllegal",
  "witnessScriptDecoding",
  "scriptIntegrityHashMissing",
  "transactionOutputNonCanonical",
  "mintItemNonCanonical",
  "resolvedOutputNonCanonical",
  "mintDeclaredAssetLimit",
  "spendInputSignerMissing",
  "protectedOutputSignerMissing",
  "observersForbiddenOnUntaggedNetwork",
  "observerOrderInvalid",
  "redeemerCanonicity",
  "outputReferenceScriptDecoding",
  "executionSourceScriptDecoding",
  "receivePurposeLanguage",
  "unusedScriptWitness",
  "missingScriptSource",
  "missingRedeemer",
  "unusedRedeemer",
  "executionNativeScriptInvalid",
  "scriptIntegrityHashMismatch",
  "distinctAssetAccumulationLimit",
] as const satisfies readonly (keyof SDK.FaultProofContractChains)[];

type RegisteredLinearFaultProofCategory =
  (typeof REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES)[number];

const upperFirst = (value: string): string =>
  `${value.slice(0, 1).toUpperCase()}${value.slice(1)}`;

const faultProofStepContractName = (
  category: RegisteredLinearFaultProofCategory,
  stepIndex: number,
): string => {
  if (category === "nativeScriptDecoding") {
    const names = [
      "fraudProofNativeScriptDecoding",
      "fraudProofNativeScriptDecodingStep02",
      "fraudProofNativeScriptDecodingStep03OpenSubject",
      "fraudProofNativeScriptDecodingStep03BindDescriptor",
      "fraudProofNativeScriptDecodingStep03AdvanceOrClose",
      "fraudProofNativeScriptDecodingStep04",
    ] as const;
    const name = names[stepIndex];
    if (name === undefined) {
      throw new Error(
        `native-script-decoding exposes an unexpected step index ${stepIndex.toString()}`,
      );
    }
    return name;
  }
  if (category === "fieldPreimageLengthMismatch") {
    const names = [
      "fraudProofFieldPreimageLengthMismatch",
      "fraudProofFieldPreimageLengthMismatchStep02Accepted",
      "fraudProofFieldPreimageLengthMismatchStep02Forced",
      "fraudProofFieldPreimageLengthMismatchStep03",
    ] as const;
    const name = names[stepIndex];
    if (name === undefined)
      throw new Error(
        `field-preimage-length-mismatch exposes an unexpected step index ${stepIndex.toString()}`,
      );
    return name;
  }
  if (category === "scriptIntegrityHashMissing") {
    const names = [
      "fraudProofScriptIntegrityHashMissing",
      "fraudProofScriptIntegrityHashMissingStep02",
      "fraudProofScriptIntegrityHashMissingStep03",
      "fraudProofScriptIntegrityHashMissingScriptGrammar",
      "fraudProofScriptIntegrityHashMissingScriptScan",
      "fraudProofScriptIntegrityHashMissingRedeemerGrammar",
      "fraudProofScriptIntegrityHashMissingStep04",
    ] as const;
    const name = names[stepIndex];
    if (name === undefined)
      throw new Error(
        `script-integrity-hash-missing exposes an unexpected step index ${stepIndex.toString()}`,
      );
    return name;
  }
  if (category === "missingRedeemer") {
    const names = [
      "fraudProofMissingRedeemer",
      "fraudProofMissingRedeemerStep02",
      "fraudProofMissingRedeemerStep02a",
      "fraudProofMissingRedeemerStep02b",
      "fraudProofMissingRedeemerStep03",
      "fraudProofMissingRedeemerStep04",
      "fraudProofMissingRedeemerStep05",
    ] as const;
    const name = names[stepIndex];
    if (name === undefined)
      throw new Error(
        `missing-redeemer exposes an unexpected step index ${stepIndex.toString()}`,
      );
    return name;
  }
  if (category === "unusedRedeemer") {
    const names = [
      "fraudProofUnusedRedeemer",
      "fraudProofUnusedRedeemerStep02",
      "fraudProofUnusedRedeemerStep02a",
      "fraudProofUnusedRedeemerStep02b",
      "fraudProofUnusedRedeemerStep02c",
      "fraudProofUnusedRedeemerStep03",
      "fraudProofUnusedRedeemerStep04",
      "fraudProofUnusedRedeemerStep05",
      "fraudProofUnusedRedeemerStep06",
    ] as const;
    const name = names[stepIndex];
    if (name === undefined)
      throw new Error(
        `unused-redeemer exposes an unexpected step index ${stepIndex.toString()}`,
      );
    return name;
  }
  if (category === "executionNativeScriptInvalid") {
    const names = [
      "fraudProofExecutionNativeScriptInvalid",
      "fraudProofExecutionNativeScriptInvalidStep02",
      "fraudProofExecutionNativeScriptInvalidStep03",
      "fraudProofExecutionNativeScriptInvalidStep04",
      "fraudProofExecutionNativeScriptInvalidStep05",
      "fraudProofExecutionNativeScriptInvalidStep06",
      "fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit",
      "fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix",
      "fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix",
      "fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix",
      "fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix",
      "fraudProofExecutionNativeScriptInvalidAcceptedInlineSource",
      "fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource",
    ] as const;
    const name = names[stepIndex];
    if (name === undefined)
      throw new Error(
        `execution-native-script-invalid exposes an unexpected step index ${stepIndex.toString()}`,
      );
    return name;
  }
  return `fraudProof${upperFirst(category)}${
    stepIndex === 0 ? "" : `Step${(stepIndex + 1).toString().padStart(2, "0")}`
  }`;
};

const linearFaultProofChainFromManifest = <
  Category extends RegisteredLinearFaultProofCategory,
>(
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
  baseContracts: SDK.MidgardValidators,
  category: Category,
): SDK.FaultProofContractChains[Category] => {
  const baseChain = baseContracts.fraudProofContracts[category];
  const steps = baseChain.steps.map((_validator, stepIndex) =>
    spendingValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      faultProofStepContractName(category, stepIndex),
    ),
  );
  const firstStep = steps[0];
  if (firstStep === undefined) {
    throw new Error(`Fault-proof chain has no first step: ${category}`);
  }
  if (category === "valueNotPreserved") {
    return {
      firstStep,
      steps,
      unionAcceptedSource: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionAcceptedSource",
      ),
      unionForcedSource: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionForcedSource",
      ),
      unionEvent: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionEvent",
      ),
      unionPreState: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionPreState",
      ),
      unionInputs: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionInputs",
      ),
      unionInputValue: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionInputValue",
      ),
      unionAssets: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionAssets",
      ),
      unionFieldGrammar: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionFieldGrammar",
      ),
      unionOutputs: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionOutputs",
      ),
      unionOutputScan: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionOutputScan",
      ),
      unionMint: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionMint",
      ),
      unionUpdate: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionUpdate",
      ),
      unionTerminal: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofValueNotPreservedUnionTerminal",
      ),
    } as unknown as SDK.FaultProofContractChains[Category];
  }
  if (category === "missingSignature") {
    return {
      firstStep,
      steps,
      forcedStep: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofMissingSignatureForcedStep",
      ),
      forcedSigner: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofMissingSignatureForcedSigner",
      ),
      forcedWitness: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofMissingSignatureForcedWitness",
      ),
    } as unknown as SDK.FaultProofContractChains[Category];
  }
  if (category === "networkId") {
    // The forced (wrongful-rejection) door and the resumable output scan it
    // hands off to are side entrances into step 02, not third and fourth links
    // in the chain, so `buildNetworkIdChain` returns them outside `steps`.
    // Restoring either by step index would silently bind step 02's script to
    // an auxiliary role, so both are resolved by their own manifest names.
    return {
      firstStep,
      steps,
      forcedStep: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofNetworkIdForcedStep",
      ),
      forcedScan: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofNetworkIdForcedScan",
      ),
    } as unknown as SDK.FaultProofContractChains[Category];
  }
  if (category === "fieldPreimageLengthMismatch") {
    return {
      firstStep,
      steps,
      acceptedStep02: steps[1],
      forcedStep02: steps[2],
    } as unknown as SDK.FaultProofContractChains[Category];
  }
  if (category === "scriptIntegrityHashMissing") {
    return {
      firstStep,
      steps,
      scriptGrammar: steps[3],
      scriptScan: steps[4],
      redeemerGrammar: steps[5],
    } as unknown as SDK.FaultProofContractChains[Category];
  }
  return {
    firstStep,
    steps,
  } as unknown as SDK.FaultProofContractChains[Category];
};

const legacyFaultProofChainFromManifest = <Chain extends SDK.FraudProofChain>(
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
  baseChain: Chain,
  firstStepContractName: string,
): Chain => {
  const steps = baseChain.steps.map((_validator, stepIndex) =>
    spendingValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      stepIndex === 0
        ? firstStepContractName
        : `${firstStepContractName}Step${(stepIndex + 1)
            .toString()
            .padStart(2, "0")}`,
    ),
  );
  const firstStep = steps[0];
  if (firstStep === undefined) {
    throw new Error(`Legacy fault-proof chain has no first step`);
  }
  return {
    ...baseChain,
    firstStep,
    steps,
  } as unknown as Chain;
};

export const midgardContractsFromDeploymentManifest = (
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
  baseContracts: SDK.MidgardValidators,
): SDK.MidgardValidators => {
  const referenceScriptAuth = mintingValidatorFromManifest(
    manifest,
    sourcePath,
    "referenceScriptAuthMint",
  );
  const referenceScriptAuthPolicyId = requireManifestString(
    manifest.referenceScriptAuthPolicy?.policyId,
    "referenceScriptAuthPolicy.policyId",
    sourcePath,
  ).toLowerCase();
  if (referenceScriptAuth.policyId !== referenceScriptAuthPolicyId) {
    throw new Error(
      `Deployment manifest at "${sourcePath}" reference-script auth policy mismatch: contracts.referenceScriptAuthMint=${referenceScriptAuth.policyId}, referenceScriptAuthPolicy.policyId=${referenceScriptAuthPolicyId}`,
    );
  }
  const hubOracleMint = mintingValidatorFromManifest(
    manifest,
    sourcePath,
    "hubOracleMint",
  );
  const hubOracle: SDK.AuthenticatedValidator = {
    spendingScriptCBOR: baseContracts.hubOracle.spendingScriptCBOR,
    spendingScript: baseContracts.hubOracle.spendingScript,
    spendingScriptHash: hubOracleMint.policyId,
    spendingScriptAddress: credentialToAddress(
      network,
      scriptHashToCredential(hubOracleMint.policyId),
    ),
    ...hubOracleMint,
  };
  const txOrder = authenticatedValidatorFromManifest(
    network,
    manifest,
    sourcePath,
    "txOrderSpend",
    "txOrderMint",
  );
  // #579: no `txOrderFieldPreimage` or `txOrderFieldReceipt` resolution here.
  // The manifest no longer registers any of the three retired tx-field names,
  // so asking for one would throw on every manifest-sourced load.
  const fieldPreimageCertificate = {
    ...spendingValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "fieldPreimageCertificateSpend",
    ),
    ...mintingValidatorFromManifest(
      manifest,
      sourcePath,
      "fieldPreimageCertificateMint",
    ),
  };
  const cekProgramMaterial = spendingValidatorFromManifest(
    network,
    manifest,
    sourcePath,
    "cekProgramMaterialSpend",
  );
  const transitionTraceRoute = spendingValidatorFromManifest(
    network,
    manifest,
    sourcePath,
    "fraudProofTransitionTrace",
  );
  const transitionTraceFinals = [
    "fraudProofTransitionTraceControl",
    "fraudProofTransitionTraceSource",
    "fraudProofTransitionTraceWithdrawal",
    "fraudProofTransitionTraceForced",
    "fraudProofTransitionTraceAcceptedTransaction",
    "fraudProofTransitionTraceDeposit",
    "fraudProofTransitionTraceL1Event",
    "fraudProofTransitionTraceDuplicate",
  ].map((contractName) =>
    spendingValidatorFromManifest(network, manifest, sourcePath, contractName),
  ) as unknown as SDK.FaultProofContractChains["transitionTrace"]["finals"];
  const transitionTrace: SDK.FaultProofContractChains["transitionTrace"] = {
    firstStep: transitionTraceRoute,
    route: transitionTraceRoute,
    finals: transitionTraceFinals,
    steps: [transitionTraceRoute, ...transitionTraceFinals],
    yields: {
      l2Open: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionL2OpenWithdraw",
      ),
      l2Summaries: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionL2SummariesWithdraw",
      ),
      l2Replay: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionL2ReplayWithdraw",
      ),
      claimStructure: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionClaimStructureWithdraw",
      ),
      claimSource: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionClaimSourceWithdraw",
      ),
      claimEndpoints: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionClaimEndpointsWithdraw",
      ),
      depositProjection: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceDepositProjectionWithdraw",
      ),
      depositSummaries: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceDepositSummariesWithdraw",
      ),
      l2Assembly: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionL2AssemblyWithdraw",
      ),
      l2Scan: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionL2ScanWithdraw",
      ),
      l2Value: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceAcceptedTransactionL2ValueWithdraw",
      ),
      depositAssembly: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceDepositAssemblyWithdraw",
      ),
      depositScan: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceDepositScanWithdraw",
      ),
      depositValue: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceDepositValueWithdraw",
      ),
      depositReplay: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "fraudProofTransitionTraceDepositReplayWithdraw",
      ),
    },
  };
  const validationTraceOpener = spendingValidatorFromManifest(
    network,
    manifest,
    sourcePath,
    "validationTraceDispute",
  );
  const validationTraceSource = spendingValidatorFromManifest(
    network,
    manifest,
    sourcePath,
    "validationTraceDisputeSource",
  );
  const validationTraceGame = spendingValidatorFromManifest(
    network,
    manifest,
    sourcePath,
    "validationTraceDisputeGame",
  );
  const validationTraceBoundary = spendingValidatorFromManifest(
    network,
    manifest,
    sourcePath,
    "validationTraceDisputeBoundary",
  );
  const validationTraceTimeout = spendingValidatorFromManifest(
    network,
    manifest,
    sourcePath,
    "validationTraceDisputeTimeout",
  );
  const validationTraceAward = spendingValidatorFromManifest(
    network,
    manifest,
    sourcePath,
    "validationTraceDisputeAward",
  );
  const baseValidationTrace =
    baseContracts.fraudProofContracts.validationTraceDispute;
  const validationTraceDispute = {
    ...baseValidationTrace,
    firstStep: validationTraceOpener,
    opener: validationTraceOpener,
    source: validationTraceSource,
    game: validationTraceGame,
    boundary: validationTraceBoundary,
    timeout: validationTraceTimeout,
    award: validationTraceAward,
    steps: [
      validationTraceOpener,
      validationTraceSource,
      validationTraceGame,
      validationTraceBoundary,
      validationTraceTimeout,
      validationTraceAward,
      ...baseValidationTrace.steps.slice(6),
    ],
  } as unknown as SDK.FaultProofContractChains["validationTraceDispute"];
  const fraudProofContracts: SDK.FaultProofContractChains = {
    doubleSpend: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts.fraudProofContracts.doubleSpend,
      "fraudProofDoubleSpend",
    ),
    nonExistentInput: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts.fraudProofContracts.nonExistentInput,
      "fraudProofNonExistentInput",
    ),
    nonExistentInputNoIndex: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts.fraudProofContracts.nonExistentInputNoIndex,
      "fraudProofNonExistentInputNoIndex",
    ),
    invalidRange: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts.fraudProofContracts.invalidRange,
      "fraudProofInvalidRange",
    ),
    transitionTrace,
    zeroInput: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts.fraudProofContracts.zeroInput,
      "fraudProofZeroInput",
    ),
    validationTraceDispute,
    daHashPreimage: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts.fraudProofContracts.daHashPreimage,
      "fraudProofDaHashPreimage",
    ),
    noReferenceInput: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts.fraudProofContracts.noReferenceInput,
      "fraudProofNoReferenceInput",
    ),
    referenceInputNoIdx: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts.fraudProofContracts.referenceInputNoIdx,
      "fraudProofReferenceInputNoIdx",
    ),
    invalidSignature: legacyFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts.fraudProofContracts.invalidSignature,
      "fraudProofInvalidSignature",
    ),
    fabricatedDeposit: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "fabricatedDeposit",
    ),
    fabricatedWithdrawal: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "fabricatedWithdrawal",
    ),
    nativeScriptDecoding: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "nativeScriptDecoding",
    ),
    missingSignature: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "missingSignature",
    ),
    missingNativeScriptTx: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "missingNativeScriptTx",
    ),
    withdrawnReferenceInput: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "withdrawnReferenceInput",
    ),
    canonicalDecodability: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "canonicalDecodability",
    ),
    committedFieldShape: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "committedFieldShape",
    ),
    minFee: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "minFee",
    ),
    withdrawalMistag: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "withdrawalMistag",
    ),
    doubleWithdraw: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "doubleWithdraw",
    ),
    crossBlockDuplicateEvent: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "crossBlockDuplicateEvent",
    ),
    l2TxMistag: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "l2TxMistag",
    ),
    withdrawnInput: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "withdrawnInput",
    ),
    valueNotPreserved: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "valueNotPreserved",
    ),
    inputSetUniqueness: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "inputSetUniqueness",
    ),
    mintAuthorization: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "mintAuthorization",
    ),
    networkId: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "networkId",
    ),
    missingNativeScriptUtxo: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "missingNativeScriptUtxo",
    ),
    nativeScriptInvalid: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "nativeScriptInvalid",
    ),
    minAda: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "minAda",
    ),
    fieldPreimageLengthMismatch: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "fieldPreimageLengthMismatch",
    ),
    fieldItemWidthIllegal: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "fieldItemWidthIllegal",
    ),
    witnessScriptDecoding: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "witnessScriptDecoding",
    ),
    scriptIntegrityHashMissing: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "scriptIntegrityHashMissing",
    ),
    transactionOutputNonCanonical: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "transactionOutputNonCanonical",
    ),
    mintItemNonCanonical: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "mintItemNonCanonical",
    ),
    resolvedOutputNonCanonical: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "resolvedOutputNonCanonical",
    ),
    mintDeclaredAssetLimit: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "mintDeclaredAssetLimit",
    ),
    spendInputSignerMissing: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "spendInputSignerMissing",
    ),
    protectedOutputSignerMissing: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "protectedOutputSignerMissing",
    ),
    observersForbiddenOnUntaggedNetwork: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "observersForbiddenOnUntaggedNetwork",
    ),
    outputReferenceScriptDecoding: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "outputReferenceScriptDecoding",
    ),
    executionSourceScriptDecoding: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "executionSourceScriptDecoding",
    ),
    observerOrderInvalid: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "observerOrderInvalid",
    ),
    redeemerCanonicity: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "redeemerCanonicity",
    ),
    receivePurposeLanguage: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "receivePurposeLanguage",
    ),
    unusedScriptWitness: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "unusedScriptWitness",
    ),
    missingScriptSource: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "missingScriptSource",
    ),
    missingRedeemer: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "missingRedeemer",
    ),
    unusedRedeemer: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "unusedRedeemer",
    ),
    executionNativeScriptInvalid: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "executionNativeScriptInvalid",
    ),
    scriptIntegrityHashMismatch: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "scriptIntegrityHashMismatch",
    ),
    distinctAssetAccumulationLimit: linearFaultProofChainFromManifest(
      network,
      manifest,
      sourcePath,
      baseContracts,
      "distinctAssetAccumulationLimit",
    ),
  };
  const fraudProofs = SDK.fraudProofContractsToFirstSteps(fraudProofContracts);

  return {
    referenceScriptAuth,
    hubOracle,
    daParamsGovernor: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "daParamsGovernorSpend",
      "daParamsGovernorMint",
    ),
    daAttestation: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "daAttestationSpend",
      "daAttestationMint",
    ),
    availabilityChallenge: {
      ...authenticatedValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "availabilityChallengeSpend",
        "availabilityChallengeMint",
      ),
      yields: {
        bond: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "availabilityChallengeBondWithdraw",
        ),
        open: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "availabilityChallengeOpenWithdraw",
        ),
        settle: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "availabilityChallengeSettleWithdraw",
        ),
        close: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "availabilityChallengeCloseWithdraw",
        ),
        timeout: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "availabilityChallengeTimeoutWithdraw",
        ),
      },
    },
    correctionLock: spendingValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "correctionLockSpend",
    ),
    stateQueue: {
      ...authenticatedValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "stateQueueSpend",
        "stateQueueMint",
      ),
      yields: {
        commit: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "stateQueueCommitWithdraw",
        ),
        unattestedTimeout: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "stateQueueUnattestedTimeoutWithdraw",
        ),
        unavailableTimeout: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "stateQueueUnavailableTimeoutWithdraw",
        ),
        fraudRemoval: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "stateQueueFraudRemovalWithdraw",
        ),
        merge: withdrawalValidatorFromManifest(
          manifest,
          sourcePath,
          "stateQueueMergeWithdraw",
        ),
      },
    },
    scheduler: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "schedulerSpend",
      "schedulerMint",
    ),
    registeredOperators: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "registeredOperatorsSpend",
      "registeredOperatorsMint",
    ),
    activeOperators: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "activeOperatorsSpend",
      "activeOperatorsMint",
    ),
    retiredOperators: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "retiredOperatorsSpend",
      "retiredOperatorsMint",
    ),
    escapeHatch: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "escapeHatchSpend",
      "escapeHatchMint",
    ),
    fraudProofCatalogue: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "fraudProofCatalogueSpend",
      "fraudProofCatalogueMint",
    ),
    computationThread: mintingValidatorFromManifest(
      manifest,
      sourcePath,
      "computationThreadMint",
    ),
    fraudProof: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "fraudProofSpend",
      "fraudProofMint",
    ),
    chunkedVerify: withdrawalValidatorFromManifest(
      manifest,
      sourcePath,
      "chunkedVerifyWithdraw",
    ),
    pexcludes: withdrawalValidatorFromManifest(
      manifest,
      sourcePath,
      "pexcludesWithdraw",
    ),
    deposit: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "depositSpend",
      "depositMint",
    ),
    withdrawal: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "withdrawalSpend",
      "withdrawalMint",
    ),
    txOrder,
    fieldPreimageCertificate,
    cekProgramMaterial,
    settlement: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "settlementSpend",
      "settlementMint",
    ),
    reserve: {
      ...spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "reserveSpend",
      ),
      ...withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        "reserveWithdraw",
      ),
    },
    payout: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "payoutSpend",
      "payoutMint",
    ),
    fraudProofContracts,
    fraudProofs,
  };
};

/**
 * Blueprint titles for the real state-queue scripts.
 */
export const REAL_STATE_QUEUE_SCRIPT_TITLES = SDK.STATE_QUEUE_SCRIPT_TITLES;

export const REAL_CORRECTION_LOCK_SCRIPT_TITLES =
  SDK.CORRECTION_LOCK_SCRIPT_TITLES;

export const REAL_DA_PARAMS_GOVERNOR_SCRIPT_TITLES =
  SDK.DA_PARAMS_GOVERNOR_SCRIPT_TITLES;

export const REAL_DA_ATTESTATION_SCRIPT_TITLES =
  SDK.DA_ATTESTATION_SCRIPT_TITLES;

export const REAL_AVAILABILITY_CHALLENGE_SCRIPT_TITLES =
  SDK.AVAILABILITY_CHALLENGE_SCRIPT_TITLES;

/**
 * Blueprint titles for the real hub-oracle scripts.
 */
export const REAL_HUB_ORACLE_SCRIPT_TITLES =
  SDK.USER_EVENT_CONTRACT_TITLES.hubOracle;

/**
 * Blueprint titles for the real registered-operators scripts.
 */
export const REAL_REGISTERED_OPERATORS_SCRIPT_TITLES =
  SDK.REGISTERED_OPERATORS_SCRIPT_TITLES;

/**
 * Blueprint titles for the real active-operators scripts.
 */
export const REAL_ACTIVE_OPERATORS_SCRIPT_TITLES =
  SDK.ACTIVE_OPERATORS_SCRIPT_TITLES;

/**
 * Blueprint titles for the real retired-operators scripts.
 */
export const REAL_RETIRED_OPERATORS_SCRIPT_TITLES =
  SDK.RETIRED_OPERATORS_SCRIPT_TITLES;

/**
 * Blueprint titles for the real scheduler scripts.
 */
export const REAL_SCHEDULER_SCRIPT_TITLES = SDK.SCHEDULER_SCRIPT_TITLES;

/**
 * Blueprint titles for the real deposit scripts.
 */
export const REAL_DEPOSIT_SCRIPT_TITLES =
  SDK.USER_EVENT_CONTRACT_TITLES.deposit;

/**
 * Blueprint titles for the real tx-order scripts.
 */
export const REAL_TX_ORDER_SCRIPT_TITLES =
  SDK.USER_EVENT_CONTRACT_TITLES.txOrder;

/**
 * Blueprint titles for the real withdrawal scripts.
 */
export const REAL_WITHDRAWAL_SCRIPT_TITLES =
  SDK.USER_EVENT_CONTRACT_TITLES.withdrawal;

/**
 * Blueprint titles for the real settlement scripts.
 */
export const REAL_SETTLEMENT_SCRIPT_TITLES = SDK.SETTLEMENT_SCRIPT_TITLES;

/**
 * Blueprint titles for the real reserve scripts.
 */
export const REAL_RESERVE_SCRIPT_TITLES = SDK.RESERVE_SCRIPT_TITLES;

/**
 * Blueprint titles for the real payout scripts.
 */
export const REAL_PAYOUT_SCRIPT_TITLES = SDK.PAYOUT_SCRIPT_TITLES;

export const REAL_FRAUD_PROOF_CATALOGUE_SCRIPT_TITLES =
  SDK.FRAUD_PROOF_CATALOGUE_SCRIPT_TITLES;

export const REAL_COMPUTATION_THREAD_SCRIPT_TITLES =
  SDK.COMPUTATION_THREAD_SCRIPT_TITLES;

export const REAL_FRAUD_PROOF_SCRIPT_TITLES = SDK.FRAUD_PROOF_SCRIPT_TITLES;

/**
 * One-shot outref used to parameterize the real hub-oracle policy.
 */
export type HubOracleOneShotOutRef = {
  readonly txHash: string;
  readonly outputIndex: number;
};

export type RealContractDeploymentParameters = {
  readonly referenceScriptAuth: SDK.MintingValidator;
  readonly availabilityChallengeParameters: SDK.DaAvailabilityParameters;
  readonly daParamsGovernorInitOutRef?: HubOracleOneShotOutRef;
  readonly daParamsMaxCommitteeSize?: number;
  readonly daParamsMaxOwnerCount?: number;
};

/**
 * Normalizes the configured one-shot outref used to parameterize the real
 * hub-oracle policy.
 */
const normalizeHubOracleOneShotOutRef = (
  outRef: HubOracleOneShotOutRef,
): Effect.Effect<HubOracleOneShotOutRef, Error> =>
  Effect.try({
    try: () => normalizeOutRef(outRef),
    catch: (cause) =>
      new Error(`Invalid hub-oracle one-shot outref: ${String(cause)}`),
  });

/**
 * Builds the real hub-oracle minting validator parameterized by the configured
 * one-shot outref.
 */
const buildRealHubOracleValidator = (
  network: Network,
  fallbackSpendingValidator: SDK.SpendingValidator,
  oneShotOutRef: HubOracleOneShotOutRef,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const { mintingScriptCBOR, mintingScript, policyId } = yield* Effect.try({
      try: () =>
        SDK.buildHubOracleMintingValidator({ blueprint, oneShotOutRef }),
      catch: (cause) =>
        new Error("Failed to derive hub-oracle minting validator", { cause }),
    });
    return {
      spendingScriptCBOR: fallbackSpendingValidator.spendingScriptCBOR,
      spendingScript: fallbackSpendingValidator.spendingScript,
      // The canonical Aiken tree only ships the one-shot mint policy. The
      // witness UTxO lives at the script credential derived from that policy id.
      spendingScriptHash: policyId,
      spendingScriptAddress: credentialToAddress(
        network,
        scriptHashToCredential(policyId),
      ),
      mintingScriptCBOR,
      mintingScript,
      policyId,
    };
  });

const buildRealFraudProofCatalogueValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildFraudProofCatalogueValidator(blueprint, network, contracts),
      catch: (cause) =>
        new Error("Failed to build FraudProofCatalogueValidator", { cause }),
    });
  });

const buildRealComputationThreadValidator = (
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.MintingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () => SDK.buildComputationThreadValidator(blueprint, contracts),
      catch: (cause) =>
        new Error("Failed to build ComputationThreadValidator", { cause }),
    });
  });

const buildRealFraudProofValidator = (
  network: Network,
  computationThread: SDK.MintingValidator,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildFraudProofValidator(blueprint, network, computationThread),
      catch: (cause) =>
        new Error("Failed to build FraudProofValidator", { cause }),
    });
  });

const buildRealFraudProofSharedWithdrawalValidators = (): Effect.Effect<
  Readonly<{
    chunkedVerify: SDK.WithdrawalValidator;
    pexcludes: SDK.WithdrawalValidator;
  }>,
  Error
> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () => SDK.buildFraudProofSharedWithdrawalValidators(blueprint),
      catch: (cause) =>
        new Error("Failed to build FraudProofSharedWithdrawalValidators", {
          cause,
        }),
    });
  });

const buildRealDaParamsGovernorValidator = (
  network: Network,
  initOutRef: HubOracleOneShotOutRef,
  maxCommitteeSize: number,
  maxOwnerCount: number,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildDaParamsGovernorValidator(
          blueprint,
          network,
          initOutRef,
          maxCommitteeSize,
          maxOwnerCount,
        ),
      catch: (cause) =>
        new Error("Failed to build DaParamsGovernorValidator", { cause }),
    });
  });

const buildRealDaAttestationValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  referenceScriptAuthPolicyId: string,
  availabilityParameters: SDK.DaAvailabilityParameters,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildDaAttestationValidator(
          blueprint,
          network,
          contracts,
          referenceScriptAuthPolicyId,
          availabilityParameters,
        ),
      catch: (cause) =>
        new Error("Failed to build DaAttestationValidator", { cause }),
    });
  });

const buildRealAvailabilityChallengeValidator = (
  network: Network,
  hubOraclePolicyId: string,
  referenceScriptAuthPolicyId: string,
  parameters: SDK.DaAvailabilityParameters,
): Effect.Effect<SDK.AvailabilityChallengeValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildAvailabilityChallengeValidator(
          blueprint,
          network,
          hubOraclePolicyId,
          referenceScriptAuthPolicyId,
          parameters,
        ),
      catch: (cause) =>
        new Error("Failed to build AvailabilityChallengeValidator", { cause }),
    });
  });

const expectDerivedScriptHash = (
  label: string,
  expected: string,
  actual: string,
): Effect.Effect<void, Error> =>
  expected === actual
    ? Effect.void
    : Effect.fail(
        new Error(
          `${label} mismatch while deriving real fault-proof contracts: expected=${expected}, actual=${actual}`,
        ),
      );

const buildRealFaultProofContracts = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.FaultProofContractChains, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const derived = yield* SDK.buildFaultProofContracts({
      blueprint,
      network,
      hubOraclePolicyId: contracts.hubOracle.policyId,
      fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      referenceScriptAuthPolicyId: contracts.referenceScriptAuth.policyId,
    });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      derived.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      derived.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      derived.fraudProof.spendingScriptHash,
    );

    return derived;
  });

export const buildRealDoubleSpendFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const doubleSpendContracts = yield* SDK.buildDoubleSpendFaultProofContracts(
      {
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      },
    );

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      doubleSpendContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      doubleSpendContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      doubleSpendContracts.fraudProof.spendingScriptHash,
    );

    return doubleSpendContracts.doubleSpend.firstStep;
  });

export const buildRealTransitionTraceProofValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const transitionTraceContracts =
      yield* SDK.buildTransitionTraceFaultProofContracts({
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        referenceScriptAuthPolicyId: contracts.referenceScriptAuth.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      transitionTraceContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      transitionTraceContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      transitionTraceContracts.fraudProof.spendingScriptHash,
    );

    return transitionTraceContracts.transitionTrace.firstStep;
  });

export const buildRealValidationTraceDisputeValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.ValidationTraceDisputeValidators, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const validationTraceContracts =
      yield* SDK.buildValidationTraceDisputeFaultProofContracts({
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
        referenceScriptAuthPolicyId: contracts.referenceScriptAuth.policyId,
      });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      validationTraceContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      validationTraceContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      validationTraceContracts.fraudProof.spendingScriptHash,
    );

    const chain = validationTraceContracts.validationTraceDispute;
    return {
      ...chain.opener,
      source: chain.source,
      game: chain.game,
      boundary: chain.boundary,
      timeout: chain.timeout,
      award: chain.award,
    };
  });

export const buildRealNonExistentInputFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const nonExistentInputContracts =
      yield* SDK.buildNonExistentInputFaultProofContracts({
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      nonExistentInputContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      nonExistentInputContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      nonExistentInputContracts.fraudProof.spendingScriptHash,
    );

    return nonExistentInputContracts.nonExistentInput.firstStep;
  });

export const buildRealZeroInputFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const zeroInputContracts = yield* SDK.buildZeroInputFaultProofContracts({
      blueprint,
      network,
      hubOraclePolicyId: contracts.hubOracle.policyId,
      fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
    });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      zeroInputContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      zeroInputContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      zeroInputContracts.fraudProof.spendingScriptHash,
    );

    return zeroInputContracts.zeroInput.firstStep;
  });

export const buildRealDaHashPreimageFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const daHashPreimageContracts =
      yield* SDK.buildDaHashPreimageFaultProofContracts({
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      daHashPreimageContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      daHashPreimageContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      daHashPreimageContracts.fraudProof.spendingScriptHash,
    );

    return daHashPreimageContracts.daHashPreimage.firstStep;
  });

export const buildRealNoReferenceInputFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const noReferenceInputContracts =
      yield* SDK.buildNoReferenceInputFaultProofContracts({
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      noReferenceInputContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      noReferenceInputContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      noReferenceInputContracts.fraudProof.spendingScriptHash,
    );

    return noReferenceInputContracts.noReferenceInput.firstStep;
  });

export const buildRealReferenceInputNoIdxFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const referenceInputNoIdxContracts =
      yield* SDK.buildReferenceInputNoIdxFaultProofContracts({
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      referenceInputNoIdxContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      referenceInputNoIdxContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      referenceInputNoIdxContracts.fraudProof.spendingScriptHash,
    );

    return referenceInputNoIdxContracts.referenceInputNoIdx.firstStep;
  });

export const buildRealInvalidSignatureFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const invalidSignatureContracts =
      yield* SDK.buildInvalidSignatureFaultProofContracts({
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      });

    yield* expectDerivedScriptHash(
      "computation-thread policy",
      computationThread.policyId,
      invalidSignatureContracts.computationThread.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof policy",
      fraudProof.policyId,
      invalidSignatureContracts.fraudProof.policyId,
    );
    yield* expectDerivedScriptHash(
      "fraud-proof spend",
      fraudProof.spendingScriptHash,
      invalidSignatureContracts.fraudProof.spendingScriptHash,
    );

    return invalidSignatureContracts.invalidSignature.firstStep;
  });

/**
 * Builds the real state-queue authenticated validator.
 */
const buildRealStateQueueValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  referenceScriptAuthPolicyId: string,
): Effect.Effect<SDK.StateQueueValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* SDK.buildStateQueueValidator({
      blueprint,
      network,
      hubOraclePolicyId: contracts.hubOracle.policyId,
      correctionLockScriptHash: contracts.correctionLock.spendingScriptHash,
      activeOperatorsPolicyId: contracts.activeOperators.policyId,
      activeOperatorsAddress: contracts.activeOperators.spendingScriptAddress,
      retiredOperatorsPolicyId: contracts.retiredOperators.policyId,
      schedulerPolicyId: contracts.scheduler.policyId,
      fraudProofPolicyId: contracts.fraudProof.policyId,
      settlementPolicyId: contracts.settlement.policyId,
      daAttestationPolicyId: contracts.daAttestation.policyId,
      availabilityChallengePolicyId: contracts.availabilityChallenge.policyId,
      referenceScriptAuthPolicyId,
    });
  });

const buildRealCorrectionLockValidator = (
  network: Network,
  hubOraclePolicyId: string,
  availabilityChallengePolicyId: string,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* SDK.buildCorrectionLockValidator({
      blueprint,
      network,
      hubOraclePolicyId,
      availabilityChallengePolicyId,
    });
  });

/**
 * Builds the real registered-operators authenticated validator.
 */
const buildRealRegisteredOperatorsValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildRegisteredOperatorsValidator(blueprint, network, contracts),
      catch: (cause) =>
        new Error("Failed to build RegisteredOperatorsValidator", { cause }),
    });
  });

/**
 * Builds the real active-operators authenticated validator.
 */
const buildRealActiveOperatorsValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildActiveOperatorsValidator(blueprint, network, contracts),
      catch: (cause) =>
        new Error("Failed to build ActiveOperatorsValidator", { cause }),
    });
  });

/**
 * Builds the real retired-operators authenticated validator.
 */
const buildRealRetiredOperatorsValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildRetiredOperatorsValidator(blueprint, network, contracts),
      catch: (cause) =>
        new Error("Failed to build RetiredOperatorsValidator", { cause }),
    });
  });

/**
 * Builds the real scheduler authenticated validator.
 */
const buildRealSchedulerValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () => SDK.buildSchedulerValidator(blueprint, network, contracts),
      catch: (cause) =>
        new Error("Failed to build SchedulerValidator", { cause }),
    });
  });

/**
 * Builds the real deposit authenticated validator.
 */
const buildRealDepositValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildDepositValidators({
          blueprint,
          network,
          hubOraclePolicyId: contracts.hubOracle.policyId,
        }),
      catch: (cause) =>
        new Error("Failed to derive deposit validators", { cause }),
    });
  });

export type TxOrderContracts = {
  readonly txOrder: SDK.AuthenticatedValidator;
  readonly fieldPreimageCertificate: SDK.SpendingValidator &
    SDK.MintingValidator;
  readonly cekProgramMaterial: SDK.SpendingValidator;
};

/** Derives tx-order and its certificate/material dependencies with the SDK recipe. */
export const buildRealTxOrderContracts = (
  network: Network,
  hubOraclePolicyId: string,
): Effect.Effect<TxOrderContracts, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildTxOrderValidators({ blueprint, network, hubOraclePolicyId }),
      catch: (cause) =>
        new Error("Failed to derive tx-order validators", { cause }),
    });
  });

const buildRealWithdrawalValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildWithdrawalValidators({
          blueprint,
          network,
          hubOraclePolicyId: contracts.hubOracle.policyId,
        }),
      catch: (cause) =>
        new Error("Failed to derive withdrawal validators", { cause }),
    });
  });

const buildRealSettlementValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () => SDK.buildSettlementValidator(blueprint, network, contracts),
      catch: (cause) =>
        new Error("Failed to build SettlementValidator", { cause }),
    });
  });

const buildRealReserveValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.SpendingValidator & SDK.WithdrawalValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () => SDK.buildReserveValidator(blueprint, network, contracts),
      catch: (cause) =>
        new Error("Failed to build ReserveValidator", { cause }),
    });
  });

const buildRealPayoutValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () => SDK.buildPayoutValidator(blueprint, network, contracts),
      catch: (cause) => new Error("Failed to build PayoutValidator", { cause }),
    });
  });

/**
 * Replaces hub-oracle, deposit, operator-list, scheduler, and state-queue
 * contracts with their real blueprint-derived counterparts.
 */
export const withRealStateQueueAndOperatorContracts = (
  network: Network,
  baseContracts: SDK.MidgardValidators,
  hubOracleOneShotOutRef: HubOracleOneShotOutRef,
  deploymentParameters: RealContractDeploymentParameters,
): Effect.Effect<SDK.MidgardValidators, Error> =>
  Effect.gen(function* () {
    const normalizedOneShotOutRef = yield* normalizeHubOracleOneShotOutRef(
      hubOracleOneShotOutRef,
    );
    const daParamsGovernorInitOutRef = yield* normalizeHubOracleOneShotOutRef(
      deploymentParameters.daParamsGovernorInitOutRef ??
        normalizedOneShotOutRef,
    );
    const daParamsMaxCommitteeSize =
      deploymentParameters.daParamsMaxCommitteeSize ?? 256;
    const daParamsMaxOwnerCount =
      deploymentParameters.daParamsMaxOwnerCount ?? 16;

    const realHubOracle = yield* buildRealHubOracleValidator(
      network,
      baseContracts.hubOracle,
      normalizedOneShotOutRef,
    );
    // The availability-challenge policy id is a `correction_lock.spend`
    // parameter, so it has to exist before the correction lock is applied.
    const realAvailabilityChallenge =
      yield* buildRealAvailabilityChallengeValidator(
        network,
        realHubOracle.policyId,
        deploymentParameters.referenceScriptAuth.policyId,
        deploymentParameters.availabilityChallengeParameters,
      );
    const realCorrectionLock = yield* buildRealCorrectionLockValidator(
      network,
      realHubOracle.policyId,
      realAvailabilityChallenge.policyId,
    );
    const withRealHubOracle: SDK.MidgardValidators = {
      ...baseContracts,
      referenceScriptAuth: deploymentParameters.referenceScriptAuth,
      hubOracle: realHubOracle,
      correctionLock: realCorrectionLock,
      availabilityChallenge: realAvailabilityChallenge,
    };

    const realFraudProofCatalogue =
      yield* buildRealFraudProofCatalogueValidator(network, withRealHubOracle);
    const withRealFraudProofCatalogue: SDK.MidgardValidators = {
      ...withRealHubOracle,
      fraudProofCatalogue: realFraudProofCatalogue,
    };

    const realComputationThread = yield* buildRealComputationThreadValidator(
      withRealFraudProofCatalogue,
    );
    const realFraudProof = yield* buildRealFraudProofValidator(
      network,
      realComputationThread,
    );
    const realFraudProofSharedWithdrawals =
      yield* buildRealFraudProofSharedWithdrawalValidators();
    const realFaultProofContracts = yield* buildRealFaultProofContracts(
      network,
      withRealFraudProofCatalogue,
      realComputationThread,
      realFraudProof,
    );
    const withRealFraudProof: SDK.MidgardValidators = {
      ...withRealFraudProofCatalogue,
      computationThread: realComputationThread,
      fraudProof: realFraudProof,
      ...realFraudProofSharedWithdrawals,
      fraudProofContracts: realFaultProofContracts,
      fraudProofs: SDK.fraudProofContractsToFirstSteps(realFaultProofContracts),
    };

    const realRetiredOperators = yield* buildRealRetiredOperatorsValidator(
      network,
      withRealFraudProof,
    );
    const withRealRetiredOperators: SDK.MidgardValidators = {
      ...withRealFraudProof,
      retiredOperators: realRetiredOperators,
    };

    const realRegisteredOperators =
      yield* buildRealRegisteredOperatorsValidator(
        network,
        withRealRetiredOperators,
      );
    const withRealRegisteredOperators: SDK.MidgardValidators = {
      ...withRealRetiredOperators,
      registeredOperators: realRegisteredOperators,
    };

    const realActiveOperators = yield* buildRealActiveOperatorsValidator(
      network,
      withRealRegisteredOperators,
    );
    const withRealOperatorSets: SDK.MidgardValidators = {
      ...withRealRegisteredOperators,
      activeOperators: realActiveOperators,
    };

    const realDeposit = yield* buildRealDepositValidator(
      network,
      withRealOperatorSets,
    );
    const withRealHubOracleAndDeposit: SDK.MidgardValidators = {
      ...withRealOperatorSets,
      deposit: realDeposit,
    };

    const realTxOrderContracts = yield* buildRealTxOrderContracts(
      network,
      withRealHubOracleAndDeposit.hubOracle.policyId,
    );
    const withRealHubOracleDepositAndTxOrder: SDK.MidgardValidators = {
      ...withRealHubOracleAndDeposit,
      txOrder: realTxOrderContracts.txOrder,
      // #579 ruling A. The real certificate has to be propagated, not left as
      // the always-succeeds stand-in it inherits from the base set: the tx-order
      // mint above is parameterized by THIS policy id, so a set that reported
      // the stand-in would describe a door the deployed script does not consult.
      fieldPreimageCertificate: realTxOrderContracts.fieldPreimageCertificate,
      cekProgramMaterial: realTxOrderContracts.cekProgramMaterial,
    };

    const realWithdrawal = yield* buildRealWithdrawalValidator(
      network,
      withRealHubOracleDepositAndTxOrder,
    );
    const withRealUserEvents: SDK.MidgardValidators = {
      ...withRealHubOracleDepositAndTxOrder,
      withdrawal: realWithdrawal,
    };

    const realScheduler = yield* buildRealSchedulerValidator(
      network,
      withRealUserEvents,
    );
    const withRealScheduler: SDK.MidgardValidators = {
      ...withRealUserEvents,
      scheduler: realScheduler,
    };

    const realSettlement = yield* buildRealSettlementValidator(
      network,
      withRealScheduler,
    );
    const withRealSettlement: SDK.MidgardValidators = {
      ...withRealScheduler,
      settlement: realSettlement,
    };

    const realDaParamsGovernor = yield* buildRealDaParamsGovernorValidator(
      network,
      daParamsGovernorInitOutRef,
      daParamsMaxCommitteeSize,
      daParamsMaxOwnerCount,
    );
    const withRealDaParamsGovernor: SDK.MidgardValidators = {
      ...withRealSettlement,
      daParamsGovernor: realDaParamsGovernor,
    };

    const realDaAttestation = yield* buildRealDaAttestationValidator(
      network,
      withRealDaParamsGovernor,
      deploymentParameters.referenceScriptAuth.policyId,
      deploymentParameters.availabilityChallengeParameters,
    );
    const withRealDaAttestation: SDK.MidgardValidators = {
      ...withRealDaParamsGovernor,
      daAttestation: realDaAttestation,
    };

    const realStateQueue = yield* buildRealStateQueueValidator(
      network,
      withRealDaAttestation,
      deploymentParameters.referenceScriptAuth.policyId,
    );

    const withRealStateQueue: SDK.MidgardValidators = {
      ...withRealDaAttestation,
      stateQueue: realStateQueue,
    };

    const realPayout = yield* buildRealPayoutValidator(
      network,
      withRealStateQueue,
    );
    const withRealPayout: SDK.MidgardValidators = {
      ...withRealStateQueue,
      payout: realPayout,
    };

    const realReserve = yield* buildRealReserveValidator(
      network,
      withRealPayout,
    );
    return {
      ...withRealPayout,
      reserve: realReserve,
    };
  });

/**
 * Resolves the production validator bundle from node configuration.
 *
 * The effect fails fast if the one-shot hub-oracle parameters are missing so a
 * node cannot boot into an ambiguous real-contract configuration.
 */
const makeMidgardContractRuntime = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const baseContracts = yield* AlwaysSucceedsContract;
  const configuredManifest = yield* Effect.try({
    try: () => readConfiguredDeploymentManifest(),
    catch: (cause) =>
      new Error(
        `Failed to read configured deployment manifest: ${formatUnknownError(
          cause,
        )}`,
      ),
  });
  if (configuredManifest !== undefined) {
    yield* Effect.try({
      try: () =>
        assertDeploymentManifestMatchesConfig(
          configuredManifest.manifest,
          configuredManifest.path,
          nodeConfig,
        ),
      catch: (cause) =>
        new Error(
          `Configured deployment manifest cannot be used as contract source: ${formatUnknownError(
            cause,
          )}`,
        ),
    });
    const manifestContracts = yield* Effect.try({
      try: () =>
        midgardContractsFromDeploymentManifest(
          nodeConfig.NETWORK,
          configuredManifest.manifest,
          configuredManifest.path,
          baseContracts,
        ),
      catch: (cause) =>
        new Error(
          `Failed to derive contracts from configured deployment manifest: ${formatUnknownError(
            cause,
          )}`,
        ),
    });
    const runStatePath = defaultDeploymentRunStatePath();
    const runState = yield* Effect.tryPromise({
      try: () => loadDeploymentRunState(runStatePath),
      catch: (cause) =>
        new Error(
          `Failed to inspect deployment run state at ${runStatePath}: ${formatUnknownError(
            cause,
          )}`,
        ),
    });
    if (runState !== null) {
      yield* Effect.try({
        try: () => {
          const marker = makeDeploymentMarker(
            configuredManifest.manifest.manifestId,
          );
          assertDeploymentMarkerMatches(
            marker,
            runState.identity.deploymentMarker,
            "node deployment run state",
          );
          const expectedManifestSha256 = createHash("sha256")
            .update(readFileSync(configuredManifest.path))
            .digest("hex");
          if (
            runState.identity.manifestSha256 !== expectedManifestSha256 ||
            runState.identity.manifestPath === undefined ||
            path.resolve(runState.identity.manifestPath) !==
              path.resolve(configuredManifest.path)
          ) {
            throw new Error(
              `deployment run-state manifest binding does not match configured manifest path/hash`,
            );
          }
        },
        catch: (cause) =>
          new Error(
            `Configured deployment manifest cannot use run state ${runStatePath}: ${formatUnknownError(
              cause,
            )}`,
          ),
      });
    }
    yield* Effect.logInfo(
      `🔐 Contract source selected: deployment-manifest path=${configuredManifest.path},manifestId=${String(
        configuredManifest.manifest.manifestId ?? "unknown",
      )}`,
    );
    const runtime: MidgardContractRuntimeValue = {
      contracts: manifestContracts,
      identity: {
        kind: "manifest",
        manifestId: configuredManifest.manifest.manifestId,
        deploymentMarker: makeDeploymentMarker(
          configuredManifest.manifest.manifestId,
        ),
        path: configuredManifest.path,
        consensusProfile: configuredManifest.manifest.consensusProfile,
        l1Finality: configuredManifest.manifest.l1Finality,
        manifest: configuredManifest.manifest,
      },
    };
    return runtime;
  }
  const oneShotOutRef: HubOracleOneShotOutRef = {
    txHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
    outputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
  };
  const referenceScriptAuth = yield* loadReferenceScriptAuthValidator();
  const resolvedContracts = yield* withRealStateQueueAndOperatorContracts(
    nodeConfig.NETWORK,
    baseContracts,
    oneShotOutRef,
    {
      referenceScriptAuth,
      availabilityChallengeParameters:
        availabilityParametersFromExplicitEnvironment(),
    },
  );
  yield* Effect.logInfo(
    "🔐 Contract source selected: state_queue=real, da_attestation=real, da_params_governor=real, hub_oracle=real, deposit=real, tx_order=real, withdrawal=real, settlement=real, reserve=real, payout=real, registered_operators=real, active_operators=real, retired_operators=real, scheduler=real, fraud_proofs.all_registered_chains=real",
  );
  const runtime: MidgardContractRuntimeValue = {
    contracts: resolvedContracts,
    identity: {
      kind: "derived",
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    },
  };
  return runtime;
}).pipe(Effect.orDie);

class MidgardContractRuntime extends Effect.Service<MidgardContractRuntime>()(
  "MidgardContractRuntime",
  {
    effect: makeMidgardContractRuntime,
    dependencies: [AlwaysSucceedsContract.Default, NodeConfig.layer],
  },
) {}

/**
 * Service providing the validator bundle used by the node.
 */
export class MidgardContracts extends Effect.Service<MidgardContracts>()(
  "MidgardContracts",
  {
    effect: Effect.map(MidgardContractRuntime, ({ contracts, identity }) => ({
      ...contracts,
      consensusProfile: identity.consensusProfile,
    })),
    dependencies: [MidgardContractRuntime.Default],
  },
) {}

/** Identity of the exact contract source selected by {@link MidgardContracts}. */
export class ContractDeploymentIdentity extends Effect.Service<ContractDeploymentIdentity>()(
  "ContractDeploymentIdentity",
  {
    effect: Effect.map(MidgardContractRuntime, ({ identity }) => identity),
    dependencies: [MidgardContractRuntime.Default],
  },
) {}

/** Shared layer so contract bytes and their deployment identity resolve once. */
export const MidgardContractServices = Layer.merge(
  MidgardContracts.Default,
  ContractDeploymentIdentity.Default,
);
