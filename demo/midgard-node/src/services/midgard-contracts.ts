import { existsSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { normalizeOutRef } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  applyParamsToScript,
  Constr,
  credentialToAddress,
  Data,
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

import {
  DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  type DeploymentManifestV2Value,
  parseDeploymentManifestV2Value,
} from "@/deployment-manifest-v2.js";

import { AlwaysSucceedsContract } from "./always-succeeds.js";
import { NodeConfig, type NodeConfigDep } from "./config.js";

/**
 * Contract-loading service for Midgard validators.
 *
 * This module can either expose the always-succeeds bundle for test flows or
 * derive the real script set from a blueprint, applying protocol parameters
 * where required.
 */
type BlueprintValidator = {
  title: string;
  compiledCode: string;
};

type Blueprint = {
  validators: BlueprintValidator[];
};

type DeploymentManifestContractEntry = {
  readonly contract?: {
    readonly type?: unknown;
    readonly cborHex?: unknown;
  };
  readonly scriptHash?: unknown;
};

type DeploymentManifestCandidate = DeploymentManifestV2Value & {
  readonly contracts: Readonly<Record<string, DeploymentManifestContractEntry>>;
};

export type ContractDeploymentIdentityValue = {
  readonly kind: "manifest" | "derived";
  readonly manifestId?: string;
  readonly path?: string;
};

type MidgardContractRuntimeValue = {
  readonly contracts: SDK.MidgardValidators;
  readonly identity: ContractDeploymentIdentityValue;
};

const moduleDir = path.dirname(fileURLToPath(import.meta.url));
const DEFAULT_REAL_BLUEPRINT_CANDIDATES = [
  path.resolve(moduleDir, "../../../../onchain/aiken/plutus.json"),
  path.resolve(moduleDir, "../../../onchain/aiken/plutus.json"),
  path.resolve(process.cwd(), "../../onchain/aiken/plutus.json"),
  path.resolve(process.cwd(), "onchain/aiken/plutus.json"),
] as const;
const DEFAULT_CONTRACT_DEPLOYMENT_INFO_CANDIDATES = [
  path.resolve(moduleDir, "../../deploymentInfo/contract-deployment-info.json"),
  path.resolve(process.cwd(), "deploymentInfo/contract-deployment-info.json"),
  path.resolve(
    process.cwd(),
    "demo/midgard-node/deploymentInfo/contract-deployment-info.json",
  ),
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
  const parsed = JSON.parse(raw) as unknown;
  if (
    typeof parsed !== "object" ||
    parsed === null ||
    !Array.isArray((parsed as { validators?: unknown }).validators)
  ) {
    throw new Error(
      `Blueprint at "${sourcePath}" does not have a validators array`,
    );
  }
  return parsed as Blueprint;
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

/**
 * Loads the real-contract blueprint, optionally honoring an override path from
 * the environment.
 */
const loadRealBlueprint = (): Effect.Effect<Blueprint, Error> =>
  Effect.try({
    try: () => {
      const configuredPath = process.env.MIDGARD_REAL_BLUEPRINT_PATH?.trim();
      const blueprintPath = configuredPath
        ? configuredPath
        : resolveDefaultRealBlueprintPath();

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

const resolveDefaultContractDeploymentInfoPath = (): string => {
  for (const candidate of new Set(
    DEFAULT_CONTRACT_DEPLOYMENT_INFO_CANDIDATES,
  )) {
    if (existsSync(candidate)) {
      return candidate;
    }
  }
  throw new Error(
    `Failed to locate contract deployment info. Looked in: ${DEFAULT_CONTRACT_DEPLOYMENT_INFO_CANDIDATES.join(", ")}`,
  );
};

const configuredContractDeploymentInfoPath = (): string => {
  const configuredPath =
    process.env.MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH?.trim();
  return configuredPath === undefined || configuredPath.length === 0
    ? resolveDefaultContractDeploymentInfoPath()
    : path.resolve(configuredPath);
};

const loadReferenceScriptAuthValidator = (): Effect.Effect<
  SDK.MintingValidator,
  Error
> =>
  Effect.try({
    try: () => {
      const deploymentInfoPath = configuredContractDeploymentInfoPath();
      const parsed = JSON.parse(
        readFileSync(deploymentInfoPath, "utf8"),
      ) as unknown;
      const referenceScriptAuthPolicy =
        typeof parsed === "object" &&
        parsed !== null &&
        typeof (
          parsed as {
            referenceScriptAuthPolicy?: {
              policyId?: unknown;
              nativeScript?: { cborHex?: unknown; type?: unknown };
            };
          }
        ).referenceScriptAuthPolicy === "object"
          ? (
              parsed as {
                referenceScriptAuthPolicy: {
                  policyId?: unknown;
                  nativeScript?: { cborHex?: unknown; type?: unknown };
                };
              }
            ).referenceScriptAuthPolicy
          : undefined;
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
          `Deployment info at "${deploymentInfoPath}" does not contain a valid referenceScriptAuthPolicy.policyId`,
        );
      }
      if (!/^[0-9a-fA-F]+$/.test(cborHex)) {
        throw new Error(
          `Deployment info at "${deploymentInfoPath}" does not contain a valid referenceScriptAuthPolicy.nativeScript.cborHex`,
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
        `Failed to load reference-script auth policy id from deployment info: ${formatUnknownError(
          cause,
        )}`,
      ),
  });

export const parseRuntimeDeploymentManifest = (
  raw: unknown,
): DeploymentManifestCandidate =>
  parseDeploymentManifestV2Value(raw) as DeploymentManifestCandidate;

export const readRuntimeDeploymentManifestFile = (
  deploymentInfoPath: string,
  required: boolean,
):
  | {
      readonly path: string;
      readonly manifest: DeploymentManifestCandidate;
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
  const claimsV2 =
    typeof parsed === "object" &&
    parsed !== null &&
    !Array.isArray(parsed) &&
    (parsed as { readonly schemaVersion?: unknown }).schemaVersion ===
      DEPLOYMENT_MANIFEST_SCHEMA_VERSION;
  if (!required && !claimsV2) {
    return undefined;
  }
  return {
    path: deploymentInfoPath,
    manifest: parseRuntimeDeploymentManifest(parsed),
  };
};

const readConfiguredDeploymentManifest = () => {
  const explicitlyConfigured = Boolean(
    process.env.MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH?.trim(),
  );
  return readRuntimeDeploymentManifestFile(
    configuredContractDeploymentInfoPath(),
    explicitlyConfigured,
  );
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

const assertDeploymentManifestMatchesConfig = (
  manifest: DeploymentManifestCandidate,
  sourcePath: string,
  nodeConfig: NodeConfigDep,
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
  if (mismatches.length > 0) {
    throw new Error(
      `Deployment manifest at "${sourcePath}" does not match node config: ${mismatches.join(
        "; ",
      )}`,
    );
  }
};

const manifestScript = (
  manifest: DeploymentManifestCandidate,
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
  manifest: DeploymentManifestCandidate,
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
  manifest: DeploymentManifestCandidate,
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
  manifest: DeploymentManifestCandidate,
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
  manifest: DeploymentManifestCandidate,
  sourcePath: string,
  spendName: string,
  mintName: string,
): SDK.AuthenticatedValidator => ({
  ...spendingValidatorFromManifest(network, manifest, sourcePath, spendName),
  ...mintingValidatorFromManifest(manifest, sourcePath, mintName),
});

export const midgardContractsFromDeploymentManifest = (
  network: Network,
  manifest: DeploymentManifestCandidate,
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
    stateQueue: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "stateQueueSpend",
      "stateQueueMint",
    ),
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
    fraudProof: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "fraudProofSpend",
      "fraudProofMint",
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
    txOrder: authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      "txOrderSpend",
      "txOrderMint",
    ),
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
    fraudProofs: {
      doubleSpend: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofDoubleSpend",
      ),
      nonExistentInput: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofNonExistentInput",
      ),
      nonExistentInputNoIndex: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofNonExistentInputNoIndex",
      ),
      invalidRange: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofInvalidRange",
      ),
      transitionTrace: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        "fraudProofTransitionTrace",
      ),
    },
  };
};

/**
 * Blueprint titles for the real state-queue scripts.
 */
export const REAL_STATE_QUEUE_SCRIPT_TITLES = {
  mint: "state_queue.mint.mint",
  spend: "state_queue.spend.spend",
} as const;

export const REAL_DA_PARAMS_GOVERNOR_SCRIPT_TITLES = {
  mint: "da_params_governor.da_params_governor.mint",
  spend: "da_params_governor.da_params_governor.spend",
} as const;

export const REAL_DA_ATTESTATION_SCRIPT_TITLES = {
  mint: "da_attestation.da_attestation.mint",
  spend: "da_attestation.da_attestation.spend",
} as const;

/**
 * Blueprint titles for the real hub-oracle scripts.
 */
export const REAL_HUB_ORACLE_SCRIPT_TITLES = {
  mint: "hub_oracle.mint.mint",
} as const;

/**
 * Blueprint titles for the real registered-operators scripts.
 */
export const REAL_REGISTERED_OPERATORS_SCRIPT_TITLES = {
  mint: "operator_directory/registered_operators.mint.mint",
  spend: "operator_directory/registered_operators.spend.spend",
} as const;

/**
 * Blueprint titles for the real active-operators scripts.
 */
export const REAL_ACTIVE_OPERATORS_SCRIPT_TITLES = {
  mint: "operator_directory/active_operators.mint.mint",
  spend: "operator_directory/active_operators.spend.spend",
} as const;

/**
 * Blueprint titles for the real retired-operators scripts.
 */
export const REAL_RETIRED_OPERATORS_SCRIPT_TITLES = {
  mint: "operator_directory/retired_operators.mint.mint",
  spend: "operator_directory/retired_operators.spend.spend",
} as const;

/**
 * Blueprint titles for the real scheduler scripts.
 */
export const REAL_SCHEDULER_SCRIPT_TITLES = {
  mint: "scheduler.mint.mint",
  spend: "scheduler.spend.spend",
} as const;

/**
 * Blueprint titles for the real deposit scripts.
 */
export const REAL_DEPOSIT_SCRIPT_TITLES = {
  mint: "user_events/deposit.mint.mint",
  spend: "user_events/deposit.spend.spend",
} as const;

/**
 * Blueprint titles for the real tx-order scripts.
 */
export const REAL_TX_ORDER_SCRIPT_TITLES = {
  mint: "user_events/tx_order.mint.mint",
  spend: "user_events/tx_order.spend.spend",
} as const;

/**
 * Blueprint titles for the real withdrawal scripts.
 */
export const REAL_WITHDRAWAL_SCRIPT_TITLES = {
  mint: "user_events/withdrawal.mint.mint",
  spend: "user_events/withdrawal.spend.spend",
} as const;

/**
 * Blueprint titles for the real settlement scripts.
 */
export const REAL_SETTLEMENT_SCRIPT_TITLES = {
  mint: "settlement.mint.mint",
  spend: "settlement.spend.spend",
} as const;

/**
 * Blueprint titles for the real reserve scripts.
 */
export const REAL_RESERVE_SCRIPT_TITLES = {
  spend: "reserve.spend.spend",
  withdraw: "reserve.withdraw.else",
} as const;

/**
 * Blueprint titles for the real payout scripts.
 */
export const REAL_PAYOUT_SCRIPT_TITLES = {
  mint: "payout.mint.mint",
  spend: "payout.spend.spend",
} as const;

export const REAL_FRAUD_PROOF_CATALOGUE_SCRIPT_TITLES = {
  mint: "fraud_proof_catalogue.mint.mint",
  spend: "fraud_proof_catalogue.spend.else",
} as const;

export const REAL_COMPUTATION_THREAD_SCRIPT_TITLES = {
  mint: "computation_thread.mint.mint",
} as const;

export const REAL_FRAUD_PROOF_SCRIPT_TITLES = {
  mint: "fraud_proof.mint.mint",
  spend: "fraud_proof.spend.else",
} as const;

/**
 * One-shot outref used to parameterize the real hub-oracle policy.
 */
export type HubOracleOneShotOutRef = {
  readonly txHash: string;
  readonly outputIndex: number;
};

export type RealContractDeploymentParameters = {
  readonly referenceScriptAuth: SDK.MintingValidator;
  readonly daParamsGovernorInitOutRef?: HubOracleOneShotOutRef;
  readonly daParamsMaxCommitteeSize?: number;
  readonly daParamsMaxOwnerCount?: number;
};

type ScriptParams = Data[];

type AuthenticatedScriptTitles = {
  readonly mint: string;
  readonly spend: string;
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
 * Looks up a compiled script by title inside the resolved blueprint.
 */
const getCompiledScript = (
  blueprint: Blueprint,
  title: string,
): Effect.Effect<string, Error> =>
  Effect.gen(function* () {
    const found = blueprint.validators.find(
      (validator) => validator.title === title,
    );
    if (found === undefined) {
      return yield* Effect.fail(
        new Error(`Validator with title "${title}" not found in blueprint`),
      );
    }
    return found.compiledCode;
  });

const makeMintingPolicy = (mintingScriptCBOR: string): SDK.MintingValidator => {
  const mintingScript: MintingPolicy = {
    type: "PlutusV3",
    script: mintingScriptCBOR,
  };
  return {
    mintingScriptCBOR,
    mintingScript,
    policyId: mintingPolicyToId(mintingScript),
  };
};

const makeSpendingValidator = (
  network: Network,
  spendingScriptCBOR: string,
): SDK.SpendingValidator => {
  const spendingScript: SpendingValidator = {
    type: "PlutusV3",
    script: spendingScriptCBOR,
  };
  return {
    spendingScriptCBOR,
    spendingScript,
    spendingScriptAddress: validatorToAddress(network, spendingScript),
    spendingScriptHash: validatorToScriptHash(spendingScript),
  };
};

const makeWithdrawalValidator = (
  withdrawalScriptCBOR: string,
): SDK.WithdrawalValidator => {
  const withdrawalScript: WithdrawalValidator = {
    type: "PlutusV3",
    script: withdrawalScriptCBOR,
  };
  return {
    withdrawalScriptCBOR,
    withdrawalScript,
    withdrawalScriptHash: validatorToScriptHash(withdrawalScript),
  };
};

const makeAuthenticatedValidator = (
  network: Network,
  mintingScriptCBOR: string,
  spendingScriptCBOR: string,
): SDK.AuthenticatedValidator => ({
  ...makeSpendingValidator(network, spendingScriptCBOR),
  ...makeMintingPolicy(mintingScriptCBOR),
});

const buildRealAuthenticatedValidator = (
  network: Network,
  titles: AuthenticatedScriptTitles,
  mintParams: ScriptParams,
  spendParams?: (policyId: string) => ScriptParams,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const mintBase = yield* getCompiledScript(blueprint, titles.mint);
    const spendBase = yield* getCompiledScript(blueprint, titles.spend);
    const mintingScriptCBOR = applyParamsToScript(mintBase, mintParams);
    const { policyId } = makeMintingPolicy(mintingScriptCBOR);
    const spendingScriptCBOR =
      spendParams === undefined
        ? spendBase
        : applyParamsToScript(spendBase, spendParams(policyId));
    return makeAuthenticatedValidator(
      network,
      mintingScriptCBOR,
      spendingScriptCBOR,
    );
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
    const mintBase = yield* getCompiledScript(
      blueprint,
      REAL_HUB_ORACLE_SCRIPT_TITLES.mint,
    );
    const initOutRef = new Constr(0, [
      oneShotOutRef.txHash,
      BigInt(oneShotOutRef.outputIndex),
    ]);
    const mintingScriptCBOR = applyParamsToScript(mintBase, [
      initOutRef,
      SDK.HUB_ORACLE_ASSET_NAME,
    ]);
    const mintingScript: MintingPolicy = {
      type: "PlutusV3",
      script: mintingScriptCBOR,
    };
    const policyId = mintingPolicyToId(mintingScript);
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
  buildRealAuthenticatedValidator(
    network,
    REAL_FRAUD_PROOF_CATALOGUE_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
  );

const buildRealComputationThreadValidator = (
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.MintingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const mintBase = yield* getCompiledScript(
      blueprint,
      REAL_COMPUTATION_THREAD_SCRIPT_TITLES.mint,
    );
    return makeMintingPolicy(
      applyParamsToScript(mintBase, [
        contracts.fraudProofCatalogue.policyId,
        contracts.hubOracle.policyId,
      ]),
    );
  });

const buildRealFraudProofValidator = (
  network: Network,
  computationThread: SDK.MintingValidator,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(network, REAL_FRAUD_PROOF_SCRIPT_TITLES, [
    computationThread.policyId,
  ]);

const outputReferenceParam = (outRef: HubOracleOneShotOutRef): Constr<Data> =>
  new Constr(0, [outRef.txHash, BigInt(outRef.outputIndex)]);

const buildRealDaParamsGovernorValidator = (
  network: Network,
  initOutRef: HubOracleOneShotOutRef,
  maxCommitteeSize: number,
  maxOwnerCount: number,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_DA_PARAMS_GOVERNOR_SCRIPT_TITLES,
    [
      outputReferenceParam(initOutRef),
      BigInt(maxCommitteeSize),
      BigInt(maxOwnerCount),
    ],
    () => [
      outputReferenceParam(initOutRef),
      BigInt(maxCommitteeSize),
      BigInt(maxOwnerCount),
    ],
  );

const buildRealDaAttestationValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  referenceScriptAuthPolicyId: string,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_DA_ATTESTATION_SCRIPT_TITLES,
    [contracts.daParamsGovernor.policyId, referenceScriptAuthPolicyId],
    () => [contracts.daParamsGovernor.policyId, referenceScriptAuthPolicyId],
  );

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

const buildRealDoubleSpendFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = SDK.parseFaultProofBlueprint(yield* loadRealBlueprint());
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

const buildRealTransitionTraceProofValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = SDK.parseFaultProofBlueprint(yield* loadRealBlueprint());
    const transitionTraceContracts =
      yield* SDK.buildTransitionTraceFaultProofContracts({
        blueprint,
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
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

const buildRealNonExistentInputFirstStepValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
  computationThread: SDK.MintingValidator,
  fraudProof: SDK.AuthenticatedValidator,
): Effect.Effect<SDK.SpendingValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = SDK.parseFaultProofBlueprint(yield* loadRealBlueprint());
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

/**
 * Builds the real state-queue authenticated validator.
 */
const buildRealStateQueueValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const activeOperatorsAddress = yield* Effect.mapError(
      Effect.map(
        SDK.addressDataFromBech32(
          contracts.activeOperators.spendingScriptAddress,
        ),
        (addressData) => Data.from(Data.to(addressData, SDK.AddressData)),
      ),
      (cause) =>
        new Error(
          `Failed to encode active-operators address for state_queue mint parameters: ${String(cause)}`,
        ),
    );
    return yield* buildRealAuthenticatedValidator(
      network,
      REAL_STATE_QUEUE_SCRIPT_TITLES,
      [
        contracts.hubOracle.policyId,
        contracts.activeOperators.policyId,
        activeOperatorsAddress,
        contracts.retiredOperators.policyId,
        contracts.scheduler.policyId,
        contracts.fraudProof.policyId,
        contracts.settlement.policyId,
        contracts.daAttestation.policyId,
      ],
      (policyId) => [policyId, contracts.daAttestation.policyId],
    );
  });

/**
 * Builds the real registered-operators authenticated validator.
 */
const buildRealRegisteredOperatorsValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_REGISTERED_OPERATORS_SCRIPT_TITLES,
    [contracts.retiredOperators.policyId, contracts.hubOracle.policyId],
    (policyId) => [policyId],
  );

/**
 * Builds the real active-operators authenticated validator.
 */
const buildRealActiveOperatorsValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_ACTIVE_OPERATORS_SCRIPT_TITLES,
    [
      contracts.hubOracle.policyId,
      contracts.registeredOperators.policyId,
      contracts.retiredOperators.policyId,
    ],
    (policyId) => [policyId, contracts.hubOracle.policyId],
  );

/**
 * Builds the real retired-operators authenticated validator.
 */
const buildRealRetiredOperatorsValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_RETIRED_OPERATORS_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
    (policyId) => [policyId],
  );

/**
 * Builds the real scheduler authenticated validator.
 */
const buildRealSchedulerValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  Effect.gen(function* () {
    const activeOperatorsAddress = yield* Effect.mapError(
      Effect.map(
        SDK.addressDataFromBech32(
          contracts.activeOperators.spendingScriptAddress,
        ),
        (addressData) => Data.from(Data.to(addressData, SDK.AddressData)),
      ),
      (cause) =>
        new Error(
          `Failed to encode active-operators address for scheduler spend parameters: ${String(cause)}`,
        ),
    );
    return yield* buildRealAuthenticatedValidator(
      network,
      REAL_SCHEDULER_SCRIPT_TITLES,
      [contracts.hubOracle.policyId],
      (policyId) => [
        contracts.registeredOperators.policyId,
        activeOperatorsAddress,
        contracts.activeOperators.policyId,
        policyId,
        contracts.hubOracle.policyId,
      ],
    );
  });

/**
 * Builds the real deposit authenticated validator.
 */
const buildRealDepositValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_DEPOSIT_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
    () => [contracts.hubOracle.policyId],
  );

const buildRealTxOrderValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_TX_ORDER_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
    () => [contracts.hubOracle.policyId],
  );

const buildRealWithdrawalValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_WITHDRAWAL_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
    () => [contracts.hubOracle.policyId],
  );

const buildRealSettlementValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_SETTLEMENT_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
    (policyId) => [contracts.hubOracle.policyId, policyId],
  );

const buildRealReserveValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.SpendingValidator & SDK.WithdrawalValidator, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    const spendBase = yield* getCompiledScript(
      blueprint,
      REAL_RESERVE_SCRIPT_TITLES.spend,
    );
    const withdrawScriptCBOR = yield* getCompiledScript(
      blueprint,
      REAL_RESERVE_SCRIPT_TITLES.withdraw,
    );

    const spendingScriptCBOR = applyParamsToScript(spendBase, [
      contracts.hubOracle.policyId,
    ]);

    return {
      ...makeSpendingValidator(network, spendingScriptCBOR),
      ...makeWithdrawalValidator(withdrawScriptCBOR),
    };
  });

const buildRealPayoutValidator = (
  network: Network,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.AuthenticatedValidator, Error> =>
  buildRealAuthenticatedValidator(
    network,
    REAL_PAYOUT_SCRIPT_TITLES,
    [contracts.hubOracle.policyId],
    () => [contracts.hubOracle.policyId],
  );

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
    const withRealHubOracle: SDK.MidgardValidators = {
      ...baseContracts,
      referenceScriptAuth: deploymentParameters.referenceScriptAuth,
      hubOracle: realHubOracle,
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
    const realDoubleSpendFirstStep =
      yield* buildRealDoubleSpendFirstStepValidator(
        network,
        withRealFraudProofCatalogue,
        realComputationThread,
        realFraudProof,
      );
    const realTransitionTrace = yield* buildRealTransitionTraceProofValidator(
      network,
      withRealFraudProofCatalogue,
      realComputationThread,
      realFraudProof,
    );
    const realNonExistentInput =
      yield* buildRealNonExistentInputFirstStepValidator(
        network,
        withRealFraudProofCatalogue,
        realComputationThread,
        realFraudProof,
      );
    const withRealFraudProof: SDK.MidgardValidators = {
      ...withRealFraudProofCatalogue,
      fraudProof: realFraudProof,
      fraudProofs: {
        ...withRealFraudProofCatalogue.fraudProofs,
        doubleSpend: realDoubleSpendFirstStep,
        transitionTrace: realTransitionTrace,
        nonExistentInput: realNonExistentInput,
      },
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

    const realTxOrder = yield* buildRealTxOrderValidator(
      network,
      withRealHubOracleAndDeposit,
    );
    const withRealHubOracleDepositAndTxOrder: SDK.MidgardValidators = {
      ...withRealHubOracleAndDeposit,
      txOrder: realTxOrder,
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
    );
    const withRealDaAttestation: SDK.MidgardValidators = {
      ...withRealDaParamsGovernor,
      daAttestation: realDaAttestation,
    };

    const realStateQueue = yield* buildRealStateQueueValidator(
      network,
      withRealDaAttestation,
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
        path: configuredManifest.path,
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
    },
  );
  yield* Effect.logInfo(
    "🔐 Contract source selected: state_queue=real, da_attestation=real, da_params_governor=real, hub_oracle=real, deposit=real, tx_order=real, withdrawal=real, settlement=real, reserve=real, payout=real, registered_operators=real, active_operators=real, retired_operators=real, scheduler=real, fraud_proofs.double_spend=real, fraud_proofs.transition_trace=real, fraud_proofs.non_existent_input=real",
  );
  const runtime: MidgardContractRuntimeValue = {
    contracts: resolvedContracts,
    identity: { kind: "derived" },
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
    effect: Effect.map(MidgardContractRuntime, ({ contracts }) => contracts),
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
