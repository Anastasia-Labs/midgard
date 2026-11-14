import type {
  AuthenticatedValidator,
  AvailabilityChallengeValidator,
  AvailabilityChallengeYieldValidators,
  SpendingValidator,
  StateQueueValidator,
  StateQueueYieldValidators,
  WithdrawalValidator,
} from "@al-ft/midgard-sdk";
import {
  mintingPolicyToId,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { normalizeHex } from "../utils/hex.js";

export type LucidNetwork = "Mainnet" | "Preprod" | "Preview" | "Custom";

export const correctionLockValidatorFromDeploymentInfo = (
  deploymentInfo: Record<string, unknown>,
  network: string,
): SpendingValidator => {
  const contract = deploymentContract(
    deploymentInfo,
    "correctionLockSpend",
    "correction lock spend",
    "spend",
  );
  return {
    spendingScriptCBOR: contract.script.script,
    spendingScript: contract.script,
    spendingScriptHash: contract.scriptHash,
    spendingScriptAddress: validatorToAddress(
      normalizeLucidNetwork(network),
      contract.script,
    ),
  };
};

export type MidgardDeploymentScript = {
  readonly type: "Native" | "PlutusV1" | "PlutusV2" | "PlutusV3";
  readonly script: string;
};

export type MidgardDeploymentOutRef = {
  readonly txHash: string;
  readonly outputIndex: number;
};

export type MidgardDeploymentContract = {
  readonly key: string;
  readonly purpose: "mint" | "spend" | "withdraw";
  readonly script: MidgardDeploymentScript;
  readonly scriptHash: string;
  // Null for contracts the deployment manifest gives no reference-script
  // role; consumers that dereference a UTxO must require a non-null value.
  readonly refScriptOutRef: MidgardDeploymentOutRef | null;
};

export type MidgardAuthenticatedDeployment = {
  readonly mint: MidgardDeploymentContract;
  readonly spend: MidgardDeploymentContract;
  readonly policyId: string;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

export type MidgardNodeDeployment = {
  readonly referenceScriptAuthPolicyId: string;
  readonly hubOraclePolicyId: string;
  readonly correctionLockAddress: string;
  readonly hubOracle: MidgardAuthenticatedDeployment;
  readonly availabilityChallenge: MidgardAuthenticatedDeployment;
  readonly availabilityChallengeYields: Readonly<
    Record<
      keyof AvailabilityChallengeYieldValidators,
      MidgardDeploymentContract
    >
  >;
  readonly fraudProof: MidgardAuthenticatedDeployment;
  readonly daAttestation: MidgardAuthenticatedDeployment;
  /**
   * The pooled DA committee bond: one pool UTxO at the pool's spending script
   * holding the pool NFT. Apply reads it as a reference input and requires it
   * to back at least one `da_bond_lovelace` above its floor.
   */
  readonly daBondPool: MidgardAuthenticatedDeployment;
  readonly daParamsGovernor: MidgardAuthenticatedDeployment;
  readonly stateQueue: MidgardAuthenticatedDeployment;
  /**
   * The state-queue spend yields its five state transitions to withdrawal
   * scripts, so the spend contract alone no longer describes the validator the
   * SDK builders need. Each name is a first-class deployment-manifest contract
   * and is parsed like any other.
   */
  readonly stateQueueYields: Readonly<
    Record<keyof StateQueueYieldValidators, MidgardDeploymentContract>
  >;
};

export type DaAttestationValidatorSet = {
  readonly hubOracle: AuthenticatedValidator;
  readonly availabilityChallenge: AvailabilityChallengeValidator;
  readonly daAttestation: AuthenticatedValidator;
  readonly daBondPool: AuthenticatedValidator;
  readonly daParamsGovernor: AuthenticatedValidator;
  readonly stateQueue: StateQueueValidator;
};

export const normalizeLucidNetwork = (value: string): LucidNetwork => {
  if (
    value === "Mainnet" ||
    value === "Preprod" ||
    value === "Preview" ||
    value === "Custom"
  ) {
    return value;
  }
  throw new Error(
    `unsupported Cardano network ${value}; expected Mainnet, Preprod, Preview, or Custom`,
  );
};

export const withdrawalValidatorFromDeployment = (
  contract: MidgardDeploymentContract,
): WithdrawalValidator => ({
  withdrawalScriptCBOR: contract.script.script,
  withdrawalScript: contract.script as never,
  withdrawalScriptHash: contract.scriptHash,
});

export const authenticatedValidatorFromDeployment = (
  contract: MidgardAuthenticatedDeployment,
): AuthenticatedValidator => ({
  mintingScriptCBOR: contract.mint.script.script,
  mintingScript: contract.mint.script as never,
  policyId: contract.policyId,
  spendingScriptCBOR: contract.spend.script.script,
  spendingScript: contract.spend.script as never,
  spendingScriptHash: contract.spendingScriptHash,
  spendingScriptAddress: contract.spendingScriptAddress,
});

export const deploymentContract = (
  deploymentInfo: Record<string, unknown>,
  key: string,
  label: string,
  purpose: "mint" | "spend" | "withdraw",
): MidgardDeploymentContract => {
  const root = objectAt(deploymentInfo, ["contracts", key]);
  if (root === undefined) {
    throw new Error(`${label} contract deployment entry is required`);
  }
  requireExactKeys(
    root,
    ["refScriptUTxO", "contract", "scriptHash"],
    `${label} contract deployment entry`,
  );
  const contract = objectAt(root, ["contract"]);
  if (contract === undefined) {
    throw new Error(`${label} contract object is required`);
  }
  requireExactKeys(contract, ["type", "cborHex"], `${label} contract`);
  const script = deploymentScript(contract, label);
  const derivedScriptHash =
    purpose === "mint"
      ? mintingPolicyToId(script as never)
      : validatorToScriptHash(script as never);
  const configuredScriptHash = stringAt(root, ["scriptHash"]);
  if (
    configuredScriptHash === undefined ||
    configuredScriptHash.trim() === ""
  ) {
    throw new Error(`${label} scriptHash is required`);
  }
  const scriptHash = normalizeHex(configuredScriptHash, {
    fieldName: `${label} scriptHash`,
    byteLength: 28,
  });
  if (scriptHash !== derivedScriptHash) {
    throw new Error(
      `${label} scriptHash mismatch: configured=${scriptHash}, derived=${derivedScriptHash}`,
    );
  }
  return {
    key,
    purpose,
    script,
    scriptHash,
    refScriptOutRef: deploymentOutRef(root, label),
  };
};

const deploymentScript = (
  contract: Record<string, unknown>,
  label: string,
): MidgardDeploymentScript => {
  const scriptType = stringAt(contract, ["type"]);
  const cborHex = stringAt(contract, ["cborHex"]);
  if (!isLucidScriptType(scriptType)) {
    throw new Error(`${label} contract.type must be a supported script type`);
  }
  if (cborHex === undefined || cborHex.trim() === "") {
    throw new Error(`${label} contract.cborHex is required`);
  }
  return {
    type: scriptType,
    script: normalizeHex(cborHex, { fieldName: `${label} contract.cborHex` }),
  };
};

const deploymentOutRef = (
  root: Record<string, unknown>,
  label: string,
): MidgardDeploymentOutRef | null => {
  const rawRefScriptUTxO = valueAt(root, ["refScriptUTxO"]);
  if (rawRefScriptUTxO === null) {
    return null;
  }
  const refScriptUTxO = objectAt(root, ["refScriptUTxO"]);
  if (refScriptUTxO === undefined) {
    throw new Error(`${label} refScriptUTxO is required`);
  }
  requireExactKeys(
    refScriptUTxO,
    ["txHash", "outputIndex"],
    `${label} refScriptUTxO`,
  );
  const txHash = stringAt(refScriptUTxO, ["txHash"]);
  const outputIndex = valueAt(refScriptUTxO, ["outputIndex"]);
  if (txHash === undefined) {
    throw new Error(`${label} refScriptUTxO.txHash is required`);
  }
  if (
    typeof outputIndex !== "number" ||
    !Number.isSafeInteger(outputIndex) ||
    outputIndex < 0
  ) {
    throw new Error(
      `${label} refScriptUTxO.outputIndex must be a non-negative integer`,
    );
  }
  return {
    txHash: normalizeHex(txHash, {
      fieldName: `${label} refScriptUTxO.txHash`,
      byteLength: 32,
    }),
    outputIndex,
  };
};

const isLucidScriptType = (
  value: string | undefined,
): value is MidgardDeploymentScript["type"] =>
  value === "Native" ||
  value === "PlutusV1" ||
  value === "PlutusV2" ||
  value === "PlutusV3";

const stringAt = (
  root: Record<string, unknown>,
  path: readonly string[],
): string | undefined => {
  const value = valueAt(root, path);
  return typeof value === "string" ? value : undefined;
};

const objectAt = (
  root: Record<string, unknown>,
  path: readonly string[],
): Record<string, unknown> | undefined => {
  const value = valueAt(root, path);
  return isRecord(value) ? value : undefined;
};

const valueAt = (
  root: Record<string, unknown>,
  path: readonly string[],
): unknown =>
  path.reduce<unknown>(
    (current, key) => (isRecord(current) ? current[key] : undefined),
    root,
  );

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const requireExactKeys = (
  value: Record<string, unknown>,
  keys: readonly string[],
  fieldName: string,
): void => {
  const expected = new Set(keys);
  for (const key of Object.keys(value)) {
    if (!expected.has(key)) {
      throw new Error(`${fieldName}.${key} is unexpected`);
    }
  }
  for (const key of keys) {
    if (!Object.hasOwn(value, key)) {
      throw new Error(`${fieldName}.${key} is required`);
    }
  }
};
