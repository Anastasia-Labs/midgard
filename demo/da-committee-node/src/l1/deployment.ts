import type { AuthenticatedValidator } from "@al-ft/midgard-sdk";
import {
  mintingPolicyToId,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { normalizeHex } from "../utils/hex.js";

export type LucidNetwork = "Mainnet" | "Preprod" | "Preview" | "Custom";

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
  readonly purpose: "mint" | "spend";
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
  readonly hubOraclePolicyId: string;
  readonly correctionLockAddress: string;
  readonly hubOracle: MidgardAuthenticatedDeployment;
  readonly availabilityChallenge: MidgardAuthenticatedDeployment;
  readonly fraudProof: MidgardAuthenticatedDeployment;
  readonly daAttestation: MidgardAuthenticatedDeployment;
  readonly daParamsGovernor: MidgardAuthenticatedDeployment;
  readonly stateQueue: MidgardAuthenticatedDeployment;
};

export type DaAttestationValidatorSet = {
  readonly hubOracle: AuthenticatedValidator;
  readonly availabilityChallenge: AuthenticatedValidator;
  readonly daAttestation: AuthenticatedValidator;
  readonly daParamsGovernor: AuthenticatedValidator;
  readonly stateQueue: AuthenticatedValidator;
};

export const daAttestationValidatorsFromDeployment = (
  deployment: MidgardNodeDeployment,
): DaAttestationValidatorSet => ({
  hubOracle: authenticatedValidatorFromDeployment(deployment.hubOracle),
  availabilityChallenge: authenticatedValidatorFromDeployment(
    deployment.availabilityChallenge,
  ),
  daAttestation: authenticatedValidatorFromDeployment(deployment.daAttestation),
  daParamsGovernor: authenticatedValidatorFromDeployment(
    deployment.daParamsGovernor,
  ),
  stateQueue: authenticatedValidatorFromDeployment(deployment.stateQueue),
});

export const parseMidgardNodeDeploymentInfo = (
  deploymentInfo: Record<string, unknown>,
  network: string,
): MidgardNodeDeployment => {
  const lucidNetwork = normalizeLucidNetwork(network);
  const hubOracleMint = deploymentContract(
    deploymentInfo,
    "hubOracleMint",
    "hub oracle mint",
    "mint",
  );
  const correctionLockSpend = deploymentContract(
    deploymentInfo,
    "correctionLockSpend",
    "correction lock spend",
    "spend",
  );
  return {
    hubOraclePolicyId: hubOracleMint.scriptHash,
    correctionLockAddress: validatorToAddress(
      lucidNetwork,
      correctionLockSpend.script as never,
    ),
    // The hub oracle is a mint-only script: no `hubOracleSpend` entry exists in
    // any deployment document, and its beacon UTxO sits at the address whose
    // payment credential is the minting policy hash itself.
    hubOracle: {
      mint: hubOracleMint,
      spend: hubOracleMint,
      policyId: hubOracleMint.scriptHash,
      spendingScriptHash: hubOracleMint.scriptHash,
      spendingScriptAddress: validatorToAddress(
        lucidNetwork,
        hubOracleMint.script as never,
      ),
    },
    availabilityChallenge: authenticatedDeployment(
      deploymentInfo,
      "availabilityChallenge",
      "availability challenge",
      lucidNetwork,
    ),
    fraudProof: authenticatedDeployment(
      deploymentInfo,
      "fraudProof",
      "fraud proof",
      lucidNetwork,
    ),
    daAttestation: authenticatedDeployment(
      deploymentInfo,
      "daAttestation",
      "DA attestation",
      lucidNetwork,
    ),
    daParamsGovernor: authenticatedDeployment(
      deploymentInfo,
      "daParamsGovernor",
      "DA params governor",
      lucidNetwork,
    ),
    stateQueue: authenticatedDeployment(
      deploymentInfo,
      "stateQueue",
      "state queue",
      lucidNetwork,
    ),
  };
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

const authenticatedDeployment = (
  deploymentInfo: Record<string, unknown>,
  prefix:
    | "hubOracle"
    | "availabilityChallenge"
    | "daAttestation"
    | "daParamsGovernor"
    | "stateQueue"
    | "fraudProof",
  label: string,
  network: LucidNetwork,
): MidgardAuthenticatedDeployment => {
  const mint = deploymentContract(
    deploymentInfo,
    `${prefix}Mint`,
    `${label} mint`,
    "mint",
  );
  const spend = deploymentContract(
    deploymentInfo,
    `${prefix}Spend`,
    `${label} spend`,
    "spend",
  );
  return {
    mint,
    spend,
    policyId: mint.scriptHash,
    spendingScriptHash: spend.scriptHash,
    spendingScriptAddress: validatorToAddress(network, spend.script as never),
  };
};

const authenticatedValidatorFromDeployment = (
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

const deploymentContract = (
  deploymentInfo: Record<string, unknown>,
  key: string,
  label: string,
  purpose: "mint" | "spend",
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
