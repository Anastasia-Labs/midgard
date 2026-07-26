import { createHash } from "node:crypto";

import {
  isMidgardConsensusProfileV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
  MIDGARD_V1_RELEASE_EVIDENCE_DIGEST,
  type MidgardConsensusProfileV1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import {
  computeDeploymentManifestV1Id,
  verifyDeploymentManifestV1Identity,
  verifyFinalizedDeploymentManifestV1,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  hashHexWithBlake2b,
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  referenceScriptAuthUnit,
} from "@al-ft/midgard-sdk";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export const DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION =
  MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION;

export const REQUIRED_TRANSACTION_ORDER_CONTRACTS = Object.freeze([
  "txOrderSpend",
  "txOrderMint",
  "txOrderFieldPreimageSpend",
  "txOrderFieldReceiptSpend",
  "txOrderFieldReceiptMint",
  "cekProgramMaterialSpend",
  "validationTraceDispute",
  "validationTraceDisputeSource",
  "validationTraceDisputeGame",
  "validationTraceDisputeBoundary",
  "validationTraceDisputeTimeout",
  "validationTraceDisputeAward",
] as const);

export const DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES = Object.freeze([
  "referenceScriptAuthMint",
  "hubOracleMint",
  "daParamsGovernorSpend",
  "daParamsGovernorMint",
  "daAttestationSpend",
  "daAttestationMint",
  "stateQueueSpend",
  "stateQueueMint",
  "schedulerSpend",
  "schedulerMint",
  "registeredOperatorsSpend",
  "registeredOperatorsMint",
  "activeOperatorsSpend",
  "activeOperatorsMint",
  "retiredOperatorsSpend",
  "retiredOperatorsMint",
  "escapeHatchSpend",
  "escapeHatchMint",
  "fraudProofCatalogueSpend",
  "fraudProofCatalogueMint",
  "fraudProofSpend",
  "fraudProofMint",
  "depositSpend",
  "depositMint",
  "withdrawalSpend",
  "withdrawalMint",
  "txOrderSpend",
  "txOrderMint",
  "txOrderFieldPreimageSpend",
  "txOrderFieldReceiptSpend",
  "txOrderFieldReceiptMint",
  "cekProgramMaterialSpend",
  "settlementSpend",
  "settlementMint",
  "payoutSpend",
  "payoutMint",
  "reserveSpend",
  "reserveWithdraw",
  "phasMembershipWithdraw",
  "fraudProofDoubleSpend",
  "fraudProofNonExistentInput",
  "fraudProofNonExistentInputNoIndex",
  "fraudProofInvalidRange",
  "fraudProofTransitionTrace",
  "validationTraceDispute",
  "validationTraceDisputeSource",
  "validationTraceDisputeGame",
  "validationTraceDisputeBoundary",
  "validationTraceDisputeTimeout",
  "validationTraceDisputeAward",
] as const);

export const DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE =
  Object.freeze({
    "reference-script-auth minting": "referenceScriptAuthMint",
    "hub-oracle minting": "hubOracleMint",
    "da-params-governor spending": "daParamsGovernorSpend",
    "da-params-governor minting": "daParamsGovernorMint",
    "da-attestation spending": "daAttestationSpend",
    "da-attestation minting": "daAttestationMint",
    "state-queue spending": "stateQueueSpend",
    "state-queue minting": "stateQueueMint",
    "scheduler spending": "schedulerSpend",
    "scheduler minting": "schedulerMint",
    "registered-operators spending": "registeredOperatorsSpend",
    "registered-operators minting": "registeredOperatorsMint",
    "active-operators spending": "activeOperatorsSpend",
    "active-operators minting": "activeOperatorsMint",
    "retired-operators spending": "retiredOperatorsSpend",
    "retired-operators minting": "retiredOperatorsMint",
    "fraud-proof-catalogue minting": "fraudProofCatalogueMint",
    "deposit spending": "depositSpend",
    "deposit minting": "depositMint",
    "withdrawal spending": "withdrawalSpend",
    "withdrawal minting": "withdrawalMint",
    "settlement minting": "settlementMint",
    "payout spending": "payoutSpend",
    "payout minting": "payoutMint",
    "reserve spending": "reserveSpend",
    "reserve observer": "reserveWithdraw",
    "membership proof withdrawal": "phasMembershipWithdraw",
    "V1 transaction-field preimage publication": "txOrderFieldPreimageSpend",
    "V1 transaction-field receipt": "txOrderFieldReceiptSpend",
    "V1 transaction-field receipt minting": "txOrderFieldReceiptMint",
    "V1 immutable CEK program-material publication": "cekProgramMaterialSpend",
    "V1 validation-trace dispute": "validationTraceDispute",
    "V1 validation-trace source": "validationTraceDisputeSource",
    "V1 validation-trace game": "validationTraceDisputeGame",
    "V1 validation-trace boundary": "validationTraceDisputeBoundary",
    "V1 validation-trace timeout": "validationTraceDisputeTimeout",
    "V1 validation-trace award": "validationTraceDisputeAward",
  } as const);

export const DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_ROLES = Object.freeze(
  Object.keys(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
);

export const DEPLOYMENT_MANIFEST_V1_STEP_NAMES = Object.freeze([
  "prepareHubOracleNonce",
  "deployNodeRuntimeReferenceScripts",
  "initProtocol",
  "phasRegistration",
  "operatorRegistration",
  "operatorActivation",
] as const);

const DEPLOYMENT_MANIFEST_V1_STEP_STATUSES = Object.freeze([
  "pending",
  "in_progress",
  "submitted",
  "complete",
  "attached",
  "failed",
  "blocked_requires_fresh_redeploy",
] as const);

const DEPLOYMENT_MANIFEST_V1_SCRIPT_TYPES = Object.freeze([
  "Native",
  "PlutusV1",
  "PlutusV2",
  "PlutusV3",
] as const);

const DEPLOYMENT_MANIFEST_NETWORKS = new Set([
  "Mainnet",
  "Preprod",
  "Preview",
  "Custom",
]);

type DeploymentManifestV1JsonValue =
  | null
  | boolean
  | number
  | string
  | readonly DeploymentManifestV1JsonValue[]
  | { readonly [key: string]: DeploymentManifestV1JsonValue };

type DeploymentManifestV1OutRef = {
  readonly txHash: string;
  readonly outputIndex: number;
};

type DeploymentManifestV1ContractEntry = {
  readonly refScriptUTxO: DeploymentManifestV1OutRef | null;
  readonly contract: {
    readonly type: (typeof DEPLOYMENT_MANIFEST_V1_SCRIPT_TYPES)[number];
    readonly cborHex: string;
  };
  readonly scriptHash: string;
  readonly fraudProofCatalogue?: {
    readonly root: string;
    readonly categories: Readonly<
      Record<
        (typeof FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER)[number],
        {
          readonly categoryId: string;
          readonly scriptHash: string;
          readonly membershipProofCbor: string;
        }
      >
    >;
  };
};

export type DeploymentManifestV1Value = {
  readonly schemaVersion: typeof DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION;
  readonly manifestId: string;
  readonly consensusProfile: MidgardConsensusProfileV1;
  readonly consensusProfileDigest: string;
  readonly network: string;
  readonly cardanoProtocolParameters: {
    readonly snapshot: DeploymentManifestV1JsonValue;
    readonly digest: string;
  };
  readonly genesis: {
    readonly headerHash: string;
    readonly utxoSetDigest: string;
  };
  readonly createdAt: string;
  readonly updatedAt: string;
  readonly referenceScriptDeployAddress: string;
  readonly hubOracleOneShot: {
    readonly txHash: string;
    readonly outputIndex: number;
    readonly outRef: string;
    readonly status: "consumed_by_init";
  };
  readonly referenceScriptAuthPolicy: {
    readonly policyId: string;
    readonly nativeScript: {
      readonly type: "Native";
      readonly cborHex: string;
      readonly expiresAtSlot: number;
      readonly expiresAtUnixTime: number;
      readonly timelockDurationMs: number;
    };
    readonly tokenNames: Readonly<
      Record<keyof typeof REFERENCE_SCRIPT_AUTH_TOKEN_NAMES, string>
    >;
    readonly postTimelockAudit: {
      readonly required: true;
      readonly rule: string;
    };
  };
  readonly contracts: Readonly<
    Record<string, DeploymentManifestV1ContractEntry>
  >;
  readonly referenceScripts: Readonly<
    Record<
      string,
      {
        readonly status: "confirmed";
        readonly roleUnit: string;
        readonly scriptHash: string;
        readonly outRef: string;
      }
    >
  >;
  readonly da: {
    readonly committeeVkeys: readonly string[];
    readonly committeeSignersHash: string;
    readonly threshold: number;
    readonly transportProfile: {
      readonly protocolVersion: typeof DA_TRANSPORT_V1_PROTOCOL_VERSION;
      readonly runtimeManifestSchemaVersion: typeof DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION;
      readonly envelopeEncoding: "identity" | "zstd";
      readonly zstdLevel: number;
      readonly limits: typeof DA_TRANSPORT_LIMITS_V1;
      readonly retentionDays: number;
    };
  };
  readonly proofEvidence: {
    readonly digest: string | null;
    readonly blueprintHash: string;
  };
  readonly steps: Readonly<
    Record<
      string,
      {
        readonly status: (typeof DEPLOYMENT_MANIFEST_V1_STEP_STATUSES)[number];
        readonly txHash?: string;
      }
    >
  >;
  readonly validationDispute: {
    readonly version: number;
    readonly responseWindowMs: number;
    readonly maxBisectionRounds: number;
    readonly maturityMs: number;
  };
};

const stableJson = (value: unknown): string => {
  if (value === null || typeof value !== "object") {
    return JSON.stringify(value);
  }
  if (Array.isArray(value)) {
    return `[${value.map(stableJson).join(",")}]`;
  }
  const entries = Object.entries(value as Record<string, unknown>)
    .filter(([, entryValue]) => entryValue !== undefined)
    .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0));
  return `{${entries
    .map(
      ([key, entryValue]) => `${JSON.stringify(key)}:${stableJson(entryValue)}`,
    )
    .join(",")}}`;
};

export const normalizeDeploymentManifestV1JsonValue = (
  value: unknown,
  field = "value",
): DeploymentManifestV1JsonValue => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return value;
  }
  if (typeof value === "bigint") {
    return value.toString(10);
  }
  if (typeof value === "number") {
    if (!Number.isFinite(value)) {
      throw new Error(
        `Deployment manifest ${field} must contain only finite numbers`,
      );
    }
    return value;
  }
  if (Array.isArray(value)) {
    return value.map((entry, index) =>
      normalizeDeploymentManifestV1JsonValue(
        entry,
        `${field}[${index.toString()}]`,
      ),
    );
  }
  if (typeof value !== "object" || value === null) {
    throw new Error(
      `Deployment manifest ${field} must contain only JSON-safe values`,
    );
  }
  const prototype = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    throw new Error(
      `Deployment manifest ${field} must contain only plain records`,
    );
  }
  if (Reflect.ownKeys(value).length !== Object.keys(value).length) {
    throw new Error(
      `Deployment manifest ${field} must contain only string keys`,
    );
  }
  return Object.fromEntries(
    Object.entries(value as Record<string, unknown>).map(([key, entry]) => {
      if (entry === undefined) {
        throw new Error(
          `Deployment manifest ${field}.${key} must not be undefined`,
        );
      }
      return [
        key,
        normalizeDeploymentManifestV1JsonValue(entry, `${field}.${key}`),
      ];
    }),
  );
};

export const computeDeploymentManifestV1JsonDigest = (
  value: DeploymentManifestV1JsonValue,
): string => createHash("sha256").update(stableJson(value)).digest("hex");

export const computeDeploymentManifestV1DaCommitteeSignersHash = (
  committeeVkeys: readonly string[],
): string => Effect.runSync(hashHexWithBlake2b(committeeVkeys.join(""), 32));

const deploymentManifestIdentityInput = (
  manifest: Omit<DeploymentManifestV1Value, "manifestId">,
): unknown => manifest;

export const computeDeploymentManifestId = (
  manifest: Omit<DeploymentManifestV1Value, "manifestId">,
): string =>
  computeDeploymentManifestV1Id(
    deploymentManifestIdentityInput(manifest) as Record<string, unknown>,
  );

const requireObject = (
  value: unknown,
  field: string,
): Record<string, unknown> => {
  if (typeof value === "object" && value !== null && !Array.isArray(value)) {
    return value as Record<string, unknown>;
  }
  throw new Error(`Deployment manifest ${field} must be an object`);
};

const requireExactKeys = (
  value: Record<string, unknown>,
  requiredKeys: readonly string[],
  optionalKeys: readonly string[],
  field: string,
): void => {
  const allowed = new Set([...requiredKeys, ...optionalKeys]);
  for (const key of Object.keys(value)) {
    if (!allowed.has(key)) {
      throw new Error(`Deployment manifest ${field}.${key} is unexpected`);
    }
  }
  for (const key of requiredKeys) {
    if (!Object.hasOwn(value, key)) {
      throw new Error(`Deployment manifest ${field}.${key} is required`);
    }
  }
};

const requireNonEmptyString = (value: unknown, field: string): string => {
  if (typeof value === "string" && value.length > 0) {
    return value;
  }
  throw new Error(`Deployment manifest ${field} must be a non-empty string`);
};

const requireLowercaseHex = (
  value: unknown,
  bytes: number,
  field: string,
): string => {
  const parsed = requireNonEmptyString(value, field);
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(parsed)) {
    throw new Error(
      `Deployment manifest ${field} must be ${bytes.toString()}-byte lowercase hex`,
    );
  }
  return parsed;
};

const requireNonNegativeSafeInteger = (
  value: unknown,
  field: string,
): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(
      `Deployment manifest ${field} must be a non-negative safe integer`,
    );
  }
  return value;
};

const requireIsoTimestamp = (value: unknown, field: string): string => {
  const parsed = requireNonEmptyString(value, field);
  const timestamp = new Date(parsed);
  if (
    !Number.isFinite(timestamp.getTime()) ||
    timestamp.toISOString() !== parsed
  ) {
    throw new Error(
      `Deployment manifest ${field} must be a canonical ISO timestamp`,
    );
  }
  return parsed;
};

const requireOutRef = (
  value: unknown,
  field: string,
): { readonly txHash: string; readonly outputIndex: number } => {
  const outRef = requireObject(value, field);
  requireExactKeys(outRef, ["txHash", "outputIndex"], [], field);
  return {
    txHash: requireLowercaseHex(outRef.txHash, 32, `${field}.txHash`),
    outputIndex: requireNonNegativeSafeInteger(
      outRef.outputIndex,
      `${field}.outputIndex`,
    ),
  };
};

const requireLowercaseVariableHex = (value: unknown, field: string): string => {
  const parsed = requireNonEmptyString(value, field);
  if (!/^(?:[0-9a-f]{2})+$/u.test(parsed)) {
    throw new Error(
      `Deployment manifest ${field} must be non-empty even-length lowercase hex`,
    );
  }
  return parsed;
};

const requirePositiveSafeInteger = (value: unknown, field: string): number => {
  const parsed = requireNonNegativeSafeInteger(value, field);
  if (parsed === 0) {
    throw new Error(
      `Deployment manifest ${field} must be a positive safe integer`,
    );
  }
  return parsed;
};

const requireOutRefString = (
  value: unknown,
  field: string,
): DeploymentManifestV1OutRef => {
  const parsed = requireNonEmptyString(value, field);
  const match = /^([0-9a-f]{64})#([0-9]+)$/u.exec(parsed);
  if (match === null) {
    throw new Error(
      `Deployment manifest ${field} must be a canonical lowercase transaction outref`,
    );
  }
  const outputIndex = Number(match[2]);
  if (
    !Number.isSafeInteger(outputIndex) ||
    outputIndex < 0 ||
    outputIndex.toString() !== match[2]
  ) {
    throw new Error(
      `Deployment manifest ${field} output index must be canonical`,
    );
  }
  return { txHash: match[1], outputIndex };
};

const requireScriptType = (
  value: unknown,
  field: string,
): (typeof DEPLOYMENT_MANIFEST_V1_SCRIPT_TYPES)[number] => {
  if (
    typeof value === "string" &&
    DEPLOYMENT_MANIFEST_V1_SCRIPT_TYPES.some((entry) => entry === value)
  ) {
    return value as (typeof DEPLOYMENT_MANIFEST_V1_SCRIPT_TYPES)[number];
  }
  throw new Error(
    `Deployment manifest ${field} must be Native, PlutusV1, PlutusV2, or PlutusV3`,
  );
};

const validateReferenceScriptAuthPolicy = (
  candidate: Record<string, unknown>,
): void => {
  requireExactKeys(
    candidate,
    ["policyId", "nativeScript", "tokenNames", "postTimelockAudit"],
    [],
    "referenceScriptAuthPolicy",
  );
  const policyId = requireLowercaseHex(
    candidate.policyId,
    28,
    "referenceScriptAuthPolicy.policyId",
  );
  const nativeScript = requireObject(
    candidate.nativeScript,
    "referenceScriptAuthPolicy.nativeScript",
  );
  requireExactKeys(
    nativeScript,
    [
      "type",
      "cborHex",
      "expiresAtSlot",
      "expiresAtUnixTime",
      "timelockDurationMs",
    ],
    [],
    "referenceScriptAuthPolicy.nativeScript",
  );
  if (nativeScript.type !== "Native") {
    throw new Error(
      "Deployment manifest referenceScriptAuthPolicy.nativeScript.type must be Native",
    );
  }
  const nativeScriptCbor = requireLowercaseVariableHex(
    nativeScript.cborHex,
    "referenceScriptAuthPolicy.nativeScript.cborHex",
  );
  requireNonNegativeSafeInteger(
    nativeScript.expiresAtSlot,
    "referenceScriptAuthPolicy.nativeScript.expiresAtSlot",
  );
  requireNonNegativeSafeInteger(
    nativeScript.expiresAtUnixTime,
    "referenceScriptAuthPolicy.nativeScript.expiresAtUnixTime",
  );
  requirePositiveSafeInteger(
    nativeScript.timelockDurationMs,
    "referenceScriptAuthPolicy.nativeScript.timelockDurationMs",
  );
  let derivedPolicyId: string;
  try {
    derivedPolicyId = validatorToScriptHash({
      type: "Native",
      script: nativeScriptCbor,
    });
  } catch (cause) {
    throw new Error(
      `Deployment manifest referenceScriptAuthPolicy.nativeScript.cborHex is invalid: ${String(cause)}`,
    );
  }
  if (derivedPolicyId !== policyId) {
    throw new Error(
      `Deployment manifest referenceScriptAuthPolicy.policyId mismatch: expected ${derivedPolicyId}`,
    );
  }

  const tokenNames = requireObject(
    candidate.tokenNames,
    "referenceScriptAuthPolicy.tokenNames",
  );
  const tokenNameKeys = Object.keys(REFERENCE_SCRIPT_AUTH_TOKEN_NAMES);
  requireExactKeys(
    tokenNames,
    tokenNameKeys,
    [],
    "referenceScriptAuthPolicy.tokenNames",
  );
  for (const [role, expectedTokenName] of Object.entries(
    REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  )) {
    if (tokenNames[role] !== expectedTokenName) {
      throw new Error(
        `Deployment manifest referenceScriptAuthPolicy.tokenNames.${role} must equal ${expectedTokenName}`,
      );
    }
  }

  const postTimelockAudit = requireObject(
    candidate.postTimelockAudit,
    "referenceScriptAuthPolicy.postTimelockAudit",
  );
  requireExactKeys(
    postTimelockAudit,
    ["required", "rule"],
    [],
    "referenceScriptAuthPolicy.postTimelockAudit",
  );
  if (postTimelockAudit.required !== true) {
    throw new Error(
      "Deployment manifest referenceScriptAuthPolicy.postTimelockAudit.required must be true",
    );
  }
  requireNonEmptyString(
    postTimelockAudit.rule,
    "referenceScriptAuthPolicy.postTimelockAudit.rule",
  );
};

const validateFraudProofCatalogue = (
  candidate: Record<string, unknown>,
  contracts: Record<string, unknown>,
): void => {
  requireExactKeys(
    candidate,
    ["root", "categories"],
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
  );
  requireLowercaseHex(
    candidate.root,
    32,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
  );
  const categories = requireObject(
    candidate.categories,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  requireExactKeys(
    categories,
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  const contractNameByCategory = {
    doubleSpend: "fraudProofDoubleSpend",
    nonExistentInput: "fraudProofNonExistentInput",
    nonExistentInputNoIndex: "fraudProofNonExistentInputNoIndex",
    invalidRange: "fraudProofInvalidRange",
    transitionTrace: "fraudProofTransitionTrace",
    validationTraceDispute: "validationTraceDispute",
  } as const;
  for (const categoryName of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const field = `contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.${categoryName}`;
    const category = requireObject(categories[categoryName], field);
    requireExactKeys(
      category,
      ["categoryId", "scriptHash", "membershipProofCbor"],
      [],
      field,
    );
    requireLowercaseHex(category.categoryId, 4, `${field}.categoryId`);
    const scriptHash = requireLowercaseHex(
      category.scriptHash,
      28,
      `${field}.scriptHash`,
    );
    requireLowercaseVariableHex(
      category.membershipProofCbor,
      `${field}.membershipProofCbor`,
    );
    const expectedContract = requireObject(
      contracts[contractNameByCategory[categoryName]],
      `contracts.${contractNameByCategory[categoryName]}`,
    );
    if (expectedContract.scriptHash !== scriptHash) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash must match contracts.${contractNameByCategory[categoryName]}.scriptHash`,
      );
    }
  }
};

const validateContracts = (contracts: Record<string, unknown>): void => {
  requireExactKeys(
    contracts,
    DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
    [],
    "contracts",
  );
  for (const contractName of DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES) {
    const field = `contracts.${contractName}`;
    const entry = requireObject(contracts[contractName], field);
    requireExactKeys(
      entry,
      ["refScriptUTxO", "contract", "scriptHash"],
      contractName === "fraudProofCatalogueMint" ? ["fraudProofCatalogue"] : [],
      field,
    );
    const refScriptUTxO =
      entry.refScriptUTxO === null
        ? null
        : requireOutRef(entry.refScriptUTxO, `${field}.refScriptUTxO`);
    if (
      refScriptUTxO !== null &&
      `${refScriptUTxO.txHash}#${refScriptUTxO.outputIndex.toString()}` !==
        `${(entry.refScriptUTxO as Record<string, unknown>).txHash as string}#${(entry.refScriptUTxO as Record<string, unknown>).outputIndex as number}`
    ) {
      throw new Error(
        `Deployment manifest ${field}.refScriptUTxO must be canonical`,
      );
    }
    const contract = requireObject(entry.contract, `${field}.contract`);
    requireExactKeys(contract, ["type", "cborHex"], [], `${field}.contract`);
    const scriptType = requireScriptType(
      contract.type,
      `${field}.contract.type`,
    );
    const cborHex = requireLowercaseVariableHex(
      contract.cborHex,
      `${field}.contract.cborHex`,
    );
    const scriptHash = requireLowercaseHex(
      entry.scriptHash,
      28,
      `${field}.scriptHash`,
    );
    let derivedScriptHash: string;
    try {
      derivedScriptHash = validatorToScriptHash({
        type: scriptType,
        script: cborHex,
      });
    } catch (cause) {
      throw new Error(
        `Deployment manifest ${field}.contract.cborHex is invalid: ${String(cause)}`,
      );
    }
    if (derivedScriptHash !== scriptHash) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash mismatch: expected ${derivedScriptHash}`,
      );
    }
  }
  const fraudProofCatalogueMint = requireObject(
    contracts.fraudProofCatalogueMint,
    "contracts.fraudProofCatalogueMint",
  );
  if (fraudProofCatalogueMint.fraudProofCatalogue !== undefined) {
    validateFraudProofCatalogue(
      requireObject(
        fraudProofCatalogueMint.fraudProofCatalogue,
        "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
      ),
      contracts,
    );
  }
};

const validateReferenceScripts = (
  referenceScripts: Record<string, unknown>,
  referenceScriptAuthPolicy: Record<string, unknown>,
  contracts: Record<string, unknown>,
): void => {
  requireExactKeys(
    referenceScripts,
    DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_ROLES,
    [],
    "referenceScripts",
  );
  const policyId = requireNonEmptyString(
    referenceScriptAuthPolicy.policyId,
    "referenceScriptAuthPolicy.policyId",
  );
  for (const role of DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_ROLES) {
    const field = `referenceScripts.${role}`;
    const record = requireObject(referenceScripts[role], field);
    requireExactKeys(
      record,
      ["status", "roleUnit", "scriptHash", "outRef"],
      [],
      field,
    );
    if (record.status !== "confirmed") {
      throw new Error(`Deployment manifest ${field}.status must be confirmed`);
    }
    const expectedRoleUnit = referenceScriptAuthUnit(
      policyId,
      role as keyof typeof REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
    );
    if (record.roleUnit !== expectedRoleUnit) {
      throw new Error(
        `Deployment manifest ${field}.roleUnit mismatch: expected ${expectedRoleUnit}`,
      );
    }
    const contractName =
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
        role as keyof typeof DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
      ];
    const contract = requireObject(
      contracts[contractName],
      `contracts.${contractName}`,
    );
    if (record.scriptHash !== contract.scriptHash) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash must match contracts.${contractName}.scriptHash`,
      );
    }
    const contractOutRef = requireOutRef(
      contract.refScriptUTxO,
      `contracts.${contractName}.refScriptUTxO`,
    );
    const outRef = requireOutRefString(record.outRef, `${field}.outRef`);
    if (
      outRef.txHash !== contractOutRef.txHash ||
      outRef.outputIndex !== contractOutRef.outputIndex
    ) {
      throw new Error(
        `Deployment manifest ${field}.outRef must match contracts.${contractName}.refScriptUTxO`,
      );
    }
  }
};

const validateSteps = (steps: Record<string, unknown>): void => {
  requireExactKeys(steps, DEPLOYMENT_MANIFEST_V1_STEP_NAMES, [], "steps");
  for (const stepName of DEPLOYMENT_MANIFEST_V1_STEP_NAMES) {
    const field = `steps.${stepName}`;
    const step = requireObject(steps[stepName], field);
    requireExactKeys(step, ["status"], ["txHash"], field);
    if (
      typeof step.status !== "string" ||
      !DEPLOYMENT_MANIFEST_V1_STEP_STATUSES.some(
        (status) => status === step.status,
      )
    ) {
      throw new Error(`Deployment manifest ${field}.status is unsupported`);
    }
    if (step.txHash !== undefined) {
      requireLowercaseHex(step.txHash, 32, `${field}.txHash`);
    }
  }
};

const validateDaIdentity = (candidate: Record<string, unknown>): void => {
  requireExactKeys(
    candidate,
    ["committeeVkeys", "committeeSignersHash", "threshold", "transportProfile"],
    [],
    "da",
  );
  if (
    !Array.isArray(candidate.committeeVkeys) ||
    candidate.committeeVkeys.length === 0
  ) {
    throw new Error(
      "Deployment manifest da.committeeVkeys must be a non-empty array",
    );
  }
  const committeeVkeys = candidate.committeeVkeys.map((vkey, index) =>
    requireLowercaseHex(vkey, 32, `da.committeeVkeys[${index.toString()}]`),
  );
  if (new Set(committeeVkeys).size !== committeeVkeys.length) {
    throw new Error(
      "Deployment manifest da.committeeVkeys must not contain duplicates",
    );
  }
  const committeeSignersHash = requireLowercaseHex(
    candidate.committeeSignersHash,
    32,
    "da.committeeSignersHash",
  );
  const expectedCommitteeSignersHash =
    computeDeploymentManifestV1DaCommitteeSignersHash(committeeVkeys);
  if (committeeSignersHash !== expectedCommitteeSignersHash) {
    throw new Error(
      `Deployment manifest da.committeeSignersHash mismatch: expected ${expectedCommitteeSignersHash}`,
    );
  }
  const threshold = requirePositiveSafeInteger(
    candidate.threshold,
    "da.threshold",
  );
  if (threshold > committeeVkeys.length) {
    throw new Error(
      "Deployment manifest da.threshold must not exceed committee size",
    );
  }
  const transportProfile = requireObject(
    candidate.transportProfile,
    "da.transportProfile",
  );
  requireExactKeys(
    transportProfile,
    [
      "protocolVersion",
      "runtimeManifestSchemaVersion",
      "envelopeEncoding",
      "zstdLevel",
      "limits",
      "retentionDays",
    ],
    [],
    "da.transportProfile",
  );
  if (transportProfile.protocolVersion !== DA_TRANSPORT_V1_PROTOCOL_VERSION) {
    throw new Error(
      `Deployment manifest da.transportProfile.protocolVersion must equal ${DA_TRANSPORT_V1_PROTOCOL_VERSION.toString()}`,
    );
  }
  if (
    transportProfile.runtimeManifestSchemaVersion !==
    DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION
  ) {
    throw new Error(
      `Deployment manifest da.transportProfile.runtimeManifestSchemaVersion must equal ${DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION}`,
    );
  }
  if (
    transportProfile.envelopeEncoding !== "identity" &&
    transportProfile.envelopeEncoding !== "zstd"
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.envelopeEncoding must be identity or zstd",
    );
  }
  const zstdLevel = requirePositiveSafeInteger(
    transportProfile.zstdLevel,
    "da.transportProfile.zstdLevel",
  );
  if (zstdLevel > 19) {
    throw new Error(
      "Deployment manifest da.transportProfile.zstdLevel must not exceed 19",
    );
  }
  const limits = requireObject(
    transportProfile.limits,
    "da.transportProfile.limits",
  );
  requireExactKeys(
    limits,
    Object.keys(DA_TRANSPORT_LIMITS_V1),
    [],
    "da.transportProfile.limits",
  );
  if (stableJson(limits) !== stableJson(DA_TRANSPORT_LIMITS_V1)) {
    throw new Error(
      "Deployment manifest da.transportProfile.limits must exactly match canonical V1",
    );
  }
  const retentionDays = requirePositiveSafeInteger(
    transportProfile.retentionDays,
    "da.transportProfile.retentionDays",
  );
  if (retentionDays < DA_TRANSPORT_LIMITS_V1.minimumRetentionDays) {
    throw new Error(
      `Deployment manifest da.transportProfile.retentionDays must be at least ${DA_TRANSPORT_LIMITS_V1.minimumRetentionDays.toString()}`,
    );
  }
};

const validateValidationDispute = (
  candidate: Record<string, unknown>,
): void => {
  requireExactKeys(
    candidate,
    ["version", "responseWindowMs", "maxBisectionRounds", "maturityMs"],
    [],
    "validationDispute",
  );
  if (
    candidate.version !==
      MIDGARD_CONSENSUS_PROFILE_V1.validationDisputeVersion ||
    candidate.responseWindowMs !==
      MIDGARD_CONSENSUS_PROFILE_V1.limits.validationDisputeResponseWindowMs ||
    candidate.maxBisectionRounds !==
      MIDGARD_CONSENSUS_PROFILE_V1.limits.maxValidationBisectionRounds
  ) {
    throw new Error(
      "Deployment manifest validationDispute must exactly match canonical V1",
    );
  }
  if (
    candidate.maturityMs !==
      MIDGARD_CONSENSUS_PROFILE_V1.limits.blockMaturityMs ||
    (candidate.maturityMs as number) <
      MIDGARD_CONSENSUS_PROFILE_V1.limits.minValidationDisputeMaturityMs
  ) {
    throw new Error(
      "Deployment manifest validationDispute.maturityMs must equal the canonical V1 maturity and cover the dispute schedule",
    );
  }
};

const parseDeploymentManifestCommon = (
  candidate: Record<string, unknown>,
): DeploymentManifestV1Value => {
  const network = requireNonEmptyString(candidate.network, "network");
  if (!DEPLOYMENT_MANIFEST_NETWORKS.has(network)) {
    throw new Error(
      "Deployment manifest network must be Mainnet, Preprod, Preview, or Custom",
    );
  }
  const createdAt = requireIsoTimestamp(candidate.createdAt, "createdAt");
  const updatedAt = requireIsoTimestamp(candidate.updatedAt, "updatedAt");
  if (updatedAt < createdAt) {
    throw new Error("Deployment manifest updatedAt must not precede createdAt");
  }
  requireNonEmptyString(
    candidate.referenceScriptDeployAddress,
    "referenceScriptDeployAddress",
  );
  const hubOracleOneShot = requireObject(
    candidate.hubOracleOneShot,
    "hubOracleOneShot",
  );
  requireExactKeys(
    hubOracleOneShot,
    ["txHash", "outputIndex", "outRef", "status"],
    [],
    "hubOracleOneShot",
  );
  const txHash = requireLowercaseHex(
    hubOracleOneShot.txHash,
    32,
    "hubOracleOneShot.txHash",
  );
  const outputIndex = requireNonNegativeSafeInteger(
    hubOracleOneShot.outputIndex,
    "hubOracleOneShot.outputIndex",
  );
  const expectedOutRef = `${txHash}#${outputIndex.toString()}`;
  if (hubOracleOneShot.outRef !== expectedOutRef) {
    throw new Error(
      `Deployment manifest hubOracleOneShot.outRef mismatch: expected ${expectedOutRef}`,
    );
  }
  if (hubOracleOneShot.status !== "consumed_by_init") {
    throw new Error(
      "Deployment manifest hubOracleOneShot.status must be consumed_by_init",
    );
  }
  const referenceScriptAuthPolicy = requireObject(
    candidate.referenceScriptAuthPolicy,
    "referenceScriptAuthPolicy",
  );
  validateReferenceScriptAuthPolicy(referenceScriptAuthPolicy);
  const cardanoProtocolParameters = requireObject(
    candidate.cardanoProtocolParameters,
    "cardanoProtocolParameters",
  );
  requireExactKeys(
    cardanoProtocolParameters,
    ["snapshot", "digest"],
    [],
    "cardanoProtocolParameters",
  );
  const cardanoSnapshot = normalizeDeploymentManifestV1JsonValue(
    cardanoProtocolParameters.snapshot,
    "cardanoProtocolParameters.snapshot",
  );
  const cardanoDigest = requireLowercaseHex(
    cardanoProtocolParameters.digest,
    32,
    "cardanoProtocolParameters.digest",
  );
  const expectedCardanoDigest =
    computeDeploymentManifestV1JsonDigest(cardanoSnapshot);
  if (cardanoDigest !== expectedCardanoDigest) {
    throw new Error(
      `Deployment manifest cardanoProtocolParameters.digest mismatch: expected ${expectedCardanoDigest}`,
    );
  }
  const genesis = requireObject(candidate.genesis, "genesis");
  requireExactKeys(genesis, ["headerHash", "utxoSetDigest"], [], "genesis");
  requireLowercaseHex(genesis.headerHash, 28, "genesis.headerHash");
  requireLowercaseHex(genesis.utxoSetDigest, 32, "genesis.utxoSetDigest");

  const contracts = requireObject(candidate.contracts, "contracts");
  validateContracts(contracts);
  const referenceScriptAuthContract = requireObject(
    contracts.referenceScriptAuthMint,
    "contracts.referenceScriptAuthMint",
  );
  if (
    referenceScriptAuthContract.scriptHash !==
    referenceScriptAuthPolicy.policyId
  ) {
    throw new Error(
      "Deployment manifest contracts.referenceScriptAuthMint.scriptHash must match referenceScriptAuthPolicy.policyId",
    );
  }
  validateReferenceScripts(
    requireObject(candidate.referenceScripts, "referenceScripts"),
    referenceScriptAuthPolicy,
    contracts,
  );
  validateDaIdentity(requireObject(candidate.da, "da"));
  const proofEvidence = requireObject(candidate.proofEvidence, "proofEvidence");
  requireExactKeys(
    proofEvidence,
    ["digest", "blueprintHash"],
    [],
    "proofEvidence",
  );
  if (
    proofEvidence.digest !== null &&
    !/^[0-9a-f]{64}$/u.test(
      requireNonEmptyString(proofEvidence.digest, "proofEvidence.digest"),
    )
  ) {
    throw new Error(
      "Deployment manifest proofEvidence.digest must be null or lowercase SHA-256 hex",
    );
  }
  if (proofEvidence.digest !== MIDGARD_V1_RELEASE_EVIDENCE_DIGEST) {
    throw new Error(
      "Deployment manifest proofEvidence.digest must exactly match the compiled V1 release evidence",
    );
  }
  requireLowercaseHex(
    proofEvidence.blueprintHash,
    32,
    "proofEvidence.blueprintHash",
  );
  validateSteps(requireObject(candidate.steps, "steps"));
  validateValidationDispute(
    requireObject(candidate.validationDispute, "validationDispute"),
  );
  const manifestId = requireNonEmptyString(candidate.manifestId, "manifestId");
  if (!/^[0-9a-f]{64}$/.test(manifestId)) {
    throw new Error(
      "Deployment manifest manifestId must be lowercase SHA-256 hex",
    );
  }
  const parsed = candidate as unknown as DeploymentManifestV1Value;
  const { manifestId: _manifestId, ...identityInput } = parsed;
  const expectedManifestId = computeDeploymentManifestId(identityInput);
  if (manifestId !== expectedManifestId) {
    throw new Error(
      `Deployment manifest id mismatch: expected ${expectedManifestId}, found ${manifestId}`,
    );
  }
  return parsed;
};

export const parseDeploymentManifestV1Value = (
  value: unknown,
): DeploymentManifestV1Value => {
  const candidate = verifyDeploymentManifestV1Identity(value);
  requireExactKeys(
    candidate,
    [
      "schemaVersion",
      "manifestId",
      "consensusProfile",
      "consensusProfileDigest",
      "network",
      "cardanoProtocolParameters",
      "genesis",
      "createdAt",
      "updatedAt",
      "referenceScriptDeployAddress",
      "hubOracleOneShot",
      "referenceScriptAuthPolicy",
      "contracts",
      "referenceScripts",
      "da",
      "proofEvidence",
      "steps",
      "validationDispute",
    ],
    [],
    "value",
  );
  if (!isMidgardConsensusProfileV1(candidate.consensusProfile)) {
    throw new Error(
      "Deployment manifest consensusProfile must exactly match canonical V1",
    );
  }
  if (
    candidate.consensusProfileDigest !== MIDGARD_CONSENSUS_PROFILE_V1_DIGEST
  ) {
    throw new Error(
      "Deployment manifest consensusProfileDigest must exactly match canonical V1",
    );
  }
  const parsed = parseDeploymentManifestCommon(candidate);
  verifyFinalizedDeploymentManifestV1(parsed);
  return parsed;
};
