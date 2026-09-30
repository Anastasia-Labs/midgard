import { createHash } from "node:crypto";

import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

export const WORKFLOW_FUNDING_REQUIREMENTS =
  "midgard-production-workflow-funding-requirements-v1" as const;

const DIGEST = /^[0-9a-f]{64}$/u;

const NATURAL = /^(0|[1-9][0-9]*)$/u;

export const ACTION_KIND = /^[a-z][a-z0-9]*(?:[-._:][a-z0-9]+)*$/u;

export const REFERENCE_ROLE = /^[a-z][a-zA-Z0-9]*$/u;

export const MEASUREMENT_VERSION =
  /^[a-z][a-z0-9]*(?:[-.:][a-z0-9]+)*-v[1-9][0-9]*$/u;

const UNIT = /^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u;

export const PAYMENT_KEY_HASH = /^[0-9a-f]{56}$/u;

export const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export type WorkflowFundingScope =
  | Readonly<{
      kind: "fraud_proof_category";
      category: FraudProofCatalogueCategoryName;
    }>
  | Readonly<{
      kind: "da_availability_lifecycle";
      lifecycle: "challenge_response_timeout_correction";
    }>;

export type WorkflowFundingAsset = Readonly<{
  unit: string;
  quantity: string;
}>;

export type WorkflowFundingReferenceInput = Readonly<{
  role: string;
  outRef: string;
  scriptHash: string | null;
  scriptBytes: number | null;
}>;

export type WorkflowFundingControlledInput = Readonly<{
  outRef: string;
  resolvedOutputCborHex: string;
  role: "wallet_funding" | "released_locked" | "protocol";
  semanticRole:
    | "wallet_funding"
    | "protocol_state"
    | "proof_thread"
    | "field_carrier"
    | "prover_bond"
    | "prover_reward"
    | "challenger_bond"
    | "availability_carrier"
    | "correction_lock";
  contractAddress: string;
  identityAssets: readonly WorkflowFundingAsset[];
  fundingLovelace: string;
  fundingAssets: readonly WorkflowFundingAsset[];
  sourceActionKind: string | null;
  sourceOutputIndex: number | null;
}>;

export type WorkflowFundingControlledOutput = Readonly<{
  outputIndex: number;
  role:
    | "wallet_change"
    | "locked_reusable"
    | "locked_permanent"
    | "protocol"
    | "protocol_reward";
  custodyRole: "none" | "bond" | "reward" | "native_asset" | "carrier";
  semanticRole:
    | "wallet_change"
    | "protocol_state"
    | "proof_thread"
    | "field_carrier"
    | "prover_bond"
    | "prover_reward"
    | "challenger_bond"
    | "availability_carrier"
    | "correction_lock";
  contractAddress: string;
  fundingLovelace: string;
  fundingAssets: readonly WorkflowFundingAsset[];
}>;

/** Exact measured input emitted by the transaction measurement harness. */
export type WorkflowFundingActionMeasurement = Readonly<{
  /** Stable semantic action, never a run-specific transaction/out-ref ID. */
  actionKind: string;
  /** Exact canonical signed Cardano transaction used for the measurement. */
  signedTransactionCborHex: string;
  /** Exact resolved values for inputs whose capital belongs to the prover. */
  fundingControlledInputs: readonly WorkflowFundingControlledInput[];
  /** Exact roles for prover-controlled outputs in this measured transaction. */
  fundingControlledOutputs: readonly WorkflowFundingControlledOutput[];
  /** Every reference input, including its script identity when script-bearing. */
  referenceInputs: readonly WorkflowFundingReferenceInput[];
  /** Exact bytes of the resolved reference scripts read by this transaction. */
  referenceScriptBytes: number;
  requiredBondLovelace: string;
  requiredRewardCustodyLovelace: string;
  requiredNativeAssets: readonly WorkflowFundingAsset[];
  collateralRequired: boolean;
  conflictRetryCount: number;
}>;

export type WorkflowFundingAction = WorkflowFundingActionMeasurement &
  Readonly<{
    transactionHash: string;
    inputOutRefs: readonly string[];
    referenceInputOutRefs: readonly string[];
    txBodyCborHex: string;
    txBodyBytes: number;
    signedTransactionBytes: number;
    signedTransactionSha256: string;
    executionUnits: Readonly<{
      memory: string;
      steps: string;
    }>;
    /** Exact canonical outputs; consumers must use CML min_ada_required. */
    outputCborHex: readonly string[];
  }>;

export type WorkflowFundingRequirementsInput = Readonly<{
  scope: WorkflowFundingScope;
  deploymentFingerprint: string;
  blueprintSha256: string;
  protocolParametersDigest: string;
  economicsPolicyDigest: string;
  fundingPaymentKeyHash: string;
  measurementToolVersion: string;
  measurementArtifactSha256: string;
  actions: readonly WorkflowFundingActionMeasurement[];
}>;

/** Protocol-only actions spend no prover principal; rewards are not prefunding. */
export const isProtocolFundedWorkflowAction = (
  action: Pick<
    WorkflowFundingActionMeasurement,
    "fundingControlledInputs" | "fundingControlledOutputs"
  >,
): boolean =>
  action.fundingControlledInputs.length > 0 &&
  action.fundingControlledInputs.every(({ role }) => role === "protocol") &&
  action.fundingControlledOutputs.some(
    ({ role }) => role === "protocol_reward",
  );

export type WorkflowFundingRequirements = Readonly<{
  schemaVersion: typeof WORKFLOW_FUNDING_REQUIREMENTS;
  scope: WorkflowFundingScope;
  deploymentFingerprint: string;
  blueprintSha256: string;
  protocolParametersDigest: string;
  economicsPolicyDigest: string;
  fundingPaymentKeyHash: string;
  measurementToolVersion: string;
  measurementArtifactSha256: string;
  actions: readonly WorkflowFundingAction[];
  profileDigest: string;
}>;

export const isPlainObject = (
  value: unknown,
): value is Record<string, unknown> =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  (Object.getPrototypeOf(value) === Object.prototype ||
    Object.getPrototypeOf(value) === null);

export const exact = (
  value: unknown,
  keys: readonly string[],
  field: string,
): Record<string, unknown> => {
  if (!isPlainObject(value)) {
    throw new Error(`${field} must be a plain object`);
  }
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${field} has unknown or missing fields`);
  }
  return value;
};

export const digest = (value: unknown): string =>
  createHash("sha256").update(JSON.stringify(value)).digest("hex");

export const safeNaturalNumber = (value: unknown, field: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${field} must be a non-negative safe integer`);
  }
  return value as number;
};

export const natural = (value: unknown, field: string): string => {
  if (typeof value !== "string" || !NATURAL.test(value)) {
    throw new Error(`${field} must be a canonical non-negative decimal`);
  }
  return value;
};

export const digestField = (value: unknown, field: string): string => {
  if (typeof value !== "string" || !DIGEST.test(value)) {
    throw new Error(`${field} must be 32-byte lowercase hex`);
  }
  return value;
};

export const fundingAsset = (
  value: unknown,
  field: string,
): WorkflowFundingAsset => {
  const record = exact(value, ["unit", "quantity"], field);
  if (typeof record.unit !== "string" || !UNIT.test(record.unit)) {
    throw new Error(`${field}.unit is not a canonical Cardano asset unit`);
  }
  const quantity = natural(record.quantity, `${field}.quantity`);
  if (quantity === "0") {
    throw new Error(`${field}.quantity must be positive`);
  }
  return Object.freeze({ unit: record.unit, quantity });
};
