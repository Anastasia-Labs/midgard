import { createHash } from "node:crypto";

import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import {
  fundingRequirementsForRunnerIdentityV1,
  isAdmittedProductionFundingRequirementsIdentityV1,
} from "./production-funding-requirements-admission-v1.js";
import { isAdmittedProductionWorkflowRunnerV1 } from "./production-runner-admission-v1.js";

export const PRODUCTION_WORKFLOW_FUNDING_REQUIREMENTS_V1 =
  "midgard-production-workflow-funding-requirements-v1" as const;

const DIGEST = /^[0-9a-f]{64}$/u;
const NATURAL = /^(0|[1-9][0-9]*)$/u;
const ACTION_KIND = /^[a-z][a-z0-9]*(?:[-.:][a-z0-9]+)*$/u;
const REFERENCE_ROLE = /^[a-z][a-zA-Z0-9]*$/u;
const MEASUREMENT_VERSION = /^[a-z][a-z0-9]*(?:[-.:][a-z0-9]+)*-v[1-9][0-9]*$/u;
const UNIT = /^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u;
const PAYMENT_KEY_HASH = /^[0-9a-f]{56}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export type ProductionWorkflowFundingScopeV1 =
  | Readonly<{
      kind: "fraud_proof_category";
      category: FraudProofCatalogueCategoryName;
    }>
  | Readonly<{
      kind: "da_availability_lifecycle";
      lifecycle: "challenge_response_timeout_correction";
    }>;

export type ProductionWorkflowFundingAssetV1 = Readonly<{
  unit: string;
  quantity: string;
}>;

export type ProductionWorkflowFundingReferenceInputV1 = Readonly<{
  role: string;
  outRef: string;
  scriptHash: string | null;
  scriptBytes: number | null;
}>;

export type ProductionWorkflowFundingControlledInputV1 = Readonly<{
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
  identityAssets: readonly ProductionWorkflowFundingAssetV1[];
  fundingLovelace: string;
  fundingAssets: readonly ProductionWorkflowFundingAssetV1[];
  sourceActionKind: string | null;
  sourceOutputIndex: number | null;
}>;

export type ProductionWorkflowFundingControlledOutputV1 = Readonly<{
  outputIndex: number;
  role: "wallet_change" | "locked_reusable" | "locked_permanent" | "protocol";
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
  fundingAssets: readonly ProductionWorkflowFundingAssetV1[];
}>;

/** Exact measured input emitted by the transaction measurement harness. */
export type ProductionWorkflowFundingActionMeasurementV1 = Readonly<{
  /** Stable semantic action, never a run-specific transaction/out-ref ID. */
  actionKind: string;
  /** Exact canonical signed Cardano transaction used for the measurement. */
  signedTransactionCborHex: string;
  /** Exact resolved values for inputs whose capital belongs to the prover. */
  fundingControlledInputs: readonly ProductionWorkflowFundingControlledInputV1[];
  /** Exact roles for prover-controlled outputs in this measured transaction. */
  fundingControlledOutputs: readonly ProductionWorkflowFundingControlledOutputV1[];
  /** Every reference input, including its script identity when script-bearing. */
  referenceInputs: readonly ProductionWorkflowFundingReferenceInputV1[];
  /** Exact bytes of the resolved reference scripts read by this transaction. */
  referenceScriptBytes: number;
  requiredBondLovelace: string;
  requiredRewardCustodyLovelace: string;
  requiredNativeAssets: readonly ProductionWorkflowFundingAssetV1[];
  collateralRequired: boolean;
  conflictRetryCount: number;
}>;

export type ProductionWorkflowFundingActionV1 =
  ProductionWorkflowFundingActionMeasurementV1 &
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

export type ProductionWorkflowFundingRequirementsInputV1 = Readonly<{
  scope: ProductionWorkflowFundingScopeV1;
  deploymentFingerprint: string;
  blueprintSha256: string;
  protocolParametersDigest: string;
  economicsPolicyDigest: string;
  fundingPaymentKeyHash: string;
  measurementToolVersion: string;
  measurementArtifactSha256: string;
  actions: readonly ProductionWorkflowFundingActionMeasurementV1[];
}>;

export type ProductionWorkflowFundingRequirementsV1 = Readonly<{
  schemaVersion: typeof PRODUCTION_WORKFLOW_FUNDING_REQUIREMENTS_V1;
  scope: ProductionWorkflowFundingScopeV1;
  deploymentFingerprint: string;
  blueprintSha256: string;
  protocolParametersDigest: string;
  economicsPolicyDigest: string;
  fundingPaymentKeyHash: string;
  measurementToolVersion: string;
  measurementArtifactSha256: string;
  actions: readonly ProductionWorkflowFundingActionV1[];
  profileDigest: string;
}>;

const isPlainObject = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  (Object.getPrototypeOf(value) === Object.prototype ||
    Object.getPrototypeOf(value) === null);

const exact = (
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

const digest = (value: unknown): string =>
  createHash("sha256").update(JSON.stringify(value)).digest("hex");

const safeNaturalNumber = (value: unknown, field: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${field} must be a non-negative safe integer`);
  }
  return value as number;
};

const natural = (value: unknown, field: string): string => {
  if (typeof value !== "string" || !NATURAL.test(value)) {
    throw new Error(`${field} must be a canonical non-negative decimal`);
  }
  return value;
};

const digestField = (value: unknown, field: string): string => {
  if (typeof value !== "string" || !DIGEST.test(value)) {
    throw new Error(`${field} must be 32-byte lowercase hex`);
  }
  return value;
};

const fundingAsset = (
  value: unknown,
  field: string,
): ProductionWorkflowFundingAssetV1 => {
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

const canonicalTransaction = (
  value: unknown,
  field: string,
  fundingPaymentKeyHash: string,
): Readonly<{
  signedTransactionCborHex: string;
  transactionHash: string;
  inputOutRefs: readonly string[];
  referenceInputOutRefs: readonly string[];
  txBodyCborHex: string;
  txBodyBytes: number;
  signedTransactionBytes: number;
  signedTransactionSha256: string;
  executionUnits: Readonly<{ memory: string; steps: string }>;
  outputCborHex: readonly string[];
}> => {
  if (typeof value !== "string" || !/^(?:[0-9a-f]{2})+$/u.test(value)) {
    throw new Error(`${field} must be non-empty lowercase hex`);
  }
  let transaction: CML.Transaction;
  try {
    transaction = CML.Transaction.from_cbor_hex(value);
  } catch {
    throw new Error(`${field} is not a Cardano transaction`);
  }
  if (transaction.to_canonical_cbor_hex() !== value) {
    throw new Error(`${field} is not canonical Cardano transaction CBOR`);
  }
  const body = transaction.body();
  const bodyHash = CML.hash_transaction(body).to_raw_bytes();
  const vkeyWitnesses = transaction.witness_set().vkeywitnesses();
  let expectedFundingWitness = false;
  for (let index = 0; index < (vkeyWitnesses?.len() ?? 0); index += 1) {
    const witness = vkeyWitnesses!.get(index);
    const vkey = witness.vkey();
    if (
      vkey.hash().to_hex() === fundingPaymentKeyHash &&
      vkey.verify(bodyHash, witness.ed25519_signature())
    ) {
      expectedFundingWitness = true;
    }
  }
  if (!expectedFundingWitness) {
    throw new Error(
      `${field} lacks a valid witness from the authenticated funding credential`,
    );
  }
  const txBodyCborHex = body.to_canonical_cbor_hex();
  const inputOutRefs: string[] = [];
  const inputs = body.inputs();
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    inputOutRefs.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  const referenceInputOutRefs: string[] = [];
  const referenceInputs = body.reference_inputs();
  for (let index = 0; index < (referenceInputs?.len() ?? 0); index += 1) {
    const input = referenceInputs!.get(index);
    referenceInputOutRefs.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  const outputs = body.outputs();
  const outputCborHex: string[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    outputCborHex.push(outputs.get(index).to_canonical_cbor_hex());
  }
  if (outputCborHex.length === 0) {
    throw new Error(`${field} transaction has no outputs`);
  }
  let memory = 0n;
  let steps = 0n;
  const redeemers = transaction.witness_set().redeemers()?.to_flat_format();
  if (redeemers !== undefined) {
    for (let index = 0; index < redeemers.len(); index += 1) {
      const units = redeemers.get(index).ex_units();
      memory += units.mem();
      steps += units.steps();
    }
  }
  return Object.freeze({
    signedTransactionCborHex: value,
    transactionHash: CML.hash_transaction(body).to_hex(),
    inputOutRefs: Object.freeze(inputOutRefs.sort()),
    referenceInputOutRefs: Object.freeze(referenceInputOutRefs.sort()),
    txBodyCborHex,
    txBodyBytes: txBodyCborHex.length / 2,
    signedTransactionBytes: value.length / 2,
    signedTransactionSha256: createHash("sha256")
      .update(Buffer.from(value, "hex"))
      .digest("hex"),
    executionUnits: Object.freeze({
      memory: memory.toString(),
      steps: steps.toString(),
    }),
    outputCborHex: Object.freeze(outputCborHex),
  });
};

const ACTION_MEASUREMENT_FIELDS = [
  "actionKind",
  "signedTransactionCborHex",
  "fundingControlledInputs",
  "fundingControlledOutputs",
  "referenceInputs",
  "referenceScriptBytes",
  "requiredBondLovelace",
  "requiredRewardCustodyLovelace",
  "requiredNativeAssets",
  "collateralRequired",
  "conflictRetryCount",
] as const;

const ACTION_DERIVED_FIELDS = [
  "transactionHash",
  "inputOutRefs",
  "referenceInputOutRefs",
  "txBodyCborHex",
  "txBodyBytes",
  "signedTransactionBytes",
  "signedTransactionSha256",
  "executionUnits",
  "outputCborHex",
] as const;

const canonicalOutputCbor = (value: unknown, field: string): string => {
  if (typeof value !== "string" || !/^(?:[0-9a-f]{2})+$/u.test(value)) {
    throw new Error(`${field} must be non-empty lowercase hex`);
  }
  let output: CML.TransactionOutput;
  try {
    output = CML.TransactionOutput.from_cbor_hex(value);
  } catch {
    throw new Error(`${field} is not a Cardano transaction output`);
  }
  if (output.to_canonical_cbor_hex() !== value) {
    throw new Error(`${field} is not canonical Cardano output CBOR`);
  }
  return value;
};

const hasFundingPaymentCredential = (
  outputCborHex: string,
  fundingPaymentKeyHash: string,
): boolean => {
  const raw = CML.TransactionOutput.from_cbor_hex(outputCborHex)
    .address()
    .to_raw_bytes();
  return (
    raw.length === 29 &&
    raw[0]! >> 4 === 6 &&
    Buffer.from(raw.subarray(1)).toString("hex") === fundingPaymentKeyHash
  );
};

const outputValue = (outputCborHex: string) =>
  coreToTxOutput(CML.TransactionOutput.from_cbor_hex(outputCborHex));

const fundingContribution = ({
  value,
  field,
  outputCborHex,
}: {
  readonly value: Readonly<Record<string, unknown>>;
  readonly field: string;
  readonly outputCborHex: string;
}): Readonly<{
  fundingLovelace: string;
  fundingAssets: readonly ProductionWorkflowFundingAssetV1[];
}> => {
  const fundingLovelace = natural(
    value.fundingLovelace,
    `${field}.fundingLovelace`,
  );
  if (!Array.isArray(value.fundingAssets)) {
    throw new Error(`${field}.fundingAssets must be an array`);
  }
  const fundingAssets = value.fundingAssets.map((asset, assetIndex) =>
    fundingAsset(asset, `${field}.fundingAssets[${assetIndex.toString()}]`),
  );
  if (
    fundingAssets.some(
      (asset, assetIndex) =>
        assetIndex > 0 &&
        fundingAssets[assetIndex - 1]!.unit.localeCompare(asset.unit) >= 0,
    )
  ) {
    throw new Error(`${field}.fundingAssets must be strictly unit-sorted`);
  }
  const exactValue = outputValue(outputCborHex).assets;
  if (
    BigInt(fundingLovelace) > (exactValue.lovelace ?? 0n) ||
    fundingAssets.some(
      ({ unit, quantity }) => BigInt(quantity) > (exactValue[unit] ?? 0n),
    )
  ) {
    throw new Error(`${field} funding contribution exceeds its exact value`);
  }
  return Object.freeze({
    fundingLovelace,
    fundingAssets: Object.freeze(fundingAssets),
  });
};

const fundingControlledInput = (
  value: unknown,
  field: string,
  transactionInputOutRefs: readonly string[],
  fundingPaymentKeyHash: string,
): ProductionWorkflowFundingControlledInputV1 => {
  const record = exact(
    value,
    [
      "outRef",
      "resolvedOutputCborHex",
      "role",
      "semanticRole",
      "contractAddress",
      "identityAssets",
      "fundingLovelace",
      "fundingAssets",
      "sourceActionKind",
      "sourceOutputIndex",
    ],
    field,
  );
  if (
    typeof record.outRef !== "string" ||
    !OUT_REF.test(record.outRef) ||
    !transactionInputOutRefs.includes(record.outRef) ||
    (record.role !== "wallet_funding" &&
      record.role !== "released_locked" &&
      record.role !== "protocol")
  ) {
    throw new Error(`${field} is not an exact transaction input`);
  }
  const resolvedOutputCborHex = canonicalOutputCbor(
    record.resolvedOutputCborHex,
    `${field}.resolvedOutputCborHex`,
  );
  const contribution = fundingContribution({
    value: record,
    field,
    outputCborHex: resolvedOutputCborHex,
  });
  const exactValue = outputValue(resolvedOutputCborHex).assets;
  const exactOutput = outputValue(resolvedOutputCborHex);
  if (
    typeof record.contractAddress !== "string" ||
    record.contractAddress !== exactOutput.address ||
    !Array.isArray(record.identityAssets)
  ) {
    throw new Error(`${field} semantic input authority is invalid`);
  }
  const identityAssets = record.identityAssets.map((asset, assetIndex) =>
    fundingAsset(asset, `${field}.identityAssets[${assetIndex.toString()}]`),
  );
  if (
    identityAssets.some(
      (asset, assetIndex) =>
        assetIndex > 0 &&
        identityAssets[assetIndex - 1]!.unit.localeCompare(asset.unit) >= 0,
    ) ||
    identityAssets.length !==
      Object.keys(exactValue).filter((unit) => unit !== "lovelace").length ||
    identityAssets.some(
      ({ unit, quantity }) => exactValue[unit] !== BigInt(quantity),
    )
  ) {
    throw new Error(`${field} semantic input assets are not exact`);
  }
  if (record.role === "wallet_funding") {
    if (
      record.semanticRole !== "wallet_funding" ||
      record.sourceActionKind !== null ||
      record.sourceOutputIndex !== null ||
      !hasFundingPaymentCredential(
        resolvedOutputCborHex,
        fundingPaymentKeyHash,
      ) ||
      BigInt(contribution.fundingLovelace) !== (exactValue.lovelace ?? 0n) ||
      contribution.fundingAssets.length !==
        Object.keys(exactValue).filter((unit) => unit !== "lovelace").length ||
      contribution.fundingAssets.some(
        ({ unit, quantity }) => exactValue[unit] !== BigInt(quantity),
      )
    ) {
      throw new Error(`${field} wallet-funding authority is invalid`);
    }
  } else if (
    record.role === "released_locked" &&
    ((record.semanticRole !== "proof_thread" &&
      record.semanticRole !== "field_carrier" &&
      record.semanticRole !== "prover_bond" &&
      record.semanticRole !== "prover_reward" &&
      record.semanticRole !== "challenger_bond" &&
      record.semanticRole !== "availability_carrier" &&
      record.semanticRole !== "correction_lock") ||
      typeof record.sourceActionKind !== "string" ||
      !ACTION_KIND.test(record.sourceActionKind) ||
      !Number.isSafeInteger(record.sourceOutputIndex) ||
      (record.sourceOutputIndex as number) < 0)
  ) {
    throw new Error(`${field} released-lock source is invalid`);
  } else if (
    record.role === "protocol" &&
    (record.semanticRole !== "protocol_state" ||
      record.sourceActionKind !== null ||
      record.sourceOutputIndex !== null ||
      hasFundingPaymentCredential(
        resolvedOutputCborHex,
        fundingPaymentKeyHash,
      ) ||
      contribution.fundingLovelace !== "0" ||
      contribution.fundingAssets.length !== 0)
  ) {
    throw new Error(`${field} protocol input claims funding authority`);
  }
  return Object.freeze({
    outRef: record.outRef,
    resolvedOutputCborHex,
    role: record.role,
    semanticRole:
      record.semanticRole as ProductionWorkflowFundingControlledInputV1["semanticRole"],
    contractAddress: record.contractAddress,
    identityAssets: Object.freeze(identityAssets),
    ...contribution,
    sourceActionKind:
      record.role !== "released_locked"
        ? null
        : (record.sourceActionKind as string),
    sourceOutputIndex:
      record.role !== "released_locked"
        ? null
        : (record.sourceOutputIndex as number),
  });
};

const fundingControlledOutput = (
  value: unknown,
  field: string,
  outputCborHex: readonly string[],
  fundingPaymentKeyHash: string,
): ProductionWorkflowFundingControlledOutputV1 => {
  const record = exact(
    value,
    [
      "outputIndex",
      "role",
      "custodyRole",
      "semanticRole",
      "contractAddress",
      "fundingLovelace",
      "fundingAssets",
    ],
    field,
  );
  if (
    !Number.isSafeInteger(record.outputIndex) ||
    (record.outputIndex as number) < 0 ||
    (record.outputIndex as number) >= outputCborHex.length ||
    (record.role !== "wallet_change" &&
      record.role !== "locked_reusable" &&
      record.role !== "locked_permanent" &&
      record.role !== "protocol") ||
    (record.custodyRole !== "none" &&
      record.custodyRole !== "bond" &&
      record.custodyRole !== "reward" &&
      record.custodyRole !== "native_asset" &&
      record.custodyRole !== "carrier")
  ) {
    throw new Error(`${field} is invalid`);
  }
  const index = record.outputIndex as number;
  const exactOutput = outputValue(outputCborHex[index]!);
  if (
    typeof record.contractAddress !== "string" ||
    record.contractAddress !== exactOutput.address ||
    (record.semanticRole !== "wallet_change" &&
      record.semanticRole !== "protocol_state" &&
      record.semanticRole !== "proof_thread" &&
      record.semanticRole !== "field_carrier" &&
      record.semanticRole !== "prover_bond" &&
      record.semanticRole !== "prover_reward" &&
      record.semanticRole !== "challenger_bond" &&
      record.semanticRole !== "availability_carrier" &&
      record.semanticRole !== "correction_lock")
  ) {
    throw new Error(`${field} semantic authority is invalid`);
  }
  const contribution = fundingContribution({
    value: record,
    field,
    outputCborHex: outputCborHex[index]!,
  });
  const isFundingChange = hasFundingPaymentCredential(
    outputCborHex[index]!,
    fundingPaymentKeyHash,
  );
  if (
    (record.role === "wallet_change") !== isFundingChange ||
    (record.role === "wallet_change" &&
      (record.custodyRole !== "none" ||
        record.semanticRole !== "wallet_change" ||
        BigInt(contribution.fundingLovelace) !==
          (exactOutput.assets.lovelace ?? 0n) ||
        contribution.fundingAssets.length !==
          Object.keys(exactOutput.assets).filter((unit) => unit !== "lovelace")
            .length ||
        contribution.fundingAssets.some(
          ({ unit, quantity }) => exactOutput.assets[unit] !== BigInt(quantity),
        ))) ||
    (record.role === "protocol" &&
      (record.custodyRole !== "none" ||
        record.semanticRole !== "protocol_state" ||
        contribution.fundingLovelace !== "0" ||
        contribution.fundingAssets.length !== 0)) ||
    ((record.role === "locked_reusable" ||
      record.role === "locked_permanent") &&
      (record.custodyRole === "none" ||
        record.semanticRole === "wallet_change" ||
        record.semanticRole === "protocol_state" ||
        (contribution.fundingLovelace === "0" &&
          contribution.fundingAssets.length === 0)))
  ) {
    throw new Error(`${field} role differs from its exact output authority`);
  }
  return Object.freeze({
    outputIndex: index,
    role: record.role,
    custodyRole: record.custodyRole,
    semanticRole: record.semanticRole,
    contractAddress: record.contractAddress,
    ...contribution,
  });
};

const fundingReferenceInput = (
  value: unknown,
  field: string,
): ProductionWorkflowFundingReferenceInputV1 => {
  const record = exact(
    value,
    ["role", "outRef", "scriptHash", "scriptBytes"],
    field,
  );
  if (
    typeof record.role !== "string" ||
    !REFERENCE_ROLE.test(record.role) ||
    typeof record.outRef !== "string" ||
    !OUT_REF.test(record.outRef) ||
    !(
      (record.scriptHash === null && record.scriptBytes === null) ||
      (typeof record.scriptHash === "string" &&
        /^[0-9a-f]{56}$/u.test(record.scriptHash) &&
        Number.isSafeInteger(record.scriptBytes) &&
        (record.scriptBytes as number) >= 1)
    )
  ) {
    throw new Error(`${field} is invalid`);
  }
  return Object.freeze({
    role: record.role,
    outRef: record.outRef,
    scriptHash: record.scriptHash as string | null,
    scriptBytes: record.scriptBytes as number | null,
  });
};

const fundingAction = (
  value: unknown,
  index: number,
  requireDerivedFields: boolean,
  fundingPaymentKeyHash: string,
): ProductionWorkflowFundingActionV1 => {
  const field = `funding requirements actions[${index.toString()}]`;
  const record = exact(
    value,
    [
      ...ACTION_MEASUREMENT_FIELDS,
      ...(requireDerivedFields ? ACTION_DERIVED_FIELDS : []),
    ],
    field,
  );
  if (
    typeof record.actionKind !== "string" ||
    !ACTION_KIND.test(record.actionKind)
  ) {
    throw new Error(`${field}.actionKind is not a stable action identifier`);
  }
  if (!Array.isArray(record.requiredNativeAssets)) {
    throw new Error(`${field}.requiredNativeAssets must be an array`);
  }
  if (
    !Array.isArray(record.fundingControlledInputs) ||
    !Array.isArray(record.fundingControlledOutputs) ||
    !Array.isArray(record.referenceInputs)
  ) {
    throw new Error(`${field} funding-controlled flow must be arrays`);
  }
  if (typeof record.collateralRequired !== "boolean") {
    throw new Error(`${field}.collateralRequired must be boolean`);
  }
  const requiredNativeAssets = record.requiredNativeAssets.map(
    (asset, assetIndex) =>
      fundingAsset(
        asset,
        `${field}.requiredNativeAssets[${assetIndex.toString()}]`,
      ),
  );
  for (
    let assetIndex = 1;
    assetIndex < requiredNativeAssets.length;
    assetIndex += 1
  ) {
    if (
      requiredNativeAssets[assetIndex - 1]!.unit >=
      requiredNativeAssets[assetIndex]!.unit
    ) {
      throw new Error(
        `${field}.requiredNativeAssets must be strictly unit-sorted`,
      );
    }
  }
  const transaction = canonicalTransaction(
    record.signedTransactionCborHex,
    `${field}.signedTransactionCborHex`,
    fundingPaymentKeyHash,
  );
  const fundingControlledInputs = record.fundingControlledInputs.map(
    (entry, controlledIndex) =>
      fundingControlledInput(
        entry,
        `${field}.fundingControlledInputs[${controlledIndex.toString()}]`,
        transaction.inputOutRefs,
        fundingPaymentKeyHash,
      ),
  );
  if (
    fundingControlledInputs.length !== transaction.inputOutRefs.length ||
    new Set(fundingControlledInputs.map(({ outRef }) => outRef)).size !==
      fundingControlledInputs.length ||
    fundingControlledInputs.some(
      ({ outRef }, inputIndex) =>
        outRef !== transaction.inputOutRefs[inputIndex],
    ) ||
    !fundingControlledInputs.some(({ role }) => role !== "protocol")
  ) {
    throw new Error(
      `${field} must classify every exact transaction input once`,
    );
  }
  const fundingControlledOutputs = record.fundingControlledOutputs.map(
    (entry, controlledIndex) =>
      fundingControlledOutput(
        entry,
        `${field}.fundingControlledOutputs[${controlledIndex.toString()}]`,
        transaction.outputCborHex,
        fundingPaymentKeyHash,
      ),
  );
  const referenceInputs = record.referenceInputs.map((entry, referenceIndex) =>
    fundingReferenceInput(
      entry,
      `${field}.referenceInputs[${referenceIndex.toString()}]`,
    ),
  );
  const referenceOutRefs = referenceInputs.map(({ outRef }) => outRef).sort();
  if (
    referenceOutRefs.length !== transaction.referenceInputOutRefs.length ||
    referenceOutRefs.some(
      (outRef, referenceIndex) =>
        outRef !== transaction.referenceInputOutRefs[referenceIndex],
    ) ||
    referenceInputs.some((reference, referenceIndex) => {
      if (referenceIndex === 0) return false;
      const previous = referenceInputs[referenceIndex - 1]!;
      return (
        previous.role.localeCompare(reference.role) > 0 ||
        (previous.role === reference.role &&
          previous.outRef.localeCompare(reference.outRef) >= 0)
      );
    })
  ) {
    throw new Error(`${field} reference-script identity set is not exact`);
  }
  if (
    fundingControlledOutputs.length !== transaction.outputCborHex.length ||
    !fundingControlledOutputs.some(({ role }) => role === "wallet_change") ||
    new Set(fundingControlledOutputs.map(({ outputIndex }) => outputIndex))
      .size !== fundingControlledOutputs.length ||
    fundingControlledOutputs.some(
      ({ outputIndex }, controlledIndex) => outputIndex !== controlledIndex,
    )
  ) {
    throw new Error(
      `${field} must classify every exact output once with wallet change`,
    );
  }
  for (
    let outputIndex = 0;
    outputIndex < transaction.outputCborHex.length;
    outputIndex += 1
  ) {
    if (
      hasFundingPaymentCredential(
        transaction.outputCborHex[outputIndex]!,
        fundingPaymentKeyHash,
      ) !==
      fundingControlledOutputs.some(
        (output) =>
          output.outputIndex === outputIndex && output.role === "wallet_change",
      )
    ) {
      throw new Error(
        `${field} wallet change output classification is incomplete`,
      );
    }
  }
  if (requireDerivedFields) {
    const executionUnits = exact(
      record.executionUnits,
      ["memory", "steps"],
      `${field}.executionUnits`,
    );
    const suppliedOutputs = record.outputCborHex;
    if (
      record.txBodyCborHex !== transaction.txBodyCborHex ||
      record.transactionHash !== transaction.transactionHash ||
      !Array.isArray(record.inputOutRefs) ||
      record.inputOutRefs.length !== transaction.inputOutRefs.length ||
      record.inputOutRefs.some(
        (outRef, inputIndex) => outRef !== transaction.inputOutRefs[inputIndex],
      ) ||
      !Array.isArray(record.referenceInputOutRefs) ||
      record.referenceInputOutRefs.length !==
        transaction.referenceInputOutRefs.length ||
      record.referenceInputOutRefs.some(
        (outRef, inputIndex) =>
          outRef !== transaction.referenceInputOutRefs[inputIndex],
      ) ||
      record.txBodyBytes !== transaction.txBodyBytes ||
      record.signedTransactionBytes !== transaction.signedTransactionBytes ||
      record.signedTransactionSha256 !== transaction.signedTransactionSha256 ||
      executionUnits.memory !== transaction.executionUnits.memory ||
      executionUnits.steps !== transaction.executionUnits.steps ||
      !Array.isArray(suppliedOutputs) ||
      suppliedOutputs.length !== transaction.outputCborHex.length ||
      suppliedOutputs.some(
        (output, outputIndex) =>
          output !== transaction.outputCborHex[outputIndex],
      )
    ) {
      throw new Error(
        `${field} derived Cardano transaction measurements differ`,
      );
    }
  }
  const referenceScriptBytes = safeNaturalNumber(
    record.referenceScriptBytes,
    `${field}.referenceScriptBytes`,
  );
  if (
    referenceInputs.reduce(
      (total, reference) => total + (reference.scriptBytes ?? 0),
      0,
    ) !== referenceScriptBytes
  ) {
    throw new Error(`${field} reference-script byte total is inconsistent`);
  }
  return Object.freeze({
    actionKind: record.actionKind,
    ...transaction,
    fundingControlledInputs: Object.freeze(fundingControlledInputs),
    fundingControlledOutputs: Object.freeze(fundingControlledOutputs),
    referenceInputs: Object.freeze(referenceInputs),
    referenceScriptBytes,
    requiredBondLovelace: natural(
      record.requiredBondLovelace,
      `${field}.requiredBondLovelace`,
    ),
    requiredRewardCustodyLovelace: natural(
      record.requiredRewardCustodyLovelace,
      `${field}.requiredRewardCustodyLovelace`,
    ),
    requiredNativeAssets: Object.freeze(requiredNativeAssets),
    collateralRequired: record.collateralRequired,
    conflictRetryCount: safeNaturalNumber(
      record.conflictRetryCount,
      `${field}.conflictRetryCount`,
    ),
  });
};

type NormalizedRequirementsV1 = Omit<
  ProductionWorkflowFundingRequirementsV1,
  "schemaVersion" | "profileDigest"
> &
  Partial<Pick<ProductionWorkflowFundingRequirementsV1, "profileDigest">>;

const normalizedRequirements = (
  value: unknown,
  requireDerivedFields: boolean,
): NormalizedRequirementsV1 => {
  const record = exact(
    value,
    [
      "scope",
      "deploymentFingerprint",
      "blueprintSha256",
      "protocolParametersDigest",
      "economicsPolicyDigest",
      "fundingPaymentKeyHash",
      "measurementToolVersion",
      "measurementArtifactSha256",
      "actions",
      ...(requireDerivedFields ? ["schemaVersion", "profileDigest"] : []),
    ],
    "funding requirements",
  );
  const scopeRecord = exact(
    record.scope,
    isPlainObject(record.scope) && record.scope.kind === "fraud_proof_category"
      ? ["kind", "category"]
      : ["kind", "lifecycle"],
    "funding requirements scope",
  );
  const scope: ProductionWorkflowFundingScopeV1 = (() => {
    if (scopeRecord.kind === "fraud_proof_category") {
      if (
        typeof scopeRecord.category !== "string" ||
        !FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.includes(
          scopeRecord.category as FraudProofCatalogueCategoryName,
        )
      ) {
        throw new Error(
          "funding requirements category is not in the canonical catalogue",
        );
      }
      return Object.freeze({
        kind: "fraud_proof_category" as const,
        category: scopeRecord.category as FraudProofCatalogueCategoryName,
      });
    }
    if (
      scopeRecord.kind !== "da_availability_lifecycle" ||
      scopeRecord.lifecycle !== "challenge_response_timeout_correction"
    ) {
      throw new Error("funding requirements scope is unsupported");
    }
    return Object.freeze({
      kind: "da_availability_lifecycle" as const,
      lifecycle: "challenge_response_timeout_correction" as const,
    });
  })();
  if (
    typeof record.fundingPaymentKeyHash !== "string" ||
    !PAYMENT_KEY_HASH.test(record.fundingPaymentKeyHash)
  ) {
    throw new Error(
      "funding requirements payment credential must be a 28-byte key hash",
    );
  }
  if (
    typeof record.measurementToolVersion !== "string" ||
    !MEASUREMENT_VERSION.test(record.measurementToolVersion)
  ) {
    throw new Error(
      "funding requirements measurement tool version is not canonical",
    );
  }
  if (!Array.isArray(record.actions) || record.actions.length === 0) {
    throw new Error("funding requirements actions cannot be empty");
  }
  const actions = record.actions.map((action, index) =>
    fundingAction(
      action,
      index,
      requireDerivedFields,
      record.fundingPaymentKeyHash as string,
    ),
  );
  if (
    new Set(actions.map((action) => action.actionKind)).size !== actions.length
  ) {
    throw new Error("funding requirements action kinds must be unique");
  }
  const actionIndexByKind = new Map(
    actions.map((action, actionIndex) => [action.actionKind, actionIndex]),
  );
  const releasedSources = new Set<string>();
  for (let actionIndex = 0; actionIndex < actions.length; actionIndex += 1) {
    const action = actions[actionIndex]!;
    const inputFundingAssets = new Map<string, bigint>();
    const outputFundingAssets = new Map<string, bigint>();
    const inputFundingLovelace = action.fundingControlledInputs.reduce(
      (total, controlled) => {
        for (const { unit, quantity } of controlled.fundingAssets) {
          inputFundingAssets.set(
            unit,
            (inputFundingAssets.get(unit) ?? 0n) + BigInt(quantity),
          );
        }
        return total + BigInt(controlled.fundingLovelace);
      },
      0n,
    );
    const outputFundingLovelace = action.fundingControlledOutputs.reduce(
      (total, controlled) => {
        for (const { unit, quantity } of controlled.fundingAssets) {
          outputFundingAssets.set(
            unit,
            (outputFundingAssets.get(unit) ?? 0n) + BigInt(quantity),
          );
        }
        return total + BigInt(controlled.fundingLovelace);
      },
      0n,
    );
    const exactFee = CML.Transaction.from_cbor_hex(
      action.signedTransactionCborHex,
    )
      .body()
      .fee();
    if (
      inputFundingLovelace !== outputFundingLovelace + exactFee ||
      inputFundingAssets.size !== outputFundingAssets.size ||
      [...inputFundingAssets].some(
        ([unit, quantity]) => outputFundingAssets.get(unit) !== quantity,
      )
    ) {
      throw new Error(
        `${action.actionKind} funding-controlled value is not conserved`,
      );
    }
    let exactBond = 0n;
    let exactReward = 0n;
    const exactNativeAssets = new Map<string, bigint>();
    for (const controlled of action.fundingControlledOutputs) {
      if (controlled.role === "wallet_change") continue;
      const lovelace = BigInt(controlled.fundingLovelace);
      if (controlled.custodyRole === "bond") exactBond += lovelace;
      if (controlled.custodyRole === "reward") exactReward += lovelace;
      for (const { unit, quantity: rawQuantity } of controlled.fundingAssets) {
        const quantity = BigInt(rawQuantity);
        exactNativeAssets.set(
          unit,
          (exactNativeAssets.get(unit) ?? 0n) + quantity,
        );
      }
    }
    if (
      BigInt(action.requiredBondLovelace) !== exactBond ||
      BigInt(action.requiredRewardCustodyLovelace) !== exactReward
    ) {
      throw new Error(
        `${action.actionKind} declared custody differs from locked outputs`,
      );
    }
    const declaredNativeAssets = new Map(
      action.requiredNativeAssets.map(({ unit, quantity }) => [
        unit,
        BigInt(quantity),
      ]),
    );
    if (
      declaredNativeAssets.size !== exactNativeAssets.size ||
      [...exactNativeAssets].some(
        ([unit, quantity]) => declaredNativeAssets.get(unit) !== quantity,
      )
    ) {
      throw new Error(
        `${action.actionKind} declared native custody differs from locked outputs`,
      );
    }
    for (const controlled of action.fundingControlledInputs) {
      if (controlled.role !== "released_locked") continue;
      const sourceActionIndex = actionIndexByKind.get(
        controlled.sourceActionKind!,
      );
      if (sourceActionIndex === undefined || sourceActionIndex >= actionIndex) {
        throw new Error(
          `${action.actionKind} released-lock source is not an earlier action`,
        );
      }
      const source = actions[sourceActionIndex]!;
      const sourceOutput = source.fundingControlledOutputs.find(
        ({ outputIndex }) => outputIndex === controlled.sourceOutputIndex,
      );
      const sourceIdentity = `${source.actionKind}#${controlled.sourceOutputIndex!.toString()}`;
      if (
        sourceOutput?.role !== "locked_reusable" ||
        controlled.outRef !==
          `${source.transactionHash}#${controlled.sourceOutputIndex!.toString()}` ||
        controlled.resolvedOutputCborHex !==
          source.outputCborHex[controlled.sourceOutputIndex!] ||
        controlled.semanticRole !== sourceOutput.semanticRole ||
        controlled.contractAddress !== sourceOutput.contractAddress ||
        JSON.stringify(controlled.identityAssets) !==
          JSON.stringify(
            Object.entries(
              outputValue(source.outputCborHex[controlled.sourceOutputIndex!]!)
                .assets,
            )
              .filter(([unit]) => unit !== "lovelace")
              .sort(([left], [right]) => left.localeCompare(right))
              .map(([unit, quantity]) => ({
                unit,
                quantity: quantity.toString(),
              })),
          ) ||
        controlled.fundingLovelace !== sourceOutput.fundingLovelace ||
        JSON.stringify(controlled.fundingAssets) !==
          JSON.stringify(sourceOutput.fundingAssets) ||
        releasedSources.has(sourceIdentity)
      ) {
        throw new Error(
          `${action.actionKind} released-lock source is not exact and reusable`,
        );
      }
      releasedSources.add(sourceIdentity);
    }
  }
  if (
    requireDerivedFields &&
    record.schemaVersion !== PRODUCTION_WORKFLOW_FUNDING_REQUIREMENTS_V1
  ) {
    throw new Error("funding requirements schema version is unsupported");
  }
  return Object.freeze({
    scope,
    deploymentFingerprint: digestField(
      record.deploymentFingerprint,
      "funding requirements deployment fingerprint",
    ),
    blueprintSha256: digestField(
      record.blueprintSha256,
      "funding requirements blueprint digest",
    ),
    protocolParametersDigest: digestField(
      record.protocolParametersDigest,
      "funding requirements protocol-parameters digest",
    ),
    economicsPolicyDigest: digestField(
      record.economicsPolicyDigest,
      "funding requirements economics-policy digest",
    ),
    fundingPaymentKeyHash: record.fundingPaymentKeyHash,
    measurementToolVersion: record.measurementToolVersion,
    measurementArtifactSha256: digestField(
      record.measurementArtifactSha256,
      "funding requirements measurement artifact digest",
    ),
    actions: Object.freeze(actions),
    ...(requireDerivedFields
      ? {
          profileDigest: digestField(
            record.profileDigest,
            "funding requirements profile digest",
          ),
        }
      : {}),
  });
};

const digestInput = (value: NormalizedRequirementsV1): unknown => ({
  scope: value.scope,
  deploymentFingerprint: value.deploymentFingerprint,
  blueprintSha256: value.blueprintSha256,
  protocolParametersDigest: value.protocolParametersDigest,
  economicsPolicyDigest: value.economicsPolicyDigest,
  fundingPaymentKeyHash: value.fundingPaymentKeyHash,
  measurementToolVersion: value.measurementToolVersion,
  measurementArtifactSha256: value.measurementArtifactSha256,
  actions: value.actions,
});

export const computeProductionWorkflowFundingRequirementsDigestV1 = (
  value: ProductionWorkflowFundingRequirementsInputV1,
): string => digest(digestInput(normalizedRequirements(value, false)));

export const createProductionWorkflowFundingRequirementsV1 = (
  value: ProductionWorkflowFundingRequirementsInputV1,
): ProductionWorkflowFundingRequirementsV1 => {
  const normalized = normalizedRequirements(value, false);
  return Object.freeze({
    schemaVersion: PRODUCTION_WORKFLOW_FUNDING_REQUIREMENTS_V1,
    scope: normalized.scope,
    deploymentFingerprint: normalized.deploymentFingerprint,
    blueprintSha256: normalized.blueprintSha256,
    protocolParametersDigest: normalized.protocolParametersDigest,
    economicsPolicyDigest: normalized.economicsPolicyDigest,
    fundingPaymentKeyHash: normalized.fundingPaymentKeyHash,
    measurementToolVersion: normalized.measurementToolVersion,
    measurementArtifactSha256: normalized.measurementArtifactSha256,
    actions: normalized.actions,
    profileDigest: digest(digestInput(normalized)),
  });
};

/**
 * Strict structural parser for a measured profile. This does not make the
 * profile production authority: a fixed category factory must separately bind
 * its exact admitted runner to the measured profile identity.
 */
export const admitProductionWorkflowFundingRequirementsV1 = (
  value: unknown,
): ProductionWorkflowFundingRequirementsV1 => {
  const normalized = normalizedRequirements(value, true);
  if (normalized.profileDigest !== digest(digestInput(normalized))) {
    throw new Error("funding requirements profile digest mismatch");
  }
  return Object.freeze({
    schemaVersion: PRODUCTION_WORKFLOW_FUNDING_REQUIREMENTS_V1,
    scope: normalized.scope,
    deploymentFingerprint: normalized.deploymentFingerprint,
    blueprintSha256: normalized.blueprintSha256,
    protocolParametersDigest: normalized.protocolParametersDigest,
    economicsPolicyDigest: normalized.economicsPolicyDigest,
    fundingPaymentKeyHash: normalized.fundingPaymentKeyHash,
    measurementToolVersion: normalized.measurementToolVersion,
    measurementArtifactSha256: normalized.measurementArtifactSha256,
    actions: normalized.actions,
    profileDigest: normalized.profileDigest!,
  });
};

/**
 * Returns only a profile selected by the fixed module-admitted runner factory.
 * A structurally valid measurement profile is deliberately insufficient.
 */
export const productionWorkflowFundingRequirementsForRunnerV1 = ({
  category,
  runner,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly runner: object;
}): ProductionWorkflowFundingRequirementsV1 => {
  if (!isAdmittedProductionWorkflowRunnerV1({ category, runner })) {
    throw new Error("funding requirements runner is not category-admitted");
  }
  const requirements = fundingRequirementsForRunnerIdentityV1(runner);
  if (requirements === null) {
    throw new Error(
      `${category} production runner has no admitted measured funding profile`,
    );
  }
  return requirements;
};

/** Used by the Q58 application after its non-catalogue fixed factory admits it. */
export const assertAdmittedProductionWorkflowFundingRequirementsV1 = (
  requirements: ProductionWorkflowFundingRequirementsV1,
): void => {
  if (!isAdmittedProductionFundingRequirementsIdentityV1(requirements)) {
    throw new Error("production funding requirements are not factory-admitted");
  }
};
