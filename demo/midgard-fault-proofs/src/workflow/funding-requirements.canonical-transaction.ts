import { createHash } from "node:crypto";

import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import {
  fundingAsset,
  natural,
  type WorkflowFundingAsset,
} from "./funding-requirements.workflow-funding-controlled-output.js";

export const canonicalTransaction = (
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

export const ACTION_MEASUREMENT_FIELDS = [
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

export const ACTION_DERIVED_FIELDS = [
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

export const canonicalOutputCbor = (value: unknown, field: string): string => {
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

export const hasFundingPaymentCredential = (
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

export const outputValue = (outputCborHex: string) =>
  coreToTxOutput(CML.TransactionOutput.from_cbor_hex(outputCborHex));

export const fundingContribution = ({
  value,
  field,
  outputCborHex,
}: {
  readonly value: Readonly<Record<string, unknown>>;
  readonly field: string;
  readonly outputCborHex: string;
}): Readonly<{
  fundingLovelace: string;
  fundingAssets: readonly WorkflowFundingAsset[];
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
