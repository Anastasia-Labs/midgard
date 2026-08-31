import type { UTxO } from "@lucid-evolution/lucid";
import { Data, validatorToScriptHash } from "@lucid-evolution/lucid";

import {
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
} from "../runtime.js";
import { requireComputationThreadToken } from "../submit-step-01.js";
import {
  MISSING_NATIVE_SCRIPT_TX_CATEGORY_LABEL,
  type MissingNativeScriptTxContractsV1,
} from "./contracts-v1.js";

export type MissingNativeScriptTxCatalogueCategoryV1 = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export const missingNativeScriptTxSubmitError = (message: string): Error =>
  new Error(`${MISSING_NATIVE_SCRIPT_TX_CATEGORY_LABEL}: ${message}`);

export type MissingNativeScriptTxStepIndexV1 = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7;

export const missingNativeScriptTxStepLabelV1 = (
  stepIndex: MissingNativeScriptTxStepIndexV1,
): string =>
  `${MISSING_NATIVE_SCRIPT_TX_CATEGORY_LABEL} step 0${(
    stepIndex + 1
  ).toString()}`;

export const requireMissingNativeScriptTxThreadUtxoV1 = async ({
  lucid,
  contracts,
  categoryId,
  stepIndex,
  threadOutRef,
}: {
  readonly lucid: Parameters<typeof fetchUtxoByOutRef>[0]["lucid"];
  readonly contracts: MissingNativeScriptTxContractsV1;
  readonly categoryId: string;
  readonly stepIndex: MissingNativeScriptTxStepIndexV1;
  readonly threadOutRef: string;
}): Promise<{
  readonly threadUtxo: UTxO;
  readonly threadToken: ReturnType<typeof requireComputationThreadToken>;
}> => {
  const label = missingNativeScriptTxStepLabelV1(stepIndex);
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${label} computation-thread UTxO`,
  });
  const step = contracts.steps[stepIndex];
  if (threadUtxo.address !== step.spendingScriptAddress) {
    throw missingNativeScriptTxSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} is not locked at ${label}.`,
    );
  }
  return {
    threadUtxo,
    threadToken: requireComputationThreadToken({
      utxo: threadUtxo,
      computationThreadPolicyId: contracts.computationThread.policyId,
      categoryId,
      categoryLabel: MISSING_NATIVE_SCRIPT_TX_CATEGORY_LABEL,
    }),
  };
};

export const requireMissingNativeScriptTxReferenceScriptV1 = ({
  utxo,
  expectedScriptHash,
  stepIndex,
}: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly stepIndex: MissingNativeScriptTxStepIndexV1;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw missingNativeScriptTxSubmitError(
      `reference UTxO ${outRefLabel(utxo)} for ${missingNativeScriptTxStepLabelV1(stepIndex)} carries no reference script.`,
    );
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  if (actual !== expectedScriptHash) {
    throw missingNativeScriptTxSubmitError(
      `reference script at ${outRefLabel(utxo)} hashes to ${actual}, not ${expectedScriptHash}.`,
    );
  }
  return utxo;
};

export const requireMissingNativeScriptTxStepStateV1 = <State>({
  threadUtxo,
  signer,
  schema,
  stepIndex,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly schema: { fraud_prover: string; data: State | null };
  readonly stepIndex: MissingNativeScriptTxStepIndexV1;
}): State => {
  const label = missingNativeScriptTxStepLabelV1(stepIndex);
  if (threadUtxo.datum == null) {
    throw missingNativeScriptTxSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} at ${label} has no inline datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, schema);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw missingNativeScriptTxSubmitError(
      `${label} thread names fraud prover ${datum.fraud_prover}, not ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw missingNativeScriptTxSubmitError(
      `${label} thread datum carries no step state.`,
    );
  }
  return datum.data;
};
