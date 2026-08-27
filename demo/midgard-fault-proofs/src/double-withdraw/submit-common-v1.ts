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
  DOUBLE_WITHDRAW_CATEGORY_LABEL,
  type DoubleWithdrawContractsV1,
} from "./contracts-v1.js";

export type DoubleWithdrawCatalogueCategoryV1 = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export const doubleWithdrawSubmitError = (message: string): Error =>
  new Error(`${DOUBLE_WITHDRAW_CATEGORY_LABEL}: ${message}`);

export const doubleWithdrawStepLabelV1 = (stepIndex: 0 | 1): string =>
  `${DOUBLE_WITHDRAW_CATEGORY_LABEL} step 0${(stepIndex + 1).toString()}`;

export const requireDoubleWithdrawThreadUtxoV1 = async ({
  lucid,
  contracts,
  categoryId,
  stepIndex,
  threadOutRef,
}: {
  readonly lucid: Parameters<typeof fetchUtxoByOutRef>[0]["lucid"];
  readonly contracts: DoubleWithdrawContractsV1;
  readonly categoryId: string;
  readonly stepIndex: 0 | 1;
  readonly threadOutRef: string;
}): Promise<{
  readonly threadUtxo: UTxO;
  readonly threadToken: ReturnType<typeof requireComputationThreadToken>;
}> => {
  const label = doubleWithdrawStepLabelV1(stepIndex);
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${label} computation-thread UTxO`,
  });
  if (threadUtxo.address !== contracts.steps[stepIndex].spendingScriptAddress) {
    throw doubleWithdrawSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} is not locked at ${label}.`,
    );
  }
  return {
    threadUtxo,
    threadToken: requireComputationThreadToken({
      utxo: threadUtxo,
      computationThreadPolicyId: contracts.computationThread.policyId,
      categoryId,
      categoryLabel: DOUBLE_WITHDRAW_CATEGORY_LABEL,
    }),
  };
};

export const requireDoubleWithdrawReferenceScriptV1 = ({
  utxo,
  expectedScriptHash,
  stepIndex,
}: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly stepIndex: 0 | 1;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw doubleWithdrawSubmitError(
      `reference UTxO ${outRefLabel(utxo)} for ${doubleWithdrawStepLabelV1(stepIndex)} carries no reference script.`,
    );
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  if (actual !== expectedScriptHash) {
    throw doubleWithdrawSubmitError(
      `reference script at ${outRefLabel(utxo)} hashes to ${actual}, not ${expectedScriptHash}.`,
    );
  }
  return utxo;
};

export const requireDoubleWithdrawStepStateV1 = <State>({
  threadUtxo,
  signer,
  schema,
  stepIndex,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly schema: { fraud_prover: string; data: State | null };
  readonly stepIndex: 0 | 1;
}): State => {
  const label = doubleWithdrawStepLabelV1(stepIndex);
  if (threadUtxo.datum == null) {
    throw doubleWithdrawSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} at ${label} has no inline datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, schema);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw doubleWithdrawSubmitError(
      `${label} thread names fraud prover ${datum.fraud_prover}, not the signing wallet ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw doubleWithdrawSubmitError(`${label} thread datum carries no state.`);
  }
  return datum.data;
};
