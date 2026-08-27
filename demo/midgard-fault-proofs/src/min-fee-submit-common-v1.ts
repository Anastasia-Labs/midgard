import type { UTxO } from "@lucid-evolution/lucid";
import { Data, validatorToScriptHash } from "@lucid-evolution/lucid";

import {
  MIN_FEE_CATEGORY_LABEL,
  type MinFeeContractsV1,
} from "./min-fee-contracts-v1.js";
import {
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
} from "./runtime.js";
import { requireComputationThreadToken } from "./submit-step-01.js";

export type MinFeeCatalogueCategoryV1 = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export const minFeeSubmitError = (message: string): Error =>
  new Error(`${MIN_FEE_CATEGORY_LABEL}: ${message}`);

export const minFeeStepLabelV1 = (stepIndex: 0 | 1): string =>
  `${MIN_FEE_CATEGORY_LABEL} step 0${(stepIndex + 1).toString()}`;

export const requireMinFeeThreadUtxoV1 = async ({
  lucid,
  contracts,
  categoryId,
  stepIndex,
  threadOutRef,
}: {
  readonly lucid: Parameters<typeof fetchUtxoByOutRef>[0]["lucid"];
  readonly contracts: MinFeeContractsV1;
  readonly categoryId: string;
  readonly stepIndex: 0 | 1;
  readonly threadOutRef: string;
}): Promise<{
  readonly threadUtxo: UTxO;
  readonly threadToken: ReturnType<typeof requireComputationThreadToken>;
}> => {
  const label = minFeeStepLabelV1(stepIndex);
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${label} computation-thread UTxO`,
  });
  const step = contracts.steps[stepIndex];
  if (threadUtxo.address !== step.spendingScriptAddress) {
    throw minFeeSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} is not locked at ${label}.`,
    );
  }
  return {
    threadUtxo,
    threadToken: requireComputationThreadToken({
      utxo: threadUtxo,
      computationThreadPolicyId: contracts.computationThread.policyId,
      categoryId,
      categoryLabel: MIN_FEE_CATEGORY_LABEL,
    }),
  };
};

/** Reference-script-only: absence is an error, never an inline fallback. */
export const requireMinFeeReferenceScriptV1 = ({
  utxo,
  expectedScriptHash,
  stepIndex,
}: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly stepIndex: 0 | 1;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw minFeeSubmitError(
      `reference UTxO ${outRefLabel(utxo)} for ${minFeeStepLabelV1(stepIndex)} carries no reference script.`,
    );
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  if (actual !== expectedScriptHash) {
    throw minFeeSubmitError(
      `reference script at ${outRefLabel(utxo)} hashes to ${actual}, not ${expectedScriptHash}.`,
    );
  }
  return utxo;
};

export const requireMinFeeStepStateV1 = <State>({
  threadUtxo,
  signer,
  schema,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly schema: { fraud_prover: string; data: State | null };
}): State => {
  if (threadUtxo.datum == null) {
    throw minFeeSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} has no inline datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, schema);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw minFeeSubmitError(
      `thread names prover ${datum.fraud_prover}, not ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw minFeeSubmitError("thread datum carries no min-fee step state.");
  }
  return datum.data;
};
