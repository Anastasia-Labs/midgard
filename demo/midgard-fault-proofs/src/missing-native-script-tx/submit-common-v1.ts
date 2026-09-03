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
  type MissingNativeScriptTxContracts,
} from "./contracts-v1.js";

export type MissingNativeScriptTxCatalogueCategory = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export const missingNativeScriptTxSubmitError = (message: string): Error =>
  new Error(`${MISSING_NATIVE_SCRIPT_TX_CATEGORY_LABEL}: ${message}`);

export type MissingNativeScriptTxStepIndex = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7;

export const missingNativeScriptTxStepLabel = (
  stepIndex: MissingNativeScriptTxStepIndex,
): string =>
  `${MISSING_NATIVE_SCRIPT_TX_CATEGORY_LABEL} step 0${(
    stepIndex + 1
  ).toString()}`;

export const requireMissingNativeScriptTxThreadUtxo = async ({
  lucid,
  contracts,
  categoryId,
  stepIndex,
  threadOutRef,
}: {
  readonly lucid: Parameters<typeof fetchUtxoByOutRef>[0]["lucid"];
  readonly contracts: MissingNativeScriptTxContracts;
  readonly categoryId: string;
  readonly stepIndex: MissingNativeScriptTxStepIndex;
  readonly threadOutRef: string;
}): Promise<{
  readonly threadUtxo: UTxO;
  readonly threadToken: ReturnType<typeof requireComputationThreadToken>;
}> => {
  const label = missingNativeScriptTxStepLabel(stepIndex);
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

export const requireMissingNativeScriptTxReferenceScript = ({
  utxo,
  expectedScriptHash,
  stepIndex,
}: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly stepIndex: MissingNativeScriptTxStepIndex;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw missingNativeScriptTxSubmitError(
      `reference UTxO ${outRefLabel(utxo)} for ${missingNativeScriptTxStepLabel(stepIndex)} carries no reference script.`,
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

export const requireMissingNativeScriptTxStepState = <State>({
  threadUtxo,
  signer,
  schema,
  stepIndex,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly schema: { fraud_prover: string; data: State | null };
  readonly stepIndex: MissingNativeScriptTxStepIndex;
}): State => {
  const label = missingNativeScriptTxStepLabel(stepIndex);
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
