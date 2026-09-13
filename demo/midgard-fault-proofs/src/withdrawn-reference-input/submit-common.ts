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
  WITHDRAWN_REFERENCE_INPUT_CATEGORY_LABEL,
  type WithdrawnReferenceInputContracts,
} from "./contracts.js";

export type WithdrawnReferenceInputCatalogueCategory = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export const withdrawnReferenceInputSubmitError = (message: string): Error =>
  new Error(`${WITHDRAWN_REFERENCE_INPUT_CATEGORY_LABEL}: ${message}`);

export const withdrawnReferenceInputStepLabel = (stepIndex: 0 | 1 | 2) =>
  `${WITHDRAWN_REFERENCE_INPUT_CATEGORY_LABEL} step 0${(stepIndex + 1).toString()}`;

export const requireWithdrawnReferenceInputThreadUtxo = async ({
  lucid,
  contracts,
  categoryId,
  stepIndex,
  threadOutRef,
}: {
  readonly lucid: Parameters<typeof fetchUtxoByOutRef>[0]["lucid"];
  readonly contracts: WithdrawnReferenceInputContracts;
  readonly categoryId: string;
  readonly stepIndex: 0 | 1 | 2;
  readonly threadOutRef: string;
}): Promise<{
  readonly threadUtxo: UTxO;
  readonly threadToken: ReturnType<typeof requireComputationThreadToken>;
}> => {
  const label = withdrawnReferenceInputStepLabel(stepIndex);
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${label} computation-thread UTxO`,
  });
  if (threadUtxo.address !== contracts.steps[stepIndex].spendingScriptAddress) {
    throw withdrawnReferenceInputSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} is not locked at ${label}.`,
    );
  }
  return {
    threadUtxo,
    threadToken: requireComputationThreadToken({
      utxo: threadUtxo,
      computationThreadPolicyId: contracts.computationThread.policyId,
      categoryId,
      categoryLabel: WITHDRAWN_REFERENCE_INPUT_CATEGORY_LABEL,
    }),
  };
};

/** Reference scripts are mandatory for every family step. */
export const requireWithdrawnReferenceInputReferenceScript = ({
  utxo,
  expectedScriptHash,
  stepIndex,
}: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly stepIndex: 0 | 1 | 2;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw withdrawnReferenceInputSubmitError(
      `reference UTxO ${outRefLabel(utxo)} for ${withdrawnReferenceInputStepLabel(stepIndex)} carries no reference script.`,
    );
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  if (actual !== expectedScriptHash) {
    throw withdrawnReferenceInputSubmitError(
      `reference script at ${outRefLabel(utxo)} hashes to ${actual}, not the ${withdrawnReferenceInputStepLabel(stepIndex)} validator ${expectedScriptHash}.`,
    );
  }
  return utxo;
};

export const requireWithdrawnReferenceInputStepState = <State>({
  threadUtxo,
  signer,
  schema,
  stepIndex,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly schema: { fraud_prover: string; data: State | null };
  readonly stepIndex: 0 | 1 | 2;
}): State => {
  const label = withdrawnReferenceInputStepLabel(stepIndex);
  if (threadUtxo.datum == null) {
    throw withdrawnReferenceInputSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} at ${label} has no inline datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, schema);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw withdrawnReferenceInputSubmitError(
      `${label} thread names fraud prover ${datum.fraud_prover}, not the signing wallet ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw withdrawnReferenceInputSubmitError(
      `${label} thread datum carries no step state.`,
    );
  }
  return datum.data;
};
