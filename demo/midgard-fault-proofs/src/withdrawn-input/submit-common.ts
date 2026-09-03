import { Data, type UTxO, validatorToScriptHash } from "@lucid-evolution/lucid";

import {
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
} from "../runtime.js";
import { requireComputationThreadToken } from "../submit-step-01.js";
import {
  WITHDRAWN_INPUT_CATEGORY_LABEL,
  type WithdrawnInputContracts,
} from "./contracts.js";

export type WithdrawnInputCatalogueCategory = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export const withdrawnInputSubmitError = (message: string): Error =>
  new Error(`${WITHDRAWN_INPUT_CATEGORY_LABEL}: ${message}`);

export const withdrawnInputStepLabel = (stepIndex: 0 | 1 | 2) =>
  `${WITHDRAWN_INPUT_CATEGORY_LABEL} step 0${(stepIndex + 1).toString()}`;

export const requireWithdrawnInputThreadUtxo = async ({
  lucid,
  contracts,
  categoryId,
  stepIndex,
  threadOutRef,
}: {
  readonly lucid: Parameters<typeof fetchUtxoByOutRef>[0]["lucid"];
  readonly contracts: WithdrawnInputContracts;
  readonly categoryId: string;
  readonly stepIndex: 0 | 1 | 2;
  readonly threadOutRef: string;
}) => {
  const label = withdrawnInputStepLabel(stepIndex);
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${label} computation-thread UTxO`,
  });
  if (threadUtxo.address !== contracts.steps[stepIndex].spendingScriptAddress) {
    throw withdrawnInputSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} is not locked at ${label}.`,
    );
  }
  return {
    threadUtxo,
    threadToken: requireComputationThreadToken({
      utxo: threadUtxo,
      computationThreadPolicyId: contracts.computationThread.policyId,
      categoryId,
      categoryLabel: WITHDRAWN_INPUT_CATEGORY_LABEL,
    }),
  };
};

/** All family step spends are reference-script-only. */
export const requireWithdrawnInputReferenceScript = ({
  utxo,
  contracts,
  stepIndex,
}: {
  readonly utxo: UTxO;
  readonly contracts: WithdrawnInputContracts;
  readonly stepIndex: 0 | 1 | 2;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw withdrawnInputSubmitError(
      `reference UTxO ${outRefLabel(utxo)} for ${withdrawnInputStepLabel(stepIndex)} carries no reference script.`,
    );
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  const expected = contracts.steps[stepIndex].spendingScriptHash;
  if (actual !== expected) {
    throw withdrawnInputSubmitError(
      `reference script at ${outRefLabel(utxo)} hashes to ${actual}, not ${withdrawnInputStepLabel(stepIndex)} validator ${expected}.`,
    );
  }
  return utxo;
};

export const requireWithdrawnInputStepState = <State>({
  threadUtxo,
  signer,
  schema,
  stepIndex,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly schema: {
    readonly fraud_prover: string;
    readonly data: State | null;
  };
  readonly stepIndex: 1 | 2;
}): State => {
  const label = withdrawnInputStepLabel(stepIndex);
  if (threadUtxo.datum == null) {
    throw withdrawnInputSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} at ${label} has no inline datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, schema);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw withdrawnInputSubmitError(
      `${label} names fraud prover ${datum.fraud_prover}, not ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw withdrawnInputSubmitError(`${label} datum carries no step state.`);
  }
  return datum.data;
};
