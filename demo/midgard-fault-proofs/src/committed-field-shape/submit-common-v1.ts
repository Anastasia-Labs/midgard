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
  COMMITTED_FIELD_SHAPE_CATEGORY_LABEL,
  type CommittedFieldShapeContracts,
} from "./contracts-v1.js";

export type CommittedFieldShapeCatalogueCategory = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export const committedFieldShapeSubmitError = (message: string): Error =>
  new Error(`${COMMITTED_FIELD_SHAPE_CATEGORY_LABEL}: ${message}`);

export const committedFieldShapeStepLabel = (stepIndex: 0 | 1): string =>
  `${COMMITTED_FIELD_SHAPE_CATEGORY_LABEL} step 0${(stepIndex + 1).toString()}`;

export const requireCommittedFieldShapeThreadUtxo = async ({
  lucid,
  contracts,
  categoryId,
  stepIndex,
  threadOutRef,
}: {
  readonly lucid: Parameters<typeof fetchUtxoByOutRef>[0]["lucid"];
  readonly contracts: CommittedFieldShapeContracts;
  readonly categoryId: string;
  readonly stepIndex: 0 | 1;
  readonly threadOutRef: string;
}): Promise<{
  readonly threadUtxo: UTxO;
  readonly threadToken: ReturnType<typeof requireComputationThreadToken>;
}> => {
  const label = committedFieldShapeStepLabel(stepIndex);
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${label} computation-thread UTxO`,
  });
  const step = contracts.steps[stepIndex];
  if (threadUtxo.address !== step.spendingScriptAddress) {
    throw committedFieldShapeSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} is not locked at ${label}.`,
    );
  }
  return {
    threadUtxo,
    threadToken: requireComputationThreadToken({
      utxo: threadUtxo,
      computationThreadPolicyId: contracts.computationThread.policyId,
      categoryId,
      categoryLabel: COMMITTED_FIELD_SHAPE_CATEGORY_LABEL,
    }),
  };
};

/** Requires that a published script is the exact step being consumed. */
export const requireCommittedFieldShapeReferenceScript = ({
  utxo,
  expectedScriptHash,
  stepIndex,
}: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly stepIndex: 0 | 1;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw committedFieldShapeSubmitError(
      `reference UTxO ${outRefLabel(utxo)} for ${committedFieldShapeStepLabel(stepIndex)} carries no reference script.`,
    );
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  if (actual !== expectedScriptHash) {
    throw committedFieldShapeSubmitError(
      `reference script at ${outRefLabel(utxo)} hashes to ${actual}, not the ${committedFieldShapeStepLabel(stepIndex)} validator ${expectedScriptHash}.`,
    );
  }
  return utxo;
};

export const requireCommittedFieldShapeStepState = <State>({
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
  const label = committedFieldShapeStepLabel(stepIndex);
  if (threadUtxo.datum == null) {
    throw committedFieldShapeSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} at ${label} has no inline datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, schema);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw committedFieldShapeSubmitError(
      `${label} thread names fraud prover ${datum.fraud_prover}, not the signing wallet ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw committedFieldShapeSubmitError(
      `${label} thread datum carries no step state.`,
    );
  }
  return datum.data;
};
