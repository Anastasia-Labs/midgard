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
  L2_TX_MISTAG_CATEGORY_LABEL,
  type L2TxMistagContractsV1,
} from "./contracts-v1.js";

export type L2TxMistagCatalogueCategoryV1 = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export const l2TxMistagSubmitError = (message: string): Error =>
  new Error(`${L2_TX_MISTAG_CATEGORY_LABEL}: ${message}`);

export const l2TxMistagStepLabelV1 = (stepIndex: 0 | 1) =>
  `${L2_TX_MISTAG_CATEGORY_LABEL} step 0${(stepIndex + 1).toString()}`;

export const requireL2TxMistagThreadUtxoV1 = async ({
  lucid,
  contracts,
  categoryId,
  stepIndex,
  threadOutRef,
}: {
  readonly lucid: Parameters<typeof fetchUtxoByOutRef>[0]["lucid"];
  readonly contracts: L2TxMistagContractsV1;
  readonly categoryId: string;
  readonly stepIndex: 0 | 1;
  readonly threadOutRef: string;
}) => {
  const label = l2TxMistagStepLabelV1(stepIndex);
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${label} computation-thread UTxO`,
  });
  if (threadUtxo.address !== contracts.steps[stepIndex].spendingScriptAddress) {
    throw l2TxMistagSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} is not locked at ${label}.`,
    );
  }
  return {
    threadUtxo,
    threadToken: requireComputationThreadToken({
      utxo: threadUtxo,
      computationThreadPolicyId: contracts.computationThread.policyId,
      categoryId,
      categoryLabel: L2_TX_MISTAG_CATEGORY_LABEL,
    }),
  };
};

/** Reference scripts are mandatory for this family; there is no inline path. */
export const requireL2TxMistagReferenceScriptV1 = ({
  utxo,
  expectedScriptHash,
  stepIndex,
}: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly stepIndex: 0 | 1;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw l2TxMistagSubmitError(
      `reference UTxO ${outRefLabel(utxo)} for ${l2TxMistagStepLabelV1(stepIndex)} carries no reference script.`,
    );
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  if (actual !== expectedScriptHash) {
    throw l2TxMistagSubmitError(
      `reference script at ${outRefLabel(utxo)} hashes to ${actual}, not ${expectedScriptHash}.`,
    );
  }
  return utxo;
};

export const requireL2TxMistagStepStateV1 = <State>({
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
  if (threadUtxo.datum == null) {
    throw l2TxMistagSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} has no inline datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, schema);
  if (datum.fraud_prover !== signer.paymentKeyHash || datum.data === null) {
    throw l2TxMistagSubmitError(
      `${l2TxMistagStepLabelV1(stepIndex)} datum does not carry state for the signing fraud prover.`,
    );
  }
  return datum.data;
};
