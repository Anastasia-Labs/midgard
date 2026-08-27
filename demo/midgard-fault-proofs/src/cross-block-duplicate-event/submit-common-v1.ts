import {
  CROSS_BLOCK_DUPLICATE_EVENT_FRAUD_CATEGORY_ID_V1,
  type CrossBlockDuplicateEventStep02State,
} from "@al-ft/midgard-sdk";
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
  CROSS_BLOCK_DUPLICATE_EVENT_CATEGORY_LABEL,
  type CrossBlockDuplicateEventContractsV1,
} from "./contracts-v1.js";

export type CrossBlockDuplicateEventCatalogueCategoryV1 = {
  readonly categoryId: typeof CROSS_BLOCK_DUPLICATE_EVENT_FRAUD_CATEGORY_ID_V1;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export const crossBlockDuplicateEventSubmitError = (message: string): Error =>
  new Error(`${CROSS_BLOCK_DUPLICATE_EVENT_CATEGORY_LABEL}: ${message}`);

export const crossBlockDuplicateEventStepLabelV1 = (stepIndex: 0 | 1) =>
  `${CROSS_BLOCK_DUPLICATE_EVENT_CATEGORY_LABEL} step 0${(stepIndex + 1).toString()}`;

export const requireCrossBlockDuplicateEventReferenceScriptV1 = ({
  utxo,
  contracts,
  stepIndex,
}: {
  readonly utxo: UTxO;
  readonly contracts: CrossBlockDuplicateEventContractsV1;
  readonly stepIndex: 0 | 1;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw crossBlockDuplicateEventSubmitError(
      `reference UTxO ${outRefLabel(utxo)} carries no reference script`,
    );
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  const expected = contracts.steps[stepIndex].spendingScriptHash;
  if (actual !== expected) {
    throw crossBlockDuplicateEventSubmitError(
      `reference script ${actual} does not match ${crossBlockDuplicateEventStepLabelV1(stepIndex)} ${expected}`,
    );
  }
  return utxo;
};

export const requireCrossBlockDuplicateEventThreadV1 = async ({
  lucid,
  contracts,
  threadOutRef,
  stepIndex,
}: {
  readonly lucid: Parameters<typeof fetchUtxoByOutRef>[0]["lucid"];
  readonly contracts: CrossBlockDuplicateEventContractsV1;
  readonly threadOutRef: string;
  readonly stepIndex: 0 | 1;
}) => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${crossBlockDuplicateEventStepLabelV1(stepIndex)} thread`,
  });
  if (threadUtxo.address !== contracts.steps[stepIndex].spendingScriptAddress) {
    throw crossBlockDuplicateEventSubmitError(
      `thread ${outRefLabel(threadUtxo)} is not at ${crossBlockDuplicateEventStepLabelV1(stepIndex)}`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: CROSS_BLOCK_DUPLICATE_EVENT_FRAUD_CATEGORY_ID_V1,
    categoryLabel: CROSS_BLOCK_DUPLICATE_EVENT_CATEGORY_LABEL,
  });
  return { threadUtxo, threadToken };
};

export const requireCrossBlockDuplicateEventStep02StateV1 = ({
  threadUtxo,
  signer,
  schema,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly schema: {
    fraud_prover: string;
    data: CrossBlockDuplicateEventStep02State | null;
  };
}): CrossBlockDuplicateEventStep02State => {
  if (threadUtxo.datum == null) {
    throw crossBlockDuplicateEventSubmitError(
      "step-02 thread has no inline datum",
    );
  }
  const datum = Data.from(threadUtxo.datum, schema);
  if (datum.fraud_prover !== signer.paymentKeyHash || datum.data === null) {
    throw crossBlockDuplicateEventSubmitError(
      "step-02 datum does not carry this prover's authenticated handoff state",
    );
  }
  return datum.data;
};
