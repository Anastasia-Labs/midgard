import { WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID_V1 } from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { validatorToScriptHash } from "@lucid-evolution/lucid";

import { fetchUtxoByOutRef, outRefLabel, parseOutRef } from "../runtime.js";
import { requireComputationThreadToken } from "../submit-step-01.js";
import {
  WITHDRAWAL_MISTAG_CATEGORY_LABEL,
  type WithdrawalMistagContractsV1,
} from "./contracts-v1.js";

export const withdrawalMistagError = (message: string): Error =>
  new Error(`${WITHDRAWAL_MISTAG_CATEGORY_LABEL}: ${message}`);

export const withdrawalMistagStepLabelV1 = (
  stepIndex: 0 | 1 | 2 | 3 | 4,
): string => `${WITHDRAWAL_MISTAG_CATEGORY_LABEL} step 0${stepIndex + 1}`;

export const requireWithdrawalMistagThreadUtxoV1 = async ({
  lucid,
  contracts,
  stepIndex,
  threadOutRef,
}: {
  readonly lucid: Parameters<typeof fetchUtxoByOutRef>[0]["lucid"];
  readonly contracts: WithdrawalMistagContractsV1;
  readonly stepIndex: 0 | 1 | 2 | 3 | 4;
  readonly threadOutRef: string;
}) => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: withdrawalMistagStepLabelV1(stepIndex),
  });
  if (threadUtxo.address !== contracts.steps[stepIndex].spendingScriptAddress) {
    throw withdrawalMistagError(
      `${outRefLabel(threadUtxo)} is not at ${withdrawalMistagStepLabelV1(stepIndex)}`,
    );
  }
  return {
    threadUtxo,
    threadToken: requireComputationThreadToken({
      utxo: threadUtxo,
      computationThreadPolicyId: contracts.computationThread.policyId,
      categoryId: WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID_V1,
      categoryLabel: WITHDRAWAL_MISTAG_CATEGORY_LABEL,
    }),
  };
};

export const requireWithdrawalMistagReferenceScriptV1 = ({
  utxo,
  contracts,
  stepIndex,
}: {
  readonly utxo: UTxO;
  readonly contracts: WithdrawalMistagContractsV1;
  readonly stepIndex: 0 | 1 | 2 | 3 | 4;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw withdrawalMistagError(`${outRefLabel(utxo)} has no reference script`);
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  const expected = contracts.steps[stepIndex].spendingScriptHash;
  if (actual !== expected) {
    throw withdrawalMistagError(
      `${outRefLabel(utxo)} hashes to ${actual}, expected ${expected}`,
    );
  }
  return utxo;
};
