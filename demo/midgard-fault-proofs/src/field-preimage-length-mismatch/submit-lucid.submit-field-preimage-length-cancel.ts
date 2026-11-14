import { type CommittedFieldClaim } from "@al-ft/midgard-sdk";
import { type UTxO, validatorToScriptHash } from "@lucid-evolution/lucid";

import { submitCommittedFieldShapeInit } from "../committed-field-shape/submit-committed-field-shape-init.js";
import { submitLinearFaultCancel } from "../linear-fault-cancel.js";
import { fetchUtxoByOutRef, outRefLabel, parseOutRef } from "../runtime.js";
import { requireComputationThreadToken } from "../step-support.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ManifestBoundFieldPreimageLengthConfig } from "./config.js";

export const LABEL = "field-preimage-length-mismatch";

const initAdapterContracts = (
  config: ManifestBoundFieldPreimageLengthConfig,
) => {
  const chain = config.contracts.fieldPreimageLengthMismatch;
  return {
    steps: [chain.steps[0], chain.steps[1]] as const,
    computationThread: config.contracts.computationThread,
    fraudProof: config.contracts.fraudProof,
    hubOraclePolicyId: config.binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId: config.binding.definition.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      config.contracts.fieldPreimageCertificate.policyId,
  };
};

/** Generic registered-category Init, specialized to this family's first step. */
export const submitFieldPreimageLengthInit = async ({
  config,
  fraudulentBlockOutRef,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfig;
  readonly fraudulentBlockOutRef: string;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) =>
  await submitCommittedFieldShapeInit({
    lucid: config.lucid,
    blueprint: config.binding.blueprint,
    network: config.binding.network,
    contracts: initAdapterContracts(config),
    category: config.binding.resolvedContracts.category,
    catalogue: config.binding.catalogue,
    signer: config.signer,
    fraudulentBlockOutRef,
    fraudulentHeaderHash: config.binding.definition.headerHash,
    witnessReferenceScripts: config.referenceScripts.witnesses,
    preSubmitBoundary,
    awaitConfirmation,
  });

/** Shared linear cancel over the four physical validators. */
export const submitFieldPreimageLengthCancel = async ({
  config,
  threadOutRef,
  stepIndex,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfig;
  readonly threadOutRef: string;
  readonly stepIndex: 0 | 1 | 2 | 3;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const referenceScriptUtxo =
    stepIndex === 0
      ? config.referenceScripts.step01
      : stepIndex === 1
        ? config.referenceScripts.step02Accepted
        : stepIndex === 2
          ? config.referenceScripts.step02Forced
          : config.referenceScripts.step03;
  return await submitLinearFaultCancel({
    lucid: config.lucid,
    family: LABEL,
    steps: config.contracts.fieldPreimageLengthMismatch.steps,
    computationThread: config.contracts.computationThread,
    categoryId: config.binding.resolvedContracts.category.categoryId,
    signer: config.signer,
    threadOutRef,
    referenceScriptUtxo,
    witnessReferenceScripts: config.referenceScripts.witnesses,
    preSubmitBoundary,
    awaitConfirmation,
  });
};

export const requireReference = ({
  utxo,
  expectedHash,
  role,
}: {
  readonly utxo: UTxO;
  readonly expectedHash: string;
  readonly role: string;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw new Error(`${LABEL}: ${role} reference carries no script`);
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  if (actual !== expectedHash) {
    throw new Error(
      `${LABEL}: ${role} reference hashes to ${actual}, expected ${expectedHash}`,
    );
  }
  return utxo;
};

export const requireThread = async ({
  config,
  threadOutRef,
  stepIndex,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfig;
  readonly threadOutRef: string;
  readonly stepIndex: 0 | 1 | 2 | 3;
}) => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid: config.lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${LABEL} thread`,
  });
  const step = config.contracts.fieldPreimageLengthMismatch.steps[stepIndex];
  if (threadUtxo.address !== step.spendingScriptAddress) {
    throw new Error(
      `${LABEL}: thread ${outRefLabel(threadUtxo)} is not at physical step ${(
        stepIndex + 1
      ).toString()}`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: config.contracts.computationThread.policyId,
    categoryId: config.binding.resolvedContracts.category.categoryId,
    categoryLabel: LABEL,
  });
  return { threadUtxo, threadToken, step };
};

export type SubmitFieldPreimageLengthForcedDispatchResult = Readonly<{
  txHash: string;
  nextThreadOutRef: string;
  computationThreadUnit: string;
  inputIndex: number;
  outputIndex: number;
}>;

export type FieldPreimageLengthClaimResolver = (
  completeReferenceInputs: readonly UTxO[],
) => CommittedFieldClaim;
