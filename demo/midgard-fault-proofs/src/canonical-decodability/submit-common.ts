/** Shared fail-closed plumbing for the canonical-decodability submitters. */
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
  CANONICAL_DECODABILITY_CATEGORY_LABEL,
  type CanonicalDecodabilityContracts,
} from "./contracts.js";

/** Explicit pre-registration catalogue record. */
export type CanonicalDecodabilityCatalogueCategory = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export const canonicalDecodabilitySubmitError = (message: string): Error =>
  new Error(`${CANONICAL_DECODABILITY_CATEGORY_LABEL}: ${message}`);

export const canonicalDecodabilityStepLabel = (stepIndex: 0 | 1): string =>
  `${CANONICAL_DECODABILITY_CATEGORY_LABEL} step 0${(stepIndex + 1).toString()}`;

export const requireCanonicalDecodabilityThreadUtxo = async ({
  lucid,
  contracts,
  categoryId,
  stepIndex,
  threadOutRef,
}: {
  readonly lucid: Parameters<typeof fetchUtxoByOutRef>[0]["lucid"];
  readonly contracts: CanonicalDecodabilityContracts;
  readonly categoryId: string;
  readonly stepIndex: 0 | 1;
  readonly threadOutRef: string;
}): Promise<{
  readonly threadUtxo: UTxO;
  readonly threadToken: ReturnType<typeof requireComputationThreadToken>;
}> => {
  const label = canonicalDecodabilityStepLabel(stepIndex);
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${label} computation-thread UTxO`,
  });
  const step = contracts.steps[stepIndex];
  if (threadUtxo.address !== step.spendingScriptAddress) {
    throw canonicalDecodabilitySubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} is not locked at ${label}.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId,
    categoryLabel: CANONICAL_DECODABILITY_CATEGORY_LABEL,
  });
  return { threadUtxo, threadToken };
};

/** Requires a published UTxO to carry the exact step validator. */
export const requireCanonicalDecodabilityReferenceScript = ({
  utxo,
  expectedScriptHash,
  stepIndex,
}: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly stepIndex: 0 | 1;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw canonicalDecodabilitySubmitError(
      `reference UTxO ${outRefLabel(utxo)} for ${canonicalDecodabilityStepLabel(stepIndex)} carries no reference script.`,
    );
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  if (actual !== expectedScriptHash) {
    throw canonicalDecodabilitySubmitError(
      `reference script at ${outRefLabel(utxo)} hashes to ${actual}, not the ${canonicalDecodabilityStepLabel(stepIndex)} validator ${expectedScriptHash}.`,
    );
  }
  return utxo;
};

/** Parses a populated mid-chain datum and authenticates the named prover. */
export const requireCanonicalDecodabilityStepState = <State>({
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
  const label = canonicalDecodabilityStepLabel(stepIndex);
  if (threadUtxo.datum == null) {
    throw canonicalDecodabilitySubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} at ${label} has no inline datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, schema);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw canonicalDecodabilitySubmitError(
      `${label} thread names fraud prover ${datum.fraud_prover}, not the signing wallet ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw canonicalDecodabilitySubmitError(
      `${label} thread datum carries no step state.`,
    );
  }
  return datum.data;
};
