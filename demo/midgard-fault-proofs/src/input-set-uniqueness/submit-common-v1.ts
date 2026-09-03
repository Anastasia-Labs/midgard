/**
 * Shared plumbing for the `input-set-uniqueness` step submitters.
 *
 * The family predates catalogue registration, so every submitter takes the
 * explicit `InputSetUniquenessContracts` record plus the category id the
 * thread NFT rides — see `contracts-v1.ts`. This module owns what the
 * submitters share: locating and validating the thread UTxO at a given step,
 * reading the step datum fail-closed, and reference-script sourcing (both
 * steps deploy as reference scripts in production per the standing ruling;
 * each submitter accepts the published reference-script UTxO and verifies the
 * carried script hashes to the step it is spending before building anything).
 */
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
  INPUT_SET_UNIQUENESS_CATEGORY_LABEL,
  type InputSetUniquenessContracts,
} from "./contracts-v1.js";

/**
 * The deployed catalogue category the thread mints under. Mirrors
 * `FraudProofCatalogueCategoryDeploymentInfo`, passed explicitly because the
 * family's catalogue entry is parent-owned and lands at registration.
 */
export type InputSetUniquenessCatalogueCategory = {
  /** 4-byte category id, hex. */
  readonly categoryId: string;
  /** The registered category script hash — must be the step-01 hash. */
  readonly scriptHash: string;
  /** MPF membership proof of `(categoryId, scriptHash)` under the catalogue root. */
  readonly membershipProofCbor: string;
};

export const inputSetUniquenessSubmitError = (message: string): Error =>
  new Error(`${INPUT_SET_UNIQUENESS_CATEGORY_LABEL}: ${message}`);

/** One-based step number → human label used in failure messages. */
export const inputSetUniquenessStepLabel = (stepIndex: 0 | 1 | 2 | 3) =>
  `${INPUT_SET_UNIQUENESS_CATEGORY_LABEL} step 0${(stepIndex + 1).toString()}`;

/**
 * Fetches the thread UTxO, requires it to sit at the expected step's address,
 * and validates the computation-thread NFT it must carry.
 */
export const requireInputSetUniquenessThreadUtxo = async ({
  lucid,
  contracts,
  categoryId,
  stepIndex,
  threadOutRef,
}: {
  readonly lucid: Parameters<typeof fetchUtxoByOutRef>[0]["lucid"];
  readonly contracts: InputSetUniquenessContracts;
  readonly categoryId: string;
  readonly stepIndex: 0 | 1 | 2 | 3;
  readonly threadOutRef: string;
}): Promise<{
  readonly threadUtxo: UTxO;
  readonly threadToken: ReturnType<typeof requireComputationThreadToken>;
}> => {
  const label = inputSetUniquenessStepLabel(stepIndex);
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${label} computation-thread UTxO`,
  });
  const step = contracts.steps[stepIndex];
  if (threadUtxo.address !== step.spendingScriptAddress) {
    throw inputSetUniquenessSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} is not locked at ${label}.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId,
    categoryLabel: INPUT_SET_UNIQUENESS_CATEGORY_LABEL,
  });
  return { threadUtxo, threadToken };
};

/**
 * Validates a published reference-script UTxO against the step being spent. A
 * carried script that does not hash to the step's own validator would make
 * the spend unexecutable, so it is refused before anything is built.
 */
export const requireInputSetUniquenessReferenceScript = ({
  utxo,
  expectedScriptHash,
  stepIndex,
}: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly stepIndex: 0 | 1 | 2 | 3;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw inputSetUniquenessSubmitError(
      `reference UTxO ${outRefLabel(utxo)} for ${inputSetUniquenessStepLabel(stepIndex)} carries no reference script.`,
    );
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  if (actual !== expectedScriptHash) {
    throw inputSetUniquenessSubmitError(
      `reference script at ${outRefLabel(utxo)} hashes to ${actual}, not the ${inputSetUniquenessStepLabel(stepIndex)} validator ${expectedScriptHash}.`,
    );
  }
  return utxo;
};

/**
 * Reads a mid-chain step's inline datum fail-closed: the datum must parse
 * under the step's schema, must name the signing prover, and must carry a
 * populated state.
 */
export const requireInputSetUniquenessStepState = <State>({
  threadUtxo,
  signer,
  schema,
  stepIndex,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly schema: { fraud_prover: string; data: State | null };
  readonly stepIndex: 0 | 1 | 2 | 3;
}): State => {
  const label = inputSetUniquenessStepLabel(stepIndex);
  if (threadUtxo.datum == null) {
    throw inputSetUniquenessSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} at ${label} has no inline datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, schema);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw inputSetUniquenessSubmitError(
      `${label} thread names fraud prover ${datum.fraud_prover}, not the signing wallet ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw inputSetUniquenessSubmitError(
      `${label} thread datum carries no step state.`,
    );
  }
  return datum.data;
};
