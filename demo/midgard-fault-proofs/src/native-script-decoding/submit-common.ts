/**
 * Shared plumbing for the `native-script-decoding` step submitters (offchain
 * plan §4.2, Q3).
 *
 * The family predates catalogue registration, so every submitter takes the
 * explicit `NativeScriptDecodingContracts` record plus the category id the
 * thread NFT rides — see `contracts.ts`. This module owns what all five
 * submitters share: locating and validating the thread UTxO at a given step,
 * reading the step datum fail-closed, and reference-script sourcing.
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
  NATIVE_SCRIPT_DECODING_CATEGORY_LABEL,
  type NativeScriptDecodingContracts,
} from "./contracts.js";

/**
 * The deployed catalogue category the thread mints under. Mirrors
 * `FraudProofCatalogueCategoryDeploymentInfo`, passed explicitly because the
 * family's catalogue entry is parent-owned and lands at registration.
 */
export type NativeScriptDecodingCatalogueCategory = {
  /** 4-byte category id, hex. */
  readonly categoryId: string;
  /** The registered category script hash — must be the step-01 hash. */
  readonly scriptHash: string;
  /** MPF membership proof of `(categoryId, scriptHash)` under the catalogue root. */
  readonly membershipProofCbor: string;
};

export const nativeScriptDecodingSubmitError = (message: string): Error =>
  new Error(`${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL}: ${message}`);

export type NativeScriptDecodingStepIndex = 0 | 1 | 2 | 3 | 4 | 5;

const STEP_LABELS = [
  "step 01",
  "step 02",
  "step 03 open-subject",
  "step 03 bind-descriptor",
  "step 03 advance-or-close",
  "step 04",
] as const;

/** Physical chain index → human label used in failure messages. */
export const nativeScriptDecodingStepLabel = (
  stepIndex: NativeScriptDecodingStepIndex,
) => `${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} ${STEP_LABELS[stepIndex]}`;

/**
 * Fetches the thread UTxO, requires it to sit at the expected step's address,
 * and validates the computation-thread NFT it must carry.
 */
export const requireNativeScriptDecodingThreadUtxo = async ({
  lucid,
  contracts,
  categoryId,
  stepIndex,
  threadOutRef,
}: {
  readonly lucid: Parameters<typeof fetchUtxoByOutRef>[0]["lucid"];
  readonly contracts: NativeScriptDecodingContracts;
  readonly categoryId: string;
  readonly stepIndex: NativeScriptDecodingStepIndex;
  readonly threadOutRef: string;
}): Promise<{
  readonly threadUtxo: UTxO;
  readonly threadToken: ReturnType<typeof requireComputationThreadToken>;
}> => {
  const label = nativeScriptDecodingStepLabel(stepIndex);
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${label} computation-thread UTxO`,
  });
  const step = contracts.steps[stepIndex];
  if (threadUtxo.address !== step.spendingScriptAddress) {
    throw nativeScriptDecodingSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} is not locked at ${label}.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId,
    categoryLabel: NATIVE_SCRIPT_DECODING_CATEGORY_LABEL,
  });
  return { threadUtxo, threadToken };
};

/**
 * Q3: validates a published reference-script UTxO against the step being
 * spent. A carried script that does not hash to the step's own validator
 * would make the spend unexecutable, so it is refused before anything is
 * built.
 */
export const requireNativeScriptDecodingReferenceScript = ({
  utxo,
  expectedScriptHash,
  stepIndex,
}: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly stepIndex: NativeScriptDecodingStepIndex;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw nativeScriptDecodingSubmitError(
      `reference UTxO ${outRefLabel(utxo)} for ${nativeScriptDecodingStepLabel(stepIndex)} carries no reference script.`,
    );
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  if (actual !== expectedScriptHash) {
    throw nativeScriptDecodingSubmitError(
      `reference script at ${outRefLabel(utxo)} hashes to ${actual}, not the ${nativeScriptDecodingStepLabel(stepIndex)} validator ${expectedScriptHash}.`,
    );
  }
  return utxo;
};

/**
 * Reads a mid-chain step's inline datum fail-closed: the datum must parse
 * under the step's schema, must name the signing prover, and must carry a
 * populated state.
 */
export const requireNativeScriptDecodingStepState = <State>({
  threadUtxo,
  signer,
  schema,
  stepIndex,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly schema: { fraud_prover: string; data: State | null };
  readonly stepIndex: NativeScriptDecodingStepIndex;
}): State => {
  const label = nativeScriptDecodingStepLabel(stepIndex);
  if (threadUtxo.datum == null) {
    throw nativeScriptDecodingSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} at ${label} has no inline datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, schema);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw nativeScriptDecodingSubmitError(
      `${label} thread names fraud prover ${datum.fraud_prover}, not the signing wallet ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw nativeScriptDecodingSubmitError(
      `${label} thread datum carries no step state.`,
    );
  }
  return datum.data;
};
