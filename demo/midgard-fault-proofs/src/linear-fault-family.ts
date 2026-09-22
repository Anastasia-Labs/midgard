import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { Script, UTxO } from "@lucid-evolution/lucid";
import { Data, validatorToScriptHash } from "@lucid-evolution/lucid";

import {
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
} from "./runtime.js";
import { requireComputationThreadToken } from "./submit-step-01.js";

export type LinearFaultStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

export type LinearFaultContracts = {
  readonly steps: readonly LinearFaultStepContract[];
  readonly computationThread: { readonly policyId: string };
};

export const linearFaultStepLabel = (
  family: string,
  stepIndex: number,
): string => `${family} step ${String(stepIndex + 1).padStart(2, "0")}`;

export const requireLinearFaultThreadUtxo = async ({
  lucid,
  contracts,
  categoryId,
  family,
  stepIndex,
  threadOutRef,
}: {
  readonly lucid: Parameters<typeof fetchUtxoByOutRef>[0]["lucid"];
  readonly contracts: LinearFaultContracts;
  readonly categoryId: string;
  readonly family: string;
  readonly stepIndex: number;
  readonly threadOutRef: string;
}) => {
  const step = contracts.steps[stepIndex];
  if (step === undefined) {
    throw new Error(`${family}: step index ${stepIndex.toString()} is absent`);
  }
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${linearFaultStepLabel(family, stepIndex)} thread`,
  });
  if (threadUtxo.address !== step.spendingScriptAddress) {
    throw new Error(
      `${family}: thread ${outRefLabel(threadUtxo)} is not locked at ${linearFaultStepLabel(family, stepIndex)}`,
    );
  }
  return {
    threadUtxo,
    threadToken: requireComputationThreadToken({
      utxo: threadUtxo,
      computationThreadPolicyId: contracts.computationThread.policyId,
      categoryId,
      categoryLabel: family,
    }),
  };
};

export const requireLinearFaultReferenceScript = ({
  utxo,
  expectedScriptHash,
  family,
  stepIndex,
}: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly family: string;
  readonly stepIndex: number;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw new Error(
      `${family}: ${linearFaultStepLabel(family, stepIndex)} reference ${outRefLabel(utxo)} carries no script`,
    );
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  if (actual !== expectedScriptHash) {
    throw new Error(
      `${family}: reference script hashes to ${actual}, not ${expectedScriptHash}`,
    );
  }
  return utxo;
};

export const requireLinearFaultStepState = <State>({
  threadUtxo,
  signer,
  schema,
  family,
  stepIndex,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly schema: { fraud_prover: string; data: State | null };
  readonly family: string;
  readonly stepIndex: number;
}): State => {
  if (threadUtxo.datum == null) {
    throw new Error(`${family}: thread carries no inline datum`);
  }
  const datum = Data.from(threadUtxo.datum, schema);
  if (datum.fraud_prover !== signer.paymentKeyHash || datum.data === null) {
    throw new Error(
      `${family}: ${linearFaultStepLabel(family, stepIndex)} datum does not name the signer and a state`,
    );
  }
  return datum.data;
};

export const requireLinearFaultInitialDatum = ({
  threadUtxo,
  signer,
  family,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly family: string;
}): void => {
  if (threadUtxo.datum == null) throw new Error(`${family}: no initial datum`);
  const datum = Data.from(
    threadUtxo.datum,
    FraudProofComputationThreadStepDatum,
  );
  if (datum.fraud_prover !== signer.paymentKeyHash || datum.data !== null) {
    throw new Error(`${family}: invalid initial computation-thread datum`);
  }
};
