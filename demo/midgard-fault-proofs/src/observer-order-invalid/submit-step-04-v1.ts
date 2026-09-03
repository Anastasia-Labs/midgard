import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { ObserverOrderInvalidContracts } from "./contracts-v1.js";
import {
  type ObserverOrderInvalidEvidence,
  observerOrderInvalidEvidenceCloses,
} from "./family-v1.js";
import {
  ObserverOrderInvalidStep04DatumSchema,
  ObserverOrderInvalidStep04RedeemerSchema,
} from "./schemas-v1.js";

export const submitObserverOrderInvalidStep04 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserverOrderInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ObserverOrderInvalidEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!observerOrderInvalidEvidenceCloses(evidence))
    throw new Error("observerOrderInvalid: terminal evidence is honest");
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "observer-order-invalid",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    observer_index: bigint;
    violation: boolean;
  }>({
    threadUtxo,
    signer,
    schema: ObserverOrderInvalidStep04DatumSchema as never,
    family: "observer-order-invalid",
    stepIndex,
  });
  if (
    state.observer_index !== BigInt(evidence.observerIndex) ||
    state.violation !== evidence.violation
  )
    throw new Error(
      "observerOrderInvalid: terminal datum differs from evidence",
    );
  return await submitLinearFaultFinalize({
    lucid,
    family: "observer-order-invalid",
    stepIndex,
    step: contracts.steps[3],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ObserverOrderInvalidStep04RedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
