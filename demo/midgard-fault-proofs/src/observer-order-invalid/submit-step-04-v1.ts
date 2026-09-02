import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { ObserverOrderInvalidContractsV1 } from "./contracts-v1.js";
import {
  observerOrderInvalidEvidenceClosesV1,
  type ObserverOrderInvalidEvidenceV1,
} from "./family-v1.js";
import {
  ObserverOrderInvalidStep04DatumV1Schema,
  ObserverOrderInvalidStep04RedeemerV1Schema,
} from "./schemas-v1.js";

export const submitObserverOrderInvalidStep04V1 = async ({
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
  readonly contracts: ObserverOrderInvalidContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ObserverOrderInvalidEvidenceV1;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!observerOrderInvalidEvidenceClosesV1(evidence))
    throw new Error("observerOrderInvalid: terminal evidence is honest");
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "observer-order-invalid",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    subject: unknown;
    observer_index: bigint;
    violation: boolean;
  }>({
    threadUtxo,
    signer,
    schema: ObserverOrderInvalidStep04DatumV1Schema as never,
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
  return await submitLinearFaultFinalizeV1({
    lucid,
    family: "observer-order-invalid",
    stepIndex,
    step: contracts.steps[3],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ObserverOrderInvalidStep04RedeemerV1Schema,
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
