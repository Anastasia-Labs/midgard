import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { MissingRedeemerContractsV1 } from "./contracts-v1.js";
import {
  missingRedeemerEvidenceClosesV1,
  type MissingRedeemerEvidenceV1,
} from "./family-v1.js";
import {
  MissingRedeemerDecisionV1Schema,
  MissingRedeemerStep05DatumV1Schema,
  MissingRedeemerStep05RedeemerV1Schema,
} from "./schemas-v1.js";

export const submitMissingRedeemerStep05V1 = async ({
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
  lucid: LucidEvolution;
  contracts: MissingRedeemerContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: MissingRedeemerEvidenceV1;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 6;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "missing-redeemer",
    stepIndex,
    threadOutRef,
  });
  const decision = requireLinearFaultStepStateV1<
    Data.Static<typeof MissingRedeemerDecisionV1Schema>
  >({
    threadUtxo,
    signer,
    schema: MissingRedeemerStep05DatumV1Schema as never,
    family: "missing-redeemer",
    stepIndex,
  });
  if (
    !missingRedeemerEvidenceClosesV1(evidence) ||
    decision.redeemer_missing !== evidence.redeemerMissing ||
    decision.bound.purpose_kind !== BigInt(evidence.purposeKind) ||
    decision.bound.purpose_index !== BigInt(evidence.purposeIndex)
  )
    throw new Error(
      "missingRedeemer: terminal decision differs from retained contradiction",
    );
  return await submitLinearFaultFinalizeV1({
    lucid,
    family: "missing-redeemer",
    stepIndex,
    step: contracts.steps[stepIndex],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: MissingRedeemerStep05RedeemerV1Schema,
    buildFamilyArgs: (layout) => ({
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
