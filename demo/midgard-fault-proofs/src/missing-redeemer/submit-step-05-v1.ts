import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { MissingRedeemerContracts } from "./contracts-v1.js";
import {
  type MissingRedeemerEvidence,
  missingRedeemerEvidenceCloses,
} from "./family-v1.js";
import {
  MissingRedeemerDecisionSchema,
  MissingRedeemerStep05DatumSchema,
  MissingRedeemerStep05RedeemerSchema,
} from "./schemas-v1.js";

export const submitMissingRedeemerStep05 = async ({
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
  contracts: MissingRedeemerContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: MissingRedeemerEvidence;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 6;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "missing-redeemer",
    stepIndex,
    threadOutRef,
  });
  const decision = requireLinearFaultStepState<
    Data.Static<typeof MissingRedeemerDecisionSchema>
  >({
    threadUtxo,
    signer,
    schema: MissingRedeemerStep05DatumSchema as never,
    family: "missing-redeemer",
    stepIndex,
  });
  if (
    !missingRedeemerEvidenceCloses(evidence) ||
    decision.redeemer_missing !== evidence.redeemerMissing ||
    decision.bound.purpose_kind !== BigInt(evidence.purposeKind) ||
    decision.bound.purpose_index !== BigInt(evidence.purposeIndex)
  )
    throw new Error(
      "missingRedeemer: terminal decision differs from retained contradiction",
    );
  return await submitLinearFaultFinalize({
    lucid,
    family: "missing-redeemer",
    stepIndex,
    step: contracts.steps[stepIndex],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: MissingRedeemerStep05RedeemerSchema,
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
