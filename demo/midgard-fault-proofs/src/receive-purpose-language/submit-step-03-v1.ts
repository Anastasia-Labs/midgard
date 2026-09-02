import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { ReceivePurposeLanguageContractsV1 } from "./contracts-v1.js";
import {
  receivePurposeLanguageEvidenceClosesV1,
  type ReceivePurposeLanguageEvidenceV1,
} from "./family-v1.js";
import {
  AuthenticatedReceiveLanguageV1Schema,
  ReceivePurposeStep03DatumV1Schema,
  ReceivePurposeStep03RedeemerV1Schema,
} from "./schemas-v1.js";

export const submitReceivePurposeLanguageStep03V1 = async ({
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
  contracts: ReceivePurposeLanguageContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: ReceivePurposeLanguageEvidenceV1;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}) => {
  const family = "receive-purpose-language";
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<
    Data.Static<typeof AuthenticatedReceiveLanguageV1Schema>
  >({
    threadUtxo,
    signer,
    schema: ReceivePurposeStep03DatumV1Schema as never,
    family,
    stepIndex,
  });
  if (
    !receivePurposeLanguageEvidenceClosesV1(evidence) ||
    state.bound.execution_index !== BigInt(evidence.finding.executionIndex) ||
    state.language_tag !== BigInt(evidence.descriptor.languageTag) ||
    state.purpose_kind !== 3n
  )
    throw new Error(
      `${family}: terminal state is not the retained contradiction`,
    );
  return await submitLinearFaultFinalizeV1({
    lucid,
    family,
    stepIndex,
    step: contracts.steps[stepIndex],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ReceivePurposeStep03RedeemerV1Schema,
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
