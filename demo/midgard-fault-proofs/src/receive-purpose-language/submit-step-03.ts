import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ReceivePurposeLanguageContracts } from "./contracts.js";
import {
  type ReceivePurposeLanguageEvidence,
  receivePurposeLanguageEvidenceCloses,
} from "./family.js";
import {
  AuthenticatedReceiveLanguageSchema,
  ReceivePurposeStep03DatumSchema,
  ReceivePurposeStep03RedeemerSchema,
} from "./schemas.js";

export const submitReceivePurposeLanguageStep03 = async ({
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
  contracts: ReceivePurposeLanguageContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: ReceivePurposeLanguageEvidence;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  const family = "receive-purpose-language";
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<
    Data.Static<typeof AuthenticatedReceiveLanguageSchema>
  >({
    threadUtxo,
    signer,
    schema: ReceivePurposeStep03DatumSchema as never,
    family,
    stepIndex,
  });
  if (
    !receivePurposeLanguageEvidenceCloses(evidence) ||
    state.bound.execution_index !== BigInt(evidence.finding.executionIndex) ||
    state.language_tag !== BigInt(evidence.descriptor.languageTag) ||
    state.purpose_kind !== 3n
  )
    throw new Error(
      `${family}: terminal state is not the retained contradiction`,
    );
  return await submitLinearFaultFinalize({
    lucid,
    family,
    stepIndex,
    step: contracts.steps[stepIndex],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ReceivePurposeStep03RedeemerSchema,
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
