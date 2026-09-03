import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { UnusedRedeemerContracts } from "./contracts-v1.js";
import {
  type UnusedRedeemerEvidence,
  unusedRedeemerEvidenceCloses,
} from "./family-v1.js";
import {
  UnusedRedeemerDecisionSchema,
  UnusedRedeemerStep06DatumSchema,
  UnusedRedeemerStep06RedeemerSchema,
} from "./schemas-v1.js";

const FAMILY = "unused-redeemer";
export const submitUnusedRedeemerStep06 = async ({
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
  contracts: UnusedRedeemerContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: UnusedRedeemerEvidence;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 8;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<
    Data.Static<typeof UnusedRedeemerDecisionSchema>
  >({
    threadUtxo,
    signer,
    schema: UnusedRedeemerStep06DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    !unusedRedeemerEvidenceCloses(evidence) ||
    state.subject.transaction_id !== evidence.finding.subject.transaction_id ||
    state.redeemer_index !== BigInt(evidence.finding.redeemerIndex) ||
    state.unused !== evidence.unused
  )
    throw new Error(
      `${FAMILY}: terminal state differs from retained contradiction`,
    );
  return await submitLinearFaultFinalize({
    lucid,
    family: FAMILY,
    stepIndex,
    step: contracts.steps[stepIndex],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: UnusedRedeemerStep06RedeemerSchema,
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
