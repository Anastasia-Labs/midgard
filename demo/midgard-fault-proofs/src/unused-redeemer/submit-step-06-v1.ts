import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { UnusedRedeemerContractsV1 } from "./contracts-v1.js";
import {
  unusedRedeemerEvidenceClosesV1,
  type UnusedRedeemerEvidenceV1,
} from "./family-v1.js";
import {
  UnusedRedeemerDecisionV1Schema,
  UnusedRedeemerStep06DatumV1Schema,
  UnusedRedeemerStep06RedeemerV1Schema,
} from "./schemas-v1.js";

const FAMILY = "unused-redeemer";
export const submitUnusedRedeemerStep06V1 = async ({
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
  contracts: UnusedRedeemerContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: UnusedRedeemerEvidenceV1;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 8;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<
    Data.Static<typeof UnusedRedeemerDecisionV1Schema>
  >({
    threadUtxo,
    signer,
    schema: UnusedRedeemerStep06DatumV1Schema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    !unusedRedeemerEvidenceClosesV1(evidence) ||
    state.subject.transaction_id !== evidence.finding.subject.transaction_id ||
    state.redeemer_index !== BigInt(evidence.finding.redeemerIndex) ||
    state.unused !== evidence.unused
  )
    throw new Error(
      `${FAMILY}: terminal state differs from retained contradiction`,
    );
  return await submitLinearFaultFinalizeV1({
    lucid,
    family: FAMILY,
    stepIndex,
    step: contracts.steps[stepIndex],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: UnusedRedeemerStep06RedeemerV1Schema,
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
