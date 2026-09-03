import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { UnusedScriptWitnessContracts } from "./contracts-v1.js";
import {
  type UnusedScriptWitnessEvidence,
  unusedScriptWitnessEvidenceCloses,
} from "./family-v1.js";
import {
  UnusedScriptDecisionSchema,
  UnusedScriptStep06DatumSchema,
  UnusedScriptStep06RedeemerSchema,
} from "./schemas-v1.js";

const FAMILY = "unused-script-witness";
export const submitUnusedScriptWitnessStep06 = async ({
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
  contracts: UnusedScriptWitnessContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: UnusedScriptWitnessEvidence;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 5;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<
    Data.Static<typeof UnusedScriptDecisionSchema>
  >({
    threadUtxo,
    signer,
    schema: UnusedScriptStep06DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    !unusedScriptWitnessEvidenceCloses(evidence) ||
    state.subject.transaction_id !== evidence.finding.subject.transaction_id ||
    state.script_index !== BigInt(evidence.finding.scriptIndex) ||
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
    spendRedeemerSchema: UnusedScriptStep06RedeemerSchema,
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
