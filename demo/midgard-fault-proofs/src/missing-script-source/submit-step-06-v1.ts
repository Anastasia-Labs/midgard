import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { MissingScriptSourceContracts } from "./contracts-v1.js";
import {
  type MissingScriptSourceEvidence,
  missingScriptSourceEvidenceCloses,
} from "./family-v1.js";
import {
  ExecutionSourceScanStateSchema,
  ExecutionSourceStep06DatumSchema,
  ExecutionSourceStep06RedeemerSchema,
} from "./schemas-v1.js";
import { missingScriptSourceOnchainCheckpoint } from "./universe-scan-v1.js";

const FAMILY = "missing-script-source";
export const submitMissingScriptSourceStep06 = async ({
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
  contracts: MissingScriptSourceContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: MissingScriptSourceEvidence;
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
    Data.Static<typeof ExecutionSourceScanStateSchema>
  >({
    threadUtxo,
    signer,
    schema: ExecutionSourceStep06DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    !missingScriptSourceEvidenceCloses(evidence) ||
    state.cursor !== state.authenticated.purpose.scan_limit ||
    state.cursor !== BigInt(evidence.sources.length) ||
    state.found !== (evidence.foundAtSourceIndex !== null) ||
    state.checkpoint_hash !==
      missingScriptSourceOnchainCheckpoint({
        sourceIdentityHex: state.authenticated.source_identity_hash,
        cursor: state.cursor,
        found: state.found,
        nextExpectedScriptHashHex: state.next_expected_script_hash,
      })
  )
    throw new Error(
      `${FAMILY}: terminal state is not the retained contradiction`,
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
    spendRedeemerSchema: ExecutionSourceStep06RedeemerSchema,
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
