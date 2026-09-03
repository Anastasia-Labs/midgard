import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { ExecutionSourceScriptDecodingContracts } from "./contracts-v1.js";
import {
  executionSourceScriptDecodingCheckpoint,
  type ExecutionSourceScriptDecodingEvidence,
  executionSourceScriptDecodingEvidenceCloses,
} from "./family-v1.js";
import {
  ExecutionSourceScanStateSchema,
  ExecutionSourceStep05DatumSchema,
  ExecutionSourceStep05RedeemerSchema,
} from "./schemas-v1.js";

const FAMILY = "execution-source-script-decoding";
export const submitExecutionSourceScriptDecodingStep05 = async ({
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
  contracts: ExecutionSourceScriptDecodingContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: ExecutionSourceScriptDecodingEvidence;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 4;
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
    schema: ExecutionSourceStep05DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    !executionSourceScriptDecodingEvidenceCloses(evidence) ||
    state.result_class === -1n ||
    state.checkpoint_hash !==
      executionSourceScriptDecodingCheckpoint({
        evidence,
        controlCbor: state.control_cbor,
        nextExpectedScriptHash: state.next_expected_script_hash,
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
    spendRedeemerSchema: ExecutionSourceStep05RedeemerSchema,
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
