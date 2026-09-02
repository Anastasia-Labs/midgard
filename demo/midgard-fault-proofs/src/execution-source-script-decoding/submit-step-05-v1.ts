import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { ExecutionSourceScriptDecodingContractsV1 } from "./contracts-v1.js";
import {
  executionSourceScriptDecodingCheckpointV1,
  executionSourceScriptDecodingEvidenceClosesV1,
  type ExecutionSourceScriptDecodingEvidenceV1,
} from "./family-v1.js";
import {
  ExecutionSourceScanStateV1Schema,
  ExecutionSourceStep05DatumV1Schema,
  ExecutionSourceStep05RedeemerV1Schema,
} from "./schemas-v1.js";

const FAMILY = "execution-source-script-decoding";
export const submitExecutionSourceScriptDecodingStep05V1 = async ({
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
  contracts: ExecutionSourceScriptDecodingContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: ExecutionSourceScriptDecodingEvidenceV1;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 4;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<
    Data.Static<typeof ExecutionSourceScanStateV1Schema>
  >({
    threadUtxo,
    signer,
    schema: ExecutionSourceStep05DatumV1Schema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    !executionSourceScriptDecodingEvidenceClosesV1(evidence) ||
    state.result_class === -1n ||
    state.checkpoint_hash !==
      executionSourceScriptDecodingCheckpointV1({
        evidence,
        controlCbor: state.control_cbor,
        nextExpectedScriptHash: state.next_expected_script_hash,
      })
  )
    throw new Error(
      `${FAMILY}: terminal state is not the retained contradiction`,
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
    spendRedeemerSchema: ExecutionSourceStep05RedeemerV1Schema,
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
