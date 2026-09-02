import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import {
  OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type OutputReferenceScriptDecodingContractsV1,
} from "./contracts-v1.js";
import {
  type OutputReferenceScriptDecodingEvidenceV1,
  outputReferenceScriptEvidenceClosesV1,
} from "./output-reference-script-decoding-v1.js";
import {
  OutputReferenceStep06DatumV1Schema,
  OutputReferenceStep06RedeemerV1Schema,
} from "./schemas-v1.js";

export const submitOutputReferenceScriptDecodingStep06V1 = async ({
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
  readonly lucid: LucidEvolution;
  readonly contracts: OutputReferenceScriptDecodingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: OutputReferenceScriptDecodingEvidenceV1;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 5;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    readonly bound: {
      readonly subject: { readonly transaction_id: string };
      readonly output_index: bigint;
    };
    readonly result_class: bigint;
  }>({
    threadUtxo,
    signer,
    schema: OutputReferenceStep06DatumV1Schema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    !outputReferenceScriptEvidenceClosesV1(evidence) ||
    state.result_class === -1n ||
    state.bound.subject.transaction_id !== evidence.subject.transaction_id ||
    state.bound.output_index !== BigInt(evidence.outputIndex)
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
    spendRedeemerSchema: OutputReferenceStep06RedeemerV1Schema,
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
