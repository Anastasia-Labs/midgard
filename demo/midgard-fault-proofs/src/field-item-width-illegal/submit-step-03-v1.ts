import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { FieldItemWidthIllegalContractsV1 } from "./contracts-v1.js";
import {
  fieldItemWidthEvidenceClosesV1,
  type FieldItemWidthEvidenceV1,
} from "./field-item-width-illegal-v1.js";
import {
  FieldItemWidthStep03DatumV1Schema,
  FieldItemWidthStep03RedeemerV1Schema,
} from "./schemas-v1.js";

export const submitFieldItemWidthIllegalStep03V1 = async ({
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
  readonly contracts: FieldItemWidthIllegalContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: FieldItemWidthEvidenceV1;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!fieldItemWidthEvidenceClosesV1(evidence)) {
    throw new Error(
      "field-item-width-illegal: authenticated width does not contradict verdict",
    );
  }
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "field-item-width-illegal",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    subject: unknown;
    field_index: bigint;
    item_index: bigint;
    item_width: bigint;
  }>({
    threadUtxo,
    signer,
    schema: FieldItemWidthStep03DatumV1Schema as never,
    family: "field-item-width-illegal",
    stepIndex,
  });
  if (
    state.field_index !== BigInt(evidence.fieldIndex) ||
    state.item_index !== BigInt(evidence.itemIndex) ||
    state.item_width !== BigInt(evidence.itemWidth)
  ) {
    throw new Error(
      "field-item-width-illegal: terminal state differs from prepared evidence",
    );
  }
  return await submitLinearFaultFinalizeV1({
    lucid,
    family: "field-item-width-illegal",
    stepIndex,
    step: contracts.steps[2],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: FieldItemWidthStep03RedeemerV1Schema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
