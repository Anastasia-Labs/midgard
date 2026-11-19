import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { buildLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FieldItemWidthIllegalContracts } from "./contracts.js";
import {
  encodeFieldItemWidthAuthenticatedItem,
  FieldItemWidthAuthenticatedItem,
  type FieldItemWidthEvidence,
  fieldItemWidthEvidenceCloses,
} from "./field-item-width-illegal.js";
import {
  FieldItemWidthStep03DatumSchema,
  FieldItemWidthStep03RedeemerSchema,
} from "./schemas.js";

export const buildFieldItemWidthIllegalStep03 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: FieldItemWidthIllegalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: FieldItemWidthEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  if (!fieldItemWidthEvidenceCloses(evidence)) {
    throw new Error(
      "field-item-width-illegal: authenticated width does not contradict verdict",
    );
  }
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "field-item-width-illegal",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<FieldItemWidthAuthenticatedItem>({
    threadUtxo,
    signer,
    schema: FieldItemWidthStep03DatumSchema as never,
    family: "field-item-width-illegal",
    stepIndex,
  });
  if (
    Data.to(state, FieldItemWidthAuthenticatedItem) !==
    encodeFieldItemWidthAuthenticatedItem(evidence)
  ) {
    throw new Error(
      "field-item-width-illegal: terminal state differs from prepared evidence",
    );
  }
  return await buildLinearFaultFinalize({
    lucid,
    family: "field-item-width-illegal",
    stepIndex,
    step: contracts.steps[2],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: FieldItemWidthStep03RedeemerSchema,
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
  });
};
