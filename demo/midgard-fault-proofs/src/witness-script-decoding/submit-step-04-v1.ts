import {
  type WitnessScriptDecodingScanStateV1,
  WitnessScriptDecodingStep04DatumV1Schema,
  WitnessScriptDecodingStep04RedeemerV1Schema,
} from "@al-ft/midgard-sdk";
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
  WITNESS_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type WitnessScriptDecodingContractsV1,
} from "./contracts-v1.js";
import {
  witnessScriptDecodingCheckpointV1,
  witnessScriptDecodingEvidenceClosesV1,
  type WitnessScriptDecodingEvidenceV1,
} from "./witness-script-decoding-v1.js";

export const submitWitnessScriptDecodingStep04V1 = async ({
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
  readonly contracts: WitnessScriptDecodingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: WitnessScriptDecodingEvidenceV1;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<WitnessScriptDecodingScanStateV1>(
    {
      threadUtxo,
      signer,
      schema: WitnessScriptDecodingStep04DatumV1Schema as never,
      family: FAMILY,
      stepIndex,
    },
  );
  if (
    !witnessScriptDecodingEvidenceClosesV1(evidence) ||
    state.result_class === -1n ||
    state.checkpoint_hash !==
      witnessScriptDecodingCheckpointV1({
        evidence,
        controlCbor: state.control_cbor,
        nextExpectedScriptHash: state.next_expected_script_hash,
      })
  ) {
    throw new Error(
      `${FAMILY}: terminal state is not the retained contradiction`,
    );
  }
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
    spendRedeemerSchema: WitnessScriptDecodingStep04RedeemerV1Schema,
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
