import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { submitLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ExecutionSourceScriptDecodingContracts } from "./contracts.js";
import {
  executionSourceScriptDecodingCheckpoint,
  type ExecutionSourceScriptDecodingEvidence,
  executionSourceScriptDecodingEvidenceCloses,
} from "./family.js";
import { ExecutionSourceStep05RedeemerSchema } from "./schemas.js";
import { readExecutionSourceScanState } from "./submit-step-04.js";

const FAMILY = "execution-source-script-decoding";

/**
 * Burns the thread and mints the permanent proof from whatever terminal state
 * the thread holds, with no off-chain polarity guard: step 05 itself is the
 * decisive twin (`terminal_contradiction_v1`), and a lifecycle suite drives an
 * honest verdict through it to prove the refusal happens on chain.
 */
export const submitExecutionSourceScriptDecodingStep05Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ExecutionSourceScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 4;
  const { threadUtxo, threadToken } = await readExecutionSourceScanState({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    stepIndex,
  });
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

export const submitExecutionSourceScriptDecodingStep05 = async ({
  evidence,
  ...rest
}: Parameters<typeof submitExecutionSourceScriptDecodingStep05Raw>[0] & {
  readonly evidence: ExecutionSourceScriptDecodingEvidence;
}) => {
  const { state } = await readExecutionSourceScanState({
    ...rest,
    stepIndex: 4,
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
  return await submitExecutionSourceScriptDecodingStep05Raw(rest);
};
