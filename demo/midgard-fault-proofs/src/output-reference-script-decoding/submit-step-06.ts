import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { buildLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type OutputReferenceScriptDecodingContracts,
} from "./contracts.js";
import {
  type OutputReferenceScriptDecodingEvidence,
  outputReferenceScriptEvidenceCloses,
} from "./output-reference-script-decoding.js";
import {
  OutputReferenceStep06DatumSchema,
  OutputReferenceStep06RedeemerSchema,
} from "./schemas.js";

type TerminalState = {
  readonly bound: {
    readonly subject: { readonly transaction_id: string };
    readonly output_index: bigint;
  };
  readonly result_class: bigint;
};

const readTerminalState = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: OutputReferenceScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
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
  const state = requireLinearFaultStepState<TerminalState>({
    threadUtxo,
    signer,
    schema: OutputReferenceStep06DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  return { stepIndex, threadUtxo, threadToken, state };
};

/**
 * The terminal mint exactly as the closed thread state names it, without the
 * retained-contradiction guard: the lifecycle suite uses it to show the
 * validator itself refuses to mint under an honest verdict. Production
 * callers use the guarded entry point below.
 */
export const buildOutputReferenceScriptDecodingStep06Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: OutputReferenceScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
}) => {
  const { stepIndex, threadUtxo, threadToken } = await readTerminalState({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
  });
  return await buildLinearFaultFinalize({
    lucid,
    family: FAMILY,
    stepIndex,
    step: contracts.steps[stepIndex],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: OutputReferenceStep06RedeemerSchema,
    buildFamilyArgs: (layout) => ({
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
  });
};

export const buildOutputReferenceScriptDecodingStep06 = async ({
  evidence,
  ...rest
}: Parameters<typeof buildOutputReferenceScriptDecodingStep06Raw>[0] & {
  readonly evidence: OutputReferenceScriptDecodingEvidence;
}) => {
  const { state } = await readTerminalState(rest);
  if (
    !outputReferenceScriptEvidenceCloses(evidence) ||
    state.result_class === -1n ||
    state.bound.subject.transaction_id !== evidence.subject.transaction_id ||
    state.bound.output_index !== evidence.outputIndex
  )
    throw new Error(
      `${FAMILY}: terminal state differs from retained contradiction`,
    );
  return await buildOutputReferenceScriptDecodingStep06Raw(rest);
};
