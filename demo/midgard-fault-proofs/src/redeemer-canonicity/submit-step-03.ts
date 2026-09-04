import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { RedeemerCanonicityContracts } from "./contracts.js";
import {
  type RedeemerCanonicityEvidence,
  redeemerCanonicityEvidenceCloses,
} from "./family.js";
import {
  RedeemerCanonicityStep03DatumSchema,
  RedeemerCanonicityStep03RedeemerSchema,
} from "./schemas.js";

export const submitRedeemerCanonicityStep03 = async (input: {
  readonly lucid: LucidEvolution;
  readonly contracts: RedeemerCanonicityContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: RedeemerCanonicityEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!redeemerCanonicityEvidenceCloses(input.evidence))
    throw new Error(
      "redeemer-canonicity: terminal state does not contradict verdict",
    );
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid: input.lucid,
    contracts: input.contracts,
    categoryId: input.categoryId,
    family: "redeemer-canonicity",
    stepIndex,
    threadOutRef: input.threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    bound: { redeemer_index: bigint };
    canonical: boolean;
  }>({
    threadUtxo,
    signer: input.signer,
    schema: RedeemerCanonicityStep03DatumSchema as never,
    family: "redeemer-canonicity",
    stepIndex,
  });
  if (
    state.bound.redeemer_index !== BigInt(input.evidence.redeemerIndex) ||
    state.canonical !== input.evidence.canonical
  )
    throw new Error(
      "redeemer-canonicity: terminal state differs from evidence",
    );
  return await submitLinearFaultFinalize({
    lucid: input.lucid,
    family: "redeemer-canonicity",
    stepIndex,
    step: input.contracts.steps[2],
    computationThread: input.contracts.computationThread,
    fraudProof: input.contracts.fraudProof,
    signer: input.signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: RedeemerCanonicityStep03RedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo: input.referenceScriptUtxo,
    witnessReferenceScripts: input.witnessReferenceScripts,
    preSubmitBoundary: input.preSubmitBoundary,
    awaitConfirmation: input.awaitConfirmation ?? true,
  });
};
