import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { RedeemerCanonicityContractsV1 } from "./contracts-v1.js";
import {
  redeemerCanonicityEvidenceClosesV1,
  type RedeemerCanonicityEvidenceV1,
} from "./family-v1.js";
import {
  RedeemerCanonicityStep03DatumV1Schema,
  RedeemerCanonicityStep03RedeemerV1Schema,
} from "./schemas-v1.js";

export const submitRedeemerCanonicityStep03V1 = async (input: {
  readonly lucid: LucidEvolution;
  readonly contracts: RedeemerCanonicityContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: RedeemerCanonicityEvidenceV1;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!redeemerCanonicityEvidenceClosesV1(input.evidence))
    throw new Error(
      "redeemer-canonicity: terminal state does not contradict verdict",
    );
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid: input.lucid,
    contracts: input.contracts,
    categoryId: input.categoryId,
    family: "redeemer-canonicity",
    stepIndex,
    threadOutRef: input.threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    bound: { redeemer_index: bigint };
    canonical: boolean;
  }>({
    threadUtxo,
    signer: input.signer,
    schema: RedeemerCanonicityStep03DatumV1Schema as never,
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
  return await submitLinearFaultFinalizeV1({
    lucid: input.lucid,
    family: "redeemer-canonicity",
    stepIndex,
    step: input.contracts.steps[2],
    computationThread: input.contracts.computationThread,
    fraudProof: input.contracts.fraudProof,
    signer: input.signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: RedeemerCanonicityStep03RedeemerV1Schema,
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
