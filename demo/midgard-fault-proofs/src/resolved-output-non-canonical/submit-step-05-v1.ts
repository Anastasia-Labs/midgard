import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { ResolvedOutputNonCanonicalContracts } from "./contracts-v1.js";
import {
  type ResolvedOutputEvidence,
  resolvedOutputEvidenceCloses,
} from "./resolved-output-non-canonical-v1.js";
import {
  ResolvedOutputStep05DatumSchema,
  ResolvedOutputStep05RedeemerSchema,
} from "./schemas-v1.js";

export const submitResolvedOutputNonCanonicalStep05 = async ({
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
  readonly contracts: ResolvedOutputNonCanonicalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ResolvedOutputEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!resolvedOutputEvidenceCloses(evidence))
    throw new Error(
      "resolved-output-non-canonical: terminal state does not contradict verdict",
    );
  const stepIndex = 4;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "resolved-output-non-canonical",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    output_is_non_canonical: boolean;
  }>({
    threadUtxo,
    signer,
    schema: ResolvedOutputStep05DatumSchema as never,
    family: "resolved-output-non-canonical",
    stepIndex,
  });
  if (state.output_is_non_canonical !== evidence.outputIsNonCanonical)
    throw new Error("resolved-output-non-canonical: terminal verdict changed");
  return await submitLinearFaultFinalize({
    lucid,
    family: "resolved-output-non-canonical",
    stepIndex,
    step: contracts.steps[4],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ResolvedOutputStep05RedeemerSchema,
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
