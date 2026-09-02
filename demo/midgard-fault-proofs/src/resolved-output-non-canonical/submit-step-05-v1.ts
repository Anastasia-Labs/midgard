import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { ResolvedOutputNonCanonicalContractsV1 } from "./contracts-v1.js";
import {
  resolvedOutputEvidenceClosesV1,
  type ResolvedOutputEvidenceV1,
} from "./resolved-output-non-canonical-v1.js";
import {
  ResolvedOutputStep05DatumV1Schema,
  ResolvedOutputStep05RedeemerV1Schema,
} from "./schemas-v1.js";

export const submitResolvedOutputNonCanonicalStep05V1 = async ({
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
  readonly contracts: ResolvedOutputNonCanonicalContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ResolvedOutputEvidenceV1;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!resolvedOutputEvidenceClosesV1(evidence))
    throw new Error(
      "resolved-output-non-canonical: terminal state does not contradict verdict",
    );
  const stepIndex = 4;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "resolved-output-non-canonical",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    subject: unknown;
    output_is_non_canonical: boolean;
  }>({
    threadUtxo,
    signer,
    schema: ResolvedOutputStep05DatumV1Schema as never,
    family: "resolved-output-non-canonical",
    stepIndex,
  });
  if (state.output_is_non_canonical !== evidence.outputIsNonCanonical)
    throw new Error("resolved-output-non-canonical: terminal verdict changed");
  return await submitLinearFaultFinalizeV1({
    lucid,
    family: "resolved-output-non-canonical",
    stepIndex,
    step: contracts.steps[4],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ResolvedOutputStep05RedeemerV1Schema,
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
