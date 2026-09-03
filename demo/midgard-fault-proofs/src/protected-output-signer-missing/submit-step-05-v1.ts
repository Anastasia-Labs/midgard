import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { ProtectedOutputSignerMissingContracts } from "./contracts-v1.js";
import {
  type ProtectedOutputSignerMissingEvidence,
  protectedOutputSignerMissingEvidenceCloses,
} from "./protected-output-signer-missing-v1.js";
import {
  ProtectedOutputSignerStep05DatumSchema,
  ProtectedOutputSignerStep05RedeemerSchema,
} from "./schemas-v1.js";

export const submitProtectedOutputSignerMissingStep05 = async ({
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
  readonly contracts: ProtectedOutputSignerMissingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ProtectedOutputSignerMissingEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!protectedOutputSignerMissingEvidenceCloses(evidence))
    throw new Error(
      "protected-output-signer-missing: terminal verdict does not contradict the block",
    );
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "protected-output-signer-missing",
    stepIndex: 4,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    signer_present: boolean;
  }>({
    threadUtxo,
    signer,
    schema: ProtectedOutputSignerStep05DatumSchema as never,
    family: "protected-output-signer-missing",
    stepIndex: 4,
  });
  if (state.signer_present !== evidence.signerPresent)
    throw new Error(
      "protected-output-signer-missing: terminal signer result changed",
    );
  return await submitLinearFaultFinalize({
    lucid,
    family: "protected-output-signer-missing",
    stepIndex: 4,
    step: contracts.steps[4],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ProtectedOutputSignerStep05RedeemerSchema,
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
