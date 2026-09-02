import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { ProtectedOutputSignerMissingContractsV1 } from "./contracts-v1.js";
import {
  protectedOutputSignerMissingEvidenceClosesV1,
  type ProtectedOutputSignerMissingEvidenceV1,
} from "./protected-output-signer-missing-v1.js";
import {
  ProtectedOutputSignerStep05DatumV1Schema,
  ProtectedOutputSignerStep05RedeemerV1Schema,
} from "./schemas-v1.js";

export const submitProtectedOutputSignerMissingStep05V1 = async ({
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
  readonly contracts: ProtectedOutputSignerMissingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ProtectedOutputSignerMissingEvidenceV1;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!protectedOutputSignerMissingEvidenceClosesV1(evidence))
    throw new Error(
      "protected-output-signer-missing: terminal verdict does not contradict the block",
    );
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "protected-output-signer-missing",
    stepIndex: 4,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    subject: unknown;
    signer_present: boolean;
  }>({
    threadUtxo,
    signer,
    schema: ProtectedOutputSignerStep05DatumV1Schema as never,
    family: "protected-output-signer-missing",
    stepIndex: 4,
  });
  if (state.signer_present !== evidence.signerPresent)
    throw new Error(
      "protected-output-signer-missing: terminal signer result changed",
    );
  return await submitLinearFaultFinalizeV1({
    lucid,
    family: "protected-output-signer-missing",
    stepIndex: 4,
    step: contracts.steps[4],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ProtectedOutputSignerStep05RedeemerV1Schema,
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
