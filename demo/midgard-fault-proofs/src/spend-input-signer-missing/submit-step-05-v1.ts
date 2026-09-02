import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { SpendInputSignerMissingContractsV1 } from "./contracts-v1.js";
import {
  SpendInputSignerStep05DatumV1Schema,
  SpendInputSignerStep05RedeemerV1Schema,
} from "./schemas-v1.js";
import {
  spendInputSignerMissingEvidenceClosesV1,
  type SpendInputSignerMissingEvidenceV1,
} from "./spend-input-signer-missing-v1.js";

export const submitSpendInputSignerMissingStep05V1 = async ({
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
  readonly contracts: SpendInputSignerMissingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: SpendInputSignerMissingEvidenceV1;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!spendInputSignerMissingEvidenceClosesV1(evidence))
    throw new Error(
      "spend-input-signer-missing: terminal state does not contradict verdict",
    );
  const stepIndex = 4;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "spend-input-signer-missing",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    subject: unknown;
    signer_missing: boolean;
  }>({
    threadUtxo,
    signer,
    schema: SpendInputSignerStep05DatumV1Schema as never,
    family: "spend-input-signer-missing",
    stepIndex,
  });
  if (state.signer_missing !== evidence.signerMissing)
    throw new Error(
      "spend-input-signer-missing: terminal signer verdict changed",
    );
  return await submitLinearFaultFinalizeV1({
    lucid,
    family: "spend-input-signer-missing",
    stepIndex,
    step: contracts.steps[4],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: SpendInputSignerStep05RedeemerV1Schema,
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
