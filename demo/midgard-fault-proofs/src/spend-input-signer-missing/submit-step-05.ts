import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { SpendInputSignerMissingContracts } from "./contracts.js";
import {
  SpendInputSignerStep05DatumSchema,
  SpendInputSignerStep05RedeemerSchema,
} from "./schemas.js";
import {
  type SpendInputSignerMissingEvidence,
  spendInputSignerMissingEvidenceCloses,
} from "./spend-input-signer-missing.js";

export const submitSpendInputSignerMissingStep05 = async ({
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
  readonly contracts: SpendInputSignerMissingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: SpendInputSignerMissingEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!spendInputSignerMissingEvidenceCloses(evidence))
    throw new Error(
      "spend-input-signer-missing: terminal state does not contradict verdict",
    );
  const stepIndex = 4;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "spend-input-signer-missing",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    signer_missing: boolean;
  }>({
    threadUtxo,
    signer,
    schema: SpendInputSignerStep05DatumSchema as never,
    family: "spend-input-signer-missing",
    stepIndex,
  });
  if (state.signer_missing !== evidence.signerMissing)
    throw new Error(
      "spend-input-signer-missing: terminal signer verdict changed",
    );
  return await submitLinearFaultFinalize({
    lucid,
    family: "spend-input-signer-missing",
    stepIndex,
    step: contracts.steps[4],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: SpendInputSignerStep05RedeemerSchema,
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
