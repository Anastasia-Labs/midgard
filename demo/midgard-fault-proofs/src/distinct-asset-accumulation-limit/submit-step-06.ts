import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { DistinctAssetAccumulationContracts } from "./contracts.js";
import type { DistinctAssetAccumulationEvidence } from "./family.js";
import { distinctAssetAccumulationEvidenceCloses } from "./family.js";
import {
  DistinctAssetStep06DatumSchema,
  DistinctAssetStep06RedeemerSchema,
} from "./schemas.js";

export const submitDistinctAssetAccumulationStep06 = async ({
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
  readonly contracts: DistinctAssetAccumulationContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: DistinctAssetAccumulationEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!distinctAssetAccumulationEvidenceCloses(evidence))
    throw new Error(
      "distinctAssetAccumulationLimit: terminal evidence is honest",
    );
  const stepIndex = 5;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    stage: bigint;
    decisive_fault_holds: boolean | null;
  }>({
    threadUtxo,
    signer,
    schema: DistinctAssetStep06DatumSchema as never,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
  });
  if (state.stage !== 3n || state.decisive_fault_holds === null)
    throw new Error(
      "distinctAssetAccumulationLimit: terminal checkpoint changed",
    );
  return await submitLinearFaultFinalize({
    lucid,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
    step: contracts.steps[5],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: DistinctAssetStep06RedeemerSchema,
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
