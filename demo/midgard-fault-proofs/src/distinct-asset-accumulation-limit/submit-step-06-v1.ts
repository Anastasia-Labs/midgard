import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { DistinctAssetAccumulationContractsV1 } from "./contracts-v1.js";
import type { DistinctAssetAccumulationEvidenceV1 } from "./family-v1.js";
import { distinctAssetAccumulationEvidenceClosesV1 } from "./family-v1.js";
import {
  DistinctAssetStep06DatumV1Schema,
  DistinctAssetStep06RedeemerV1Schema,
} from "./schemas-v1.js";

export const submitDistinctAssetAccumulationStep06V1 = async ({
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
  readonly contracts: DistinctAssetAccumulationContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: DistinctAssetAccumulationEvidenceV1;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!distinctAssetAccumulationEvidenceClosesV1(evidence))
    throw new Error(
      "distinctAssetAccumulationLimit: terminal evidence is honest",
    );
  const stepIndex = 5;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    stage: bigint;
    decisive_fault_holds: boolean | null;
  }>({
    threadUtxo,
    signer,
    schema: DistinctAssetStep06DatumV1Schema as never,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
  });
  if (state.stage !== 3n || state.decisive_fault_holds === null)
    throw new Error(
      "distinctAssetAccumulationLimit: terminal checkpoint changed",
    );
  return await submitLinearFaultFinalizeV1({
    lucid,
    family: "distinct-asset-accumulation-limit",
    stepIndex,
    step: contracts.steps[5],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: DistinctAssetStep06RedeemerV1Schema,
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
