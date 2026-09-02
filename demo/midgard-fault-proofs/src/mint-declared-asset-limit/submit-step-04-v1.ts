import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { MintDeclaredAssetLimitContractsV1 } from "./contracts-v1.js";
import {
  mintDeclaredAssetLimitEvidenceClosesV1,
  type MintDeclaredAssetLimitEvidenceV1,
} from "./family-v1.js";
import {
  MintDeclaredAssetLimitStep04DatumV1Schema,
  MintDeclaredAssetLimitStep04RedeemerV1Schema,
} from "./schemas-v1.js";

export const submitMintDeclaredAssetLimitStep04V1 = async ({
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
  readonly contracts: MintDeclaredAssetLimitContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: MintDeclaredAssetLimitEvidenceV1;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!mintDeclaredAssetLimitEvidenceClosesV1(evidence))
    throw new Error("mintDeclaredAssetLimit: terminal evidence is honest");
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "mint-declared-asset-limit",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    subject: unknown;
    policy_index: bigint;
    crossing: boolean;
  }>({
    threadUtxo,
    signer,
    schema: MintDeclaredAssetLimitStep04DatumV1Schema as never,
    family: "mint-declared-asset-limit",
    stepIndex,
  });
  if (
    state.policy_index !== BigInt(evidence.policyIndex) ||
    state.crossing !== evidence.crossing
  )
    throw new Error(
      "mintDeclaredAssetLimit: terminal datum differs from evidence",
    );
  return await submitLinearFaultFinalizeV1({
    lucid,
    family: "mint-declared-asset-limit",
    stepIndex,
    step: contracts.steps[3],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: MintDeclaredAssetLimitStep04RedeemerV1Schema,
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
