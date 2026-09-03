import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { MintDeclaredAssetLimitContracts } from "./contracts.js";
import {
  type MintDeclaredAssetLimitEvidence,
  mintDeclaredAssetLimitEvidenceCloses,
} from "./family.js";
import {
  MintDeclaredAssetLimitStep04DatumSchema,
  MintDeclaredAssetLimitStep04RedeemerSchema,
} from "./schemas.js";

export const submitMintDeclaredAssetLimitStep04 = async ({
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
  readonly contracts: MintDeclaredAssetLimitContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: MintDeclaredAssetLimitEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!mintDeclaredAssetLimitEvidenceCloses(evidence))
    throw new Error("mintDeclaredAssetLimit: terminal evidence is honest");
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "mint-declared-asset-limit",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    policy_index: bigint;
    crossing: boolean;
  }>({
    threadUtxo,
    signer,
    schema: MintDeclaredAssetLimitStep04DatumSchema as never,
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
  return await submitLinearFaultFinalize({
    lucid,
    family: "mint-declared-asset-limit",
    stepIndex,
    step: contracts.steps[3],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: MintDeclaredAssetLimitStep04RedeemerSchema,
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
