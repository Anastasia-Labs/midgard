import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { buildLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { MintDeclaredAssetLimitContracts } from "./contracts.js";
import {
  type MintDeclaredAssetLimitEvidence,
  mintDeclaredAssetLimitEvidenceCloses,
} from "./family.advance-mint-declared-fold.js";
import {
  MintDeclaredAssetLimitStep04DatumSchema,
  MintDeclaredAssetLimitStep04RedeemerSchema,
} from "./schemas.js";

/**
 * Finalizes the decision the thread carries, whatever its polarity. The
 * evidence-checked builder below refuses honest evidence off chain; this form
 * lets a lifecycle hand an honest decision to the validator and observe the
 * on-chain refusal.
 */
export const buildMintDeclaredAssetLimitStep04Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  policyIndex,
  crossing,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintDeclaredAssetLimitContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly policyIndex: bigint;
  readonly crossing: boolean;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
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
  if (state.policy_index !== policyIndex || state.crossing !== crossing)
    throw new Error(
      "mintDeclaredAssetLimit: terminal datum differs from evidence",
    );
  return await buildLinearFaultFinalize({
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
  });
};

export const buildMintDeclaredAssetLimitStep04 = async ({
  evidence,
  ...rest
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintDeclaredAssetLimitContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: MintDeclaredAssetLimitEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  if (!mintDeclaredAssetLimitEvidenceCloses(evidence))
    throw new Error("mintDeclaredAssetLimit: terminal evidence is honest");
  return await buildMintDeclaredAssetLimitStep04Raw({
    ...rest,
    policyIndex: evidence.policyIndex,
    crossing: evidence.crossing,
  });
};
