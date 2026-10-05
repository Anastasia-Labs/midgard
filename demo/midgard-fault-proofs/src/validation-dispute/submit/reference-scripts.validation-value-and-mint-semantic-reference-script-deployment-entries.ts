import {
  type LucidEvolution,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { type ContractDeploymentInfo } from "../../inspect-contracts.js";
import { fetchUtxoByOutRef, outRefLabel } from "../../runtime.js";
import { validationCekSemanticReferenceScriptDeploymentEntry } from "./reference-scripts.require-validation-dispute-reference-script.js";

export const requireValidationCekSemanticReferenceScriptOutRef = ({
  deploymentInfo,
  semanticResolverIndex,
  expectedScriptHash,
}: {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly semanticResolverIndex: number;
  readonly expectedScriptHash: string;
}): { readonly txHash: string; readonly outputIndex: number } => {
  const entryName = validationCekSemanticReferenceScriptDeploymentEntry(
    semanticResolverIndex,
  );
  if (entryName === undefined) {
    throw new Error(
      `CEK semantic resolver ${semanticResolverIndex.toString()} is not published by reference`,
    );
  }
  const entry = deploymentInfo[entryName];
  if (entry === undefined) {
    throw new Error(
      `Deployment info is missing "${entryName}"; publish the V1 CEK semantic-resolver reference script and regenerate deployment info before submitting a CEK semantic resolution`,
    );
  }
  if (entry.refScriptUTxO == null) {
    throw new Error(
      `Deployment info entry "${entryName}" is missing refScriptUTxO; publish the V1 CEK semantic-resolver reference script and regenerate deployment info before submitting a CEK semantic resolution`,
    );
  }
  if (entry.scriptHash !== expectedScriptHash) {
    throw new Error(
      `Deployment entry "${entryName}" script hash mismatch: deployment=${entry.scriptHash}, derived=${expectedScriptHash}`,
    );
  }
  return entry.refScriptUTxO;
};

export const requireValidationCekSemanticReferenceScriptUtxo = async ({
  lucid,
  deploymentInfo,
  semanticResolverIndex,
  expectedScriptHash,
}: {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly semanticResolverIndex: number;
  readonly expectedScriptHash: string;
}): Promise<UTxO> => {
  const outRef = requireValidationCekSemanticReferenceScriptOutRef({
    deploymentInfo,
    semanticResolverIndex,
    expectedScriptHash,
  });
  const utxo = await fetchUtxoByOutRef({
    lucid,
    outRef,
    label: "CEK semantic-resolver reference-script UTxO",
  });
  if (utxo.scriptRef == null) {
    throw new Error(
      `CEK semantic-resolver reference UTxO ${outRefLabel(utxo)} does not carry a reference script`,
    );
  }
  const actualScriptHash = validatorToScriptHash(utxo.scriptRef);
  if (actualScriptHash !== expectedScriptHash) {
    throw new Error(
      `CEK semantic-resolver reference script hash mismatch: actual=${actualScriptHash}, expected=${expectedScriptHash}`,
    );
  }
  return utxo;
};

/**
 * Deployment-info entries that publish the applied ValueAndMint semantic
 * resolvers (#634). The ValueAndMint decomposition is the CEK decomposition's
 * sibling — one `value_and_mint_v1` prepare validator over eleven per-kind
 * semantic resolvers — but only the CEK side ever had the reference-script
 * deployment role, so every ValueAndMint semantic attached inline. Eight of
 * the eleven applied bodies exceed `MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES`
 * on their own, before any redeemer. Applied bodies measured in THIS tree
 * (#634, 2026-08-23) by resolving each semantic through
 * `resolveValidationTraceDisputeDeploymentContracts` — the same route the
 * resolution hash-checks — against a blueprint built from this tree's
 * `onchain/`, and taking the serialized script length: replay_input 21,367,
 * replay_asset 22,046, replay_finish 21,138, output_descriptor 21,207,
 * output_asset 21,823, output_finish 20,987, mint_asset 18,622 and
 * mint_finish 17,931 bytes; begin (11,545), replay_begin (11,085) and finalize
 * (12,059) fit. (Parameter application adds 72-73 bytes over the unapplied
 * blueprint body in every case; figures predating #618 were unapplied sizes
 * mislabelled as applied and have been dropped.) A ValueAndMint semantic
 * dispute was therefore provable at the
 * validator level but not carriable on L1 — the #627 min-Ada journey's
 * output-descriptor resolution measured 21,576 complete signed bytes.
 *
 * The roster is all eleven, not just the oversized eight. Which bodies clear
 * the envelope is a compilation fact that moves with every regeneration; the
 * deployment role is a property of the resolver, so every ValueAndMint
 * semantic is *deployable* by reference and the submit path picks the route
 * from what the deployment info actually carries (see
 * `validationValueAndMintSemanticReferenceScriptDeploymentEntry`'s call site
 * in the semantic-resolution builder). These entries are consumed the way
 * `validationTraceDisputeCanonicalDecodeItemSemantic` and the CEK entries are: hash-checked
 * against the applied contract, no auth-role token.
 */
export const VALIDATION_VALUE_AND_MINT_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES =
  {
    0: "validationTraceDisputeValueAndMintBeginSemantic",
    1: "validationTraceDisputeValueAndMintReplayBeginSemantic",
    2: "validationTraceDisputeValueAndMintReplayInputSemantic",
    3: "validationTraceDisputeValueAndMintReplayAssetSemantic",
    4: "validationTraceDisputeValueAndMintReplayFinishSemantic",
    5: "validationTraceDisputeValueAndMintOutputDescriptorSemantic",
    6: "validationTraceDisputeValueAndMintOutputAssetSemantic",
    7: "validationTraceDisputeValueAndMintOutputFinishSemantic",
    8: "validationTraceDisputeValueAndMintMintAssetSemantic",
    9: "validationTraceDisputeValueAndMintMintFinishSemantic",
    10: "validationTraceDisputeValueAndMintFinalizeSemantic",
  } as const satisfies Partial<Record<number, string>>;

export type ValidationValueAndMintSemanticReferenceScriptIndex =
  keyof typeof VALIDATION_VALUE_AND_MINT_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES;

/** The ValueAndMint phase's resolver index (`VALUE_AND_MINT` = 12). */
export const VALIDATION_VALUE_AND_MINT_RESOLVER_INDEX = 12;

export const validationValueAndMintSemanticReferenceScriptDeploymentEntry = (
  semanticResolverIndex: number,
): string | undefined =>
  Number.isInteger(semanticResolverIndex) &&
  semanticResolverIndex >= 0 &&
  semanticResolverIndex <= 10
    ? VALIDATION_VALUE_AND_MINT_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES[
        semanticResolverIndex as ValidationValueAndMintSemanticReferenceScriptIndex
      ]
    : undefined;

export const requireValidationValueAndMintSemanticReferenceScriptOutRef = ({
  deploymentInfo,
  semanticResolverIndex,
  expectedScriptHash,
}: {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly semanticResolverIndex: number;
  readonly expectedScriptHash: string;
}): { readonly txHash: string; readonly outputIndex: number } => {
  const entryName =
    validationValueAndMintSemanticReferenceScriptDeploymentEntry(
      semanticResolverIndex,
    );
  if (entryName === undefined) {
    throw new Error(
      `ValueAndMint semantic resolver ${semanticResolverIndex.toString()} is not published by reference`,
    );
  }
  const entry = deploymentInfo[entryName];
  if (entry === undefined) {
    throw new Error(
      `Deployment info is missing "${entryName}"; publish the V1 ValueAndMint semantic-resolver reference script and regenerate deployment info before submitting a ValueAndMint semantic resolution`,
    );
  }
  if (entry.refScriptUTxO == null) {
    throw new Error(
      `Deployment info entry "${entryName}" is missing refScriptUTxO; publish the V1 ValueAndMint semantic-resolver reference script and regenerate deployment info before submitting a ValueAndMint semantic resolution`,
    );
  }
  if (entry.scriptHash !== expectedScriptHash) {
    throw new Error(
      `Deployment entry "${entryName}" script hash mismatch: deployment=${entry.scriptHash}, derived=${expectedScriptHash}`,
    );
  }
  return entry.refScriptUTxO;
};

export const requireValidationValueAndMintSemanticReferenceScriptUtxo = async ({
  lucid,
  deploymentInfo,
  semanticResolverIndex,
  expectedScriptHash,
}: {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly semanticResolverIndex: number;
  readonly expectedScriptHash: string;
}): Promise<UTxO> => {
  const outRef = requireValidationValueAndMintSemanticReferenceScriptOutRef({
    deploymentInfo,
    semanticResolverIndex,
    expectedScriptHash,
  });
  const utxo = await fetchUtxoByOutRef({
    lucid,
    outRef,
    label: "ValueAndMint semantic-resolver reference-script UTxO",
  });
  if (utxo.scriptRef == null) {
    throw new Error(
      `ValueAndMint semantic-resolver reference UTxO ${outRefLabel(utxo)} does not carry a reference script`,
    );
  }
  const actualScriptHash = validatorToScriptHash(utxo.scriptRef);
  if (actualScriptHash !== expectedScriptHash) {
    throw new Error(
      `ValueAndMint semantic-resolver reference script hash mismatch: actual=${actualScriptHash}, expected=${expectedScriptHash}`,
    );
  }
  return utxo;
};

/** Stable ScriptSources slots, including the slot-28 normalization door. */
export const VALIDATION_RESOLVE_INPUTS_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES =
  [
    "validationTraceDisputeResolveInputsInitialSemantic",
    "validationTraceDisputeResolveInputsFinishSemantic",
    "validationTraceDisputeResolveInputsMembershipBeginSemantic",
    "validationTraceDisputeResolveInputsMembershipStepSemantic",
    "validationTraceDisputeResolveInputsMembershipFinalizeSemantic",
    "validationTraceDisputeResolveInputsNonMembershipSemantic",
  ] as const;

export const validationResolveInputsSemanticReferenceScriptDeploymentEntry = (
  resolverIndex: number,
  semanticResolverIndex: number,
): string | undefined =>
  resolverIndex === 7 &&
  Number.isInteger(semanticResolverIndex) &&
  semanticResolverIndex >= 0
    ? VALIDATION_RESOLVE_INPUTS_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES[
        semanticResolverIndex
      ]
    : undefined;

export const VALIDATION_SCRIPT_SOURCES_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES =
  [
    "validationTraceDisputeScriptSourcesNonOutputSemantic",
    "validationTraceDisputeScriptSourcesOutputProofBeginSemantic",
    "validationTraceDisputeScriptSourcesOutputProofStepSemantic",
    "validationTraceDisputeScriptSourcesOutputProofFinalizeSemantic",
    "validationTraceDisputeScriptSourcesOutputProofFinishSemantic",
    "validationTraceDisputeScriptSourcesStageZeroBeginSemantic",
    "validationTraceDisputeScriptSourcesStageZeroFinishSemantic",
    "validationTraceDisputeScriptSourcesStageZeroHashBlockSemantic",
    "validationTraceDisputeScriptSourcesStageZeroHashAdvanceSemantic",
    "validationTraceDisputeScriptSourcesStageZeroHashTerminalSemantic",
    "validationTraceDisputeScriptSourcesStageNineMismatchSemantic",
    "validationTraceDisputeScriptSourcesStageNineNativeMatchSemantic",
    "validationTraceDisputeScriptSourcesStageNineEffectfulMatchSemantic",
    "validationTraceDisputeScriptSourcesStageNineMissingSemantic",
    "validationTraceDisputeScriptSourcesStageOneFinishSemantic",
    "validationTraceDisputeScriptSourcesStageOneRedeemerSemantic",
    "validationTraceDisputeScriptSourcesStageElevenFinishSemantic",
    "validationTraceDisputeScriptSourcesStageElevenSourceSemantic",
    "validationTraceDisputeScriptSourcesStageTwelveFinishSemantic",
    "validationTraceDisputeScriptSourcesStageTwelveRedeemerSemantic",
    "validationTraceDisputeScriptSourcesStageTenMissingSemantic",
    "validationTraceDisputeScriptSourcesStageTenMismatchSemantic",
    "validationTraceDisputeScriptSourcesStageTenMatchSemantic",
    "validationTraceDisputeScriptSourcesStageEightFinishSemantic",
    "validationTraceDisputeScriptSourcesStageEightPurposeSemantic",
    "validationTraceDisputeScriptSourcesStageSevenObserverSemantic",
    "validationTraceDisputeScriptSourcesStageSevenReceiveSemantic",
    "validationTraceDisputeScriptSourcesStageSevenFinishSemantic",
    "validationTraceDisputeScriptSourcesRedeemerNormalizationSemantic",
  ] as const;

export const validationScriptSourcesSemanticReferenceScriptDeploymentEntry = (
  resolverIndex: number,
  semanticResolverIndex: number,
): string | undefined =>
  resolverIndex === 8 &&
  Number.isInteger(semanticResolverIndex) &&
  semanticResolverIndex >= 0
    ? VALIDATION_SCRIPT_SOURCES_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES[
        semanticResolverIndex
      ]
    : undefined;

/** Applied phase-A resolvers published by the canonical deployment roster. */
export const VALIDATION_PHASE_A_NATIVE_SCRIPTS_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES =
  [
    "validationTraceDisputePhaseANativeScriptsAdvanceSemantic",
    "validationTraceDisputePhaseANativeScriptsItemSemantic",
    "validationTraceDisputePhaseANativeScriptsTokenHeadSemantic",
    "validationTraceDisputePhaseANativeScriptsAllOrAnyContainerFramePayloadSemantic",
    "validationTraceDisputePhaseANativeScriptsAllOrAnyEmptyContainerPayloadSemantic",
    "validationTraceDisputePhaseANativeScriptsAtLeastContainerFramePayloadSemantic",
    "validationTraceDisputePhaseANativeScriptsAtLeastEmptyContainerPayloadSemantic",
    "validationTraceDisputePhaseANativeScriptsTimelockPayloadSemantic",
    "validationTraceDisputePhaseANativeScriptsSignatureMembershipPayloadSemantic",
    "validationTraceDisputePhaseANativeScriptsSignatureEmptyPayloadSemantic",
    "validationTraceDisputePhaseANativeScriptsSignatureBelowFirstPayloadSemantic",
    "validationTraceDisputePhaseANativeScriptsSignatureAboveLastPayloadSemantic",
    "validationTraceDisputePhaseANativeScriptsSignatureBetweenPayloadSemantic",
    "validationTraceDisputePhaseANativeScriptsFrameSemantic",
  ] as const;

export const VALIDATION_PHASE_A_SCRIPT_PRECONDITIONS_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES =
  [
    "validationTraceDisputePhaseAScriptPreconditionsFinalizeSemantic",
    "validationTraceDisputePhaseAScriptPreconditionsItemSemantic",
  ] as const;
