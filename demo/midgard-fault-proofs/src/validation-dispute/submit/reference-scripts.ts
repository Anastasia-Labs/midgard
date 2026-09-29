import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  computeHash32,
  encodeCbor,
} from "@al-ft/midgard-core";
import {
  PreparedValidationResolutionDatum,
  PreparedValidationResolutionState,
  referenceScriptAuthUnit,
  type SharedRedeemerItemStages,
  ValidationAuxiliaryWitness,
  type ValidationAuxiliaryWitness as ValidationAuxiliaryWitnessData,
  ValidationOneStepWitness,
} from "@al-ft/midgard-sdk";
import {
  type CekProgramMaterialNecessityReceiptSet,
  type CekRouteMaterial,
} from "@al-ft/midgard-validation";
import {
  Constr,
  Data,
  type LucidEvolution,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { type ContractDeploymentInfo } from "../../inspect-contracts.js";
import { deriveScriptSourcesRedeemerItemPlan } from "../../redeemer-item-plan.js";
import { fetchUtxoByOutRef, outRefLabel } from "../../runtime.js";
import {
  exactPlutusDataFromCbor,
  type PlutusDataValue,
  requireConstr,
  validateCekSubmissionEvidence,
  validationOneStepEvidenceHashFromData,
  type ValidationOneStepSubmissionArgument,
} from "./evidence.js";
import { VALIDATION_DISPUTE_REFERENCE_SCRIPT_ROLE } from "./validity.js";

export const requireValidationDisputeReferenceScript = ({
  utxo,
  deployedScriptHash,
  expectedScriptHash,
  authPolicyId,
  role = VALIDATION_DISPUTE_REFERENCE_SCRIPT_ROLE,
}: {
  readonly utxo: UTxO;
  readonly deployedScriptHash: string;
  readonly expectedScriptHash: string;
  readonly authPolicyId: string;
  readonly role?: Parameters<typeof referenceScriptAuthUnit>[1];
}): void => {
  if (utxo.scriptRef == null) {
    throw new Error(
      `Validation-dispute reference UTxO ${outRefLabel(utxo)} does not carry a reference script`,
    );
  }
  const actualScriptHash = validatorToScriptHash(utxo.scriptRef);
  if (
    actualScriptHash !== deployedScriptHash ||
    actualScriptHash !== expectedScriptHash
  ) {
    throw new Error(
      `Validation-dispute reference script hash mismatch: actual=${actualScriptHash}, deployment=${deployedScriptHash}, expected=${expectedScriptHash}`,
    );
  }
  const expectedRoleUnit = referenceScriptAuthUnit(authPolicyId, role);
  const authPolicyAssets = Object.entries(utxo.assets).filter(
    ([unit, amount]) =>
      unit.slice(0, authPolicyId.length) === authPolicyId && amount !== 0n,
  );
  if (
    authPolicyAssets.length !== 1 ||
    authPolicyAssets[0]![0] !== expectedRoleUnit ||
    authPolicyAssets[0]![1] !== 1n
  ) {
    throw new Error(
      `Validation-dispute reference UTxO ${outRefLabel(utxo)} must carry exactly one ${expectedRoleUnit} auth-role token`,
    );
  }
};

/**
 * Deployment-info entry that publishes the applied
 * `canonical_decode_item_semantic_v1` validator as an L1 reference script.
 * The complete-item semantic-resolution proof transaction must consume the
 * validator by reference: embedding the validator body in the proof
 * transaction spends the 16,384-byte L1 envelope that the measured
 * complete-item redeemer needs (docs/exec-plans ledger row
 * C21-DISPUTE-SUBMIT).
 */
export const VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY =
  "validationTraceDisputeItemSemantic";

export const requireValidationItemSemanticReferenceScriptOutRef = ({
  deploymentInfo,
  expectedScriptHash,
}: {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly expectedScriptHash: string;
}): { readonly txHash: string; readonly outputIndex: number } => {
  const entry =
    deploymentInfo[VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY];
  if (entry === undefined) {
    throw new Error(
      `Deployment info is missing "${VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY}"; publish the V1 canonical-decode item-semantic reference script and regenerate deployment info before submitting a complete-item semantic resolution`,
    );
  }
  if (entry.refScriptUTxO == null) {
    throw new Error(
      `Deployment info entry "${VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY}" is missing refScriptUTxO; publish the V1 canonical-decode item-semantic reference script and regenerate deployment info before submitting a complete-item semantic resolution`,
    );
  }
  if (entry.scriptHash !== expectedScriptHash) {
    throw new Error(
      `Deployment entry "${VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY}" script hash mismatch: deployment=${entry.scriptHash}, derived=${expectedScriptHash}`,
    );
  }
  return entry.refScriptUTxO;
};

export const requireValidationItemSemanticReferenceScriptUtxo = async ({
  lucid,
  deploymentInfo,
  expectedScriptHash,
}: {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly expectedScriptHash: string;
}): Promise<UTxO> => {
  const outRef = requireValidationItemSemanticReferenceScriptOutRef({
    deploymentInfo,
    expectedScriptHash,
  });
  const utxo = await fetchUtxoByOutRef({
    lucid,
    outRef,
    label: "validation item-semantic reference-script UTxO",
  });
  if (utxo.scriptRef == null) {
    throw new Error(
      `Validation item-semantic reference UTxO ${outRefLabel(utxo)} does not carry a reference script`,
    );
  }
  const actualScriptHash = validatorToScriptHash(utxo.scriptRef);
  if (actualScriptHash !== expectedScriptHash) {
    throw new Error(
      `Validation item-semantic reference script hash mismatch: actual=${actualScriptHash}, expected=${expectedScriptHash}`,
    );
  }
  return utxo;
};

/**
 * Deployment-info entry that publishes the applied
 * `canonical_decode_item_observe_v1` validator as an L1 reference script.
 * The observe stage is the §8.8 door — the one stage that dereferences the
 * carriage — so its proof transaction must keep the 16,384-byte L1 envelope
 * for the carriage bytes rather than the ~9 KiB applied observe validator
 * body (#597 ruling a, executed inside the #617 regeneration wave; owner
 * rulings 2026-08-18, R3).
 */
export const VALIDATION_ITEM_OBSERVE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY =
  "validationTraceDisputeItemObserve";

export const requireValidationItemObserveReferenceScriptOutRef = ({
  deploymentInfo,
  expectedScriptHash,
}: {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly expectedScriptHash: string;
}): { readonly txHash: string; readonly outputIndex: number } => {
  const entry =
    deploymentInfo[VALIDATION_ITEM_OBSERVE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY];
  if (entry === undefined) {
    throw new Error(
      `Deployment info is missing "${VALIDATION_ITEM_OBSERVE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY}"; publish the V1 canonical-decode item-observe reference script and regenerate deployment info before submitting a complete-item semantic resolution`,
    );
  }
  if (entry.refScriptUTxO == null) {
    throw new Error(
      `Deployment info entry "${VALIDATION_ITEM_OBSERVE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY}" is missing refScriptUTxO; publish the V1 canonical-decode item-observe reference script and regenerate deployment info before submitting a complete-item semantic resolution`,
    );
  }
  if (entry.scriptHash !== expectedScriptHash) {
    throw new Error(
      `Deployment entry "${VALIDATION_ITEM_OBSERVE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY}" script hash mismatch: deployment=${entry.scriptHash}, derived=${expectedScriptHash}`,
    );
  }
  return entry.refScriptUTxO;
};

export const requireValidationItemObserveReferenceScriptUtxo = async ({
  lucid,
  deploymentInfo,
  expectedScriptHash,
}: {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly expectedScriptHash: string;
}): Promise<UTxO> => {
  const outRef = requireValidationItemObserveReferenceScriptOutRef({
    deploymentInfo,
    expectedScriptHash,
  });
  const utxo = await fetchUtxoByOutRef({
    lucid,
    outRef,
    label: "validation item-observe reference-script UTxO",
  });
  if (utxo.scriptRef == null) {
    throw new Error(
      `Validation item-observe reference UTxO ${outRefLabel(utxo)} does not carry a reference script`,
    );
  }
  const actualScriptHash = validatorToScriptHash(utxo.scriptRef);
  if (actualScriptHash !== expectedScriptHash) {
    throw new Error(
      `Validation item-observe reference script hash mismatch: actual=${actualScriptHash}, expected=${expectedScriptHash}`,
    );
  }
  return utxo;
};

/**
 * Deployment-info entry that publishes the applied `canonical_decode_v1`
 * prepare-resolver validator as an L1 reference script. The prepare-selected
 * step transaction commits the one-step argument — for a tier-1 complete
 * item its redeemer carries the whole §5.1 preimage inline — so embedding
 * the ~5.6 KiB applied prepare-resolver body beside that preimage spends the
 * 16,384-byte L1 envelope the carriage bytes need (#617 follow-up to #597
 * ruling a; measured step-transaction decomposition, 2026-08-18).
 */
export const VALIDATION_CANONICAL_DECODE_PREPARE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY =
  "validationTraceDisputeCanonicalDecodePrepare";

export const requireValidationCanonicalDecodePrepareReferenceScriptOutRef = ({
  deploymentInfo,
  expectedScriptHash,
}: {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly expectedScriptHash: string;
}): { readonly txHash: string; readonly outputIndex: number } => {
  const entry =
    deploymentInfo[
      VALIDATION_CANONICAL_DECODE_PREPARE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY
    ];
  if (entry === undefined) {
    throw new Error(
      `Deployment info is missing "${VALIDATION_CANONICAL_DECODE_PREPARE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY}"; publish the V1 canonical-decode prepare reference script and regenerate deployment info before preparing a complete-item semantic resolution`,
    );
  }
  if (entry.refScriptUTxO == null) {
    throw new Error(
      `Deployment info entry "${VALIDATION_CANONICAL_DECODE_PREPARE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY}" is missing refScriptUTxO; publish the V1 canonical-decode prepare reference script and regenerate deployment info before preparing a complete-item semantic resolution`,
    );
  }
  if (entry.scriptHash !== expectedScriptHash) {
    throw new Error(
      `Deployment entry "${VALIDATION_CANONICAL_DECODE_PREPARE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY}" script hash mismatch: deployment=${entry.scriptHash}, derived=${expectedScriptHash}`,
    );
  }
  return entry.refScriptUTxO;
};

export const requireValidationCanonicalDecodePrepareReferenceScriptUtxo =
  async ({
    lucid,
    deploymentInfo,
    expectedScriptHash,
  }: {
    readonly lucid: LucidEvolution;
    readonly deploymentInfo: ContractDeploymentInfo;
    readonly expectedScriptHash: string;
  }): Promise<UTxO> => {
    const outRef = requireValidationCanonicalDecodePrepareReferenceScriptOutRef(
      {
        deploymentInfo,
        expectedScriptHash,
      },
    );
    const utxo = await fetchUtxoByOutRef({
      lucid,
      outRef,
      label: "validation canonical-decode prepare reference-script UTxO",
    });
    if (utxo.scriptRef == null) {
      throw new Error(
        `Validation canonical-decode prepare reference UTxO ${outRefLabel(utxo)} does not carry a reference script`,
      );
    }
    const actualScriptHash = validatorToScriptHash(utxo.scriptRef);
    if (actualScriptHash !== expectedScriptHash) {
      throw new Error(
        `Validation canonical-decode prepare reference script hash mismatch: actual=${actualScriptHash}, expected=${expectedScriptHash}`,
      );
    }
    return utxo;
  };

/**
 * Deployment-info entries that publish the applied cek semantic resolvers
 * whose bodies can never ride inside the 16,384-byte L1 proof envelope
 * (`MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES`): the execution selection
 * (~45 KiB applied), the context step (~94 KiB) and the core step (~68 KiB).
 * R5 item 1 split the retired `cek_v1` direct resolver (whose ~156 KiB body
 * was consumed the same way through `validationTraceDisputeCekDirectResolver`)
 * into four semantic resolvers under a `prepare_selected` validator; the
 * finish resolver fits the envelope and attaches inline like every other
 * small semantic, the three below are consumed by reference the way
 * `validationTraceDisputeItemSemantic` is (hash-checked against the applied
 * contract, no auth-role token).
 */
export const VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES = {
  1: "validationTraceDisputeCekExecutionSelectionSemantic",
  2: "validationTraceDisputeCekContextStepSemantic",
  3: "validationTraceDisputeCekCoreStepSemantic",
} as const satisfies Partial<Record<number, string>>;

export type ValidationCekSemanticReferenceScriptIndex =
  keyof typeof VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES;

export const validationCekSemanticReferenceScriptDeploymentEntry = (
  semanticResolverIndex: number,
): string | undefined =>
  semanticResolverIndex === 1 ||
  semanticResolverIndex === 2 ||
  semanticResolverIndex === 3
    ? VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES[
        semanticResolverIndex
      ]
    : undefined;

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
 * `validationTraceDisputeItemSemantic` and the CEK entries are: hash-checked
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

export const validationPhaseASemanticReferenceScriptDeploymentEntry = (
  resolverIndex: number,
  semanticResolverIndex: number,
): string | undefined => {
  if (!Number.isInteger(semanticResolverIndex) || semanticResolverIndex < 0)
    return undefined;
  return resolverIndex === 5
    ? VALIDATION_PHASE_A_NATIVE_SCRIPTS_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES[
        semanticResolverIndex
      ]
    : resolverIndex === 6
      ? VALIDATION_PHASE_A_SCRIPT_PRECONDITIONS_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES[
          semanticResolverIndex
        ]
      : undefined;
};

export const requirePublishedValidationSemanticReferenceScriptUtxo = async ({
  lucid,
  deploymentInfo,
  entryName,
  expectedScriptHash,
}: {
  lucid: LucidEvolution;
  deploymentInfo: ContractDeploymentInfo;
  entryName: string;
  expectedScriptHash: string;
}): Promise<UTxO> => {
  const entry = deploymentInfo[entryName];
  if (entry?.refScriptUTxO == null)
    throw new Error(
      `Publish the validation semantic resolver as "${entryName}" before submitting`,
    );
  if (entry.scriptHash !== expectedScriptHash)
    throw new Error(
      `Validation semantic deployment hash mismatch for "${entryName}"`,
    );
  const utxo = await fetchUtxoByOutRef({
    lucid,
    outRef: entry.refScriptUTxO,
    label: entryName,
  });
  if (
    utxo.scriptRef == null ||
    validatorToScriptHash(utxo.scriptRef) !== expectedScriptHash
  )
    throw new Error(
      `Validation semantic reference script mismatch for "${entryName}"`,
    );
  return utxo;
};

export const VALIDATION_SEMANTIC_RESOLVER_COUNTS = [
  2, 1, 1, 2, 4, 14, 2, 6, 29, 3, 4, 4, 11, 8,
] as const;
const VALIDATION_SEMANTIC_RESOLVER_OFFSETS = [
  0, 2, 3, 4, 6, 10, 24, 26, 32, 60, 63, 67, 71, 82,
] as const;

export const VALIDATION_AUXILIARY_SHAPES = {
  none: [0, 0],
  // #597: the four §8-door constructors carry a `FieldCarriageV1` where they
  // used to carry counted openings. `TransactionFieldChunkWitness` is
  // `(field_index, item_index, carriage)`, `RequiredSignerItemWitness` is
  // `(carriage, signer_proof)`, and the two begin/item constructors are
  // `(carriage)` alone. Constructor *indices* are unchanged — only the shapes
  // moved (`onchain/aiken/lib/midgard/validation-machine/`).
  transactionFieldChunk: [1, 3],
  transactionFieldItem: [30, 1],
  requiredSignerItem: [2, 2],
  nativeScriptToken: [3, 3],
  nativeScriptFrame: [4, 1],
  scheduledLedgerMembership: [5, 6],
  scheduledLedgerNonMembership: [6, 4],
  resolvedInputReplay: [7, 4],
  scriptPurposeScan: [8, 5],
  scriptSourceScan: [9, 8],
  redeemerScanBegin: [10, 5],
  redeemerItemStep: [18, 3],
  ledgerDeltaReplay: [27, 4],
  ledgerDeltaOutput: [28, 3],
  transactionRedeemerItemBegin: [29, 1],
  ledgerOutputProofBegin: [31, 4],
  ledgerOutputProofStep: [32, 1],
  ledgerOutputProofFinalize: [33, 2],
  ledgerDeltaProofFrame: [34, 2],
  ledgerDeltaOperation: [35, 4],
  scriptSourceHashBlock: [36, 2],
  nativeExecutionDescriptor: [37, 17],
  // R5 item 1: the cek and ValueAndMint auxiliary families, now that both
  // phases route through prepare + per-kind semantic resolvers.
  nativeExecutionScan: [11, 16],
  cekCoreStep: [12, 1],
  cekResolvedContextItem: [13, 5],
  cekOutputContextItem: [14, 3],
  cekSignerContextItem: [15, 4],
  cekMintContextItem: [16, 6],
  cekRedeemerContextSelect: [17, 12],
  cekContextFinalize: [19, 1],
  cekContextFinalizeSpend: [20, 5],
  cekContextAssemble: [21, 1],
  cekTxInfoFinalize: [22, 1],
  cekContextSeed: [23, 1],
  valueInputAsset: [24, 11],
  valueOutputAsset: [25, 9],
  valueMintAsset: [26, 6],
  valueOutputDescriptor: [38, 3],
} as const satisfies Record<string, readonly [number, number]>;

/**
 * The auxiliary constructors the cek context step
 * (`cek_context_step_semantic_v1`) accepts: every `Cek*Context*Witness`, the
 * redeemer-selection witnesses (`RedeemerScanBeginWitness` /
 * `RedeemerItemStepWitness`), the observer stage's authenticated field chunk,
 * plus the empty witness that the context-only stages (seed/assemble/finalize
 * without items, observer-empty fields) step on. The resolver takes the whole
 * auxiliary and branches inside `verify_cek_context_step_semantics_v1`, so the
 * builder pins the family rather than one shape.
 */
export const VALIDATION_CEK_CONTEXT_STEP_AUXILIARY_SHAPES = [
  VALIDATION_AUXILIARY_SHAPES.none,
  VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk,
  VALIDATION_AUXILIARY_SHAPES.redeemerScanBegin,
  VALIDATION_AUXILIARY_SHAPES.cekResolvedContextItem,
  VALIDATION_AUXILIARY_SHAPES.cekOutputContextItem,
  VALIDATION_AUXILIARY_SHAPES.cekSignerContextItem,
  VALIDATION_AUXILIARY_SHAPES.cekMintContextItem,
  VALIDATION_AUXILIARY_SHAPES.cekRedeemerContextSelect,
  VALIDATION_AUXILIARY_SHAPES.redeemerItemStep,
  VALIDATION_AUXILIARY_SHAPES.cekContextFinalize,
  VALIDATION_AUXILIARY_SHAPES.cekContextFinalizeSpend,
  VALIDATION_AUXILIARY_SHAPES.cekContextAssemble,
  VALIDATION_AUXILIARY_SHAPES.cekTxInfoFinalize,
  VALIDATION_AUXILIARY_SHAPES.cekContextSeed,
] as const;

/**
 * Auxiliary shape per ValueAndMint semantic resolver
 * (`value_and_mint_v1` prepare order): the stage-entry, replay-finish,
 * output-finish, mint-finish and finalize resolvers step with no auxiliary;
 * the item resolvers carry the stage body's witness.
 */
export const VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES = [
  VALIDATION_AUXILIARY_SHAPES.none,
  VALIDATION_AUXILIARY_SHAPES.none,
  VALIDATION_AUXILIARY_SHAPES.resolvedInputReplay,
  VALIDATION_AUXILIARY_SHAPES.valueInputAsset,
  VALIDATION_AUXILIARY_SHAPES.none,
  VALIDATION_AUXILIARY_SHAPES.valueOutputDescriptor,
  VALIDATION_AUXILIARY_SHAPES.valueOutputAsset,
  VALIDATION_AUXILIARY_SHAPES.none,
  VALIDATION_AUXILIARY_SHAPES.valueMintAsset,
  VALIDATION_AUXILIARY_SHAPES.none,
  VALIDATION_AUXILIARY_SHAPES.none,
] as const;

export const hasValidationAuxiliaryShape = (
  auxiliary: Constr<PlutusDataValue>,
  shape: readonly [number, number],
): boolean =>
  auxiliary.index === shape[0] && auxiliary.fields.length === shape[1];

const auxiliaryShape = ({
  resolverIndex,
  semanticResolverIndex,
  auxiliary,
}: {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly auxiliary: PlutusDataValue;
}): Constr<PlutusDataValue> => {
  if (resolverIndex === 0) {
    if (semanticResolverIndex === 0) {
      return requireConstr({
        value: auxiliary,
        index: VALIDATION_AUXILIARY_SHAPES.none[0],
        fields: VALIDATION_AUXILIARY_SHAPES.none[1],
        label: "validation CanonicalDecode empty auxiliary witness",
      });
    }
    if (
      auxiliary instanceof Constr &&
      (hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk,
      ) ||
        hasValidationAuxiliaryShape(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES.transactionFieldItem,
        ))
    ) {
      return auxiliary;
    }
    throw new Error(
      "validation CanonicalDecode auxiliary witness must carry an authenticated chunk or complete item",
    );
  }
  if (resolverIndex === 13) {
    const expected =
      semanticResolverIndex === 2 ||
      semanticResolverIndex === 4 ||
      semanticResolverIndex === 6 ||
      semanticResolverIndex === 7
        ? VALIDATION_AUXILIARY_SHAPES.none
        : semanticResolverIndex === 0
          ? VALIDATION_AUXILIARY_SHAPES.ledgerDeltaOperation
          : semanticResolverIndex === 1
            ? VALIDATION_AUXILIARY_SHAPES.ledgerDeltaReplay
            : semanticResolverIndex === 3
              ? VALIDATION_AUXILIARY_SHAPES.ledgerDeltaOutput
              : VALIDATION_AUXILIARY_SHAPES.ledgerDeltaProofFrame;
    return requireConstr({
      value: auxiliary,
      index: expected[0],
      fields: expected[1],
      label: "validation LedgerDelta auxiliary witness",
    });
  }
  if (resolverIndex === 7) {
    const expected =
      semanticResolverIndex === 0 || semanticResolverIndex === 1
        ? VALIDATION_AUXILIARY_SHAPES.none
        : semanticResolverIndex === 2
          ? VALIDATION_AUXILIARY_SHAPES.scheduledLedgerMembership
          : semanticResolverIndex === 3
            ? VALIDATION_AUXILIARY_SHAPES.ledgerOutputProofStep
            : semanticResolverIndex === 4
              ? VALIDATION_AUXILIARY_SHAPES.ledgerOutputProofFinalize
              : semanticResolverIndex === 5
                ? VALIDATION_AUXILIARY_SHAPES.scheduledLedgerNonMembership
                : VALIDATION_AUXILIARY_SHAPES.resolvedInputReplay;
    return requireConstr({
      value: auxiliary,
      index: expected[0],
      fields: expected[1],
      label: "validation ResolveInputs auxiliary witness",
    });
  }
  if (resolverIndex === 8) {
    if (!(auxiliary instanceof Constr)) {
      throw new Error("validation auxiliary witness must be a constructor");
    }
    const isRedeemerItemStage = hasValidationAuxiliaryShape(
      auxiliary,
      VALIDATION_AUXILIARY_SHAPES.redeemerItemStep,
    );
    if (semanticResolverIndex === 28 && !isRedeemerItemStage) {
      throw new Error(
        "validation auxiliary witness does not match the selected ScriptSources split stage-one proof family",
      );
    }
    if (
      semanticResolverIndex === 15 &&
      !hasValidationAuxiliaryShape(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES.transactionRedeemerItemBegin,
      )
    ) {
      throw new Error(
        "validation auxiliary witness does not match the selected ScriptSources redeemer-ingestion proof family",
      );
    }
    if (
      (semanticResolverIndex === 19 ||
        semanticResolverIndex === 21 ||
        semanticResolverIndex === 22) &&
      !(
        hasValidationAuxiliaryShape(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES.redeemerScanBegin,
        ) || isRedeemerItemStage
      )
    ) {
      throw new Error(
        "validation auxiliary witness does not match the selected ScriptSources redeemer-scan proof family",
      );
    }
    const outputExpected =
      semanticResolverIndex === 0
        ? null
        : semanticResolverIndex === 1
          ? VALIDATION_AUXILIARY_SHAPES.ledgerOutputProofBegin
          : semanticResolverIndex === 2
            ? VALIDATION_AUXILIARY_SHAPES.ledgerOutputProofStep
            : semanticResolverIndex === 3
              ? VALIDATION_AUXILIARY_SHAPES.ledgerOutputProofFinalize
              : semanticResolverIndex === 5
                ? VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk
                : semanticResolverIndex === 7
                  ? VALIDATION_AUXILIARY_SHAPES.scriptSourceHashBlock
                  : semanticResolverIndex >= 10 && semanticResolverIndex <= 12
                    ? VALIDATION_AUXILIARY_SHAPES.scriptSourceScan
                    : semanticResolverIndex === 17
                      ? VALIDATION_AUXILIARY_SHAPES.scriptSourceScan
                      : semanticResolverIndex === 19
                        ? null
                        : semanticResolverIndex === 21 ||
                            semanticResolverIndex === 22
                          ? null
                          : semanticResolverIndex === 24
                            ? VALIDATION_AUXILIARY_SHAPES.scriptPurposeScan
                            : semanticResolverIndex === 25
                              ? VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk
                              : semanticResolverIndex === 26
                                ? VALIDATION_AUXILIARY_SHAPES.scriptPurposeScan
                                : semanticResolverIndex === 15 ||
                                    semanticResolverIndex === 28
                                  ? null
                                  : VALIDATION_AUXILIARY_SHAPES.none;
    const outputAuxiliary = auxiliary;
    if (
      outputExpected !== null &&
      (outputAuxiliary.index !== outputExpected[0] ||
        outputAuxiliary.fields.length !== outputExpected[1])
    ) {
      throw new Error(
        "validation auxiliary witness does not match the selected ScriptSources proof family",
      );
    }
    return outputAuxiliary;
  }
  if (resolverIndex === 11) {
    if (semanticResolverIndex === 2) {
      if (
        auxiliary instanceof Constr &&
        VALIDATION_CEK_CONTEXT_STEP_AUXILIARY_SHAPES.some((shape) =>
          hasValidationAuxiliaryShape(auxiliary, shape),
        )
      ) {
        return auxiliary;
      }
      throw new Error(
        "validation Cek context-step auxiliary witness must carry a cek context witness or no auxiliary",
      );
    }
    const expected =
      semanticResolverIndex === 0
        ? VALIDATION_AUXILIARY_SHAPES.none
        : semanticResolverIndex === 1
          ? VALIDATION_AUXILIARY_SHAPES.nativeExecutionScan
          : VALIDATION_AUXILIARY_SHAPES.cekCoreStep;
    return requireConstr({
      value: auxiliary,
      index: expected[0],
      fields: expected[1],
      label: "validation Cek auxiliary witness",
    });
  }
  if (resolverIndex === 12) {
    const expected =
      VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES[semanticResolverIndex];
    if (expected === undefined) {
      throw new Error(
        "validation ValueAndMint semantic resolver index is out of range",
      );
    }
    return requireConstr({
      value: auxiliary,
      index: expected[0],
      fields: expected[1],
      label: "validation ValueAndMint auxiliary witness",
    });
  }
  if (resolverIndex === 9) {
    const expected =
      semanticResolverIndex === 0
        ? VALIDATION_AUXILIARY_SHAPES.none
        : VALIDATION_AUXILIARY_SHAPES.nativeExecutionDescriptor;
    return requireConstr({
      value: auxiliary,
      index: expected[0],
      fields: expected[1],
      label: "validation NativeScripts auxiliary witness",
    });
  }
  const expected =
    resolverIndex === 1 || resolverIndex === 2 || resolverIndex === 10
      ? VALIDATION_AUXILIARY_SHAPES.none
      : resolverIndex === 3
        ? semanticResolverIndex === 0
          ? VALIDATION_AUXILIARY_SHAPES.none
          : VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk
        : resolverIndex === 4
          ? semanticResolverIndex === 0 || semanticResolverIndex === 3
            ? VALIDATION_AUXILIARY_SHAPES.none
            : semanticResolverIndex === 1
              ? VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk
              : VALIDATION_AUXILIARY_SHAPES.requiredSignerItem
          : resolverIndex === 5
            ? semanticResolverIndex === 0
              ? VALIDATION_AUXILIARY_SHAPES.none
              : semanticResolverIndex === 1
                ? VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk
                : semanticResolverIndex === 13
                  ? VALIDATION_AUXILIARY_SHAPES.nativeScriptFrame
                  : VALIDATION_AUXILIARY_SHAPES.nativeScriptToken
            : resolverIndex === 6
              ? semanticResolverIndex === 0
                ? VALIDATION_AUXILIARY_SHAPES.none
                : VALIDATION_AUXILIARY_SHAPES.transactionFieldChunk
              : null;
  if (expected === null) {
    throw new Error(
      `Validation resolver ${resolverIndex.toString()} has no staged semantic proof family`,
    );
  }
  return requireConstr({
    value: auxiliary,
    index: expected[0],
    fields: expected[1],
    label: "validation auxiliary witness",
  });
};

export const requireStagedOneStepArgument = (
  argument: ValidationOneStepSubmissionArgument,
): {
  readonly transition: ValidationOneStepWitness;
  readonly transitionData: PlutusDataValue;
  readonly auxiliaryData: PlutusDataValue;
  readonly auxiliaryWitness: ValidationAuxiliaryWitnessData;
  readonly auxiliary: Constr<PlutusDataValue>;
  readonly semanticResolverIndex: number;
  readonly semanticResolverGlobalIndex: number;
  readonly evidenceHash: string;
  readonly cekContextSuccessorWorkWitnessCbor?: Uint8Array;
  readonly cekRouteMaterial?: CekRouteMaterial;
  readonly cekIncrementalNecessityReceiptSet?: CekProgramMaterialNecessityReceiptSet;
} => {
  const validatedCekEvidence = validateCekSubmissionEvidence(argument);
  if (
    !Number.isSafeInteger(argument.resolverIndex) ||
    argument.resolverIndex < 0 ||
    argument.resolverIndex >= VALIDATION_SEMANTIC_RESOLVER_COUNTS.length
  ) {
    throw new Error(
      "Staged validation one-step argument must select a prepare resolver",
    );
  }
  const semanticResolverIndex = argument.semanticResolverIndex;
  const semanticResolverCount =
    VALIDATION_SEMANTIC_RESOLVER_COUNTS[argument.resolverIndex]!;
  if (
    !Number.isSafeInteger(semanticResolverIndex) ||
    semanticResolverIndex < 0 ||
    semanticResolverIndex >= semanticResolverCount
  ) {
    throw new Error(
      "Validation one-step argument selects an unavailable semantic resolver",
    );
  }
  const transitionData = exactPlutusDataFromCbor(
    argument.transitionCbor,
    "validation transition",
  );
  const auxiliaryData = exactPlutusDataFromCbor(
    argument.auxiliaryCbor,
    "validation auxiliary witness",
  );
  const auxiliaryWitness = Data.from(
    Buffer.from(argument.auxiliaryCbor).toString("hex"),
    ValidationAuxiliaryWitness,
  );
  const transition = Data.from(
    Buffer.from(argument.transitionCbor).toString("hex"),
    ValidationOneStepWitness,
  );
  const auxiliary = auxiliaryShape({
    resolverIndex: argument.resolverIndex,
    semanticResolverIndex,
    auxiliary: auxiliaryData,
  });
  return {
    transition,
    transitionData,
    auxiliaryData,
    auxiliaryWitness,
    auxiliary,
    semanticResolverIndex,
    semanticResolverGlobalIndex: validationSemanticResolverGlobalIndex(
      argument.resolverIndex,
      semanticResolverIndex,
    ),
    // Option B (#620): the canonical-decode resolver commits to the transition
    // alone — the auxiliary hashed into `evidence_hash` is `NoAuxiliaryWitness`
    // whatever carriage the auxiliary witness names, because the carriage is
    // dereferenced (and content-checked) only at the observe stage's §8.8 door.
    // Every other resolver still freezes its auxiliary into the commitment.
    evidenceHash: validationOneStepEvidenceHashFromData(
      transitionData,
      argument.resolverIndex === 0 ? new Constr(0, []) : auxiliaryData,
    ),
    ...validatedCekEvidence,
  };
};

/** Reconstruct immutable output bindings from the exact retained preparation and claim. */
export const deriveScriptSourcesItemSubmissionPlan = ({
  preparedCbor,
  oneStepArgument,
  stages,
  deploymentId,
}: {
  readonly preparedCbor: string;
  readonly oneStepArgument: ValidationOneStepSubmissionArgument;
  readonly stages: SharedRedeemerItemStages;
  readonly deploymentId: string;
}) => {
  if (!/^(?:[0-9a-f]{2})+$/.test(preparedCbor))
    throw new Error(
      "ScriptSources prepared datum is not exact hexadecimal CBOR",
    );
  aikenSerialisedPlutusDataCborPreservingMapOrder(preparedCbor);
  const prepared = Data.from(preparedCbor, PreparedValidationResolutionDatum);
  const staged = requireStagedOneStepArgument(oneStepArgument);
  if (
    prepared.data === null ||
    prepared.data.resolution.pre_state.phase !== "ScriptSources" ||
    oneStepArgument.resolverIndex !== 8 ||
    staged.semanticResolverIndex !== 28 ||
    staged.evidenceHash !== prepared.data.evidence_hash
  )
    throw new Error(
      "Retained ScriptSources item evidence differs from its authenticated preparation",
    );
  const bindings = deriveScriptSourcesRedeemerItemPlan({
    preparedResolution: Data.from(
      Data.to(prepared.data, PreparedValidationResolutionState),
    ),
    transition: staged.transitionData,
    auxiliary: staged.auxiliary,
    stages,
    deploymentId,
  });
  const datum = (state: PlutusDataValue) =>
    Data.to(new Constr(0, [prepared.fraud_prover, new Constr(0, [state])]));
  const identity = computeHash32(
    Buffer.concat([
      Buffer.from("MidgardScriptSourcesItemSubmissionV1", "ascii"),
      encodeCbor([
        Buffer.from(preparedCbor, "hex"),
        Buffer.from(oneStepArgument.transitionCbor),
        Buffer.from(oneStepArgument.auxiliaryCbor),
        Buffer.from(deploymentId, "hex"),
        bindings.map((binding) =>
          Buffer.from(binding.validator.spendingScriptHash, "hex"),
        ),
      ]),
    ]),
  ).toString("hex");
  return {
    identity,
    preparedCbor,
    fraudProver: prepared.fraud_prover,
    bindings: bindings.map((binding) => ({
      ...binding,
      inputDatumCbor: datum(binding.inputState),
      outputDatumCbor: datum(binding.outputState),
    })),
  };
};

/** A checkpoint is resumable only at one exact canonical address-and-datum pair. */
export const scriptSourcesItemResumeIndex = ({
  plan,
  thread,
}: {
  readonly plan: ReturnType<typeof deriveScriptSourcesItemSubmissionPlan>;
  readonly thread: Pick<UTxO, "address" | "datum">;
}): number => {
  if (thread.datum == null)
    throw new Error("ScriptSources item checkpoint has no inline datum");
  const actual = aikenSerialisedPlutusDataCborPreservingMapOrder(thread.datum);
  const matching = plan.bindings.flatMap((binding, index) =>
    binding.validator.spendingScriptAddress === thread.address &&
    aikenSerialisedPlutusDataCborPreservingMapOrder(binding.inputDatumCbor) ===
      actual
      ? [index]
      : [],
  );
  if (matching.length !== 1)
    throw new Error(
      "ScriptSources item checkpoint is not an exact canonical stage",
    );
  return matching[0]!;
};

export const validationSemanticResolverGlobalIndex = (
  resolverIndex: number,
  semanticResolverIndex: number,
): number =>
  resolverIndex === 8 && semanticResolverIndex === 28
    ? 90
    : VALIDATION_SEMANTIC_RESOLVER_OFFSETS[resolverIndex]! +
      semanticResolverIndex;
