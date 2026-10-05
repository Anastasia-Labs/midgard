import { referenceScriptAuthUnit } from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { type ContractDeploymentInfo } from "../../inspect-contracts.js";
import { fetchUtxoByOutRef, outRefLabel } from "../../runtime.js";
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
  "validationTraceDisputeCanonicalDecodeItemSemantic";

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
  "validationTraceDisputeCanonicalDecodeItemObserve";

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
 * `validationTraceDisputeCanonicalDecodeItemSemantic` is (hash-checked against the applied
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
