import {
  Constr,
  type LucidEvolution,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { type ContractDeploymentInfo } from "../../inspect-contracts.js";
import { fetchUtxoByOutRef } from "../../runtime.js";
import { type PlutusDataValue } from "./evidence.js";
import {
  VALIDATION_PHASE_A_NATIVE_SCRIPTS_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES,
  VALIDATION_PHASE_A_SCRIPT_PRECONDITIONS_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES,
} from "./reference-scripts.validation-value-and-mint-semantic-reference-script-deployment-entries.js";

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

export const VALIDATION_SEMANTIC_RESOLVER_OFFSETS = [
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
  ledgerOutputProofFinalize: [33, 1],
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
  cekRedeemerContextSkip: [40, 4],
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
 * `RedeemerItemStepWitness` / `CekRedeemerContextSkipWitness`), the observer stage's authenticated field chunk,
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
  VALIDATION_AUXILIARY_SHAPES.cekRedeemerContextSkip,
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
