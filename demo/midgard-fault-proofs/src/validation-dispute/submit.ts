import {
  advanceMidgardRedeemerItemProofV1,
  asArray,
  asBytes,
  canOpenMidgardValidationDisputeBeforeMaturity,
  computeHash32,
  decodeMidgardCekProgramMaterialSidecarV1,
  decodeMidgardNativeTxProofFieldLengthsV1,
  decodeSingleCbor,
  encodeCbor,
  encodeCborArrayRaw,
  encodeMidgardCekDataTraverseControlV1,
  encodeMidgardRedeemerItemProofControlV1,
  hashMidgardRedeemerItemProofControlV1,
  type MidgardCekDataFrameV1,
  type MidgardCekDataSummaryV1,
  type MidgardCekDataTraverseActionV1,
  type MidgardCekDataTraverseControlV1,
  type MidgardRedeemerItemProofControlV1,
  type MidgardValidationTraceProofV1,
  openMidgardValidationDispute,
  revealMidgardValidationChallengerMidpoint,
  revealMidgardValidationOperatorMidpoint,
  timeoutMidgardValidationDispute,
  verifyMidgardNativeTxProofSourceV1,
} from "@al-ft/midgard-core";
import type { MidgardFieldCarriagePlanV1 } from "@al-ft/midgard-core/codec/native-tx-carriage-v1";
import {
  decodeMidgardFieldPreimageV1,
  type MidgardFieldCarriageV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile-v1";
import { midgardV1TxFieldCommitmentsFromSourceV1 } from "@al-ft/midgard-core/consensus-validation-v1";
import {
  assertMidgardFieldCarriageResolvesAtDoorV1,
  AuthenticatedCanonicalDecodeItemDatumV1,
  buildUnsignedValidationProofItemPublicationV1Program,
  deriveCekProgramMaterialPublicationsV1,
  deriveCekSinglePublicationV1,
  deriveValidationProofItemPublicationV1,
  deriveValidationTraceDeploymentIdV1,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  getHeaderV1FromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  hashBlockHeaderV1,
  HUB_ORACLE_ASSET_NAME,
  ObservedCanonicalDecodeItemDatumV1,
  PendingValidationClaimDatumV1,
  type PendingValidationClaimDatumV1 as PendingValidationClaimDatumV1Data,
  PreparedCanonicalDecodeItemDatumV1,
  PreparedValidationResolutionDatumV1,
  type PreparedValidationResolutionDatumV1 as PreparedValidationResolutionDatumV1Data,
  PreparedValidationResolutionStateV1,
  referenceScriptAuthUnit,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  ValidationAuxiliaryWitnessV1,
  type ValidationAuxiliaryWitnessV1 as ValidationAuxiliaryWitnessV1Data,
  ValidationAwardSpendRedeemerV1,
  ValidationBoundarySpendRedeemerV1,
  ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1Schema,
  type ValidationCekMaterialRouteV1,
  ValidationCekSpendRedeemerV1Schema,
  type ValidationClaimWitnessV1,
  ValidationDirectResolveSpendRedeemerV1Schema,
  validationDisputeCoreFromData,
  validationDisputeDataFromCore,
  ValidationDisputeDatumV1,
  type ValidationDisputeDatumV1 as ValidationDisputeDatumV1Data,
  ValidationDisputeOpenSpendRedeemerV1,
  ValidationGameSpendRedeemerV1,
  type ValidationMachineStateV1,
  ValidationOneStepEvidenceV1,
  ValidationOneStepWitnessV1,
  ValidationPrepareSelectedSpendRedeemerV1Schema,
  ValidationProofItemDatumV1,
  ValidationResolutionDatumV1,
  type ValidationResolutionDatumV1 as ValidationResolutionDatumV1Data,
  ValidationSourceSpendRedeemerV1,
  ValidationTimeoutSpendRedeemerV1,
  validationTraceDescriptorCoreFromData,
  validationTraceDescriptorDataFromCore,
  type ValidationTraceDescriptorV1,
  validationTraceProofCoreFromData,
  validationTraceProofDataFromCore,
  type ValidationTraceProofV1,
  VerifiedCanonicalDecodeItemDatumV1,
  WinningValidationResolutionDatumV1,
  type WinningValidationResolutionDatumV1 as WinningValidationResolutionDatumV1Data,
} from "@al-ft/midgard-sdk";
import {
  type CekProgramMaterialNecessityReceiptSetV1,
  type CekRouteMaterialV1,
  parseCekProgramMaterialNecessityReceiptSetV1,
  validateCekRouteMaterialV1,
} from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  CML,
  Constr,
  coreToTxOutput,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
  type TxSigned,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type ContractDeploymentInfo } from "../inspect-contracts.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
  resolveValidationTraceDisputeDeploymentContracts,
} from "../runtime.js";
import {
  requireComputationThreadToken,
  requireInitialStepDatum,
  selectFeeInput,
} from "../submit-step-01.js";
import {
  computationThreadOutputPredicate,
  outputWithDatumAndUnitPredicate,
} from "../tx-layout.js";

export const VALIDATION_DISPUTE_VALIDITY_BACKOFF_MS = 60_000;
export const VALIDATION_DISPUTE_VALIDITY_LEEWAY_MS = 60_000;
export const MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES = 16 * 1024;
const VALIDATION_DISPUTE_REFERENCE_SCRIPT_ROLE = "V1 validation-trace dispute";

export const selectValidationCompleteItemCarriageV1 = (
  itemBytes: number,
): "direct" | "reference" => {
  if (
    !Number.isSafeInteger(itemBytes) ||
    itemBytes < 0 ||
    itemBytes >
      MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes
  ) {
    throw new Error(
      "Complete validation proof item exceeds the measured single-publication envelope",
    );
  }
  return itemBytes <=
    MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes
    ? "direct"
    : "reference";
};

export type ValidationDisputeValidityRange = {
  readonly validFrom: number;
  readonly validTo: number;
};

const safeUnsignedNumber = (value: bigint, field: string): number => {
  const number = Number(value);
  if (!Number.isSafeInteger(number) || number < 0) {
    throw new Error(`${field} must be a non-negative safe integer`);
  }
  return number;
};

export const validationDisputeValidityRange = (
  now: number,
): ValidationDisputeValidityRange => {
  if (
    !Number.isSafeInteger(now) ||
    now < VALIDATION_DISPUTE_VALIDITY_BACKOFF_MS
  ) {
    throw new Error(
      "Validation-dispute current time must be a safe POSIX time",
    );
  }
  return {
    validFrom: now - VALIDATION_DISPUTE_VALIDITY_BACKOFF_MS,
    validTo: now + VALIDATION_DISPUTE_VALIDITY_LEEWAY_MS,
  };
};

export const validationDisputeTimeoutValidityRange = (
  now: number,
  responseDeadline: number,
): ValidationDisputeValidityRange => {
  const ordinary = validationDisputeValidityRange(now);
  if (!Number.isSafeInteger(responseDeadline) || responseDeadline < 0) {
    throw new Error(
      "Validation-dispute response deadline must be a non-negative safe integer",
    );
  }
  if (now <= responseDeadline) {
    throw new Error("Validation-dispute response deadline has not passed");
  }
  return requireValidityRange({
    validFrom: Math.max(ordinary.validFrom, responseDeadline + 1),
    validTo: ordinary.validTo,
  });
};

const requireValidityRange = (
  range: ValidationDisputeValidityRange | null | undefined,
): ValidationDisputeValidityRange => {
  if (
    range == null ||
    !Number.isSafeInteger(range.validFrom) ||
    !Number.isSafeInteger(range.validTo) ||
    range.validFrom < 0 ||
    range.validTo <= range.validFrom ||
    range.validTo - range.validFrom >
      VALIDATION_DISPUTE_VALIDITY_BACKOFF_MS +
        VALIDATION_DISPUTE_VALIDITY_LEEWAY_MS
  ) {
    throw new Error(
      "Validation-dispute validity range must yield a non-empty, non-negative closed range no longer than 120 seconds",
    );
  }
  return range;
};

export const refreshExpiredValidationDisputeValidityRange = ({
  range,
  currentLedgerTime,
}: {
  readonly range: ValidationDisputeValidityRange;
  readonly currentLedgerTime: number;
}): ValidationDisputeValidityRange => {
  const checked = requireValidityRange(range);
  if (!Number.isSafeInteger(currentLedgerTime) || currentLedgerTime < 0) {
    throw new Error(
      "Validation-dispute current ledger time must be a non-negative safe integer",
    );
  }
  if (currentLedgerTime < checked.validTo) {
    return checked;
  }
  const width = checked.validTo - checked.validFrom;
  const backoff = Math.min(VALIDATION_DISPUTE_VALIDITY_BACKOFF_MS, width - 1);
  const validFrom = Math.max(0, currentLedgerTime - backoff);
  return requireValidityRange({
    validFrom,
    validTo: validFrom + width,
  });
};

const inclusiveValidityUpperBound = (
  range: ValidationDisputeValidityRange,
): number => range.validTo - 1;

export const openValidationDisputeAfterSourceVerification = ({
  operatorDescriptor,
  challengerDescriptor,
  openTimeUpper,
  challengedBlockEndTime,
  sourceValidityRange,
}: {
  readonly operatorDescriptor: Parameters<
    typeof openMidgardValidationDispute
  >[0]["operatorDescriptor"];
  readonly challengerDescriptor: Parameters<
    typeof openMidgardValidationDispute
  >[0]["challengerDescriptor"];
  readonly openTimeUpper: bigint;
  readonly challengedBlockEndTime: bigint;
  readonly sourceValidityRange: ValidationDisputeValidityRange;
}): ReturnType<typeof openMidgardValidationDispute> => {
  const range = requireValidityRange(sourceValidityRange);
  const authenticatedOpenTimeUpper = safeUnsignedNumber(
    openTimeUpper,
    "pending.open_time_upper",
  );
  const sourceTimeUpper = inclusiveValidityUpperBound(range);
  if (sourceTimeUpper < authenticatedOpenTimeUpper) {
    throw new Error(
      "Validation-dispute source verification cannot precede the authenticated open transaction",
    );
  }
  const authenticatedBlockEndTime = safeUnsignedNumber(
    challengedBlockEndTime,
    "pending.challenged_header.endTime",
  );
  if (
    !canOpenMidgardValidationDisputeBeforeMaturity({
      currentTimeUpper: sourceTimeUpper,
      challengedBlockEndTime: authenticatedBlockEndTime,
      maturityDuration: MIDGARD_CONSENSUS_LIMITS_V1.blockMaturityMs,
    })
  ) {
    throw new Error(
      "Validation dispute cannot complete before the challenged block matures after source verification",
    );
  }
  return openMidgardValidationDispute({
    operatorDescriptor,
    challengerDescriptor,
    currentTime: sourceTimeUpper,
  });
};

const threadAssets = (threadUtxo: UTxO, threadUnit: string) => ({
  lovelace: threadUtxo.assets.lovelace ?? 0n,
  [threadUnit]: 1n,
});

const requireL1ProofEnvelope = (
  transactionCbor: string,
  label: string,
): void => {
  const bytes = transactionCbor.length / 2;
  if (
    transactionCbor.length % 2 !== 0 ||
    !/^[0-9a-f]+$/u.test(transactionCbor) ||
    bytes > MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES
  ) {
    throw new Error(
      `${label} transaction is ${bytes.toString()} bytes; the complete signed L1 proof transaction must be no larger than ${MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES.toString()} bytes`,
    );
  }
};

const findUniqueInlineDatumOutputIndex = ({
  transactionCbor,
  address,
  datum,
  label,
}: {
  readonly transactionCbor: string;
  readonly address: string;
  readonly datum: string;
  readonly label: string;
}): number => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  const outputs = transaction.body().outputs();
  const matches: number[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (output.address === address && output.datum === datum) {
      matches.push(index);
    }
  }
  if (matches.length !== 1) {
    throw new Error(
      `${label} must contain exactly one matching inline-datum output; found ${matches.length.toString()}`,
    );
  }
  return matches[0]!;
};

const requireValidationDisputeReferenceScript = ({
  utxo,
  deployedScriptHash,
  expectedScriptHash,
  authPolicyId,
}: {
  readonly utxo: UTxO;
  readonly deployedScriptHash: string;
  readonly expectedScriptHash: string;
  readonly authPolicyId: string;
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
  const expectedRoleUnit = referenceScriptAuthUnit(
    authPolicyId,
    VALIDATION_DISPUTE_REFERENCE_SCRIPT_ROLE,
  );
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

const requireValidationItemSemanticReferenceScriptUtxo = async ({
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

const requireValidationItemObserveReferenceScriptUtxo = async ({
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

const requireValidationCanonicalDecodePrepareReferenceScriptUtxo = async ({
  lucid,
  deploymentInfo,
  expectedScriptHash,
}: {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly expectedScriptHash: string;
}): Promise<UTxO> => {
  const outRef = requireValidationCanonicalDecodePrepareReferenceScriptOutRef({
    deploymentInfo,
    expectedScriptHash,
  });
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
 * Auth-role name minted onto the published CEK direct-resolver
 * reference-script UTxO (`REFERENCE_SCRIPT_AUTH_TOKEN_NAMES` in
 * `@al-ft/midgard-sdk`).
 */
export const VALIDATION_CEK_DIRECT_RESOLVER_REFERENCE_SCRIPT_ROLE =
  "V1 validation-trace CEK direct resolver";

/**
 * Deployment-info entry that publishes the applied `cek_v1` direct resolver
 * (direct resolver 0) as an authenticated L1 reference script. Every CEK
 * finalization transaction must consume the resolver by reference: the
 * applied resolver body is 156,467 bytes, so embedding it in the proof
 * transaction can never fit the 16,384-byte L1 envelope
 * (`MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES`).
 */
export const VALIDATION_CEK_DIRECT_RESOLVER_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY =
  "validationTraceDisputeCekDirectResolver";

export const requireValidationCekDirectResolverReferenceScriptOutRef = ({
  deploymentInfo,
  expectedScriptHash,
}: {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly expectedScriptHash: string;
}): { readonly txHash: string; readonly outputIndex: number } => {
  const entry =
    deploymentInfo[
      VALIDATION_CEK_DIRECT_RESOLVER_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY
    ];
  if (entry === undefined) {
    throw new Error(
      `Deployment info is missing "${VALIDATION_CEK_DIRECT_RESOLVER_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY}"; publish the authenticated V1 CEK direct-resolver reference script and regenerate deployment info before submitting a CEK direct resolution`,
    );
  }
  if (entry.refScriptUTxO == null) {
    throw new Error(
      `Deployment info entry "${VALIDATION_CEK_DIRECT_RESOLVER_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY}" is missing refScriptUTxO; publish the authenticated V1 CEK direct-resolver reference script and regenerate deployment info before submitting a CEK direct resolution`,
    );
  }
  if (entry.scriptHash !== expectedScriptHash) {
    throw new Error(
      `Deployment entry "${VALIDATION_CEK_DIRECT_RESOLVER_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY}" script hash mismatch: deployment=${entry.scriptHash}, derived=${expectedScriptHash}`,
    );
  }
  return entry.refScriptUTxO;
};

export const requireValidationCekDirectResolverReferenceScriptUtxo = async ({
  lucid,
  deploymentInfo,
  expectedScriptHash,
  authPolicyId,
}: {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly expectedScriptHash: string;
  readonly authPolicyId: string;
}): Promise<UTxO> => {
  const outRef = requireValidationCekDirectResolverReferenceScriptOutRef({
    deploymentInfo,
    expectedScriptHash,
  });
  const utxo = await fetchUtxoByOutRef({
    lucid,
    outRef,
    label: "CEK direct-resolver reference-script UTxO",
  });
  if (utxo.scriptRef == null) {
    throw new Error(
      `CEK direct-resolver reference UTxO ${outRefLabel(utxo)} does not carry a reference script`,
    );
  }
  const actualScriptHash = validatorToScriptHash(utxo.scriptRef);
  if (actualScriptHash !== expectedScriptHash) {
    throw new Error(
      `CEK direct-resolver reference script hash mismatch: actual=${actualScriptHash}, expected=${expectedScriptHash}`,
    );
  }
  const expectedRoleUnit = referenceScriptAuthUnit(
    authPolicyId,
    VALIDATION_CEK_DIRECT_RESOLVER_REFERENCE_SCRIPT_ROLE,
  );
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
      `CEK direct-resolver reference UTxO ${outRefLabel(utxo)} must carry exactly one ${expectedRoleUnit} auth-role token`,
    );
  }
  return utxo;
};

const VALIDATION_ONE_STEP_EVIDENCE_DOMAIN_V1 = Buffer.from(
  "MidgardValidationOneStepEvidenceV1",
  "ascii",
);

type PlutusDataValue = Data;
type PlutusDataSchema = Parameters<typeof Data.Nullable>[0];

type RuntimeSchemaEncoder = (
  data: unknown,
  schema: PlutusDataSchema,
) => ReturnType<typeof Data.to>;

/*
 * Lucid's runtime encoder accepts a TypeBox schema as its second argument, but
 * its public generic types model that argument as the decoded static value.
 * Expanding Exact<T> for the 42-variant auxiliary witness exceeds TypeScript's
 * instantiation limit. Keep each caller's value exactly typed and isolate that
 * declaration mismatch at this runtime-schema boundary.
 */
const encodeWithRuntimeSchema = Data.to as unknown as RuntimeSchemaEncoder;
const validationDirectResolveSpendRedeemerV1RuntimeSchema =
  ValidationDirectResolveSpendRedeemerV1Schema as unknown as PlutusDataSchema;
const validationCekSpendRedeemerV1RuntimeSchema =
  ValidationCekSpendRedeemerV1Schema as unknown as PlutusDataSchema;
const validationPrepareSelectedSpendRedeemerV1RuntimeSchema =
  ValidationPrepareSelectedSpendRedeemerV1Schema as unknown as PlutusDataSchema;
const validationCanonicalDecodePrepareSelectedSpendRedeemerV1RuntimeSchema =
  ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1Schema as unknown as PlutusDataSchema;

type ValidationDirectResolveActionV1Input = {
  readonly input_index: bigint;
  readonly output_index: bigint;
  readonly fraud_proof_mint_redeemer_index: bigint;
  readonly challenger_evidence: ValidationOneStepEvidenceV1;
};

export const encodeValidationDirectResolveSpendRedeemerV1 = (
  action: ValidationDirectResolveActionV1Input,
): ReturnType<typeof Data.to> =>
  encodeWithRuntimeSchema(
    { Continue: [action] },
    validationDirectResolveSpendRedeemerV1RuntimeSchema,
  );

type ValidationCekResolveActionV1Input =
  ValidationDirectResolveActionV1Input & {
    readonly material_route: ValidationCekMaterialRouteV1;
  };

export const encodeValidationCekSpendRedeemerV1 = (
  action: ValidationCekResolveActionV1Input,
): ReturnType<typeof Data.to> =>
  encodeWithRuntimeSchema(
    { Continue: [action] },
    validationCekSpendRedeemerV1RuntimeSchema,
  );

export type ValidationOneStepSubmissionArgumentV1 = {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number | null;
  readonly transitionCbor: Uint8Array;
  readonly auxiliaryCbor: Uint8Array;
  readonly cekRouteMaterial?: CekRouteMaterialV1;
  /** Presence selects the receipt-justified incremental route. */
  readonly cekIncrementalNecessityReceiptSet?: CekProgramMaterialNecessityReceiptSetV1;
};

export type ValidatedCekSubmissionEvidenceV1 = {
  readonly cekRouteMaterial?: CekRouteMaterialV1;
  readonly cekIncrementalNecessityReceiptSet?: CekProgramMaterialNecessityReceiptSetV1;
};

/**
 * The §8 carriage a tiers-2/3 one-step argument already committed to, together
 * with the ledger UTxOs its positional indices name (#600, ruling D3-A).
 *
 * A tier-1 argument needs none of this: `Inline` carries its own bytes and
 * indexes nothing. Above §8.3's 14,336-byte cap the auxiliary is *only*
 * indices, so a submitter has to hand the builder the material those indices
 * point at — otherwise the door-running transaction cannot reference the
 * carriage at all, and nothing off chain can tell whether the committed indices
 * still resolve.
 *
 * `plan` is the producer's own `planMidgardFieldCarriageV1` output, never a
 * hand-assembled record: it is what lets the builder re-resolve the indices by
 * content (§8.7) against the door's final reference-input set and refuse a
 * divergence while re-staging is still free.
 */
export type ValidationFieldCarriageMaterialV1 = {
  readonly plan: MidgardFieldCarriagePlanV1;
  /**
   * The carriage UTxOs the door transaction must read: the plan's publications
   * under tier 2, and the publications plus the §8.6 certificate under tier 3.
   */
  readonly referenceUtxos: readonly UTxO[];
  /** Required at tier 3; the §8.6 minting policy the door is parameterised by. */
  readonly certificatePolicyId?: string;
};

/**
 * The committed carriage, read back out of the staged auxiliary as the
 * `MidgardFieldCarriageV1` the SDK resolvers speak.
 *
 * Constructor order is the frozen §8.1 one — `Inline` 0, `RawUtxo` 1,
 * `Certified` 2 — and every arm is checked for arity rather than pattern-matched
 * loosely, because this value is the one `evidence_hash` already committed and a
 * misread here would be a silently different carriage.
 */
const midgardFieldCarriageFromDataV1 = (
  value: PlutusDataValue,
  label: string,
): MidgardFieldCarriageV1 => {
  if (!(value instanceof Constr)) {
    throw new Error(`${label} is not a §8 FieldCarriageV1 constructor`);
  }
  if (value.index === 0 && value.fields.length === 1) {
    const preimage = value.fields[0];
    if (typeof preimage !== "string" || preimage.length % 2 !== 0) {
      throw new Error(`${label} tier-1 Inline bytes are malformed`);
    }
    return { carriage: "Inline", preimage: Buffer.from(preimage, "hex") };
  }
  if (value.index === 1 && value.fields.length === 1) {
    const refInputIndex = value.fields[0];
    if (typeof refInputIndex !== "bigint" || refInputIndex < 0n) {
      throw new Error(`${label} tier-2 reference-input index is malformed`);
    }
    return { carriage: "RawUtxo", refInputIndex: Number(refInputIndex) };
  }
  if (value.index === 2 && value.fields.length === 2) {
    const certRefInputIndex = value.fields[0];
    const chunkRefInputIndices = value.fields[1];
    if (typeof certRefInputIndex !== "bigint" || certRefInputIndex < 0n) {
      throw new Error(`${label} tier-3 certificate index is malformed`);
    }
    if (!Array.isArray(chunkRefInputIndices)) {
      throw new Error(`${label} tier-3 chunk index vector is malformed`);
    }
    return {
      carriage: "Certified",
      certRefInputIndex: Number(certRefInputIndex),
      chunkRefInputIndices: chunkRefInputIndices.map((index) => {
        if (typeof index !== "bigint" || index < 0n) {
          throw new Error(`${label} tier-3 chunk index is malformed`);
        }
        return Number(index);
      }),
    };
  }
  throw new Error(`${label} is not a §8 FieldCarriageV1 constructor`);
};

const exactPlutusDataFromCbor = (
  value: Uint8Array,
  label: string,
): PlutusDataValue => {
  const bytes = Buffer.from(value);
  if (
    bytes.length === 0 ||
    bytes.length >= MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES
  ) {
    throw new Error(
      `${label} must be non-empty and strictly below the L1 proof envelope`,
    );
  }
  const decoded = Data.from(bytes.toString("hex"));
  const encoded = Buffer.from(Data.to(decoded), "hex");
  if (!encoded.equals(bytes)) {
    throw new Error(`${label} is not exact canonical V1 Plutus Data`);
  }
  return decoded;
};

/**
 * Enforces the selection-only C28 evidence ABI before any transaction builder
 * uses it. A Plutus/Midgard CEK selection must carry complete route material;
 * later CEK steps, ValueAndMint, and every staged phase must not carry it.
 */
export const validateCekSubmissionEvidenceV1 = (
  argument: ValidationOneStepSubmissionArgumentV1,
): ValidatedCekSubmissionEvidenceV1 => {
  exactPlutusDataFromCbor(
    argument.auxiliaryCbor,
    "validation auxiliary witness",
  );
  const auxiliaryWitness = Data.from(
    Buffer.from(argument.auxiliaryCbor).toString("hex"),
    ValidationAuxiliaryWitnessV1,
  );
  const selection =
    typeof auxiliaryWitness === "object" &&
    auxiliaryWitness !== null &&
    "NativeExecutionScanWitness" in auxiliaryWitness
      ? auxiliaryWitness.NativeExecutionScanWitness
      : undefined;
  const isProgramSelection =
    argument.resolverIndex === 11 &&
    argument.semanticResolverIndex === null &&
    selection !== undefined &&
    (selection.language_tag === 3n || selection.language_tag === 128n);
  if (!isProgramSelection) {
    if (
      argument.cekRouteMaterial !== undefined ||
      argument.cekIncrementalNecessityReceiptSet !== undefined
    ) {
      throw new Error(
        "CEK route material and necessity receipts are permitted only for an exact program-selection witness",
      );
    }
    return Object.freeze({});
  }
  if (argument.cekRouteMaterial === undefined) {
    throw new Error(
      "Plutus/Midgard CEK selection requires complete route material",
    );
  }
  const cekRouteMaterial = validateCekRouteMaterialV1({
    value: argument.cekRouteMaterial,
    firstSourceChunk: Buffer.from(selection.first_chunk_proof.chunk, "hex"),
    languageTag: Number(selection.language_tag) as 3 | 128,
  });
  if (argument.cekIncrementalNecessityReceiptSet === undefined) {
    return Object.freeze({ cekRouteMaterial });
  }
  const cekIncrementalNecessityReceiptSet =
    parseCekProgramMaterialNecessityReceiptSetV1(
      argument.cekIncrementalNecessityReceiptSet,
    );
  if (
    cekIncrementalNecessityReceiptSet.programEnvelopeHash !==
    cekRouteMaterial.programEnvelopeHash.toString("hex")
  ) {
    throw new Error(
      "CEK incremental necessity receipts are bound to another program envelope",
    );
  }
  return Object.freeze({
    cekRouteMaterial,
    cekIncrementalNecessityReceiptSet,
  });
};

const requireConstr = ({
  value,
  index,
  fields,
  label,
}: {
  readonly value: PlutusDataValue;
  readonly index: number;
  readonly fields: number;
  readonly label: string;
}): Constr<PlutusDataValue> => {
  if (
    !(value instanceof Constr) ||
    value.index !== index ||
    value.fields.length !== fields
  ) {
    throw new Error(
      `${label} must be constructor ${index.toString()} with ${fields.toString()} fields`,
    );
  }
  return value;
};

const validationOneStepEvidenceHashFromDataV1 = (
  transition: PlutusDataValue,
  auxiliary: PlutusDataValue,
): string => {
  const evidencePayload = Buffer.from(Data.to([transition, auxiliary]), "hex");
  return computeHash32(
    Buffer.concat([VALIDATION_ONE_STEP_EVIDENCE_DOMAIN_V1, evidencePayload]),
  ).toString("hex");
};

export const validationOneStepEvidenceHashV1 = ({
  transitionCbor,
  auxiliaryCbor,
}: Pick<
  ValidationOneStepSubmissionArgumentV1,
  "transitionCbor" | "auxiliaryCbor"
>): string =>
  validationOneStepEvidenceHashFromDataV1(
    exactPlutusDataFromCbor(transitionCbor, "validation transition"),
    exactPlutusDataFromCbor(auxiliaryCbor, "validation auxiliary witness"),
  );

const VALIDATION_SEMANTIC_RESOLVER_COUNTS_V1 = [
  2, 1, 1, 2, 4, 14, 2, 6, 29, 3, 4, 0, 0, 8,
] as const;
const VALIDATION_SEMANTIC_RESOLVER_OFFSETS_V1 = [
  0, 2, 3, 4, 6, 10, 24, 26, 32, 60, 63, -1, -1, 67,
] as const;

const VALIDATION_AUXILIARY_SHAPES_V1 = {
  none: [0, 0],
  // #597: the four §8-door constructors carry a `FieldCarriageV1` where they
  // used to carry counted openings. `TransactionFieldChunkWitness` is
  // `(field_index, item_index, carriage)`, `RequiredSignerItemWitness` is
  // `(carriage, signer_proof)`, and the two begin/item constructors are
  // `(carriage)` alone. Constructor *indices* are unchanged — only the shapes
  // moved (`onchain/aiken/lib/midgard/validation-machine-v1.ak:119`).
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
} as const satisfies Record<string, readonly [number, number]>;

const hasValidationAuxiliaryShapeV1 = (
  auxiliary: Constr<PlutusDataValue>,
  shape: readonly [number, number],
): boolean =>
  auxiliary.index === shape[0] && auxiliary.fields.length === shape[1];

const auxiliaryShapeV1 = ({
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
        index: VALIDATION_AUXILIARY_SHAPES_V1.none[0],
        fields: VALIDATION_AUXILIARY_SHAPES_V1.none[1],
        label: "validation CanonicalDecode empty auxiliary witness",
      });
    }
    if (
      auxiliary instanceof Constr &&
      (hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldChunk,
      ) ||
        hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldItem,
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
        ? VALIDATION_AUXILIARY_SHAPES_V1.none
        : semanticResolverIndex === 0
          ? VALIDATION_AUXILIARY_SHAPES_V1.ledgerDeltaOperation
          : semanticResolverIndex === 1
            ? VALIDATION_AUXILIARY_SHAPES_V1.ledgerDeltaReplay
            : semanticResolverIndex === 3
              ? VALIDATION_AUXILIARY_SHAPES_V1.ledgerDeltaOutput
              : VALIDATION_AUXILIARY_SHAPES_V1.ledgerDeltaProofFrame;
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
        ? VALIDATION_AUXILIARY_SHAPES_V1.none
        : semanticResolverIndex === 2
          ? VALIDATION_AUXILIARY_SHAPES_V1.scheduledLedgerMembership
          : semanticResolverIndex === 3
            ? VALIDATION_AUXILIARY_SHAPES_V1.ledgerOutputProofStep
            : semanticResolverIndex === 4
              ? VALIDATION_AUXILIARY_SHAPES_V1.ledgerOutputProofFinalize
              : semanticResolverIndex === 5
                ? VALIDATION_AUXILIARY_SHAPES_V1.scheduledLedgerNonMembership
                : VALIDATION_AUXILIARY_SHAPES_V1.resolvedInputReplay;
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
    const isRedeemerItemStage = hasValidationAuxiliaryShapeV1(
      auxiliary,
      VALIDATION_AUXILIARY_SHAPES_V1.redeemerItemStep,
    );
    if (semanticResolverIndex === 28 && !isRedeemerItemStage) {
      throw new Error(
        "validation auxiliary witness does not match the selected ScriptSources split stage-one proof family",
      );
    }
    if (
      semanticResolverIndex === 15 &&
      !(
        hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.transactionRedeemerItemBegin,
        ) || isRedeemerItemStage
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
        hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.redeemerScanBegin,
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
          ? VALIDATION_AUXILIARY_SHAPES_V1.ledgerOutputProofBegin
          : semanticResolverIndex === 2
            ? VALIDATION_AUXILIARY_SHAPES_V1.ledgerOutputProofStep
            : semanticResolverIndex === 3
              ? VALIDATION_AUXILIARY_SHAPES_V1.ledgerOutputProofFinalize
              : semanticResolverIndex === 5
                ? VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldChunk
                : semanticResolverIndex === 7
                  ? VALIDATION_AUXILIARY_SHAPES_V1.scriptSourceHashBlock
                  : semanticResolverIndex >= 10 && semanticResolverIndex <= 12
                    ? VALIDATION_AUXILIARY_SHAPES_V1.scriptSourceScan
                    : semanticResolverIndex === 17
                      ? VALIDATION_AUXILIARY_SHAPES_V1.scriptSourceScan
                      : semanticResolverIndex === 19
                        ? null
                        : semanticResolverIndex === 21 ||
                            semanticResolverIndex === 22
                          ? null
                          : semanticResolverIndex === 24
                            ? VALIDATION_AUXILIARY_SHAPES_V1.scriptPurposeScan
                            : semanticResolverIndex === 25
                              ? VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldChunk
                              : semanticResolverIndex === 26
                                ? VALIDATION_AUXILIARY_SHAPES_V1.scriptPurposeScan
                                : semanticResolverIndex === 15 ||
                                    semanticResolverIndex === 28
                                  ? null
                                  : VALIDATION_AUXILIARY_SHAPES_V1.none;
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
  if (resolverIndex === 9) {
    const expected =
      semanticResolverIndex === 0
        ? VALIDATION_AUXILIARY_SHAPES_V1.none
        : VALIDATION_AUXILIARY_SHAPES_V1.nativeExecutionDescriptor;
    return requireConstr({
      value: auxiliary,
      index: expected[0],
      fields: expected[1],
      label: "validation NativeScripts auxiliary witness",
    });
  }
  const expected =
    resolverIndex === 1 || resolverIndex === 2 || resolverIndex === 10
      ? VALIDATION_AUXILIARY_SHAPES_V1.none
      : resolverIndex === 3
        ? semanticResolverIndex === 0
          ? VALIDATION_AUXILIARY_SHAPES_V1.none
          : VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldChunk
        : resolverIndex === 4
          ? semanticResolverIndex === 0 || semanticResolverIndex === 3
            ? VALIDATION_AUXILIARY_SHAPES_V1.none
            : semanticResolverIndex === 1
              ? VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldChunk
              : VALIDATION_AUXILIARY_SHAPES_V1.requiredSignerItem
          : resolverIndex === 5
            ? semanticResolverIndex === 0
              ? VALIDATION_AUXILIARY_SHAPES_V1.none
              : semanticResolverIndex === 1
                ? VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldChunk
                : semanticResolverIndex === 13
                  ? VALIDATION_AUXILIARY_SHAPES_V1.nativeScriptFrame
                  : VALIDATION_AUXILIARY_SHAPES_V1.nativeScriptToken
            : resolverIndex === 6
              ? semanticResolverIndex === 0
                ? VALIDATION_AUXILIARY_SHAPES_V1.none
                : VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldChunk
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

const requireStagedOneStepArgumentV1 = (
  argument: ValidationOneStepSubmissionArgumentV1,
): {
  readonly transition: ValidationOneStepWitnessV1;
  readonly transitionData: PlutusDataValue;
  readonly auxiliaryData: PlutusDataValue;
  readonly auxiliaryWitness: ValidationAuxiliaryWitnessV1Data;
  readonly auxiliary: Constr<PlutusDataValue>;
  readonly semanticResolverIndex: number;
  readonly semanticResolverGlobalIndex: number;
  readonly evidenceHash: string;
} => {
  validateCekSubmissionEvidenceV1(argument);
  if (
    !Number.isSafeInteger(argument.resolverIndex) ||
    argument.resolverIndex < 0 ||
    argument.resolverIndex >= VALIDATION_SEMANTIC_RESOLVER_COUNTS_V1.length
  ) {
    throw new Error(
      "Staged validation one-step argument must select a prepare resolver",
    );
  }
  const semanticResolverIndex = argument.semanticResolverIndex;
  const semanticResolverCount =
    VALIDATION_SEMANTIC_RESOLVER_COUNTS_V1[argument.resolverIndex]!;
  if (
    semanticResolverIndex === null ||
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
    ValidationAuxiliaryWitnessV1,
  );
  const transition = Data.from(
    Buffer.from(argument.transitionCbor).toString("hex"),
    ValidationOneStepWitnessV1,
  );
  const auxiliary = auxiliaryShapeV1({
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
    semanticResolverGlobalIndex: validationSemanticResolverGlobalIndexV1(
      argument.resolverIndex,
      semanticResolverIndex,
    ),
    evidenceHash: validationOneStepEvidenceHashFromDataV1(
      transitionData,
      auxiliaryData,
    ),
  };
};

export const validationSemanticResolverGlobalIndexV1 = (
  resolverIndex: number,
  semanticResolverIndex: number,
): number =>
  resolverIndex === 8 && semanticResolverIndex === 28
    ? 75
    : VALIDATION_SEMANTIC_RESOLVER_OFFSETS_V1[resolverIndex]! +
      semanticResolverIndex;

export const encodeScriptSourcesStageOneSpendRedeemerV1 = ({
  stage,
  inputIndex,
  outputIndex,
  transition,
  auxiliary,
  expectedNextItemControlHash,
  family,
  currentItemControl,
  traversalAction,
  envelope,
}: {
  readonly stage:
    | "envelope"
    | "traversal"
    | "outer"
    | "executor"
    | "settlement";
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly transition?: PlutusDataValue;
  readonly auxiliary?: PlutusDataValue;
  readonly expectedNextItemControlHash?: string;
  readonly family?: number;
  readonly currentItemControl?: PlutusDataValue;
  readonly traversalAction?: PlutusDataValue;
  readonly envelope?: PlutusDataValue;
}): string => {
  const required = <T>(value: T | undefined, label: string): T => {
    if (value === undefined) throw new Error(`${label} is required`);
    return value;
  };
  let fields: readonly PlutusDataValue[];
  if (stage === "envelope") {
    const selectedFamily = required(family, "ScriptSources action family");
    if (selectedFamily !== 0 && selectedFamily !== 1) {
      throw new Error(
        "ScriptSources action family must be FoldMap or FinalizeFrame",
      );
    }
    fields = [
      inputIndex,
      outputIndex,
      required(transition, "ScriptSources transition"),
      required(auxiliary, "ScriptSources auxiliary witness"),
      required(
        expectedNextItemControlHash,
        "ScriptSources expected next item-control hash",
      ),
      BigInt(selectedFamily),
    ];
  } else if (stage === "traversal") {
    fields = [
      inputIndex,
      outputIndex,
      required(auxiliary, "ScriptSources auxiliary witness"),
      required(currentItemControl, "ScriptSources current item control"),
      required(traversalAction, "ScriptSources traversal action"),
    ];
  } else if (stage === "outer") {
    fields = [inputIndex, outputIndex];
  } else if (stage === "executor") {
    fields = [
      inputIndex,
      outputIndex,
      required(traversalAction, "ScriptSources traversal action"),
    ];
  } else {
    fields = [
      inputIndex,
      outputIndex,
      required(envelope, "ScriptSources prepared envelope"),
    ];
  }
  return Data.to(new Constr(1, [new Constr(0, [...fields])]));
};

const requireDirectOneStepArgumentV1 = (
  argument: ValidationOneStepSubmissionArgumentV1,
): {
  readonly evidence: ValidationOneStepEvidenceV1;
  readonly cekRouteMaterial?: CekRouteMaterialV1;
  readonly cekIncrementalNecessityReceiptSet?: CekProgramMaterialNecessityReceiptSetV1;
} => {
  const validatedCekEvidence = validateCekSubmissionEvidenceV1(argument);
  if (
    !Number.isSafeInteger(argument.resolverIndex) ||
    argument.resolverIndex < 11 ||
    argument.resolverIndex > 12 ||
    argument.semanticResolverIndex !== null
  ) {
    throw new Error(
      "Direct validation one-step argument must select resolver 11 or 12",
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
  Data.from(
    Buffer.from(argument.transitionCbor).toString("hex"),
    ValidationOneStepWitnessV1,
  );
  const evidenceData = new Constr(0, [transitionData, auxiliaryData]);
  const evidenceCbor = Data.to(evidenceData);
  return {
    evidence: Data.from(evidenceCbor, ValidationOneStepEvidenceV1),
    ...validatedCekEvidence,
  };
};

type ContinueLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

type OpenLayout = ContinueLayout & {
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

const makeOpenRedeemer = ({
  threadUtxo,
  hubOracleUtxo,
  stateQueueBlockUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  claim,
  challengerDescriptor,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly hubOracleUtxo: UTxO;
  readonly stateQueueBlockUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly claim: ValidationClaimWitnessV1;
  readonly challengerDescriptor: ValidationTraceDescriptorV1;
  readonly onLayout: (layout: OpenLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "validation dispute open");
    const layout: OpenLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "validation dispute open"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "validation dispute open",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "validation dispute open hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "validation dispute open state-queue block",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            hub_ref_input_index: layout.hubOracleRefInputIndex,
            state_queue_node_ref_input_index:
              layout.stateQueueNodeRefInputIndex,
            claim,
            challenger_descriptor: challengerDescriptor,
          },
        ],
      },
      ValidationDisputeOpenSpendRedeemerV1,
    );
  }) satisfies BuildTxWithRedeemer;

const makeVerifySourceRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "validation dispute verify source");
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "validation dispute verify source",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "validation dispute verify source",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
          },
        ],
      },
      ValidationSourceSpendRedeemerV1,
    );
  }) satisfies BuildTxWithRedeemer;

const makeRevealRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  role,
  proof,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly role: "operator" | "challenger";
  readonly proof: ValidationTraceProofV1;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      `validation dispute reveal ${role}`,
    );
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        `validation dispute reveal ${role}`,
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        `validation dispute reveal ${role}`,
      ),
    };
    onLayout(layout);
    const action =
      role === "operator"
        ? {
            RevealOperator: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              proof,
            },
          }
        : {
            RevealChallenger: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              proof,
            },
          };
    return Data.to({ Continue: [action] }, ValidationGameSpendRedeemerV1);
  }) satisfies BuildTxWithRedeemer;

const makeGameHandoffRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  destination,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly destination: "resolution" | "challengerTimeout";
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    const label = `validation dispute enter ${destination}`;
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        label,
      ),
    };
    onLayout(layout);
    const action =
      destination === "resolution"
        ? {
            EnterResolution: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
            },
          }
        : {
            EnterChallengerTimeout: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
            },
          };
    return Data.to({ Continue: [action] }, ValidationGameSpendRedeemerV1);
  }) satisfies BuildTxWithRedeemer;

export type SubmitValidationDisputeOpenResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly responseDeadline: number;
  readonly awaitedConfirmation: boolean;
};

export type BuildValidationDisputeOpenParams = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly claim: ValidationClaimWitnessV1;
  readonly challengerDescriptor: ValidationTraceDescriptorV1;
  readonly validityRange?: ValidationDisputeValidityRange;
};

export type BuildValidationDisputeOpenResult = {
  readonly signed: TxSigned;
  readonly threadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly responseDeadline: number;
};

export const buildValidationDisputeOpen = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  claim,
  challengerDescriptor,
  validityRange = validationDisputeValidityRange(Date.now()),
}: BuildValidationDisputeOpenParams): Promise<BuildValidationDisputeOpenResult> => {
  const range = requireValidityRange(validityRange);
  const resolved = await resolveValidationTraceDisputeDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    requireStateQueueMint: true,
  });
  const {
    deploymentInfo: parsedDeploymentInfo,
    referenceScriptAuthPolicyId,
    validationTraceDisputeCategory,
    hubOraclePolicyId,
    contracts,
  } = resolved;
  const stateQueuePolicyId = resolved.stateQueuePolicyId!;
  const disputeContract = contracts.validationTraceDispute.firstStep;
  const disputeDeploymentEntry = parsedDeploymentInfo.validationTraceDispute;
  if (disputeDeploymentEntry === undefined) {
    throw new Error('Deployment info is missing "validationTraceDispute"');
  }
  if (disputeDeploymentEntry.refScriptUTxO == null) {
    throw new Error(
      'Deployment info entry "validationTraceDispute" is missing refScriptUTxO; publish the authenticated V1 validation-trace dispute reference script and regenerate deployment info before opening a dispute',
    );
  }
  const [threadUtxo, hubOracleUtxo, stateQueueBlockUtxo, disputeReferenceUtxo] =
    await Promise.all([
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
        label: "validation-dispute computation-thread UTxO",
      }),
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(hubOraclePolicyId),
        ),
        unit: toUnit(hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
        label: "hub oracle",
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(
          stateQueueBlockOutRef,
          "--state-queue-block-out-ref",
        ),
        label: "validation-dispute state-queue block UTxO",
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: disputeDeploymentEntry.refScriptUTxO,
        label: "validation-dispute authenticated reference-script UTxO",
      }),
    ]);
  requireValidationDisputeReferenceScript({
    utxo: disputeReferenceUtxo,
    deployedScriptHash: disputeDeploymentEntry.scriptHash,
    expectedScriptHash: disputeContract.spendingScriptHash,
    authPolicyId: referenceScriptAuthPolicyId,
  });
  if (threadUtxo.address !== disputeContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  requireInitialStepDatum({ threadUtxo, signer });
  const fraudulentHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (fraudulentHeaderHash !== token.fraudulentHeaderHash) {
    throw new Error(
      `State-queue block header hash ${fraudulentHeaderHash} does not match computation-thread header hash ${token.fraudulentHeaderHash}`,
    );
  }
  const stateQueueNodeView = await Effect.runPromise(
    getLinkedListNodeViewFromUTxO(stateQueueBlockUtxo),
  );
  const header = await Effect.runPromise(
    getHeaderV1FromStateQueueDatum(stateQueueNodeView),
  );
  const computedHeaderHash = await Effect.runPromise(hashBlockHeaderV1(header));
  if (computedHeaderHash !== fraudulentHeaderHash) {
    throw new Error(
      `State-queue datum header hashes to ${computedHeaderHash}, expected ${fraudulentHeaderHash}`,
    );
  }
  const operatorDescriptor = validationTraceDescriptorCoreFromData(
    claim.descriptor_membership.value,
  );
  const challengerDescriptorCore =
    validationTraceDescriptorCoreFromData(challengerDescriptor);
  const currentTimeUpper = inclusiveValidityUpperBound(range);
  const dispute = openMidgardValidationDispute({
    operatorDescriptor,
    challengerDescriptor: challengerDescriptorCore,
    currentTime: currentTimeUpper,
  });
  if (
    !canOpenMidgardValidationDisputeBeforeMaturity({
      currentTimeUpper,
      challengedBlockEndTime: safeUnsignedNumber(
        header.endTime,
        "header.endTime",
      ),
      maturityDuration: MIDGARD_CONSENSUS_LIMITS_V1.blockMaturityMs,
    })
  ) {
    throw new Error(
      "Validation dispute cannot complete before the challenged block matures",
    );
  }
  const outputDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        challenged_header_hash: fraudulentHeaderHash,
        challenged_header: header,
        claim,
        challenger_descriptor: challengerDescriptor,
        open_time_upper: BigInt(currentTimeUpper),
      },
    },
    PendingValidationClaimDatumV1,
  );
  let layout: OpenLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeOpenRedeemer({
        threadUtxo,
        hubOracleUtxo,
        stateQueueBlockUtxo,
        outputAddress:
          contracts.validationTraceDispute.source.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        claim,
        challengerDescriptor,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .readFrom([hubOracleUtxo, stateQueueBlockUtxo, disputeReferenceUtxo])
    .pay.ToContract(
      contracts.validationTraceDispute.source.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute open layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  return {
    signed,
    threadOutRef,
    fraudulentHeaderHash,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    hubOracleRefInputIndex: Number(layout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(layout.stateQueueNodeRefInputIndex),
    responseDeadline: dispute.responseDeadline,
  };
};

export const submitValidationDisputeOpen = async ({
  awaitConfirmation = true,
  ...params
}: BuildValidationDisputeOpenParams & {
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeOpenResult> => {
  const built = await buildValidationDisputeOpen(params);
  requireL1ProofEnvelope(built.signed.toCBOR(), "Validation-dispute open");
  const txHash = await built.signed.submit();
  if (awaitConfirmation) {
    await params.lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef: built.threadOutRef,
    nextThreadOutRef: `${txHash}#${built.outputIndex.toString()}`,
    fraudulentHeaderHash: built.fraudulentHeaderHash,
    inputIndex: built.inputIndex,
    outputIndex: built.outputIndex,
    hubOracleRefInputIndex: built.hubOracleRefInputIndex,
    stateQueueNodeRefInputIndex: built.stateQueueNodeRefInputIndex,
    responseDeadline: built.responseDeadline,
    awaitedConfirmation: awaitConfirmation,
  };
};

const requirePendingClaimDatum = (
  threadUtxo: UTxO,
): PendingValidationClaimDatumV1Data & {
  readonly data: NonNullable<PendingValidationClaimDatumV1Data["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Validation-dispute source UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(threadUtxo.datum, PendingValidationClaimDatumV1);
  if (datum.data === null) {
    throw new Error(
      "Validation-dispute source verification requires pending claim state",
    );
  }
  return datum as PendingValidationClaimDatumV1Data & {
    readonly data: NonNullable<PendingValidationClaimDatumV1Data["data"]>;
  };
};

export type SubmitValidationDisputeVerifySourceResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly responseDeadline: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputeVerifySource = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeVerifySourceResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation-dispute source-verification UTxO",
  });
  const sourceContract = contracts.validationTraceDispute.source;
  if (threadUtxo.address !== sourceContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute source validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requirePendingClaimDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation-dispute source verification requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const operatorDescriptor = validationTraceDescriptorCoreFromData(
    inputDatum.data.claim.descriptor_membership.value,
  );
  const challengerDescriptor = validationTraceDescriptorCoreFromData(
    inputDatum.data.challenger_descriptor,
  );
  const dispute = openValidationDisputeAfterSourceVerification({
    operatorDescriptor,
    challengerDescriptor,
    openTimeUpper: inputDatum.data.open_time_upper,
    challengedBlockEndTime: inputDatum.data.challenged_header.endTime,
    sourceValidityRange: range,
  });
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: {
        challenged_header_hash: inputDatum.data.challenged_header_hash,
        operator_vkey: inputDatum.data.challenged_header.operatorVkey,
        dispute: validationDisputeDataFromCore(dispute),
      },
    },
    ValidationDisputeDatumV1,
  );
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeVerifySourceRedeemer({
        threadUtxo,
        outputAddress:
          contracts.validationTraceDispute.game.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      contracts.validationTraceDispute.game.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(sourceContract.spendingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute source layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(
    signed.toCBOR(),
    "Validation-dispute source verification",
  );
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    responseDeadline: dispute.responseDeadline,
    awaitedConfirmation: awaitConfirmation,
  };
};

export type SubmitValidationDisputeRevealResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly role: "operator" | "challenger";
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly responseDeadline: number;
  readonly awaitedConfirmation: boolean;
};

type FinalizeLayout = ContinueLayout & {
  readonly fraudProofMintRedeemerIndex: bigint;
  readonly computationThreadMintRedeemerIndex: bigint;
};

const makeTimeoutSpendRedeemer = ({
  threadUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly onLayout: (
    layout: Omit<FinalizeLayout, "computationThreadMintRedeemerIndex">,
  ) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "validation dispute timeout");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "validation dispute timeout",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputWithDatumAndUnitPredicate({
          address: fraudProofAddress,
          datum: fraudProofDatum,
          unit: fraudProofUnit,
        }),
        "validation dispute timeout fraud proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        "validation dispute timeout fraud-proof mint",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
          },
        ],
      },
      ValidationTimeoutSpendRedeemerV1,
    );
  }) satisfies BuildTxWithRedeemer;

const makeFraudProofMintRedeemer = ({
  fraudProofPolicyId,
  computationThreadPolicyId,
  computationThreadAssetName,
  onComputationThreadMintRedeemerIndex,
}: {
  readonly fraudProofPolicyId: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly onComputationThreadMintRedeemerIndex: (index: bigint) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      fraudProofPolicyId,
      "validation dispute fraud-proof mint",
    );
    const computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThreadPolicyId,
      "validation dispute computation-thread burn",
    );
    onComputationThreadMintRedeemerIndex(computationThreadMintRedeemerIndex);
    return Data.to(
      {
        computation_thread_token_asset_name: computationThreadAssetName,
        computation_thread_mint_redeemer_index:
          computationThreadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

const makeComputationThreadSuccessRedeemer = ({
  computationThreadPolicyId,
  computationThreadAssetName,
}: {
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      computationThreadPolicyId,
      "validation dispute computation-thread burn",
    );
    return Data.to(
      {
        Success: { burning_token_asset_name: computationThreadAssetName },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

const requireDisputeDatum = (
  threadUtxo: UTxO,
): ValidationDisputeDatumV1Data & {
  readonly data: NonNullable<ValidationDisputeDatumV1Data["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Validation-dispute thread UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(threadUtxo.datum, ValidationDisputeDatumV1);
  if (datum.data === null) {
    throw new Error("Validation-dispute reveal requires initialized state");
  }
  return datum as ValidationDisputeDatumV1Data & {
    readonly data: NonNullable<ValidationDisputeDatumV1Data["data"]>;
  };
};

export const submitValidationDisputeReveal = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  role,
  proof,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly role: "operator" | "challenger";
  readonly proof: MidgardValidationTraceProofV1;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeRevealResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation-dispute computation-thread UTxO",
  });
  const disputeContract = contracts.validationTraceDispute.game;
  if (threadUtxo.address !== disputeContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireDisputeDatum(threadUtxo);
  const expectedSigner =
    role === "operator"
      ? inputDatum.data.operator_vkey
      : inputDatum.fraud_prover;
  if (signer.paymentKeyHash !== expectedSigner) {
    throw new Error(
      `Validation-dispute ${role} reveal requires signer ${expectedSigner}, got ${signer.paymentKeyHash}`,
    );
  }
  const inputDispute = validationDisputeCoreFromData(inputDatum.data.dispute);
  const currentTimeUpper = inclusiveValidityUpperBound(range);
  const nextDispute =
    role === "operator"
      ? revealMidgardValidationOperatorMidpoint({
          dispute: inputDispute,
          proof,
          currentTime: currentTimeUpper,
        })
      : revealMidgardValidationChallengerMidpoint({
          dispute: inputDispute,
          proof,
          currentTime: currentTimeUpper,
        });
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: {
        challenged_header_hash: inputDatum.data.challenged_header_hash,
        operator_vkey: inputDatum.data.operator_vkey,
        dispute: validationDisputeDataFromCore(nextDispute),
      },
    },
    ValidationDisputeDatumV1,
  );
  const proofData = validationTraceProofDataFromCore(proof);
  // Round-trip before construction so non-canonical or out-of-range proof
  // fields fail before wallet selection and never reach balancing.
  validationTraceProofCoreFromData(proofData);
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeRevealRedeemer({
        threadUtxo,
        outputAddress: disputeContract.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        role,
        proof: proofData,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      disputeContract.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(disputeContract.spendingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      `BuildTxWithRedeemer did not resolve validation-dispute ${role} reveal layout`,
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(signed.toCBOR(), `Validation-dispute ${role} reveal`);
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    role,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    responseDeadline: nextDispute.responseDeadline,
    awaitedConfirmation: awaitConfirmation,
  };
};

export type SubmitValidationDisputeEnterTimeoutResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputeEnterTimeout = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  validityRange,
  now = Date.now(),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly now?: number;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeEnterTimeoutResult> => {
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation-dispute game UTxO",
  });
  const gameContract = contracts.validationTraceDispute.game;
  if (threadUtxo.address !== gameContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute game validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireDisputeDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation-dispute timeout handoff requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const dispute = validationDisputeCoreFromData(inputDatum.data.dispute);
  const range = requireValidityRange(
    validityRange ??
      validationDisputeTimeoutValidityRange(now, dispute.responseDeadline),
  );
  if (
    timeoutMidgardValidationDispute({
      dispute,
      currentTime: range.validFrom,
    }) !== "challenger"
  ) {
    throw new Error(
      "Validation-dispute timeout does not award the fraud proof to the challenger",
    );
  }
  const outputDatum = Data.to(inputDatum, ValidationDisputeDatumV1);
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeGameHandoffRedeemer({
        threadUtxo,
        outputAddress:
          contracts.validationTraceDispute.timeout.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        destination: "challengerTimeout",
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      contracts.validationTraceDispute.timeout.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(gameContract.spendingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute timeout handoff layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(signed.toCBOR(), "Validation-dispute timeout handoff");
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export type SubmitValidationDisputeTimeoutResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudProofUnit: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputeTimeout = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  validityRange,
  now = Date.now(),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly now?: number;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeTimeoutResult> => {
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation-dispute computation-thread UTxO",
  });
  const disputeContract = contracts.validationTraceDispute.timeout;
  if (threadUtxo.address !== disputeContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireDisputeDatum(threadUtxo);
  const dispute = validationDisputeCoreFromData(inputDatum.data.dispute);
  const range = requireValidityRange(
    validityRange ??
      validationDisputeTimeoutValidityRange(now, dispute.responseDeadline),
  );
  if (
    timeoutMidgardValidationDispute({
      dispute,
      currentTime: range.validFrom,
    }) !== "challenger"
  ) {
    throw new Error(
      "Validation-dispute timeout does not award the fraud proof to the challenger",
    );
  }
  const fraudProofUnit = toUnit(contracts.fraudProof.policyId, token.assetName);
  const fraudProofDatum = Data.to(
    { fraud_prover: inputDatum.fraud_prover },
    FraudProofTokenDatum,
  );
  let partialLayout:
    | Omit<FinalizeLayout, "computationThreadMintRedeemerIndex">
    | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeTimeoutSpendRedeemer({
        threadUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        onLayout: (layout) => {
          partialLayout = layout;
        },
      }),
    )
    .mintAssets(
      { [token.unit]: -1n },
      makeComputationThreadSuccessRedeemer({
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: token.assetName,
      }),
    )
    .mintAssets(
      { [fraudProofUnit]: 1n },
      makeFraudProofMintRedeemer({
        fraudProofPolicyId: contracts.fraudProof.policyId,
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: token.assetName,
        onComputationThreadMintRedeemerIndex: (index) => {
          computationThreadMintRedeemerIndex = index;
        },
      }),
    )
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .attach.SpendingValidator(disputeContract.spendingScript)
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .attach.MintingPolicy(contracts.fraudProof.mintingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    partialLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute timeout layout",
    );
  }
  const layout: FinalizeLayout = {
    ...partialLayout,
    computationThreadMintRedeemerIndex,
  };
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(signed.toCBOR(), "Validation-dispute timeout");
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    fraudProofOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    fraudProofUnit,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      layout.computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(layout.fraudProofMintRedeemerIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

const makePrepareResolutionRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  resolverIndex,
  preState,
  operatorPost,
  challengerPost,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly resolverIndex: bigint;
  readonly preState: ValidationMachineStateV1;
  readonly operatorPost: ValidationTraceProofV1;
  readonly challengerPost: ValidationTraceProofV1;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "validation dispute prepare resolution",
    );
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "validation dispute prepare resolution",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "validation dispute prepare resolution",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            resolver_index: resolverIndex,
            evidence: {
              pre_state: preState,
              operator_post: operatorPost,
              challenger_post: challengerPost,
            },
          },
        ],
      },
      ValidationBoundarySpendRedeemerV1,
    );
  }) satisfies BuildTxWithRedeemer;

export type SubmitValidationDisputeEnterResolutionResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputeEnterResolution = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeEnterResolutionResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation-dispute game UTxO",
  });
  const gameContract = contracts.validationTraceDispute.game;
  if (threadUtxo.address !== gameContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute game validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireDisputeDatum(threadUtxo);
  const dispute = validationDisputeCoreFromData(inputDatum.data.dispute);
  if (dispute.turn.type !== "readyForOneStep") {
    throw new Error(
      "Validation dispute must finish bisection before one-step resolution",
    );
  }
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation-dispute resolution handoff requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const outputDatum = Data.to(inputDatum, ValidationDisputeDatumV1);
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeGameHandoffRedeemer({
        threadUtxo,
        outputAddress:
          contracts.validationTraceDispute.boundary.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        destination: "resolution",
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      contracts.validationTraceDispute.boundary.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(gameContract.spendingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute resolution handoff layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(
    signed.toCBOR(),
    "Validation-dispute resolution handoff",
  );
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

const VALIDATION_RESOLVER_PHASES_V1 = [
  "CanonicalDecode",
  "CompactBinding",
  "StaticLedgerRules",
  "InputSets",
  "Signatures",
  "PhaseANativeScripts",
  "PhaseAScriptPreconditions",
  "ResolveInputs",
  "ScriptSources",
  "NativeScripts",
  "ScriptIntegrity",
  "Cek",
  "ValueAndMint",
  "LedgerDelta",
] as const satisfies readonly ValidationMachineStateV1["phase"][];

export const validationResolverIndexV1 = (
  phase: ValidationMachineStateV1["phase"],
): number => {
  const resolverIndex = VALIDATION_RESOLVER_PHASES_V1.indexOf(
    phase as (typeof VALIDATION_RESOLVER_PHASES_V1)[number],
  );
  if (resolverIndex < 0) {
    throw new Error(`Validation phase ${phase} has no one-step resolver`);
  }
  return resolverIndex;
};

const validationPrepareResolverDeploymentIndexV1 = (
  resolverIndex: number,
): number => {
  if (resolverIndex >= 0 && resolverIndex <= 8) {
    return resolverIndex;
  }
  if (resolverIndex === 9 || resolverIndex === 10) {
    return resolverIndex;
  }
  if (resolverIndex === 13) {
    return 11;
  }
  throw new Error(
    `Validation resolver ${resolverIndex.toString()} is not staged`,
  );
};

const validationDirectResolverDeploymentIndexV1 = (
  resolverIndex: number,
): number => {
  if (resolverIndex >= 11 && resolverIndex <= 12) {
    return resolverIndex - 11;
  }
  throw new Error(
    `Validation resolver ${resolverIndex.toString()} is not direct`,
  );
};

export type SubmitValidationDisputePrepareResolutionResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly resolverIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputePrepareResolution = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  preState,
  operatorPost,
  challengerPost,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly preState: ValidationMachineStateV1;
  readonly operatorPost: ValidationTraceProofV1;
  readonly challengerPost: ValidationTraceProofV1;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputePrepareResolutionResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation-dispute boundary UTxO",
  });
  const boundaryContract = contracts.validationTraceDispute.boundary;
  if (threadUtxo.address !== boundaryContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute boundary validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireDisputeDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation-dispute boundary preparation requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const dispute = validationDisputeCoreFromData(inputDatum.data.dispute);
  if (dispute.turn.type !== "readyForOneStep") {
    throw new Error(
      "Validation dispute must finish bisection before boundary preparation",
    );
  }
  const resolverIndex = validationResolverIndexV1(preState.phase);
  const resolverContract =
    contracts.validationTraceDispute.resolvers[resolverIndex];
  if (resolverContract === undefined) {
    throw new Error(
      `Validation resolver ${resolverIndex.toString()} is missing from the deployment`,
    );
  }
  if (
    operatorPost.state_hash !== inputDatum.data.dispute.operator_high_hash ||
    challengerPost.state_hash !== inputDatum.data.dispute.challenger_high_hash
  ) {
    throw new Error(
      "Validation boundary successor proofs do not match the authenticated dispute",
    );
  }
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: {
        version: 1n,
        pre_state: preState,
        operator_successor_hash: operatorPost.state_hash,
        challenger_successor_hash: challengerPost.state_hash,
      },
    },
    ValidationResolutionDatumV1,
  );
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makePrepareResolutionRedeemer({
        threadUtxo,
        outputAddress: resolverContract.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        resolverIndex: BigInt(resolverIndex),
        preState,
        operatorPost,
        challengerPost,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      resolverContract.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(boundaryContract.spendingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute boundary layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(
    signed.toCBOR(),
    "Validation-dispute boundary preparation",
  );
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    resolverIndex,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

const requireResolutionDatum = (
  threadUtxo: UTxO,
): ValidationResolutionDatumV1Data & {
  readonly data: NonNullable<ValidationResolutionDatumV1Data["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Validation resolution UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(threadUtxo.datum, ValidationResolutionDatumV1);
  if (datum.data === null) {
    throw new Error("Validation resolution requires initialized V1 state");
  }
  return datum as ValidationResolutionDatumV1Data & {
    readonly data: NonNullable<ValidationResolutionDatumV1Data["data"]>;
  };
};

const requirePreparedResolutionDatum = (
  threadUtxo: UTxO,
): PreparedValidationResolutionDatumV1Data & {
  readonly data: NonNullable<PreparedValidationResolutionDatumV1Data["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Prepared validation resolution UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(
    threadUtxo.datum,
    PreparedValidationResolutionDatumV1,
  );
  if (datum.data === null) {
    throw new Error(
      "Prepared validation resolution requires initialized V1 state",
    );
  }
  return datum as PreparedValidationResolutionDatumV1Data & {
    readonly data: NonNullable<PreparedValidationResolutionDatumV1Data["data"]>;
  };
};

const requireWinningResolutionDatum = (
  threadUtxo: UTxO,
): WinningValidationResolutionDatumV1Data & {
  readonly data: NonNullable<WinningValidationResolutionDatumV1Data["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Winning validation resolution UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(threadUtxo.datum, WinningValidationResolutionDatumV1);
  if (datum.data === null || datum.data.version !== 1n) {
    throw new Error(
      "Winning validation resolution requires canonical V1 state",
    );
  }
  return datum as WinningValidationResolutionDatumV1Data & {
    readonly data: NonNullable<WinningValidationResolutionDatumV1Data["data"]>;
  };
};

const makePrepareSelectedRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  semanticResolverIndex,
  transition,
  auxiliary,
  evidenceHash,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly semanticResolverIndex: number;
  readonly transition: ValidationOneStepWitnessV1;
  readonly auxiliary: ValidationAuxiliaryWitnessV1Data;
  readonly evidenceHash?: string;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "validation dispute prepare selected semantic resolver",
    );
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "validation dispute prepare selected semantic resolver",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "validation dispute prepare selected semantic resolver",
      ),
    };
    onLayout(layout);
    const base = {
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      semantic_resolver_index: BigInt(semanticResolverIndex),
      transition,
    };
    return evidenceHash === undefined
      ? encodeWithRuntimeSchema(
          { Continue: [{ ...base, auxiliary }] },
          validationPrepareSelectedSpendRedeemerV1RuntimeSchema,
        )
      : encodeWithRuntimeSchema(
          {
            Continue: [
              {
                PrepareSelectedByEvidenceHash: {
                  ...base,
                  evidence_hash: evidenceHash,
                },
              },
            ],
          },
          validationCanonicalDecodePrepareSelectedSpendRedeemerV1RuntimeSchema,
        );
  }) satisfies BuildTxWithRedeemer;

const semanticActionFieldsV1 = ({
  resolverIndex,
  semanticResolverIndex,
  inputIndex,
  outputIndex,
  transition,
  auxiliary,
}: {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly transition: PlutusDataValue;
  readonly auxiliary: Constr<PlutusDataValue>;
}): readonly PlutusDataValue[] => {
  const base: readonly PlutusDataValue[] = [
    inputIndex,
    outputIndex,
    transition,
  ];
  if (resolverIndex === 0) {
    if (
      semanticResolverIndex === 0 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.none,
      )
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 1 &&
      (hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldChunk,
      ) ||
        hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldItem,
        ))
    ) {
      return [...base, auxiliary];
    }
    throw new Error(
      "CanonicalDecode auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (resolverIndex === 13) {
    if (
      (semanticResolverIndex === 2 ||
        semanticResolverIndex === 4 ||
        semanticResolverIndex === 6 ||
        semanticResolverIndex === 7) &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      (semanticResolverIndex === 0 &&
        hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.ledgerDeltaOperation,
        )) ||
      (semanticResolverIndex === 1 &&
        hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.ledgerDeltaReplay,
        )) ||
      (semanticResolverIndex === 3 &&
        hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.ledgerDeltaOutput,
        )) ||
      (semanticResolverIndex === 5 &&
        hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.ledgerDeltaProofFrame,
        ))
    ) {
      return [...base, ...auxiliary.fields];
    }
    throw new Error(
      "LedgerDelta auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (resolverIndex === 7) {
    if (
      (semanticResolverIndex === 0 || semanticResolverIndex === 1) &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      (semanticResolverIndex === 2 &&
        hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.scheduledLedgerMembership,
        )) ||
      (semanticResolverIndex === 3 &&
        hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.ledgerOutputProofStep,
        )) ||
      (semanticResolverIndex === 4 &&
        hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.ledgerOutputProofFinalize,
        )) ||
      (semanticResolverIndex === 5 &&
        hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.scheduledLedgerNonMembership,
        ))
    ) {
      return [...base, ...auxiliary.fields];
    }
    throw new Error(
      "ResolveInputs auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (resolverIndex === 8) {
    if (semanticResolverIndex === 0) {
      return [...base, auxiliary];
    }
    if (
      semanticResolverIndex === 1 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.ledgerOutputProofBegin,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 2 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.ledgerOutputProofStep,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 3 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.ledgerOutputProofFinalize,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 4 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 5 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldChunk,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 6 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 7 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.scriptSourceHashBlock,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      (semanticResolverIndex === 8 || semanticResolverIndex === 9) &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      (semanticResolverIndex === 10 || semanticResolverIndex === 12) &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.scriptSourceScan,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 11 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.scriptSourceScan,
      )
    ) {
      return [
        ...base,
        auxiliary.fields[0]!,
        auxiliary.fields[1]!,
        auxiliary.fields[2]!,
        auxiliary.fields[4]!,
        auxiliary.fields[5]!,
        auxiliary.fields[6]!,
        auxiliary.fields[7]!,
      ];
    }
    if (
      semanticResolverIndex === 13 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 14 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 15 &&
      (hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.transactionRedeemerItemBegin,
      ) ||
        hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.redeemerItemStep,
        ))
    ) {
      return [...base, auxiliary];
    }
    if (
      semanticResolverIndex === 16 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 17 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.scriptSourceScan,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 18 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 19 &&
      (hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.redeemerScanBegin,
      ) ||
        hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.redeemerItemStep,
        ))
    ) {
      return [...base, auxiliary];
    }
    if (
      semanticResolverIndex === 20 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      (semanticResolverIndex === 21 || semanticResolverIndex === 22) &&
      (hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.redeemerScanBegin,
      ) ||
        hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.redeemerItemStep,
        ))
    ) {
      return [...base, auxiliary];
    }
    if (
      semanticResolverIndex === 23 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 24 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.scriptPurposeScan,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 25 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldChunk,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 26 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.scriptPurposeScan,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    if (
      semanticResolverIndex === 27 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    throw new Error(
      "ScriptSources auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (resolverIndex === 9) {
    if (
      semanticResolverIndex === 0 &&
      auxiliary.index === 0 &&
      auxiliary.fields.length === 0
    ) {
      return base;
    }
    if (
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.nativeExecutionDescriptor,
      )
    ) {
      if (semanticResolverIndex === 1) {
        const firstChunk = requireConstr({
          value: auxiliary.fields[15]!,
          index: 0,
          fields: 1,
          label: "validation NativeScripts native first chunk",
        });
        if (auxiliary.fields[1] !== 0n) {
          throw new Error(
            "NativeScripts native semantic route requires language tag 0",
          );
        }
        return [
          ...base,
          auxiliary.fields[0]!,
          ...auxiliary.fields.slice(2, 15),
          firstChunk.fields[0]!,
          auxiliary.fields[16]!,
        ];
      }
      if (semanticResolverIndex === 2) {
        const languageTag = auxiliary.fields[1];
        const noFirstChunk = requireConstr({
          value: auxiliary.fields[15]!,
          index: 1,
          fields: 0,
          label: "validation NativeScripts effectful first chunk",
        });
        const signerPeaks = auxiliary.fields[16];
        if (
          (languageTag !== 3n && languageTag !== 128n) ||
          noFirstChunk.fields.length !== 0 ||
          !Array.isArray(signerPeaks) ||
          signerPeaks.length !== 0
        ) {
          throw new Error(
            "NativeScripts effectful semantic route has native-only evidence",
          );
        }
        return [...base, ...auxiliary.fields.slice(0, 15)];
      }
    }
    throw new Error(
      "NativeScripts auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (
    hasValidationAuxiliaryShapeV1(
      auxiliary,
      VALIDATION_AUXILIARY_SHAPES_V1.none,
    )
  ) {
    return base;
  }
  if (
    hasValidationAuxiliaryShapeV1(
      auxiliary,
      VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldChunk,
    )
  ) {
    return [...base, ...auxiliary.fields];
  }
  if (
    hasValidationAuxiliaryShapeV1(
      auxiliary,
      VALIDATION_AUXILIARY_SHAPES_V1.requiredSignerItem,
    )
  ) {
    return [...base, ...auxiliary.fields];
  }
  if (
    resolverIndex === 5 &&
    semanticResolverIndex >= 2 &&
    semanticResolverIndex <= 7 &&
    hasValidationAuxiliaryShapeV1(
      auxiliary,
      VALIDATION_AUXILIARY_SHAPES_V1.nativeScriptToken,
    )
  ) {
    return [...base, auxiliary.fields[0]!, auxiliary.fields[1]!];
  }
  if (
    resolverIndex === 5 &&
    semanticResolverIndex >= 8 &&
    semanticResolverIndex <= 12 &&
    hasValidationAuxiliaryShapeV1(
      auxiliary,
      VALIDATION_AUXILIARY_SHAPES_V1.nativeScriptToken,
    )
  ) {
    return [...base, ...auxiliary.fields];
  }
  if (
    resolverIndex === 5 &&
    semanticResolverIndex === 13 &&
    hasValidationAuxiliaryShapeV1(
      auxiliary,
      VALIDATION_AUXILIARY_SHAPES_V1.nativeScriptFrame,
    )
  ) {
    return [...base, auxiliary.fields[0]!];
  }
  if (resolverIndex === 6) {
    if (
      semanticResolverIndex === 0 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.none,
      )
    ) {
      return base;
    }
    if (
      semanticResolverIndex === 1 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldChunk,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
  }
  throw new Error(
    "Validation auxiliary witness cannot construct the selected semantic redeemer",
  );
};

export const encodeValidationSemanticResolutionRedeemerV1 = ({
  oneStepArgument,
  inputIndex,
  outputIndex,
  proofItemReferenceInputIndex,
}: {
  readonly oneStepArgument: ValidationOneStepSubmissionArgumentV1;
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly proofItemReferenceInputIndex?: bigint;
}): Buffer => {
  if (inputIndex < 0n || outputIndex < 0n) {
    throw new Error(
      "Validation semantic redeemer indexes must be non-negative",
    );
  }
  const staged = requireStagedOneStepArgumentV1(oneStepArgument);
  if (proofItemReferenceInputIndex !== undefined) {
    if (
      proofItemReferenceInputIndex < 0n ||
      oneStepArgument.resolverIndex !== 0 ||
      staged.semanticResolverIndex !== 1 ||
      !hasValidationAuxiliaryShapeV1(
        staged.auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldItem,
      )
    ) {
      throw new Error(
        "Validation proof-item reference route requires a non-negative reference-input index and a CanonicalDecode complete item",
      );
    }
    return Buffer.from(
      Data.to(
        new Constr(1, [
          new Constr(1, [
            inputIndex,
            outputIndex,
            staged.transitionData,
            proofItemReferenceInputIndex,
          ]),
        ]),
      ),
      "hex",
    );
  }
  if (
    oneStepArgument.resolverIndex === 0 &&
    staged.semanticResolverIndex === 1 &&
    hasValidationAuxiliaryShapeV1(
      staged.auxiliary,
      VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldItem,
    )
  ) {
    return Buffer.from(
      Data.to(
        new Constr(1, [
          new Constr(0, [
            inputIndex,
            outputIndex,
            staged.transitionData,
            staged.auxiliary.fields[0]!,
          ]),
        ]),
      ),
      "hex",
    );
  }
  const fields = semanticActionFieldsV1({
    resolverIndex: oneStepArgument.resolverIndex,
    semanticResolverIndex: staged.semanticResolverIndex,
    inputIndex,
    outputIndex,
    transition: staged.transitionData,
    auxiliary: staged.auxiliary,
  });
  return Buffer.from(
    Data.to(new Constr(1, [new Constr(0, [...fields])])),
    "hex",
  );
};

const makeSemanticResolutionRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  resolverIndex,
  semanticResolverIndex,
  transition,
  auxiliary,
  proofItemReferenceUtxo,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly transition: PlutusDataValue;
  readonly auxiliary: Constr<PlutusDataValue>;
  readonly proofItemReferenceUtxo?: UTxO;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "validation dispute semantic resolution",
    );
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "validation dispute semantic resolution",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "validation dispute semantic resolution",
      ),
    };
    onLayout(layout);
    if (proofItemReferenceUtxo !== undefined) {
      if (
        resolverIndex !== 0 ||
        semanticResolverIndex !== 1 ||
        !hasValidationAuxiliaryShapeV1(
          auxiliary,
          VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldItem,
        )
      ) {
        throw new Error(
          "Validation proof-item reference route requires a CanonicalDecode complete item",
        );
      }
      return Data.to(
        new Constr(1, [
          new Constr(1, [
            layout.inputIndex,
            layout.outputIndex,
            transition,
            requireReferenceInputIndex(
              ctx,
              proofItemReferenceUtxo,
              "validation complete proof item",
            ),
          ]),
        ]),
      );
    }
    if (
      resolverIndex === 0 &&
      semanticResolverIndex === 1 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldItem,
      )
    ) {
      return Data.to(
        new Constr(1, [
          new Constr(0, [
            layout.inputIndex,
            layout.outputIndex,
            transition,
            auxiliary.fields[0]!,
          ]),
        ]),
      );
    }
    const fields = semanticActionFieldsV1({
      resolverIndex,
      semanticResolverIndex,
      inputIndex: layout.inputIndex,
      outputIndex: layout.outputIndex,
      transition,
      auxiliary,
    });
    return Data.to(new Constr(1, [new Constr(0, [...fields])]));
  }) satisfies BuildTxWithRedeemer;

const makeIndexedValidationStageRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  proofItemReferenceUtxo,
  label,
  encode,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly proofItemReferenceUtxo?: UTxO;
  readonly label: string;
  readonly encode: (layout: {
    readonly inputIndex: bigint;
    readonly outputIndex: bigint;
    readonly referenceInputIndex?: bigint;
  }) => string;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        label,
      ),
    };
    onLayout(layout);
    return encode({
      ...layout,
      ...(proofItemReferenceUtxo === undefined
        ? {}
        : {
            referenceInputIndex: requireReferenceInputIndex(
              ctx,
              proofItemReferenceUtxo,
              "validation complete proof item",
            ),
          }),
    });
  }) satisfies BuildTxWithRedeemer;

type ValidationFinalizationResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudProofUnit: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly materialReferenceInputOutRefs: readonly string[];
  readonly materialReferenceInputIndices: readonly number[];
  readonly awaitedConfirmation: boolean;
};

type ValidationFinalizingSpendLayout = Omit<
  FinalizeLayout,
  "computationThreadMintRedeemerIndex"
> & {
  /** Supplied order is semantic (root order); values are canonical tx indices. */
  readonly materialReferenceInputIndices: readonly bigint[];
};

const makeValidationFinalizingSpendRedeemer = ({
  threadUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  materialReferenceUtxos,
  label,
  encodeRedeemer,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly materialReferenceUtxos: readonly UTxO[];
  readonly label: string;
  readonly encodeRedeemer: (layout: ValidationFinalizingSpendLayout) => string;
  readonly onLayout: (layout: ValidationFinalizingSpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputWithDatumAndUnitPredicate({
          address: fraudProofAddress,
          datum: fraudProofDatum,
          unit: fraudProofUnit,
        }),
        `${label} fraud proof`,
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        `${label} fraud-proof mint`,
      ),
      materialReferenceInputIndices: materialReferenceUtxos.map((utxo) =>
        requireReferenceInputIndex(ctx, utxo, `${label} CEK material`),
      ),
    };
    onLayout(layout);
    return encodeRedeemer(layout);
  }) satisfies BuildTxWithRedeemer;

type ValidationFinalizationTransactionParams = {
  readonly lucid: LucidEvolution;
  readonly contracts: Awaited<
    ReturnType<typeof resolveValidationTraceDisputeDeploymentContracts>
  >["contracts"];
  readonly signer: ResolvedProverSigner;
  readonly threadUtxo: UTxO;
  readonly threadOutRef: string;
  readonly token: ReturnType<typeof requireComputationThreadToken>;
  readonly spendingScript: {
    readonly spendingScript: Script;
  };
  /**
   * Published authenticated reference-script UTxO carrying the spending
   * validator. When present the transaction consumes the validator through
   * `readFrom` and must not embed the validator body inside the L1 proof
   * envelope.
   */
  readonly spendingScriptReferenceUtxo?: UTxO;
  readonly spendLabel: string;
  readonly encodeSpendRedeemer: (
    layout: ValidationFinalizingSpendLayout,
  ) => string;
  readonly materialReferenceUtxos?: readonly UTxO[];
  readonly validityRange: ValidationDisputeValidityRange;
};

type PreparedValidationFinalizationTransaction = {
  readonly lucid: LucidEvolution;
  readonly signed: TxSigned;
  readonly threadOutRef: string;
  readonly fraudProofUnit: string;
  readonly layout: FinalizeLayout;
  readonly materialReferenceInputOutRefs: readonly string[];
  readonly materialReferenceInputIndices: readonly number[];
};

const prepareValidationFinalizationTransaction = async ({
  lucid,
  contracts,
  signer,
  threadUtxo,
  threadOutRef,
  token,
  spendingScript,
  spendingScriptReferenceUtxo,
  spendLabel,
  encodeSpendRedeemer,
  materialReferenceUtxos = [],
  validityRange,
}: ValidationFinalizationTransactionParams): Promise<PreparedValidationFinalizationTransaction> => {
  const fraudProofUnit = toUnit(contracts.fraudProof.policyId, token.assetName);
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  let partialLayout: ValidationFinalizingSpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const materialOutRefs = materialReferenceUtxos.map(outRefLabel);
  if (new Set(materialOutRefs).size !== materialOutRefs.length) {
    throw new Error(`${spendLabel} CEK material references must be unique`);
  }
  let tx = lucid.newTx().collectFrom([feeInput]);
  if (spendingScriptReferenceUtxo !== undefined) {
    if (
      spendingScriptReferenceUtxo.scriptRef == null ||
      validatorToScriptHash(spendingScriptReferenceUtxo.scriptRef) !==
        validatorToScriptHash(spendingScript.spendingScript)
    ) {
      throw new Error(
        `${spendLabel} spending-script reference UTxO ${outRefLabel(spendingScriptReferenceUtxo)} does not carry the exact spending validator`,
      );
    }
    tx = tx.readFrom([spendingScriptReferenceUtxo]);
  }
  if (materialReferenceUtxos.length > 0) {
    tx = tx.readFrom([...materialReferenceUtxos]);
  }
  tx = tx
    .collectFrom(
      [threadUtxo],
      makeValidationFinalizingSpendRedeemer({
        threadUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        materialReferenceUtxos,
        label: spendLabel,
        encodeRedeemer: encodeSpendRedeemer,
        onLayout: (layout) => {
          partialLayout = layout;
        },
      }),
    )
    .mintAssets(
      { [token.unit]: -1n },
      makeComputationThreadSuccessRedeemer({
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: token.assetName,
      }),
    )
    .mintAssets(
      { [fraudProofUnit]: 1n },
      makeFraudProofMintRedeemer({
        fraudProofPolicyId: contracts.fraudProof.policyId,
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: token.assetName,
        onComputationThreadMintRedeemerIndex: (index) => {
          computationThreadMintRedeemerIndex = index;
        },
      }),
    )
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .validFrom(validityRange.validFrom)
    .validTo(validityRange.validTo)
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .attach.MintingPolicy(contracts.fraudProof.mintingScript);
  // The published reference script supplies the spending validator; the
  // proof transaction must not embed the validator body inside the
  // 16,384-byte L1 envelope.
  if (spendingScriptReferenceUtxo === undefined) {
    tx = tx.attach.SpendingValidator(spendingScript.spendingScript);
  }
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    partialLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error(`BuildTxWithRedeemer did not resolve ${spendLabel} layout`);
  }
  const layout: FinalizeLayout = {
    ...partialLayout,
    computationThreadMintRedeemerIndex,
  };
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(signed.toCBOR(), spendLabel);
  return {
    lucid,
    signed,
    threadOutRef,
    fraudProofUnit,
    layout,
    materialReferenceInputOutRefs: materialOutRefs,
    materialReferenceInputIndices:
      partialLayout.materialReferenceInputIndices.map(Number),
  };
};

const submitPreparedValidationFinalizationTransaction = async ({
  prepared,
  awaitConfirmation,
}: {
  readonly prepared: PreparedValidationFinalizationTransaction;
  readonly awaitConfirmation: boolean;
}): Promise<ValidationFinalizationResult> => {
  const txHash = await prepared.signed.submit();
  if (awaitConfirmation) {
    await prepared.lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef: prepared.threadOutRef,
    fraudProofOutRef: `${txHash}#${prepared.layout.outputIndex.toString()}`,
    fraudProofUnit: prepared.fraudProofUnit,
    inputIndex: Number(prepared.layout.inputIndex),
    outputIndex: Number(prepared.layout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      prepared.layout.computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      prepared.layout.fraudProofMintRedeemerIndex,
    ),
    materialReferenceInputOutRefs: prepared.materialReferenceInputOutRefs,
    materialReferenceInputIndices: prepared.materialReferenceInputIndices,
    awaitedConfirmation: awaitConfirmation,
  };
};

const submitValidationFinalizationTransaction = async (
  params: ValidationFinalizationTransactionParams & {
    readonly awaitConfirmation: boolean;
  },
): Promise<ValidationFinalizationResult> =>
  submitPreparedValidationFinalizationTransaction({
    prepared: await prepareValidationFinalizationTransaction(params),
    awaitConfirmation: params.awaitConfirmation,
  });

export type SubmitValidationDisputePrepareSelectedResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly semanticResolverGlobalIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputePrepareSelected = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  oneStepArgument,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly oneStepArgument: ValidationOneStepSubmissionArgumentV1;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputePrepareSelectedResult> => {
  const range = requireValidityRange(validityRange);
  const {
    deploymentInfo: parsedDeploymentInfo,
    validationTraceDisputeCategory,
    contracts,
  } = await resolveValidationTraceDisputeDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
  });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation prepare-resolver UTxO",
  });
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireResolutionDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation semantic preparation requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const resolverIndex = validationResolverIndexV1(
    inputDatum.data.pre_state.phase,
  );
  if (resolverIndex !== oneStepArgument.resolverIndex) {
    throw new Error(
      "Validation one-step argument does not match the authenticated phase resolver",
    );
  }
  const staged = requireStagedOneStepArgumentV1(oneStepArgument);
  const isPrepareCompleteCanonicalItem =
    oneStepArgument.resolverIndex === 0 &&
    staged.semanticResolverIndex === 1 &&
    hasValidationAuxiliaryShapeV1(
      staged.auxiliary,
      VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldItem,
    );
  // #597: `TransactionFieldItemWitness` carries one field — a `FieldCarriageV1`
  // — so the size this step budgets the prepare-selected redeemer against is
  // the tier-1 `Inline` preimage inside the carriage, never a retired second
  // field. Tiers 2-3 name reference inputs and carry no bytes, so embedding
  // them directly never needs the by-hash route.
  const prepareCompleteItemCarriage = isPrepareCompleteCanonicalItem
    ? midgardFieldCarriageFromDataV1(
        staged.auxiliary.fields[0]!,
        "Validation prepare-selected complete item §8 carriage",
      )
    : undefined;
  const prepareCompleteItemByHash =
    prepareCompleteItemCarriage?.carriage === "Inline" &&
    selectValidationCompleteItemCarriageV1(
      prepareCompleteItemCarriage.preimage.length,
    ) === "reference";
  const prepareContract =
    contracts.validationTraceDispute.prepareResolvers[
      validationPrepareResolverDeploymentIndexV1(resolverIndex)
    ];
  const semanticContract =
    contracts.validationTraceDispute.semanticResolvers[
      staged.semanticResolverGlobalIndex
    ];
  if (prepareContract === undefined || semanticContract === undefined) {
    throw new Error("Validation staged resolver deployment is incomplete");
  }
  if (threadUtxo.address !== prepareContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at resolver ${resolverIndex.toString()}`,
    );
  }
  // The complete-canonical-item step transaction sources the prepare-resolver
  // validator from the published reference script: its redeemer already
  // carries the tier-1 §5.1 preimage (direct carriage), so the ~5.6 KiB
  // applied validator body must not ride inside the 16,384-byte L1 envelope
  // beside it (#617 follow-up to #597 ruling a).
  const prepareReferenceScriptUtxo = isPrepareCompleteCanonicalItem
    ? await requireValidationCanonicalDecodePrepareReferenceScriptUtxo({
        lucid,
        deploymentInfo: parsedDeploymentInfo,
        expectedScriptHash: prepareContract.spendingScriptHash,
      })
    : undefined;
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: {
        version: 1n,
        resolution: inputDatum.data,
        evidence_hash: staged.evidenceHash,
      },
    },
    PreparedValidationResolutionDatumV1,
  );
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makePrepareSelectedRedeemer({
        threadUtxo,
        outputAddress: semanticContract.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        semanticResolverIndex: staged.semanticResolverIndex,
        transition: staged.transition,
        auxiliary: staged.auxiliaryWitness,
        ...(prepareCompleteItemByHash
          ? { evidenceHash: staged.evidenceHash }
          : {}),
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      semanticContract.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash);
  // The published reference script supplies the prepare-resolver validator;
  // the step transaction must not embed the validator body inside the
  // 16,384-byte L1 envelope.
  const readiedTx =
    prepareReferenceScriptUtxo === undefined
      ? tx.attach.SpendingValidator(prepareContract.spendingScript)
      : tx.readFrom([prepareReferenceScriptUtxo]);
  const unsigned = await readiedTx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation semantic preparation layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(signed.toCBOR(), "Validation semantic preparation");
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    resolverIndex,
    semanticResolverIndex: staged.semanticResolverIndex,
    semanticResolverGlobalIndex: staged.semanticResolverGlobalIndex,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export type SubmitValidationDisputeSemanticResolutionResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly proofItemCarriage: "direct" | "reference";
  readonly proofItemReferenceOutRef?: string;
  readonly proofItemPublication?: {
    readonly txHash: string;
    readonly outRef: string;
    readonly outputIndex: number;
    readonly completeSignedBytes: number;
    readonly lovelace: bigint;
    readonly awaitedConfirmation: true;
  };
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly semanticResolverGlobalIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
  readonly stageTransactions?: readonly {
    readonly kind:
      | "authenticate"
      | "source"
      | "observe"
      | "proof"
      | "envelope"
      | "traversal"
      | "outer"
      | "execute-fold-map"
      | "execute-finalize-frame"
      | "settle";
    readonly txHash: string;
    readonly nextThreadOutRef: string;
    readonly completeSignedBytes: number;
  }[];
};

const exactSafeCborInteger = (value: unknown, label: string): number => {
  const integer =
    typeof value === "bigint"
      ? value
      : typeof value === "number" && Number.isSafeInteger(value)
        ? BigInt(value)
        : undefined;
  if (
    integer === undefined ||
    integer < BigInt(Number.MIN_SAFE_INTEGER) ||
    integer > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(`${label} must be an exact safe CBOR integer`);
  }
  return Number(integer);
};

const SCRIPT_SOURCES_REDEEMER_DOMAINS_V1 = {
  envelope: Buffer.from("MidgardScriptSourcesRedeemerEnvelopeV1", "ascii"),
  traversal: Buffer.from("MidgardScriptSourcesTraversalNormalizedV1", "ascii"),
  outer: Buffer.from("MidgardScriptSourcesOuterNormalizedV1", "ascii"),
  attested: Buffer.from(
    "MidgardScriptSourcesRedeemerExecutionAttestedV1",
    "ascii",
  ),
  resolutionIdentity: Buffer.from(
    "MidgardScriptSourcesResolutionIdentityV1",
    "ascii",
  ),
  auxiliaryIdentity: Buffer.from(
    "MidgardScriptSourcesAuxiliaryIdentityV1",
    "ascii",
  ),
  narrowActionIdentity: Buffer.from(
    "MidgardScriptSourcesNarrowActionIdentityV1",
    "ascii",
  ),
  traversalActionIdentity: Buffer.from(
    "MidgardScriptSourcesTraversalActionIdentityV1",
    "ascii",
  ),
  envelopeCommitment: Buffer.from(
    "MidgardScriptSourcesRedeemerEnvelopeCommitmentV1",
    "ascii",
  ),
  baseProvenanceIdentity: Buffer.from(
    "MidgardScriptSourcesBaseProvenanceIdentityV1",
    "ascii",
  ),
  itemControl: Buffer.from("MidgardRedeemerItemProofControlV1", "ascii"),
} as const;

const plutusDataCbor = (value: PlutusDataValue): Buffer =>
  Buffer.from(Data.to(value as never), "hex");

const exactCborBigIntV1 = (
  value: PlutusDataValue | undefined,
  label: string,
): bigint => {
  if (typeof value !== "bigint") {
    throw new Error(`${label} must be an exact CBOR integer`);
  }
  return value;
};

const hashDomainDataV1 = (domain: Uint8Array, value: PlutusDataValue): string =>
  computeHash32(
    Buffer.concat([Buffer.from(domain), plutusDataCbor(value)]),
  ).toString("hex");

const requireOptionDataV1 = (
  value: PlutusDataValue,
  label: string,
): PlutusDataValue | null => {
  if (!(value instanceof Constr)) {
    throw new Error(`${label} must be an option constructor`);
  }
  if (value.index === 1 && value.fields.length === 0) return null;
  if (value.index === 0 && value.fields.length === 1) return value.fields[0]!;
  throw new Error(`${label} must be an exact Some or None`);
};

const dataSummaryCoreV1 = (
  value: PlutusDataValue,
  label: string,
): MidgardCekDataSummaryV1 => {
  const summary = requireConstr({ value, index: 0, fields: 3, label });
  if (typeof summary.fields[0] !== "string") {
    throw new Error(`${label}.root must be bytes`);
  }
  return {
    root: Buffer.from(summary.fields[0], "hex"),
    cborLength: exactCborBigIntV1(summary.fields[1], `${label}.cbor_length`),
    memory: exactCborBigIntV1(summary.fields[2], `${label}.memory`),
  };
};

const dataFrameCoreV1 = (
  value: PlutusDataValue,
  label: string,
): MidgardCekDataFrameV1 => {
  const frame = requireConstr({ value, index: 0, fields: 11, label });
  const bytes = (index: number, field: string): Buffer => {
    const selected = frame.fields[index];
    if (typeof selected !== "string") {
      throw new Error(`${label}.${field} must be bytes`);
    }
    return Buffer.from(selected, "hex");
  };
  const integer = (index: number, field: string): bigint =>
    exactCborBigIntV1(frame.fields[index], `${label}.${field}`);
  const frontier = frame.fields[8];
  if (!Array.isArray(frontier)) {
    throw new Error(`${label}.child_peaks must be a frontier`);
  }
  const childPeaks = frontier.map((peak, index) => {
    const fields = requireConstr({
      value: peak,
      index: 0,
      fields: 2,
      label: `${label}.child_peaks[${index.toString()}]`,
    }).fields;
    if (typeof fields[1] !== "string") {
      throw new Error(`${label}.child_peaks hash must be bytes`);
    }
    return {
      height: exactSafeCborInteger(fields[0], `${label}.child_peaks height`),
      hash: Buffer.from(fields[1], "hex"),
    };
  });
  const sequence = requireConstr({
    value: frame.fields[10]!,
    index: 0,
    fields: 4,
    label: `${label}.sequence`,
  });
  if (typeof sequence.fields[0] !== "string") {
    throw new Error(`${label}.sequence.root must be bytes`);
  }
  const common = {
    tail: bytes(5, "tail"),
    expectedChildren: exactSafeCborInteger(
      frame.fields[6],
      `${label}.expected_children`,
    ),
    childCount: exactSafeCborInteger(frame.fields[7], `${label}.child_count`),
    childFrontier: {
      count: exactSafeCborInteger(frame.fields[7], `${label}.child_count`),
      peaks: childPeaks,
    },
    foldCursor: exactSafeCborInteger(frame.fields[9], `${label}.fold_cursor`),
    sequence: {
      root: Buffer.from(sequence.fields[0], "hex"),
      length: exactCborBigIntV1(sequence.fields[1], `${label}.sequence.length`),
      payloadCborLength: exactCborBigIntV1(
        sequence.fields[2],
        `${label}.sequence.payload_cbor_length`,
      ),
      memory: exactCborBigIntV1(sequence.fields[3], `${label}.sequence.memory`),
    },
  } as const;
  const kind = exactSafeCborInteger(frame.fields[0], `${label}.kind`);
  if (kind === 0)
    return {
      ...common,
      kind: "constrSmall",
      constructor: integer(1, "constructor"),
    };
  if (kind === 1) {
    return {
      ...common,
      kind: "constrLarge",
      constructorCborRoot: bytes(2, "constructor_cbor_root"),
      constructorCborLength: integer(3, "constructor_cbor_length"),
      constructorMemory: integer(4, "constructor_memory"),
    };
  }
  if (kind === 2) return { ...common, kind: "list" };
  if (kind === 3) return { ...common, kind: "map" };
  throw new Error(`${label}.kind is not a supported data frame`);
};

const stageOneActionCoreV1 = ({
  value,
  family,
}: {
  readonly value: PlutusDataValue;
  readonly family: number;
}): MidgardCekDataTraverseActionV1 => {
  const action = requireConstr({
    value,
    index: family === 0 ? 7 : 8,
    fields: family === 0 ? 6 : 2,
    label: "ScriptSources stage-one traversal action",
  });
  if (family === 0) {
    const keySiblings = action.fields[4];
    const valueSiblings = action.fields[5];
    if (
      !Array.isArray(keySiblings) ||
      !Array.isArray(valueSiblings) ||
      !keySiblings.every((sibling) => typeof sibling === "string") ||
      !valueSiblings.every((sibling) => typeof sibling === "string")
    ) {
      throw new Error("ScriptSources FoldMap siblings must be byte strings");
    }
    return {
      kind: "foldMap",
      frame: dataFrameCoreV1(action.fields[0]!, "ScriptSources FoldMap frame"),
      pairIndex: exactSafeCborInteger(
        action.fields[1],
        "ScriptSources FoldMap pair index",
      ),
      key: dataSummaryCoreV1(action.fields[2]!, "ScriptSources FoldMap key"),
      value: dataSummaryCoreV1(
        action.fields[3]!,
        "ScriptSources FoldMap value",
      ),
      keySiblings: keySiblings.map((sibling) =>
        Buffer.from(sibling as string, "hex"),
      ),
      valueSiblings: valueSiblings.map((sibling) =>
        Buffer.from(sibling as string, "hex"),
      ),
    };
  }
  const parent = requireOptionDataV1(
    action.fields[1]!,
    "ScriptSources FinalizeFrame parent",
  );
  return {
    kind: "finalizeFrame",
    frame: dataFrameCoreV1(
      action.fields[0]!,
      "ScriptSources FinalizeFrame frame",
    ),
    parent:
      parent === null
        ? null
        : dataFrameCoreV1(parent, "ScriptSources FinalizeFrame parent frame"),
  };
};

const stageOneControlCoreV1 = (
  value: PlutusDataValue,
): MidgardRedeemerItemProofControlV1 => {
  const control = requireConstr({
    value,
    index: 0,
    fields: 16,
    label: "ScriptSources stage-one item control",
  });
  const integer = (index: number, label: string): number =>
    exactSafeCborInteger(control.fields[index], `ScriptSources item ${label}`);
  const bytes = (index: number, label: string): Buffer => {
    const selected = control.fields[index];
    if (typeof selected !== "string") {
      throw new Error(`ScriptSources item ${label} must be bytes`);
    }
    return Buffer.from(selected, "hex");
  };
  const traversalData = requireOptionDataV1(
    control.fields[15]!,
    "ScriptSources item traversal",
  );
  if (traversalData === null) {
    throw new Error("ScriptSources stage-one item traversal must be present");
  }
  const traversal = requireConstr({
    value: traversalData,
    index: 0,
    fields: 10,
    label: "ScriptSources item traversal control",
  });
  for (const [index, label] of [
    [6, "pending large constructor"],
    [7, "integer"],
    [8, "bytes"],
  ] as const) {
    if (
      requireOptionDataV1(
        traversal.fields[index]!,
        `ScriptSources traversal ${label}`,
      ) !== null
    ) {
      throw new Error(
        `ScriptSources fold-stage traversal ${label} must be absent`,
      );
    }
  }
  const resultData = requireOptionDataV1(
    traversal.fields[9]!,
    "ScriptSources traversal result",
  );
  const traversalCore: MidgardCekDataTraverseControlV1 = {
    version: integerFromDataV1(
      traversal.fields[0],
      "ScriptSources traversal version",
    ) as 1,
    stage: integerFromDataV1(
      traversal.fields[1],
      "ScriptSources traversal stage",
    ) as MidgardCekDataTraverseControlV1["stage"],
    sourceStart: integerFromDataV1(
      traversal.fields[2],
      "ScriptSources traversal source start",
    ),
    sourceLength: integerFromDataV1(
      traversal.fields[3],
      "ScriptSources traversal source length",
    ),
    offset: integerFromDataV1(
      traversal.fields[4],
      "ScriptSources traversal offset",
    ),
    frameRoot: (() => {
      if (typeof traversal.fields[5] !== "string")
        throw new Error("ScriptSources traversal frame root must be bytes");
      return Buffer.from(traversal.fields[5], "hex");
    })(),
    pendingLargeExpectedChildren: null,
    integer: null,
    bytes: null,
    result:
      resultData === null
        ? null
        : dataSummaryCoreV1(resultData, "ScriptSources traversal result"),
  };
  return {
    version: integer(0, "version") as 1,
    mode: integer(1, "mode") as MidgardRedeemerItemProofControlV1["mode"],
    stage: integer(2, "stage") as MidgardRedeemerItemProofControlV1["stage"],
    itemIndex: integer(3, "index"),
    itemCount: integer(4, "count"),
    totalLength: integer(5, "total length"),
    itemCommitment: bytes(6, "commitment"),
    expectedPurposeTag: integer(7, "expected purpose tag"),
    expectedPointerIndex: integer(8, "expected pointer index"),
    purposeTag: integer(9, "purpose tag"),
    pointerIndex: integer(10, "pointer index"),
    dataOffset: integer(11, "data offset"),
    dataLength: integer(12, "data length"),
    executionMemory: exactCborBigIntV1(
      control.fields[13],
      "ScriptSources item execution memory",
    ),
    executionSteps: exactCborBigIntV1(
      control.fields[14],
      "ScriptSources item execution steps",
    ),
    traversal: traversalCore,
  };
};

const integerFromDataV1 = (
  value: PlutusDataValue | undefined,
  label: string,
): number => exactSafeCborInteger(value, label);

const deriveScriptSourcesStageOneRouteDataV1 = ({
  preparedResolution,
  fraudProver,
  auxiliary,
  deploymentId,
  envelopeScriptHash,
  traversalScriptHash,
  outerScriptHash,
  foldMapScriptHash,
  finalizeFrameScriptHash,
  settlementScriptHash,
}: {
  readonly preparedResolution: NonNullable<
    PreparedValidationResolutionDatumV1Data["data"]
  >;
  readonly fraudProver: string;
  readonly auxiliary: Constr<PlutusDataValue>;
  readonly deploymentId: string;
  readonly envelopeScriptHash: string;
  readonly traversalScriptHash: string;
  readonly outerScriptHash: string;
  readonly foldMapScriptHash: string;
  readonly finalizeFrameScriptHash: string;
  readonly settlementScriptHash: string;
}) => {
  const exactAuxiliary = requireConstr({
    value: auxiliary,
    index: VALIDATION_AUXILIARY_SHAPES_V1.redeemerItemStep[0],
    fields: VALIDATION_AUXILIARY_SHAPES_V1.redeemerItemStep[1],
    label: "ScriptSources split stage-one auxiliary witness",
  });
  if (
    requireOptionDataV1(
      exactAuxiliary.fields[0]!,
      "ScriptSources stage-one redeemer control",
    ) !== null
  ) {
    throw new Error(
      "ScriptSources split stage-one route requires an absent CEK redeemer control",
    );
  }
  const currentControlData = exactAuxiliary.fields[1]!;
  const itemWitness = requireConstr({
    value: exactAuxiliary.fields[2]!,
    index: 0,
    fields: 3,
    label: "ScriptSources stage-one item witness",
  });
  const itemAction = requireConstr({
    value: itemWitness.fields[0]!,
    index: 2,
    fields: 1,
    label: "ScriptSources stage-one item action",
  });
  if (
    requireOptionDataV1(
      itemWitness.fields[1]!,
      "ScriptSources stage-one chunk proof",
    ) !== null ||
    requireOptionDataV1(
      itemWitness.fields[2]!,
      "ScriptSources stage-one next chunk proof",
    ) !== null
  ) {
    throw new Error(
      "ScriptSources split stage-one route forbids chunk proofs during a fold-stage action",
    );
  }
  const traversalActionData = itemAction.fields[0]!;
  if (!(traversalActionData instanceof Constr)) {
    throw new Error(
      "ScriptSources stage-one traversal action must be a constructor",
    );
  }
  const family =
    traversalActionData.index === 7
      ? 0
      : traversalActionData.index === 8
        ? 1
        : -1;
  if (family < 0) {
    throw new Error(
      "ScriptSources split stage-one route only accepts FoldMap or FinalizeFrame",
    );
  }
  const currentControl = stageOneControlCoreV1(currentControlData);
  const traversalAction = stageOneActionCoreV1({
    value: traversalActionData,
    family,
  });
  const nextControl = advanceMidgardRedeemerItemProofV1({
    control: currentControl,
    witness: {
      action: { kind: "traverseData", action: traversalAction },
      chunkProof: null,
      nextChunkProof: null,
    },
  });
  if (
    nextControl === null ||
    currentControl.traversal === null ||
    nextControl.traversal === null
  ) {
    throw new Error(
      "ScriptSources split stage-one traversal action has no valid canonical successor",
    );
  }
  const currentPendingItemControlHash =
    hashMidgardRedeemerItemProofControlV1(currentControl).toString("hex");
  const expectedNextItemControlHash =
    hashMidgardRedeemerItemProofControlV1(nextControl).toString("hex");
  const checkedTraversalControlCbor = encodeMidgardCekDataTraverseControlV1(
    currentControl.traversal,
  );
  const checkedNextTraversalControlCbor = encodeMidgardCekDataTraverseControlV1(
    nextControl.traversal,
  );
  const baseData = exactPlutusDataFromCbor(
    Buffer.from(
      Data.to(preparedResolution, PreparedValidationResolutionStateV1),
      "hex",
    ),
    "ScriptSources prepared resolution state",
  );
  const baseRecord = requireConstr({
    value: baseData,
    index: 0,
    fields: 3,
    label: "ScriptSources prepared resolution state",
  });
  const resolutionIdentity = hashDomainDataV1(
    SCRIPT_SOURCES_REDEEMER_DOMAINS_V1.resolutionIdentity,
    baseRecord.fields[1]!,
  );
  const canonicalAuxiliaryHash = hashDomainDataV1(
    SCRIPT_SOURCES_REDEEMER_DOMAINS_V1.auxiliaryIdentity,
    exactAuxiliary,
  );
  const canonicalActionHash = hashDomainDataV1(
    SCRIPT_SOURCES_REDEEMER_DOMAINS_V1.narrowActionIdentity,
    [currentControlData, traversalActionData],
  );
  const semanticExecutorScriptHash =
    family === 0 ? foldMapScriptHash : finalizeFrameScriptHash;
  const commitmentItems = [
    encodeCbor(1n),
    encodeCbor(Buffer.from(deploymentId, "hex")),
    encodeCbor(Buffer.from(preparedResolution.evidence_hash, "hex")),
    encodeCbor(Buffer.from(resolutionIdentity, "hex")),
    encodeCbor(BigInt(family)),
    encodeCbor(Buffer.from(canonicalAuxiliaryHash, "hex")),
    encodeCbor(Buffer.from(canonicalActionHash, "hex")),
    encodeCbor(Buffer.from(currentPendingItemControlHash, "hex")),
    encodeCbor(Buffer.from(expectedNextItemControlHash, "hex")),
    encodeCbor(BigInt(currentControl.itemIndex)),
    encodeCbor(BigInt(currentControl.itemCount)),
    encodeCbor(Buffer.from(envelopeScriptHash, "hex")),
    encodeCbor(Buffer.from(traversalScriptHash, "hex")),
    encodeCbor(Buffer.from(outerScriptHash, "hex")),
    encodeCbor(Buffer.from(semanticExecutorScriptHash, "hex")),
    encodeCbor(Buffer.from(settlementScriptHash, "hex")),
    encodeCbor(
      Buffer.from(
        preparedResolution.resolution.pre_state.transaction_commitment,
        "hex",
      ),
    ),
    encodeCbor(
      Buffer.from(
        preparedResolution.resolution.pre_state.validation_context_hash,
        "hex",
      ),
    ),
  ];
  const envelopeCommitment = computeHash32(
    Buffer.concat([
      SCRIPT_SOURCES_REDEEMER_DOMAINS_V1.envelopeCommitment,
      encodeCborArrayRaw(commitmentItems),
    ]),
  ).toString("hex");
  const envelopeData = new Constr(0, [
    1n,
    SCRIPT_SOURCES_REDEEMER_DOMAINS_V1.envelope.toString("hex"),
    deploymentId,
    baseData,
    resolutionIdentity,
    BigInt(family),
    canonicalAuxiliaryHash,
    canonicalActionHash,
    currentPendingItemControlHash,
    expectedNextItemControlHash,
    BigInt(currentControl.itemIndex),
    BigInt(currentControl.itemCount),
    envelopeScriptHash,
    traversalScriptHash,
    outerScriptHash,
    semanticExecutorScriptHash,
    settlementScriptHash,
    envelopeCommitment,
  ]);
  const baseProvenanceIdentity = computeHash32(
    Buffer.concat([
      SCRIPT_SOURCES_REDEEMER_DOMAINS_V1.baseProvenanceIdentity,
      encodeCborArrayRaw([
        encodeCbor(Buffer.from(preparedResolution.evidence_hash, "hex")),
        encodeCbor(Buffer.from(resolutionIdentity, "hex")),
        encodeCbor(Buffer.from(envelopeCommitment, "hex")),
      ]),
    ]),
  ).toString("hex");
  const traversalActionIdentity = hashDomainDataV1(
    SCRIPT_SOURCES_REDEEMER_DOMAINS_V1.traversalActionIdentity,
    traversalActionData,
  );
  const currentControlRecord = requireConstr({
    value: currentControlData,
    index: 0,
    fields: 16,
    label: "ScriptSources stage-one item control",
  });
  const traversalOption = requireConstr({
    value: currentControlRecord.fields[15]!,
    index: 0,
    fields: 1,
    label: "ScriptSources stage-one traversal option",
  });
  const traversalControlData = traversalOption.fields[0]!;
  const outerFieldsData = new Constr(
    0,
    currentControlRecord.fields.slice(0, 15),
  );
  const traversalData = new Constr(0, [
    1n,
    SCRIPT_SOURCES_REDEEMER_DOMAINS_V1.traversal.toString("hex"),
    deploymentId,
    baseProvenanceIdentity,
    envelopeScriptHash,
    traversalScriptHash,
    outerScriptHash,
    semanticExecutorScriptHash,
    settlementScriptHash,
    BigInt(family),
    canonicalActionHash,
    traversalActionIdentity,
    currentPendingItemControlHash,
    expectedNextItemControlHash,
    BigInt(currentControl.itemIndex),
    BigInt(currentControl.itemCount),
    outerFieldsData,
    traversalControlData,
    checkedTraversalControlCbor.toString("hex"),
  ]);
  const encodedCurrentControl =
    encodeMidgardRedeemerItemProofControlV1(currentControl);
  const nextItemControlHashPrefix = Buffer.concat([
    SCRIPT_SOURCES_REDEEMER_DOMAINS_V1.itemControl,
    encodedCurrentControl.subarray(
      0,
      encodedCurrentControl.length - checkedTraversalControlCbor.length - 1,
    ),
  ]);
  const traversalTemplate =
    family === 0
      ? new Constr(0, [
          Buffer.concat([
            Buffer.from([0x8a]),
            encodeCbor(1n),
            encodeCbor(BigInt(currentControl.traversal.stage)),
            encodeCbor(BigInt(currentControl.traversal.sourceStart)),
            encodeCbor(BigInt(currentControl.traversal.sourceLength)),
            encodeCbor(BigInt(currentControl.traversal.offset)),
          ]).toString("hex"),
          "d87a80d87a80d87a80d87a80",
        ])
      : new Constr(1, [
          Buffer.concat([Buffer.from([0x8a]), encodeCbor(1n)]).toString("hex"),
          Buffer.concat([
            encodeCbor(BigInt(currentControl.traversal.sourceStart)),
            encodeCbor(BigInt(currentControl.traversal.sourceLength)),
            encodeCbor(BigInt(currentControl.traversal.offset)),
          ]).toString("hex"),
          "d87a80d87a80d87a80",
        ]);
  const outerData = new Constr(0, [
    1n,
    SCRIPT_SOURCES_REDEEMER_DOMAINS_V1.outer.toString("hex"),
    deploymentId,
    baseProvenanceIdentity,
    envelopeScriptHash,
    traversalScriptHash,
    outerScriptHash,
    semanticExecutorScriptHash,
    settlementScriptHash,
    BigInt(family),
    canonicalActionHash,
    traversalActionIdentity,
    currentPendingItemControlHash,
    expectedNextItemControlHash,
    BigInt(currentControl.itemIndex),
    BigInt(currentControl.itemCount),
    nextItemControlHashPrefix.toString("hex"),
    traversalControlData,
    traversalTemplate,
  ]);
  const attestedData = new Constr(0, [
    1n,
    SCRIPT_SOURCES_REDEEMER_DOMAINS_V1.attested.toString("hex"),
    deploymentId,
    baseProvenanceIdentity,
    envelopeScriptHash,
    traversalScriptHash,
    outerScriptHash,
    semanticExecutorScriptHash,
    settlementScriptHash,
    BigInt(family),
    canonicalActionHash,
    traversalActionIdentity,
    currentPendingItemControlHash,
    expectedNextItemControlHash,
    expectedNextItemControlHash,
    BigInt(currentControl.itemIndex),
    BigInt(currentControl.itemCount),
  ]);
  const stageDatum = (data: PlutusDataValue): string =>
    Data.to(new Constr(0, [fraudProver, new Constr(0, [data])]));
  return {
    family,
    traversalActionData,
    currentControlData,
    envelopeData,
    traversalDatum: stageDatum(envelopeData),
    outerDatum: stageDatum(traversalData),
    executorDatum: stageDatum(outerData),
    settlementDatum: stageDatum(attestedData),
    expectedNextItemControlHash,
    checkedNextTraversalControlCbor,
  } as const;
};

const canonicalCborArgumentHeaderSize = (value: number): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("Canonical CBOR argument must be non-negative");
  }
  if (value < 24) return 1;
  if (value < 0x100) return 2;
  if (value < 0x1_0000) return 3;
  if (value < 0x1_0000_0000) return 5;
  return 9;
};

const canonicalFieldItemEncodedLength = ({
  fieldIndex,
  itemLength,
}: {
  readonly fieldIndex: number;
  readonly itemLength: number;
}): number | null => {
  if ([0, 1, 2, 3, 4, 7].includes(fieldIndex)) {
    return canonicalCborArgumentHeaderSize(itemLength) + itemLength;
  }
  if (fieldIndex === 6 || fieldIndex === 8) return itemLength;
  if (fieldIndex !== 5) {
    throw new Error(`Unknown canonical field index ${fieldIndex.toString()}`);
  }
  return itemLength === 0 ? null : itemLength - 1;
};

/**
 * #597. The staged datums observe what the §8 door established, not what a
 * prover claimed: `fieldPreimage` is the whole §5.1 preimage the carriage
 * delivers, the item count is §5.2's own decode of it, and the item's bytes are
 * a slice. The retired `collectionProof`/`itemCbor` pair claimed both, and §4
 * left the claim nothing to be checked against.
 */
export const deriveCanonicalDecodeItemStageDataV1 = ({
  preparedResolution,
  transition,
  fieldPreimage,
}: {
  readonly preparedResolution: NonNullable<
    PreparedValidationResolutionDatumV1Data["data"]
  >;
  readonly transition: ValidationOneStepWitnessV1;
  readonly fieldPreimage: string;
}) => {
  const control = asArray(
    decodeSingleCbor(Buffer.from(transition.work_witness_cbor, "hex")),
    "canonical_decode_item.control",
  );
  if (control.length !== 9) {
    throw new Error("Canonical decode item control must contain nine fields");
  }
  const compactCbor = asBytes(control[0], "canonical_decode_item.compact");
  const witnessSetCompactCbor = asBytes(
    control[1],
    "canonical_decode_item.witness_set",
  );
  const fieldPreimageLengthsCbor = asBytes(
    control[2],
    "canonical_decode_item.field_lengths",
  );
  const fieldIndex = exactSafeCborInteger(
    control[4],
    "canonical_decode_item.field_index",
  );
  const itemIndex = exactSafeCborInteger(
    control[5],
    "canonical_decode_item.item_index",
  );
  const chunkIndex = exactSafeCborInteger(
    control[6],
    "canonical_decode_item.chunk_index",
  );
  const itemCount = exactSafeCborInteger(
    control[7],
    "canonical_decode_item.item_count",
  );
  const encodedLength = exactSafeCborInteger(
    control[8],
    "canonical_decode_item.encoded_length",
  );
  const proofSource = {
    compactCbor,
    witnessSetCompactCbor,
    fieldPreimageLengthsCbor,
  };
  // Called for its verification, not its value: it binds these compact structures
  // to the disputed transaction id, which the positional extraction below does not.
  verifyMidgardNativeTxProofSourceV1({
    transactionId: Buffer.from(
      preparedResolution.resolution.pre_state.transaction_id,
      "hex",
    ),
    source: proofSource,
  });
  const lengths = decodeMidgardNativeTxProofFieldLengthsV1(
    fieldPreimageLengthsCbor,
  );
  // The §4 positional extraction, taken from the one implementation of it rather
  // than hand-copied for a third time. `verifyMidgardNativeTxProofSourceV1` above
  // stays: the helper deliberately does not authenticate the source, and binding
  // these structures to `pre_state.transaction_id` is what that call is for.
  const fieldCommitments = midgardV1TxFieldCommitmentsFromSourceV1(proofSource);
  const expectedFieldCommitment = fieldCommitments[fieldIndex];
  const expectedFieldLength = lengths[fieldIndex];
  if (
    expectedFieldCommitment === undefined ||
    expectedFieldLength === undefined
  ) {
    throw new Error("Canonical decode item field index is out of range");
  }
  // §8: the door authenticates the whole preimage against the flat §4
  // commitment, so the item count and the item's bytes are *derived* here rather
  // than claimed. Authenticating first is what makes the derivation meaningful.
  const fieldPreimageBytes = Buffer.from(fieldPreimage, "hex");
  const actualFieldCommitment = midgardFieldCommitmentV1(fieldPreimageBytes);
  if (!actualFieldCommitment.equals(Buffer.from(expectedFieldCommitment))) {
    throw new Error(
      "Canonical decode item carriage preimage does not hash to the committed field",
    );
  }
  if (fieldPreimageBytes.length !== expectedFieldLength) {
    throw new Error(
      "Canonical decode item carriage preimage contradicts its declared field length",
    );
  }
  const fieldItems = decodeMidgardFieldPreimageV1(fieldPreimageBytes);
  const itemBytes = fieldItems[itemIndex];
  if (itemBytes === undefined) {
    throw new Error("Canonical decode item index is outside the field");
  }
  const proofItemCount = fieldItems.length;
  const firstItem =
    itemIndex === 0 &&
    chunkIndex === 0 &&
    itemCount === -1 &&
    encodedLength === 0;
  const continuingItem =
    chunkIndex === 0 &&
    itemCount > 0 &&
    itemIndex < itemCount &&
    proofItemCount === itemCount;
  if (!firstItem && !continuingItem) {
    throw new Error("Canonical decode item control is not an active item");
  }
  const activeItemCount = firstItem ? proofItemCount : itemCount;
  const itemEncodedLength = canonicalFieldItemEncodedLength({
    fieldIndex,
    itemLength: itemBytes.length,
  });
  const nextEncodedLength =
    itemEncodedLength === null
      ? 0
      : (firstItem
          ? canonicalCborArgumentHeaderSize(activeItemCount)
          : encodedLength) + itemEncodedLength;
  const authenticated = {
    version: 1n,
    base: preparedResolution,
    transition,
  };
  const prepared = {
    version: 1n,
    authenticated,
    source: {
      expected_field_commitment: Buffer.from(expectedFieldCommitment).toString(
        "hex",
      ),
      expected_field_length: BigInt(expectedFieldLength),
    },
  };
  const observed = {
    version: 1n,
    prepared,
    observation: {
      item_count: BigInt(proofItemCount),
      item_length: BigInt(itemBytes.length),
    },
  };
  const verified = {
    version: 1n,
    observed,
    proof: {
      active_item_count: BigInt(activeItemCount),
      item_encoding_is_valid: itemEncodedLength !== null,
      next_encoded_length: BigInt(nextEncodedLength),
    },
  };
  return { authenticated, prepared, observed, verified };
};

export const submitValidationDisputeSemanticResolution = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  oneStepArgument,
  proofItemReferenceOutRef,
  carriageMaterial,
  validityRange,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly oneStepArgument: ValidationOneStepSubmissionArgumentV1;
  readonly proofItemReferenceOutRef?: string;
  /** Required when the committed carriage is tier 2 or tier 3 (#600). */
  readonly carriageMaterial?: ValidationFieldCarriageMaterialV1;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeSemanticResolutionResult> => {
  const {
    deploymentInfo: parsedDeploymentInfo,
    validationTraceDisputeCategory,
    fraudProofCataloguePolicyId,
    contracts,
  } = await resolveValidationTraceDisputeDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
  });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "prepared validation semantic-resolver UTxO",
  });
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requirePreparedResolutionDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation semantic resolution requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const resolverIndex = validationResolverIndexV1(
    inputDatum.data.resolution.pre_state.phase,
  );
  if (resolverIndex !== oneStepArgument.resolverIndex) {
    throw new Error(
      "Validation one-step argument does not match the prepared phase resolver",
    );
  }
  const staged = requireStagedOneStepArgumentV1(oneStepArgument);
  if (staged.evidenceHash !== inputDatum.data.evidence_hash) {
    throw new Error(
      "Validation one-step argument does not match the prepared evidence hash",
    );
  }
  const semanticContract =
    contracts.validationTraceDispute.semanticResolvers[
      staged.semanticResolverGlobalIndex
    ];
  if (semanticContract === undefined) {
    throw new Error("Validation semantic resolver deployment is incomplete");
  }
  if (threadUtxo.address !== semanticContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at semantic resolver ${staged.semanticResolverGlobalIndex.toString()}`,
    );
  }
  const isCompleteCanonicalItem =
    oneStepArgument.resolverIndex === 0 &&
    staged.semanticResolverIndex === 1 &&
    hasValidationAuxiliaryShapeV1(
      staged.auxiliary,
      VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldItem,
    );
  // #597/#600. `TransactionFieldItemWitness` carries one field — a
  // `FieldCarriageV1` — and the bytes it stands for are the field's whole §5.1
  // preimage rather than one item with an opening into it. #597 could only read
  // tier-1 `Inline`; #600 made the producer tier-free, so all three §8.4 rungs
  // reach here and the tier is decided by the preimage's own length, never by
  // this builder.
  //
  // Tier 1 is self-contained: the preimage is inside the auxiliary. Tiers 2-3
  // carry positional reference-input indices and no bytes at all, so the
  // submitter must supply the material those indices name — that is what
  // `carriageMaterial` is, and its absence is a refusal rather than a
  // transaction that references nothing.
  const committedItemCarriage = isCompleteCanonicalItem
    ? midgardFieldCarriageFromDataV1(
        staged.auxiliary.fields[0]!,
        "Validation complete proof-item §8 carriage",
      )
    : undefined;
  const completeItemCarriageMaterial = (():
    | ValidationFieldCarriageMaterialV1
    | undefined => {
    if (
      committedItemCarriage === undefined ||
      committedItemCarriage.carriage === "Inline"
    ) {
      return undefined;
    }
    if (carriageMaterial === undefined) {
      throw new Error(
        `Validation complete proof-item carriage is tier-${
          committedItemCarriage.carriage === "RawUtxo" ? "2" : "3"
        } \`${committedItemCarriage.carriage}\`, which names reference inputs and carries no ` +
          "bytes, so the submission must supply the §8 carriage material those indices name (#600)",
      );
    }
    if (carriageMaterial.plan.tier !== committedItemCarriage.carriage) {
      throw new Error(
        `Validation complete proof-item carriage material plans tier \`${carriageMaterial.plan.tier}\` ` +
          `while the committed evidence names \`${committedItemCarriage.carriage}\``,
      );
    }
    return carriageMaterial;
  })();
  const completeFieldPreimage = ((): string | undefined => {
    if (committedItemCarriage === undefined) {
      return undefined;
    }
    if (committedItemCarriage.carriage === "Inline") {
      return committedItemCarriage.preimage.toString("hex");
    }
    // §8.4's split is positional and exhaustive, so the plan's own publications
    // concatenate back to exactly the preimage the door will materialise. Taking
    // the bytes from the producer's plan rather than from a caller-supplied copy
    // is what keeps the staged datums below derived from the same bytes the door
    // authenticates.
    return Buffer.concat(
      completeItemCarriageMaterial!.plan.publications.map(
        (publication) => publication.bytes,
      ),
    ).toString("hex");
  })();
  // The complete-item proof transaction must source the semantic validator
  // from the published reference script: embedding the validator body would
  // consume the 16,384-byte envelope the measured complete-item redeemer
  // needs. Resolve it before publishing anything so a missing deployment
  // entry fails fast.
  const semanticReferenceScriptUtxo = isCompleteCanonicalItem
    ? await requireValidationItemSemanticReferenceScriptUtxo({
        lucid,
        deploymentInfo: parsedDeploymentInfo,
        expectedScriptHash: semanticContract.spendingScriptHash,
      })
    : undefined;
  // The observe stage — the §8.8 door — must source its validator from the
  // published reference script the same way: embedding the applied observe
  // body in the door transaction spends the envelope the carriage bytes need
  // (#597 ruling a / #617). Resolved up front, beside the semantic entry, so
  // a missing deployment entry fails fast.
  const observeReferenceScriptUtxo = isCompleteCanonicalItem
    ? await requireValidationItemObserveReferenceScriptUtxo({
        lucid,
        deploymentInfo: parsedDeploymentInfo,
        expectedScriptHash:
          contracts.validationTraceDispute.canonicalDecodeItemStages.observe
            .spendingScriptHash,
      })
    : undefined;
  let resolvedProofItemReferenceOutRef = proofItemReferenceOutRef;
  let proofItemReferenceUtxo =
    proofItemReferenceOutRef === undefined
      ? undefined
      : await fetchUtxoByOutRef({
          lucid,
          outRef: parseOutRef(
            proofItemReferenceOutRef,
            "--proof-item-reference-out-ref",
          ),
          label: "validation complete proof-item reference UTxO",
        });
  let proofItemPublication:
    | SubmitValidationDisputeSemanticResolutionResult["proofItemPublication"]
    | undefined;
  if (
    proofItemReferenceUtxo === undefined &&
    isCompleteCanonicalItem &&
    // The publication route reconstructs tier-1 `Inline` from a datum, so it
    // exists only inside tier 1 — as a redeemer-size optimisation, never as a
    // fourth rung (#600 Ruling 1, Q4). Above the cap the carriage already names
    // reference inputs of its own and a second one would be an unbound copy of
    // the same bytes.
    committedItemCarriage?.carriage === "Inline" &&
    selectValidationCompleteItemCarriageV1(
      Buffer.from(completeFieldPreimage as string, "hex").length,
    ) === "reference"
  ) {
    const publication = deriveValidationProofItemPublicationV1({
      transactionId: inputDatum.data.resolution.pre_state.transaction_id,
      transactionCommitment:
        inputDatum.data.resolution.pre_state.transaction_commitment,
      fieldPreimage: completeFieldPreimage as string,
    });
    signer.selectWallet(lucid);
    const publicationUnsigned = await Effect.runPromise(
      buildUnsignedValidationProofItemPublicationV1Program(
        lucid,
        contracts,
        publication,
      ),
    );
    const publicationSigned = await publicationUnsigned.sign
      .withWallet()
      .complete();
    const publicationCbor = publicationSigned.toCBOR();
    requireL1ProofEnvelope(
      publicationCbor,
      "Validation complete proof-item publication",
    );
    const publicationOutputIndex = findUniqueInlineDatumOutputIndex({
      transactionCbor: publicationCbor,
      address: contracts.validationTraceDispute.proofItem.spendingScriptAddress,
      datum: publication.datumCbor,
      label: "Validation complete proof-item publication",
    });
    const publicationTxHash = await publicationSigned.submit();
    // A reference input cannot be consumed until its creating transaction is
    // visible, even when the caller elects not to await the later resolution.
    await lucid.awaitTx(publicationTxHash, DEFAULT_CONFIRMATION_POLL_MS);
    resolvedProofItemReferenceOutRef = `${publicationTxHash}#${publicationOutputIndex.toString()}`;
    proofItemReferenceUtxo = await fetchUtxoByOutRef({
      lucid,
      outRef: {
        txHash: publicationTxHash,
        outputIndex: publicationOutputIndex,
      },
      label: "published validation complete proof-item reference UTxO",
    });
    proofItemPublication = {
      txHash: publicationTxHash,
      outRef: resolvedProofItemReferenceOutRef,
      outputIndex: publicationOutputIndex,
      completeSignedBytes: publicationCbor.length / 2,
      lovelace: proofItemReferenceUtxo.assets.lovelace ?? 0n,
      awaitedConfirmation: true,
    };
  }
  if (proofItemReferenceUtxo !== undefined) {
    if (!isCompleteCanonicalItem) {
      throw new Error(
        "Validation complete proof-item reference is only valid for a CanonicalDecode complete item",
      );
    }
    if (committedItemCarriage?.carriage !== "Inline") {
      throw new Error(
        "Validation complete proof-item reference reconstructs tier-1 `Inline` carriage and is not available above §8.3's tier-1 cap",
      );
    }
    if (
      proofItemReferenceUtxo.address !==
        contracts.validationTraceDispute.proofItem.spendingScriptAddress ||
      proofItemReferenceUtxo.datum == null ||
      proofItemReferenceUtxo.scriptRef !== undefined
    ) {
      throw new Error(
        "Validation complete proof-item reference is not locked by the deployed proof-item validator with only an inline datum",
      );
    }
    const expectedDatum = {
      version: 1n,
      transaction_id: inputDatum.data.resolution.pre_state.transaction_id,
      transaction_commitment:
        inputDatum.data.resolution.pre_state.transaction_commitment,
      field_preimage: completeFieldPreimage as string,
    };
    if (
      Data.to(expectedDatum, ValidationProofItemDatumV1) !==
      proofItemReferenceUtxo.datum
    ) {
      throw new Error(
        "Validation complete proof-item reference datum does not match the prepared evidence",
      );
    }
  }
  const range = requireValidityRange(
    validityRange ?? validationDisputeValidityRange(Date.now()),
  );
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: { version: 1n },
    },
    WinningValidationResolutionDatumV1,
  );
  const isSplitScriptSourcesStageOne =
    resolverIndex === 8 &&
    staged.semanticResolverIndex === 28 &&
    hasValidationAuxiliaryShapeV1(
      staged.auxiliary,
      VALIDATION_AUXILIARY_SHAPES_V1.redeemerItemStep,
    );
  if (isSplitScriptSourcesStageOne) {
    if (proofItemReferenceUtxo !== undefined) {
      throw new Error(
        "ScriptSources split stage-one route does not accept a proof-item reference",
      );
    }
    const stages =
      contracts.validationTraceDispute.scriptSourcesStageOneRedeemerStages;
    if (
      semanticContract.spendingScriptHash !==
        stages.envelope.spendingScriptHash ||
      semanticContract.spendingScriptAddress !==
        stages.envelope.spendingScriptAddress
    ) {
      throw new Error(
        "ScriptSources split stage-one semantic resolver is not the deployed envelope validator",
      );
    }
    const route = deriveScriptSourcesStageOneRouteDataV1({
      preparedResolution: inputDatum.data,
      fraudProver: inputDatum.fraud_prover,
      auxiliary: staged.auxiliary,
      deploymentId: deriveValidationTraceDeploymentIdV1(
        fraudProofCataloguePolicyId,
      ),
      envelopeScriptHash: stages.envelope.spendingScriptHash,
      traversalScriptHash: stages.traversalNormalizer.spendingScriptHash,
      outerScriptHash: stages.outerNormalizer.spendingScriptHash,
      foldMapScriptHash: stages.foldMapExecutor.spendingScriptHash,
      finalizeFrameScriptHash: stages.finalizeFrameExecutor.spendingScriptHash,
      settlementScriptHash: stages.settlement.spendingScriptHash,
    });
    type SplitStageContract = {
      readonly spendingScriptAddress: string;
      readonly spendingScript: Script;
    };
    type SplitStageResult = {
      readonly txHash: string;
      readonly nextThreadOutRef: string;
      readonly completeSignedBytes: number;
      readonly layout: ContinueLayout;
      readonly nextThreadUtxo?: UTxO;
    };
    const submitSplitStage = async ({
      inputUtxo,
      inputContract,
      outputContract,
      stageOutputDatum,
      label,
      awaitStage,
      encode,
    }: {
      readonly inputUtxo: UTxO;
      readonly inputContract: SplitStageContract;
      readonly outputContract: SplitStageContract;
      readonly stageOutputDatum: string;
      readonly label: string;
      readonly awaitStage: boolean;
      readonly encode: (layout: {
        readonly inputIndex: bigint;
        readonly outputIndex: bigint;
      }) => string;
    }): Promise<SplitStageResult> => {
      let stageLayout: ContinueLayout | undefined;
      signer.selectWallet(lucid);
      const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
      const currentLedgerTime = lucid.slotToUnixTime(lucid.currentSlot());
      const stageRange =
        validityRange === undefined
          ? requireValidityRange(
              validationDisputeValidityRange(currentLedgerTime),
            )
          : refreshExpiredValidationDisputeValidityRange({
              range,
              currentLedgerTime,
            });
      const stageTx = lucid
        .newTx()
        .collectFrom([feeInput])
        .collectFrom(
          [inputUtxo],
          makeIndexedValidationStageRedeemer({
            threadUtxo: inputUtxo,
            outputAddress: outputContract.spendingScriptAddress,
            outputDatum: stageOutputDatum,
            threadUnit: token.unit,
            label,
            encode,
            onLayout: (resolvedLayout) => {
              stageLayout = resolvedLayout;
            },
          }),
        )
        .pay.ToContract(
          outputContract.spendingScriptAddress,
          { kind: "inline", value: stageOutputDatum },
          threadAssets(inputUtxo, token.unit),
        )
        .validFrom(stageRange.validFrom)
        .validTo(stageRange.validTo)
        .addSignerKey(signer.paymentKeyHash)
        .attach.SpendingValidator(inputContract.spendingScript);
      let unsigned: Awaited<ReturnType<typeof stageTx.complete>>;
      try {
        unsigned = await stageTx.complete({ localUPLCEval: true });
      } catch (cause) {
        const detail = cause instanceof Error ? cause.message : String(cause);
        throw new Error(`${label} local evaluation failed: ${detail}`);
      }
      if (stageLayout === undefined) {
        throw new Error(`BuildTxWithRedeemer did not resolve ${label} layout`);
      }
      const resolvedLayout = stageLayout as ContinueLayout;
      const signed = await unsigned.sign.withWallet().complete();
      const signedCbor = signed.toCBOR();
      requireL1ProofEnvelope(signedCbor, label);
      const txHash = await signed.submit();
      const nextThreadOutRef = `${txHash}#${resolvedLayout.outputIndex.toString()}`;
      let nextThreadUtxo: UTxO | undefined;
      if (awaitStage) {
        await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
        nextThreadUtxo = await fetchUtxoByOutRef({
          lucid,
          outRef: {
            txHash,
            outputIndex: Number(resolvedLayout.outputIndex),
          },
          label: `${label} output`,
        });
      }
      return {
        txHash,
        nextThreadOutRef,
        completeSignedBytes: signedCbor.length / 2,
        layout: resolvedLayout,
        ...(nextThreadUtxo === undefined ? {} : { nextThreadUtxo }),
      };
    };
    const envelope = await submitSplitStage({
      inputUtxo: threadUtxo,
      inputContract: stages.envelope,
      outputContract: stages.traversalNormalizer,
      stageOutputDatum: route.traversalDatum,
      label: "Validation ScriptSources redeemer envelope binding",
      awaitStage: true,
      encode: ({ inputIndex, outputIndex }) =>
        encodeScriptSourcesStageOneSpendRedeemerV1({
          stage: "envelope",
          inputIndex,
          outputIndex,
          transition: staged.transitionData,
          auxiliary: staged.auxiliary,
          expectedNextItemControlHash: route.expectedNextItemControlHash,
          family: route.family,
        }),
    });
    const traversal = await submitSplitStage({
      inputUtxo: envelope.nextThreadUtxo!,
      inputContract: stages.traversalNormalizer,
      outputContract: stages.outerNormalizer,
      stageOutputDatum: route.outerDatum,
      label: "Validation ScriptSources redeemer traversal normalization",
      awaitStage: true,
      encode: ({ inputIndex, outputIndex }) =>
        encodeScriptSourcesStageOneSpendRedeemerV1({
          stage: "traversal",
          inputIndex,
          outputIndex,
          auxiliary: staged.auxiliary,
          currentItemControl: route.currentControlData,
          traversalAction: route.traversalActionData,
        }),
    });
    const outer = await submitSplitStage({
      inputUtxo: traversal.nextThreadUtxo!,
      inputContract: stages.outerNormalizer,
      outputContract:
        route.family === 0
          ? stages.foldMapExecutor
          : stages.finalizeFrameExecutor,
      stageOutputDatum: route.executorDatum,
      label: "Validation ScriptSources redeemer outer normalization",
      awaitStage: true,
      encode: ({ inputIndex, outputIndex }) =>
        encodeScriptSourcesStageOneSpendRedeemerV1({
          stage: "outer",
          inputIndex,
          outputIndex,
        }),
    });
    const execute = await submitSplitStage({
      inputUtxo: outer.nextThreadUtxo!,
      inputContract:
        route.family === 0
          ? stages.foldMapExecutor
          : stages.finalizeFrameExecutor,
      outputContract: stages.settlement,
      stageOutputDatum: route.settlementDatum,
      label:
        route.family === 0
          ? "Validation ScriptSources FoldMap execution"
          : "Validation ScriptSources FinalizeFrame execution",
      awaitStage: true,
      encode: ({ inputIndex, outputIndex }) =>
        encodeScriptSourcesStageOneSpendRedeemerV1({
          stage: "executor",
          inputIndex,
          outputIndex,
          traversalAction: route.traversalActionData,
        }),
    });
    const settle = await submitSplitStage({
      inputUtxo: execute.nextThreadUtxo!,
      inputContract: stages.settlement,
      outputContract: contracts.validationTraceDispute.award,
      stageOutputDatum: outputDatum,
      label: "Validation ScriptSources redeemer execution settlement",
      awaitStage: awaitConfirmation,
      encode: ({ inputIndex, outputIndex }) =>
        encodeScriptSourcesStageOneSpendRedeemerV1({
          stage: "settlement",
          inputIndex,
          outputIndex,
          envelope: route.envelopeData,
        }),
    });
    const stageTransactions = [
      { kind: "envelope" as const, ...envelope },
      { kind: "traversal" as const, ...traversal },
      { kind: "outer" as const, ...outer },
      {
        kind:
          route.family === 0
            ? ("execute-fold-map" as const)
            : ("execute-finalize-frame" as const),
        ...execute,
      },
      { kind: "settle" as const, ...settle },
    ].map(({ kind, txHash, nextThreadOutRef, completeSignedBytes }) => ({
      kind,
      txHash,
      nextThreadOutRef,
      completeSignedBytes,
    }));
    return {
      txHash: settle.txHash,
      threadOutRef,
      nextThreadOutRef: settle.nextThreadOutRef,
      proofItemCarriage: "direct",
      resolverIndex,
      semanticResolverIndex: staged.semanticResolverIndex,
      semanticResolverGlobalIndex: staged.semanticResolverGlobalIndex,
      inputIndex: Number(settle.layout.inputIndex),
      outputIndex: Number(settle.layout.outputIndex),
      awaitedConfirmation: awaitConfirmation,
      stageTransactions,
    };
  }
  if (isCompleteCanonicalItem) {
    const stageData = deriveCanonicalDecodeItemStageDataV1({
      preparedResolution: inputDatum.data,
      transition: staged.transition,
      fieldPreimage: completeFieldPreimage as string,
    });
    const authenticatedDatum = Data.to(
      {
        fraud_prover: inputDatum.fraud_prover,
        data: stageData.authenticated,
      },
      AuthenticatedCanonicalDecodeItemDatumV1,
    );
    const preparedDatum = Data.to(
      {
        fraud_prover: inputDatum.fraud_prover,
        data: stageData.prepared,
      },
      PreparedCanonicalDecodeItemDatumV1,
    );
    const observedDatum = Data.to(
      {
        fraud_prover: inputDatum.fraud_prover,
        data: stageData.observed,
      },
      ObservedCanonicalDecodeItemDatumV1,
    );
    const verifiedDatum = Data.to(
      {
        fraud_prover: inputDatum.fraud_prover,
        data: stageData.verified,
      },
      VerifiedCanonicalDecodeItemDatumV1,
    );
    type StageContract = {
      readonly spendingScriptAddress: string;
      readonly spendingScript: Script;
    };
    type SubmittedStage = {
      readonly txHash: string;
      readonly nextThreadOutRef: string;
      readonly completeSignedBytes: number;
      readonly layout: ContinueLayout;
      readonly nextThreadUtxo?: UTxO;
    };
    const submitStage = async ({
      inputUtxo,
      inputContract,
      outputContract,
      stageOutputDatum,
      label,
      proofReference,
      scriptReference,
      carriageReferences,
      awaitStage,
      encode,
    }: {
      readonly inputUtxo: UTxO;
      readonly inputContract: StageContract;
      readonly outputContract: StageContract;
      readonly stageOutputDatum: string;
      readonly label: string;
      readonly proofReference?: UTxO;
      readonly scriptReference?: UTxO;
      /** §8 tiers 2-3: the carriage UTxOs the committed indices name. */
      readonly carriageReferences?: readonly UTxO[];
      readonly awaitStage: boolean;
      readonly encode: (layout: {
        readonly inputIndex: bigint;
        readonly outputIndex: bigint;
        readonly referenceInputIndex?: bigint;
      }) => string;
    }): Promise<SubmittedStage> => {
      let stageLayout: ContinueLayout | undefined;
      signer.selectWallet(lucid);
      const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
      let stageTx = lucid
        .newTx()
        .collectFrom([feeInput])
        .collectFrom(
          [inputUtxo],
          makeIndexedValidationStageRedeemer({
            threadUtxo: inputUtxo,
            outputAddress: outputContract.spendingScriptAddress,
            outputDatum: stageOutputDatum,
            threadUnit: token.unit,
            proofItemReferenceUtxo: proofReference,
            label,
            encode,
            onLayout: (resolvedLayout) => {
              stageLayout = resolvedLayout;
            },
          }),
        );
      if (proofReference !== undefined) {
        stageTx = stageTx.readFrom([proofReference]);
      }
      if (scriptReference !== undefined) {
        stageTx = stageTx.readFrom([scriptReference]);
      }
      if (carriageReferences !== undefined && carriageReferences.length > 0) {
        stageTx = stageTx.readFrom([...carriageReferences]);
      }
      const currentLedgerTime = lucid.slotToUnixTime(lucid.currentSlot());
      const stageRange =
        validityRange === undefined
          ? requireValidityRange(
              validationDisputeValidityRange(currentLedgerTime),
            )
          : refreshExpiredValidationDisputeValidityRange({
              range,
              currentLedgerTime,
            });
      stageTx = stageTx.pay
        .ToContract(
          outputContract.spendingScriptAddress,
          { kind: "inline", value: stageOutputDatum },
          threadAssets(inputUtxo, token.unit),
        )
        .validFrom(stageRange.validFrom)
        .validTo(stageRange.validTo)
        .addSignerKey(signer.paymentKeyHash);
      // The published reference script supplies the spending validator; the
      // proof transaction must not embed the validator body inside the
      // 16,384-byte L1 envelope.
      if (scriptReference === undefined) {
        stageTx = stageTx.attach.SpendingValidator(
          inputContract.spendingScript,
        );
      }
      let unsigned: Awaited<ReturnType<typeof stageTx.complete>>;
      try {
        unsigned = await stageTx.complete({ localUPLCEval: true });
      } catch (cause) {
        const detail = cause instanceof Error ? cause.message : String(cause);
        throw new Error(`${label} local evaluation failed: ${detail}`);
      }
      if (stageLayout === undefined) {
        throw new Error(`BuildTxWithRedeemer did not resolve ${label} layout`);
      }
      const resolvedLayout = stageLayout as ContinueLayout;
      const signed = await unsigned.sign.withWallet().complete();
      const signedCbor = signed.toCBOR();
      requireL1ProofEnvelope(signedCbor, label);
      const txHash = await signed.submit();
      const nextThreadOutRef = `${txHash}#${resolvedLayout.outputIndex.toString()}`;
      let nextThreadUtxo: UTxO | undefined;
      if (awaitStage) {
        await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
        nextThreadUtxo = await fetchUtxoByOutRef({
          lucid,
          outRef: {
            txHash,
            outputIndex: Number(resolvedLayout.outputIndex),
          },
          label: `${label} output`,
        });
      }
      return {
        txHash,
        nextThreadOutRef,
        completeSignedBytes: signedCbor.length / 2,
        layout: resolvedLayout,
        ...(nextThreadUtxo === undefined ? {} : { nextThreadUtxo }),
      };
    };
    const stages = contracts.validationTraceDispute.canonicalDecodeItemStages;
    const authenticate = await submitStage({
      inputUtxo: threadUtxo,
      inputContract: semanticContract,
      outputContract: stages.source,
      stageOutputDatum: authenticatedDatum,
      label: "Validation canonical item authentication",
      proofReference: proofItemReferenceUtxo,
      scriptReference: semanticReferenceScriptUtxo,
      awaitStage: true,
      // #592's `Verify` is `(input_index, output_index, transition, carriage)`.
      // The carriage is the auxiliary's single field, forwarded verbatim so the
      // `hash_one_step_evidence` equality the stage checks is over the bytes
      // PrepareSelected committed and not over a re-encoding of them.
      encode: ({ inputIndex, outputIndex, referenceInputIndex }) =>
        proofItemReferenceUtxo === undefined
          ? Data.to(
              new Constr(1, [
                new Constr(0, [
                  inputIndex,
                  outputIndex,
                  staged.transitionData,
                  staged.auxiliary.fields[0]!,
                ]),
              ]),
            )
          : Data.to(
              new Constr(1, [
                new Constr(1, [
                  inputIndex,
                  outputIndex,
                  staged.transitionData,
                  referenceInputIndex!,
                ]),
              ]),
            ),
    });
    const source = await submitStage({
      inputUtxo: authenticate.nextThreadUtxo!,
      inputContract: stages.source,
      outputContract: stages.observe,
      stageOutputDatum: preparedDatum,
      label: "Validation canonical item source binding",
      awaitStage: true,
      encode: ({ inputIndex, outputIndex }) =>
        Data.to(new Constr(1, [new Constr(0, [inputIndex, outputIndex])])),
    });
    // **The §8.8 door's own transaction (#600, ruling D3-A).** The observe stage
    // is the one stage that dereferences the carriage — every earlier stage only
    // hashes it — so it is the one that has to read the carriage UTxOs, and the
    // one whose reference-input set the committed positional indices are indices
    // into.
    //
    // The indices were resolved by content (§8.7) against a reference-input set
    // chosen three transactions ago and frozen into `evidence_hash` at
    // PrepareSelected. Nothing since then has held the builder to that set. So
    // they are re-resolved here against exactly the list this stage will read,
    // and a disagreement refuses off chain — where re-staging is still free —
    // rather than on L1, where the staged evidence is already spent.
    const observeCarriageReferences =
      completeItemCarriageMaterial === undefined
        ? undefined
        : completeItemCarriageMaterial.referenceUtxos;
    if (
      committedItemCarriage !== undefined &&
      completeItemCarriageMaterial !== undefined
    ) {
      assertMidgardFieldCarriageResolvesAtDoorV1({
        carriage: committedItemCarriage,
        plan: completeItemCarriageMaterial.plan,
        // The complete set this stage reads, in the order it will hand it to
        // the ledger; the guard sorts canonically itself.
        doorReferenceInputs: [
          ...(proofItemReferenceUtxo === undefined
            ? []
            : [proofItemReferenceUtxo]),
          ...(observeCarriageReferences ?? []),
        ],
        ...(completeItemCarriageMaterial.certificatePolicyId === undefined
          ? {}
          : {
              certificatePolicyId:
                completeItemCarriageMaterial.certificatePolicyId,
            }),
        label: `Validation canonical item field ${completeItemCarriageMaterial.plan.fieldIndex.toString()}`,
      });
    }
    const observe = await submitStage({
      inputUtxo: source.nextThreadUtxo!,
      inputContract: stages.observe,
      outputContract: stages.proof,
      stageOutputDatum: observedDatum,
      label: "Validation canonical item observation",
      proofReference: proofItemReferenceUtxo,
      scriptReference: observeReferenceScriptUtxo,
      ...(observeCarriageReferences === undefined
        ? {}
        : { carriageReferences: observeCarriageReferences }),
      awaitStage: true,
      // #592's `Observe` is `(input_index, output_index, carriage)`.
      encode: ({ inputIndex, outputIndex, referenceInputIndex }) =>
        proofItemReferenceUtxo === undefined
          ? Data.to(
              new Constr(1, [
                new Constr(0, [
                  inputIndex,
                  outputIndex,
                  staged.auxiliary.fields[0]!,
                ]),
              ]),
            )
          : Data.to(
              new Constr(1, [
                new Constr(1, [inputIndex, outputIndex, referenceInputIndex!]),
              ]),
            ),
    });
    const proof = await submitStage({
      inputUtxo: observe.nextThreadUtxo!,
      inputContract: stages.proof,
      outputContract: stages.settlement,
      stageOutputDatum: verifiedDatum,
      label: "Validation canonical item proof verification",
      awaitStage: true,
      encode: ({ inputIndex, outputIndex }) =>
        Data.to(new Constr(1, [new Constr(0, [inputIndex, outputIndex])])),
    });
    const settle = await submitStage({
      inputUtxo: proof.nextThreadUtxo!,
      inputContract: stages.settlement,
      outputContract: contracts.validationTraceDispute.award,
      stageOutputDatum: outputDatum,
      label: "Validation canonical item successor settlement",
      awaitStage: awaitConfirmation,
      encode: ({ inputIndex, outputIndex }) =>
        Data.to(new Constr(1, [new Constr(0, [inputIndex, outputIndex])])),
    });
    const stageTransactions = [
      { kind: "authenticate" as const, ...authenticate },
      { kind: "source" as const, ...source },
      { kind: "observe" as const, ...observe },
      { kind: "proof" as const, ...proof },
      { kind: "settle" as const, ...settle },
    ].map(({ kind, txHash, nextThreadOutRef, completeSignedBytes }) => ({
      kind,
      txHash,
      nextThreadOutRef,
      completeSignedBytes,
    }));
    return {
      txHash: settle.txHash,
      threadOutRef,
      nextThreadOutRef: settle.nextThreadOutRef,
      proofItemCarriage:
        proofItemReferenceUtxo === undefined ? "direct" : "reference",
      ...(resolvedProofItemReferenceOutRef === undefined
        ? {}
        : { proofItemReferenceOutRef: resolvedProofItemReferenceOutRef }),
      ...(proofItemPublication === undefined ? {} : { proofItemPublication }),
      resolverIndex,
      semanticResolverIndex: staged.semanticResolverIndex,
      semanticResolverGlobalIndex: staged.semanticResolverGlobalIndex,
      inputIndex: Number(settle.layout.inputIndex),
      outputIndex: Number(settle.layout.outputIndex),
      awaitedConfirmation: awaitConfirmation,
      stageTransactions,
    };
  }
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  let tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeSemanticResolutionRedeemer({
        threadUtxo,
        outputAddress:
          contracts.validationTraceDispute.award.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        resolverIndex,
        semanticResolverIndex: staged.semanticResolverIndex,
        transition: staged.transitionData,
        auxiliary: staged.auxiliary,
        proofItemReferenceUtxo,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    );
  if (proofItemReferenceUtxo !== undefined) {
    tx = tx.readFrom([proofItemReferenceUtxo]);
  }
  tx = tx.pay
    .ToContract(
      contracts.validationTraceDispute.award.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(semanticContract.spendingScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation semantic resolution layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(signed.toCBOR(), "Validation semantic resolution");
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    proofItemCarriage:
      proofItemReferenceUtxo === undefined ? "direct" : "reference",
    ...(resolvedProofItemReferenceOutRef === undefined
      ? {}
      : { proofItemReferenceOutRef: resolvedProofItemReferenceOutRef }),
    ...(proofItemPublication === undefined ? {} : { proofItemPublication }),
    resolverIndex,
    semanticResolverIndex: staged.semanticResolverIndex,
    semanticResolverGlobalIndex: staged.semanticResolverGlobalIndex,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export type SubmitValidationDisputeAwardResult = ValidationFinalizationResult;

export const submitValidationDisputeAward = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeAwardResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "winning validation award UTxO",
  });
  const awardContract = contracts.validationTraceDispute.award;
  if (threadUtxo.address !== awardContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation award validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireWinningResolutionDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation award requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  return await submitValidationFinalizationTransaction({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadOutRef,
    token,
    spendingScript: awardContract,
    spendLabel: "Validation-dispute award",
    encodeSpendRedeemer: (layout) =>
      Data.to(
        {
          Continue: [
            {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              fraud_proof_mint_redeemer_index:
                layout.fraudProofMintRedeemerIndex,
            },
          ],
        },
        ValidationAwardSpendRedeemerV1,
      ),
    validityRange: range,
    awaitConfirmation,
  });
};

export type SubmitValidationDisputeDirectResolutionResult =
  ValidationFinalizationResult & {
    readonly resolverIndex: number;
    readonly selectedRoute:
      | "valueAndMint"
      | "noCekMaterial"
      | "directProof"
      | "completeSinglePublicationReference"
      | "minimumMultiOutputReconstruction"
      | "incrementalTraversal";
    readonly rejectedLocalRouteAttempts: readonly ValidationCekRejectedLocalRouteAttemptV1[];
  };

export type ValidationCekProgramMaterialReferenceOutRefsV1 = {
  /** Exact immutable complete-material datum outref. */
  readonly singlePublication?: string;
  /** Exact entry datums in strict material-root order. */
  readonly minimumMultiOutput?: readonly string[];
};

export type ValidationCekRejectedLocalRouteAttemptV1 = {
  readonly route:
    | "directProof"
    | "completeSinglePublicationReference"
    | "minimumMultiOutputReconstruction";
  readonly failure: string;
};

const errorMessageV1 = (cause: unknown): string =>
  cause instanceof Error ? cause.message : String(cause);

const isDeterministicLocalCekFitFailureV1 = (cause: unknown): boolean => {
  const message = errorMessageV1(cause);
  return /(?:complete signed L1 proof transaction must be no larger|maximum transaction size|maxTxSize|transaction.{0,24}(?:too large|too big)|maxValueSize|maximum value size|value.{0,24}(?:too large|too big)|maximum execution|execution (?:memory|cpu|units).{0,24}(?:exceed|too (?:large|big))|ExUnitsTooBig)/iu.test(
    message,
  );
};

const requireConfirmedCekMaterialReferenceUtxo = async ({
  lucid,
  outRef,
  expectedAddress,
  expectedDatum,
  label,
}: {
  readonly lucid: LucidEvolution;
  readonly outRef: string;
  readonly expectedAddress: string;
  readonly expectedDatum: string;
  readonly label: string;
}): Promise<UTxO> => {
  const utxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(outRef, label),
    label,
  });
  if (utxo.address !== expectedAddress) {
    throw new Error(
      `${label} ${outRefLabel(utxo)} is locked at ${utxo.address}, expected immutable CEK material address ${expectedAddress}`,
    );
  }
  if (utxo.datum !== expectedDatum) {
    throw new Error(
      `${label} ${outRefLabel(utxo)} does not carry the exact expected inline datum`,
    );
  }
  return utxo;
};

export const submitValidationDisputeDirectResolution = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  oneStepArgument,
  cekProgramMaterialReferenceOutRefs,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly oneStepArgument: ValidationOneStepSubmissionArgumentV1;
  readonly cekProgramMaterialReferenceOutRefs?: ValidationCekProgramMaterialReferenceOutRefsV1;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValidationDisputeDirectResolutionResult> => {
  const range = requireValidityRange(validityRange);
  const {
    deploymentInfo: parsedDeploymentInfo,
    referenceScriptAuthPolicyId,
    validationTraceDisputeCategory,
    contracts,
  } = await resolveValidationTraceDisputeDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    requireFraudProofSpend: true,
  });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation direct-resolver UTxO",
  });
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireResolutionDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Direct validation resolution requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const resolverIndex = validationResolverIndexV1(
    inputDatum.data.pre_state.phase,
  );
  if (resolverIndex !== oneStepArgument.resolverIndex) {
    throw new Error(
      "Validation one-step argument does not match the direct phase resolver",
    );
  }
  const direct = requireDirectOneStepArgumentV1(oneStepArgument);
  const directContract =
    contracts.validationTraceDispute.directResolvers[
      validationDirectResolverDeploymentIndexV1(resolverIndex)
    ];
  if (directContract === undefined) {
    throw new Error("Validation direct resolver deployment is incomplete");
  }
  if (threadUtxo.address !== directContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at direct resolver ${resolverIndex.toString()}`,
    );
  }
  // Resolver 11 (Cek) is direct resolver 0: its applied validator body can
  // never fit the L1 proof envelope, so every CEK finalization must consume
  // the published authenticated reference script instead of attaching it.
  const cekDirectResolverReferenceUtxo =
    resolverIndex === 11
      ? await requireValidationCekDirectResolverReferenceScriptUtxo({
          lucid,
          deploymentInfo: parsedDeploymentInfo,
          expectedScriptHash: directContract.spendingScriptHash,
          authPolicyId: referenceScriptAuthPolicyId,
        })
      : undefined;
  const baseAction = (layout: ValidationFinalizingSpendLayout) => ({
    input_index: layout.inputIndex,
    output_index: layout.outputIndex,
    fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
    challenger_evidence: direct.evidence,
  });
  if (resolverIndex === 12) {
    const finalized = await submitValidationFinalizationTransaction({
      lucid,
      contracts,
      signer,
      threadUtxo,
      threadOutRef,
      token,
      spendingScript: directContract,
      spendLabel: "Validation-dispute ValueAndMint direct resolution",
      encodeSpendRedeemer: (layout) =>
        encodeValidationDirectResolveSpendRedeemerV1(baseAction(layout)),
      validityRange: range,
      awaitConfirmation,
    });
    return {
      ...finalized,
      resolverIndex,
      selectedRoute: "valueAndMint",
      rejectedLocalRouteAttempts: [],
    };
  }
  if (direct.cekRouteMaterial === undefined) {
    const finalized = await submitValidationFinalizationTransaction({
      lucid,
      contracts,
      signer,
      threadUtxo,
      threadOutRef,
      token,
      spendingScript: directContract,
      spendingScriptReferenceUtxo: cekDirectResolverReferenceUtxo,
      spendLabel: "Validation-dispute CEK direct resolution",
      encodeSpendRedeemer: (layout) =>
        encodeValidationCekSpendRedeemerV1({
          ...baseAction(layout),
          material_route: "NoCekMaterial",
        }),
      validityRange: range,
      awaitConfirmation,
    });
    return {
      ...finalized,
      resolverIndex,
      selectedRoute: "noCekMaterial",
      rejectedLocalRouteAttempts: [],
    };
  }

  const routeMaterial = direct.cekRouteMaterial;
  const rejectedLocalRouteAttempts: ValidationCekRejectedLocalRouteAttemptV1[] =
    [];
  const prepareCekRoute = async ({
    route,
    materialReferenceUtxos = [],
    materialRoute,
  }: {
    readonly route: ValidationCekRejectedLocalRouteAttemptV1["route"];
    readonly materialReferenceUtxos?: readonly UTxO[];
    readonly materialRoute: (
      layout: ValidationFinalizingSpendLayout,
    ) => ValidationCekMaterialRouteV1;
  }): Promise<PreparedValidationFinalizationTransaction | undefined> => {
    try {
      return await prepareValidationFinalizationTransaction({
        lucid,
        contracts,
        signer,
        threadUtxo,
        threadOutRef,
        token,
        spendingScript: directContract,
        spendingScriptReferenceUtxo: cekDirectResolverReferenceUtxo,
        spendLabel: `Validation-dispute CEK ${route}`,
        encodeSpendRedeemer: (layout) =>
          encodeValidationCekSpendRedeemerV1({
            ...baseAction(layout),
            material_route: materialRoute(layout),
          }),
        materialReferenceUtxos,
        validityRange: range,
      });
    } catch (cause) {
      if (!isDeterministicLocalCekFitFailureV1(cause)) {
        throw cause;
      }
      rejectedLocalRouteAttempts.push({
        route,
        failure: errorMessageV1(cause),
      });
      return undefined;
    }
  };
  const submitSelectedRoute = async (
    prepared: PreparedValidationFinalizationTransaction,
    selectedRoute: SubmitValidationDisputeDirectResolutionResult["selectedRoute"],
  ): Promise<SubmitValidationDisputeDirectResolutionResult> => ({
    ...(await submitPreparedValidationFinalizationTransaction({
      prepared,
      awaitConfirmation,
    })),
    resolverIndex,
    selectedRoute,
    rejectedLocalRouteAttempts,
  });

  const directPrepared = await prepareCekRoute({
    route: "directProof",
    materialRoute: () => ({
      DirectCekMaterial: {
        envelope_cbor: routeMaterial.envelopeCbor.toString("hex"),
        sidecar_cbor: routeMaterial.programMaterialSidecarCbor.toString("hex"),
      },
    }),
  });
  if (directPrepared !== undefined) {
    return await submitSelectedRoute(directPrepared, "directProof");
  }

  const materialAddress =
    contracts.validationTraceDispute.cekProgramMaterial.spendingScriptAddress;
  const singlePublication = deriveCekSinglePublicationV1({
    envelopeCbor: routeMaterial.envelopeCbor,
    sidecarCbor: routeMaterial.programMaterialSidecarCbor,
  });
  const singleOutRef = cekProgramMaterialReferenceOutRefs?.singlePublication;
  if (singleOutRef === undefined) {
    throw new Error(
      "CEK direct proof did not fit; provide an already-confirmed exact single-publication material outref before selecting a more complex route",
    );
  }
  const singleReferenceUtxo = await requireConfirmedCekMaterialReferenceUtxo({
    lucid,
    outRef: singleOutRef,
    expectedAddress: materialAddress,
    expectedDatum: singlePublication.datumCbor,
    label: "CEK single-publication reference outref",
  });
  const singlePrepared = await prepareCekRoute({
    route: "completeSinglePublicationReference",
    materialReferenceUtxos: [singleReferenceUtxo],
    materialRoute: (layout) => {
      const reference_input_index = layout.materialReferenceInputIndices[0];
      if (reference_input_index === undefined) {
        throw new Error(
          "CEK single-publication reference is missing from final layout",
        );
      }
      return {
        SinglePublicationCekMaterial: {
          envelope_cbor: routeMaterial.envelopeCbor.toString("hex"),
          reference_input_index,
        },
      };
    },
  });
  if (singlePrepared !== undefined) {
    return await submitSelectedRoute(
      singlePrepared,
      "completeSinglePublicationReference",
    );
  }

  const entries = decodeMidgardCekProgramMaterialSidecarV1(
    routeMaterial.programMaterialSidecarCbor,
  );
  const expectedMultiPublications =
    deriveCekProgramMaterialPublicationsV1(entries);
  const multiOutRefs = cekProgramMaterialReferenceOutRefs?.minimumMultiOutput;
  if (multiOutRefs === undefined) {
    throw new Error(
      "CEK single-publication route did not fit; provide already-confirmed exact multi-output material outrefs in root order before selecting incremental traversal",
    );
  }
  if (multiOutRefs.length !== expectedMultiPublications.length) {
    throw new Error(
      `CEK minimum-multi route requires exactly ${expectedMultiPublications.length.toString()} root-ordered material outrefs, got ${multiOutRefs.length.toString()}`,
    );
  }
  if (new Set(multiOutRefs).size !== multiOutRefs.length) {
    throw new Error("CEK minimum-multi material outrefs must be unique");
  }
  const multiReferenceUtxos = await Promise.all(
    expectedMultiPublications.map((publication, index) =>
      requireConfirmedCekMaterialReferenceUtxo({
        lucid,
        outRef: multiOutRefs[index]!,
        expectedAddress: materialAddress,
        expectedDatum: publication.datumCbor,
        label: `CEK minimum-multi root-order outref ${index.toString()}`,
      }),
    ),
  );
  const multiPrepared = await prepareCekRoute({
    route: "minimumMultiOutputReconstruction",
    materialReferenceUtxos: multiReferenceUtxos,
    materialRoute: (layout) => ({
      MinimumMultiOutputCekMaterial: {
        envelope_cbor: routeMaterial.envelopeCbor.toString("hex"),
        reference_input_indices: [...layout.materialReferenceInputIndices],
      },
    }),
  });
  if (multiPrepared !== undefined) {
    return await submitSelectedRoute(
      multiPrepared,
      "minimumMultiOutputReconstruction",
    );
  }

  if (direct.cekIncrementalNecessityReceiptSet === undefined) {
    throw new Error(
      "CEK direct, single-publication, and minimum-multi routes did not fit; incremental traversal requires an exact receipt-bound necessity set",
    );
  }
  // The incremental route fails closed on L1. `IncrementalCekMaterial` in
  // `onchain/aiken/lib/midgard/validation-resolver-v1.ak` rejects
  // unconditionally: its former predicate compared the redeemer's
  // `program_envelope_hash` against a value derived from the disputer's own
  // selected envelope, so it verified no program material at all, and the
  // off-chain grammar it was specified against orders `proofContinuation`
  // transactions AFTER the `proofConsumption` finalization that mints the
  // fraud proof. Refuse here rather than construct a finalization that cannot
  // validate. The receipt-set machinery above and the ABI variant are retained
  // for the lease that adds the authenticated cross-transaction traversal
  // accumulator the sound route needs.
  throw new Error(
    "CEK incremental traversal is not verifiable on L1: the on-chain IncrementalCekMaterial route fails closed until an authenticated cross-transaction material-traversal accumulator is deployed. Publish the complete program material and resolve through the direct, single-publication, or minimum-multi-output route.",
  );
};

export const validationDisputeDescriptorData =
  validationTraceDescriptorDataFromCore;
