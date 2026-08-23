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
  resolveMidgardFieldCarriageAgainstReferenceInputsV1,
  ValidationAuxiliaryWitnessV1,
  type ValidationAuxiliaryWitnessV1 as ValidationAuxiliaryWitnessV1Data,
  ValidationAwardSpendRedeemerV1,
  ValidationBoundarySpendRedeemerV1,
  ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1Schema,
  type ValidationCekMaterialRouteV1,
  ValidationCekMaterialRouteV1Schema,
  type ValidationClaimWitnessV1,
  validationDisputeCoreFromData,
  validationDisputeDataFromCore,
  ValidationDisputeDatumV1,
  type ValidationDisputeDatumV1 as ValidationDisputeDatumV1Data,
  ValidationDisputeOpenSpendRedeemerV1,
  ValidationGameSpendRedeemerV1,
  type ValidationMachineStateV1,
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

/**
 * Build-time delivery cost heuristic for a tier-1 complete item (#619/#621).
 *
 * "direct" carries the §5.1 preimage in the observe redeemer; "reference"
 * routes it through a §8 proof-item publication and delivers by reference.
 * Since Option B the committed evidence is transition-only, so either route
 * can deliver any tier-1 item: this pin steers cost — one transaction versus
 * two — never soundness, and a stale pin degrades fees and latency, not
 * liveness. `maxReliableDirectCompleteItemBytes` is an owner-signed consensus
 * measurement; re-measuring the direct frontier and rebinding this heuristic
 * is #622's owner table, so the number is read here and never changed here.
 */
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

/**
 * A deterministic, known-valid ed25519 public key (CML's own documentation
 * example), used only to project a signed transaction's exact byte length
 * before any signature exists. A vkey witness's size is fixed — 32 key bytes
 * plus 64 signature bytes plus CBOR framing — so which key fills the slot
 * cannot change the projection.
 */
const PROJECTION_PLACEHOLDER_VKEY_BECH32 =
  "ed25519_pk1dgaagyh470y66p899txcl3r0jaeaxu6yd7z2dxyk55qcycdml8gszkxze2";

/**
 * Projects the exact byte length the unsigned transaction will have once the
 * prover's signature is attached, without signing anything (#621).
 *
 * The inline observe route decides pre-sign whether its redeemer-carried
 * preimage still fits the L1 proof envelope; deciding on the unsigned bytes
 * alone would under-count by one vkey witness, and guessing a delta is a
 * measurement smell. So the witness slot is filled with a placeholder key and
 * a zeroed 64-byte signature — byte-for-byte the size of the real ones — and
 * the assembled transaction is measured. `requireL1ProofEnvelope` still runs
 * on the actually-signed bytes afterwards, so a projection defect can never
 * ship an oversized transaction; the projection only decides routing.
 */
export const projectSignedL1ProofTransactionBytesV1 = (
  unsignedTransactionCbor: string,
): number => {
  const transaction = CML.Transaction.from_cbor_hex(unsignedTransactionCbor);
  const witnessSet = transaction.witness_set();
  const vkeyWitnesses = CML.VkeywitnessList.new();
  vkeyWitnesses.add(
    CML.Vkeywitness.new(
      CML.PublicKey.from_bech32(PROJECTION_PLACEHOLDER_VKEY_BECH32),
      CML.Ed25519Signature.from_raw_bytes(new Uint8Array(64)),
    ),
  );
  witnessSet.set_vkeywitnesses(vkeyWitnesses);
  const assembled = CML.Transaction.new(
    transaction.body(),
    witnessSet,
    transaction.is_valid(),
    transaction.auxiliary_data(),
  );
  return assembled.to_cbor_hex().length / 2;
};

/**
 * The pre-sign refusal of an inline observe build whose projected signed
 * bytes exceed the L1 proof envelope (#621). The staged-chain orchestration
 * catches exactly this error to fall back to the reference route, so it is a
 * distinct class rather than a message pattern.
 */
export class ValidationInlineDeliveryEnvelopeRefusedErrorV1 extends Error {
  readonly projectedSignedBytes: number;
  readonly maxTransactionBytes: number;

  constructor({
    label,
    projectedSignedBytes,
    maxTransactionBytes,
  }: {
    readonly label: string;
    readonly projectedSignedBytes: number;
    readonly maxTransactionBytes: number;
  }) {
    super(
      `${label} would sign at ${projectedSignedBytes.toString()} bytes, over the ` +
        `${maxTransactionBytes.toString()}-byte L1 proof envelope; refusing pre-sign. ` +
        "Inline delivery carries the §5.1 preimage in the observe redeemer, so an item " +
        "this large rides the §8 publication route instead.",
    );
    this.projectedSignedBytes = projectedSignedBytes;
    this.maxTransactionBytes = maxTransactionBytes;
  }
}

/** How a tier-1 complete item's preimage reaches the §8.8 door (#621). */
export type ValidationProofItemDeliveryV1 = "inline" | "reference";

/**
 * Resolves the delivery route for the CanonicalDecode complete-item path at
 * build time (#619/#621).
 *
 * Since Option B the committed evidence is transition-only, so nothing staged
 * on chain constrains how the preimage reaches the observe stage's §8.8 door:
 * inline in the redeemer or by reference to a §8 proof-item publication, both
 * of which the door authenticates by content. The choice is therefore a
 * builder-local cost decision, resolved here in precedence order — an
 * explicit `proofItemDelivery` request, then a supplied publication out-ref,
 * then the measured `selectValidationCompleteItemCarriageV1` heuristic.
 *
 * The routing choice exists only inside tier 1. Above §8.3's tier-1 cap the
 * §8.4 partition already names reference inputs of its own, so a delivery
 * request there is a refusal, not a preference — and no routing input can
 * brick an in-flight dispute: an inline build that outgrows the L1 envelope
 * falls back to the reference route pre-sign
 * ({@link ValidationInlineDeliveryEnvelopeRefusedErrorV1}).
 *
 * Returns the tier-1 route, or `undefined` when the argument is not a tier-1
 * complete item (tiers 2-3 and every other resolver path, where no such route
 * exists).
 */
export const resolveValidationProofItemDeliveryRouteV1 = ({
  requestedDelivery,
  hasProofItemReferenceOutRef,
  committedCarriage,
  preimageByteLength,
}: {
  readonly requestedDelivery: ValidationProofItemDeliveryV1 | undefined;
  readonly hasProofItemReferenceOutRef: boolean;
  /**
   * The staged auxiliary's §8.1 carriage constructor on the complete-item
   * path, `undefined` on every other resolver path.
   */
  readonly committedCarriage: "Inline" | "RawUtxo" | "Certified" | undefined;
  /** The tier-1 preimage's byte length; required when the carriage is `Inline`. */
  readonly preimageByteLength?: number;
}): ValidationProofItemDeliveryV1 | undefined => {
  if (committedCarriage === undefined) {
    if (requestedDelivery !== undefined) {
      throw new Error(
        "Validation proof-item delivery routing (`proofItemDelivery`) exists only on the " +
          "CanonicalDecode complete-item path (#621)",
      );
    }
    return undefined;
  }
  if (committedCarriage !== "Inline") {
    if (requestedDelivery !== undefined) {
      throw new Error(
        "Validation proof-item delivery routing is a tier-1 choice between the observe " +
          `redeemer and the §8 publication; tier-${committedCarriage === "RawUtxo" ? "2" : "3"} ` +
          `\`${committedCarriage}\` already names reference inputs (§8.4) and admits no ` +
          "routing override (#621)",
      );
    }
    return undefined;
  }
  if (requestedDelivery === "inline") {
    if (hasProofItemReferenceOutRef) {
      throw new Error(
        'Validation proof-item delivery "inline" contradicts `proofItemReferenceOutRef`: ' +
          "inline delivery carries the preimage in the observe redeemer and reads no " +
          "publication (#621)",
      );
    }
    return "inline";
  }
  if (requestedDelivery === "reference" || hasProofItemReferenceOutRef) {
    return "reference";
  }
  if (preimageByteLength === undefined) {
    throw new Error(
      "Validation proof-item delivery route needs the tier-1 preimage length to apply the " +
        "measured cost heuristic",
    );
  }
  return selectValidationCompleteItemCarriageV1(preimageByteLength) === "direct"
    ? "inline"
    : "reference";
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
export const VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1 = {
  1: "validationTraceDisputeCekExecutionSelectionSemantic",
  2: "validationTraceDisputeCekContextStepSemantic",
  3: "validationTraceDisputeCekCoreStepSemantic",
} as const satisfies Partial<Record<number, string>>;

export type ValidationCekSemanticReferenceScriptIndexV1 =
  keyof typeof VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1;

export const validationCekSemanticReferenceScriptDeploymentEntryV1 = (
  semanticResolverIndex: number,
): string | undefined =>
  semanticResolverIndex === 1 ||
  semanticResolverIndex === 2 ||
  semanticResolverIndex === 3
    ? VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1[
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
  const entryName = validationCekSemanticReferenceScriptDeploymentEntryV1(
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
 * on their own, before any redeemer: at #634's measurement replay_input
 * 21,203, replay_asset 21,881, replay_finish 20,962, output_descriptor 20,958,
 * output_asset 21,583, output_finish 20,740, mint_asset 18,458 and
 * mint_finish 17,767 bytes; begin (11,473), replay_begin (11,013) and finalize
 * (11,987) fit. A ValueAndMint semantic dispute was therefore provable at the
 * validator level but not carriable on L1 — the #627 min-Ada journey's
 * output-descriptor resolution measured 21,576 complete signed bytes.
 *
 * The roster is all eleven, not just the oversized eight. Which bodies clear
 * the envelope is a compilation fact that moves with every regeneration; the
 * deployment role is a property of the resolver, so every ValueAndMint
 * semantic is *deployable* by reference and the submit path picks the route
 * from what the deployment info actually carries (see
 * `validationValueAndMintSemanticReferenceScriptDeploymentEntryV1`'s call site
 * in the semantic-resolution builder). These entries are consumed the way
 * `validationTraceDisputeItemSemantic` and the CEK entries are: hash-checked
 * against the applied contract, no auth-role token.
 */
export const VALIDATION_VALUE_AND_MINT_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1 =
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

export type ValidationValueAndMintSemanticReferenceScriptIndexV1 =
  keyof typeof VALIDATION_VALUE_AND_MINT_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1;

/** The ValueAndMint phase's resolver index (`VALUE_AND_MINT` = 12). */
export const VALIDATION_VALUE_AND_MINT_RESOLVER_INDEX_V1 = 12;

export const validationValueAndMintSemanticReferenceScriptDeploymentEntryV1 = (
  semanticResolverIndex: number,
): string | undefined =>
  Number.isInteger(semanticResolverIndex) &&
  semanticResolverIndex >= 0 &&
  semanticResolverIndex <= 10
    ? VALIDATION_VALUE_AND_MINT_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1[
        semanticResolverIndex as ValidationValueAndMintSemanticReferenceScriptIndexV1
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
    validationValueAndMintSemanticReferenceScriptDeploymentEntryV1(
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
const validationCekMaterialRouteV1RuntimeSchema =
  ValidationCekMaterialRouteV1Schema as unknown as PlutusDataSchema;
const validationPrepareSelectedSpendRedeemerV1RuntimeSchema =
  ValidationPrepareSelectedSpendRedeemerV1Schema as unknown as PlutusDataSchema;
const validationCanonicalDecodePrepareSelectedSpendRedeemerV1RuntimeSchema =
  ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1Schema as unknown as PlutusDataSchema;

/**
 * The `material_route` field of the CEK execution-selection semantic action
 * (`cek_execution_selection_semantic_v1.VerifyExecutionSelection`), as Plutus
 * data. The route is resolver evidence — it names the consuming transaction's
 * own reference inputs — so it is never part of the committed evidence hash.
 */
export const validationCekMaterialRouteDataV1 = (
  route: ValidationCekMaterialRouteV1,
): PlutusDataValue =>
  Data.from(
    encodeWithRuntimeSchema(route, validationCekMaterialRouteV1RuntimeSchema),
  );

export type ValidationOneStepSubmissionArgumentV1 = {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
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
 * The §8 carriage material for a tiers-2/3 one-step argument: the producer's
 * carriage plan together with the ledger UTxOs that hold its published bytes
 * (#600 ruling D3-A, re-scoped by #619/#621).
 *
 * A tier-1 argument needs none of this: `Inline` carries its own bytes and
 * indexes nothing. Above §8.3's 14,336-byte cap the carriage is *only*
 * positional reference-input indices, so a submitter has to hand the builder
 * the material those indices will name — otherwise the door-running
 * transaction cannot reference the carriage at all.
 *
 * `plan` is the producer's own `planMidgardFieldCarriageV1` output, never a
 * hand-assembled record. Since Option B the committed evidence is
 * transition-only, so no index is frozen anywhere on chain: the builder
 * resolves the plan by content (§8.7) against the door transaction's own
 * reference-input set at build time and puts *that* carriage on the wire —
 * the plan is the content source the resolution works from.
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
 * The staged auxiliary's carriage, read back out as the
 * `MidgardFieldCarriageV1` the SDK resolvers speak.
 *
 * Constructor order is the frozen §8.1 one — `Inline` 0, `RawUtxo` 1,
 * `Certified` 2 — and every arm is checked for arity rather than
 * pattern-matched loosely. Since Option B the evidence hash no longer commits
 * this value; what a misread would silently corrupt is the §8.4 tier the
 * builder routes by and, at tier 1, the preimage bytes the delivery carries.
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

/**
 * Encodes a resolved `MidgardFieldCarriageV1` back onto the frozen §8.1 wire —
 * `Inline` 0, `RawUtxo` 1, `Certified` 2, mirroring
 * `onchain/aiken/lib/midgard/native-tx-field-access-v1.ak:168` — for the
 * observe redeemer. Since Option B the carriage on the wire is the one the
 * builder resolved against the door transaction's own reference inputs at
 * build time (#621), not a value replayed from the staged auxiliary, so the
 * encoder is the inverse of {@link midgardFieldCarriageFromDataV1} above.
 */
const midgardFieldCarriageToDataV1 = (
  carriage: MidgardFieldCarriageV1,
): PlutusDataValue => {
  switch (carriage.carriage) {
    case "Inline":
      return new Constr(0, [carriage.preimage.toString("hex")]);
    case "RawUtxo":
      return new Constr(1, [BigInt(carriage.refInputIndex)]);
    case "Certified":
      return new Constr(2, [
        BigInt(carriage.certRefInputIndex),
        carriage.chunkRefInputIndices.map((index) => BigInt(index)),
      ]);
  }
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
 * uses it. A Plutus/Midgard CEK selection (resolver 11, semantic resolver 1 —
 * `cek_execution_selection_semantic_v1`) must carry complete route material;
 * later CEK steps, ValueAndMint, and every other staged phase must not carry
 * it.
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
    argument.semanticResolverIndex === 1 &&
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
  2, 1, 1, 2, 4, 14, 2, 6, 29, 3, 4, 4, 11, 8,
] as const;
const VALIDATION_SEMANTIC_RESOLVER_OFFSETS_V1 = [
  0, 2, 3, 4, 6, 10, 24, 26, 32, 60, 63, 67, 71, 82,
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
  // R5 item 1: the cek and ValueAndMint auxiliary families, now that both
  // phases route through prepare + per-kind semantic resolvers.
  nativeExecutionScan: [11, 16],
  cekCoreStep: [12, 1],
  cekResolvedContextItem: [13, 5],
  cekOutputContextItem: [14, 3],
  cekSignerContextItem: [15, 4],
  cekMintContextItem: [16, 5],
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
const VALIDATION_CEK_CONTEXT_STEP_AUXILIARY_SHAPES_V1 = [
  VALIDATION_AUXILIARY_SHAPES_V1.none,
  VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldChunk,
  VALIDATION_AUXILIARY_SHAPES_V1.redeemerScanBegin,
  VALIDATION_AUXILIARY_SHAPES_V1.cekResolvedContextItem,
  VALIDATION_AUXILIARY_SHAPES_V1.cekOutputContextItem,
  VALIDATION_AUXILIARY_SHAPES_V1.cekSignerContextItem,
  VALIDATION_AUXILIARY_SHAPES_V1.cekMintContextItem,
  VALIDATION_AUXILIARY_SHAPES_V1.cekRedeemerContextSelect,
  VALIDATION_AUXILIARY_SHAPES_V1.redeemerItemStep,
  VALIDATION_AUXILIARY_SHAPES_V1.cekContextFinalize,
  VALIDATION_AUXILIARY_SHAPES_V1.cekContextFinalizeSpend,
  VALIDATION_AUXILIARY_SHAPES_V1.cekContextAssemble,
  VALIDATION_AUXILIARY_SHAPES_V1.cekTxInfoFinalize,
  VALIDATION_AUXILIARY_SHAPES_V1.cekContextSeed,
] as const;

/**
 * Auxiliary shape per ValueAndMint semantic resolver
 * (`value_and_mint_v1` prepare order): the stage-entry, replay-finish,
 * output-finish, mint-finish and finalize resolvers step with no auxiliary;
 * the item resolvers carry the stage body's witness.
 */
const VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES_V1 = [
  VALIDATION_AUXILIARY_SHAPES_V1.none,
  VALIDATION_AUXILIARY_SHAPES_V1.none,
  VALIDATION_AUXILIARY_SHAPES_V1.resolvedInputReplay,
  VALIDATION_AUXILIARY_SHAPES_V1.valueInputAsset,
  VALIDATION_AUXILIARY_SHAPES_V1.none,
  VALIDATION_AUXILIARY_SHAPES_V1.valueOutputDescriptor,
  VALIDATION_AUXILIARY_SHAPES_V1.valueOutputAsset,
  VALIDATION_AUXILIARY_SHAPES_V1.none,
  VALIDATION_AUXILIARY_SHAPES_V1.valueMintAsset,
  VALIDATION_AUXILIARY_SHAPES_V1.none,
  VALIDATION_AUXILIARY_SHAPES_V1.none,
] as const;

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
  if (resolverIndex === 11) {
    if (semanticResolverIndex === 2) {
      if (
        auxiliary instanceof Constr &&
        VALIDATION_CEK_CONTEXT_STEP_AUXILIARY_SHAPES_V1.some((shape) =>
          hasValidationAuxiliaryShapeV1(auxiliary, shape),
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
        ? VALIDATION_AUXILIARY_SHAPES_V1.none
        : semanticResolverIndex === 1
          ? VALIDATION_AUXILIARY_SHAPES_V1.nativeExecutionScan
          : VALIDATION_AUXILIARY_SHAPES_V1.cekCoreStep;
    return requireConstr({
      value: auxiliary,
      index: expected[0],
      fields: expected[1],
      label: "validation Cek auxiliary witness",
    });
  }
  if (resolverIndex === 12) {
    const expected =
      VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES_V1[semanticResolverIndex];
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
  readonly cekRouteMaterial?: CekRouteMaterialV1;
  readonly cekIncrementalNecessityReceiptSet?: CekProgramMaterialNecessityReceiptSetV1;
} => {
  const validatedCekEvidence = validateCekSubmissionEvidenceV1(argument);
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
    // Option B (#620): the canonical-decode resolver commits to the transition
    // alone — the auxiliary hashed into `evidence_hash` is `NoAuxiliaryWitness`
    // whatever carriage the auxiliary witness names, because the carriage is
    // dereferenced (and content-checked) only at the observe stage's §8.8 door.
    // Every other resolver still freezes its auxiliary into the commitment.
    evidenceHash: validationOneStepEvidenceHashFromDataV1(
      transitionData,
      argument.resolverIndex === 0 ? new Constr(0, []) : auxiliaryData,
    ),
    ...validatedCekEvidence,
  };
};

export const validationSemanticResolverGlobalIndexV1 = (
  resolverIndex: number,
  semanticResolverIndex: number,
): number =>
  resolverIndex === 8 && semanticResolverIndex === 28
    ? 90
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

/**
 * Every one of the fourteen resolver indices is a `prepare_selected`
 * validator since R5 item 1 split the cek and ValueAndMint direct resolvers,
 * so the prepare-resolver deployment order is the resolver order itself.
 */
const validationPrepareResolverDeploymentIndexV1 = (
  resolverIndex: number,
): number => {
  if (
    resolverIndex >= 0 &&
    resolverIndex < VALIDATION_SEMANTIC_RESOLVER_COUNTS_V1.length
  ) {
    return resolverIndex;
  }
  throw new Error(
    `Validation resolver ${resolverIndex.toString()} is not staged`,
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
  resolverIndex,
  semanticResolverIndex,
  transition,
  auxiliary,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly transition: ValidationOneStepWitnessV1;
  readonly auxiliary: ValidationAuxiliaryWitnessV1Data;
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
    // Option B (#620): the canonical-decode resolver's `PrepareSelected` is
    // transition-only — the validator computes the evidence hash on-chain from
    // `(transition, NoAuxiliaryWitness)`, so neither the auxiliary nor a
    // prover-supplied hash rides in the redeemer. Every other resolver keeps
    // the five-field aux-bearing shape.
    return resolverIndex === 0
      ? encodeWithRuntimeSchema(
          { Continue: [base] },
          validationCanonicalDecodePrepareSelectedSpendRedeemerV1RuntimeSchema,
        )
      : encodeWithRuntimeSchema(
          { Continue: [{ ...base, auxiliary }] },
          validationPrepareSelectedSpendRedeemerV1RuntimeSchema,
        );
  }) satisfies BuildTxWithRedeemer;

const semanticActionFieldsV1 = ({
  resolverIndex,
  semanticResolverIndex,
  inputIndex,
  outputIndex,
  transition,
  auxiliary,
  materialRoute,
}: {
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly transition: PlutusDataValue;
  readonly auxiliary: Constr<PlutusDataValue>;
  /**
   * The CEK execution-selection material route
   * (`validationCekMaterialRouteDataV1`); required by, and only by, resolver
   * 11 semantic resolver 1.
   */
  readonly materialRoute?: PlutusDataValue;
}): readonly PlutusDataValue[] => {
  const base: readonly PlutusDataValue[] = [
    inputIndex,
    outputIndex,
    transition,
  ];
  if (resolverIndex !== 11 || semanticResolverIndex !== 1) {
    if (materialRoute !== undefined) {
      throw new Error(
        "CEK material route is permitted only for the CEK execution-selection semantic resolver",
      );
    }
  }
  if (resolverIndex === 11) {
    // `cek_v1` prepare order: finish (no auxiliary), execution selection
    // (`VerifyExecutionSelection { …, auxiliary, material_route }`), context
    // step (`VerifyContextStep { …, auxiliary }`) and core step
    // (`VerifyCoreStep { …, step }`).
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
        VALIDATION_AUXILIARY_SHAPES_V1.nativeExecutionScan,
      )
    ) {
      if (materialRoute === undefined) {
        throw new Error(
          "CEK execution-selection semantic redeemer requires a material route",
        );
      }
      return [...base, auxiliary, materialRoute];
    }
    if (
      semanticResolverIndex === 2 &&
      VALIDATION_CEK_CONTEXT_STEP_AUXILIARY_SHAPES_V1.some((shape) =>
        hasValidationAuxiliaryShapeV1(auxiliary, shape),
      )
    ) {
      return [...base, auxiliary];
    }
    if (
      semanticResolverIndex === 3 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.cekCoreStep,
      )
    ) {
      return [...base, ...auxiliary.fields];
    }
    throw new Error(
      "Cek auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
  if (resolverIndex === 12) {
    // `value_and_mint_v1` prepare order; every item resolver flattens its
    // witness into the action (`ledger_output_index` stands in for the
    // witness's `output_index` on the output-descriptor and output-asset
    // actions, which is a field rename on the wire-identical position).
    const expected =
      VALIDATION_VALUE_AND_MINT_AUXILIARY_SHAPES_V1[semanticResolverIndex];
    if (
      expected !== undefined &&
      hasValidationAuxiliaryShapeV1(auxiliary, expected)
    ) {
      return expected[0] === 0 ? base : [...base, ...auxiliary.fields];
    }
    throw new Error(
      "ValueAndMint auxiliary witness cannot construct the selected semantic redeemer",
    );
  }
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
    // Option B (#620): the item-semantic stage re-checks the transition-only
    // commitment and takes no carriage in any form — the carriage is
    // dereferenced once, at the observe stage's §8.8 door. The retired
    // four-field `Verify` was the only wire a chunk-shaped auxiliary could
    // ever have ridden, so a chunk here is now a refusal, not a route.
    if (
      semanticResolverIndex === 1 &&
      hasValidationAuxiliaryShapeV1(
        auxiliary,
        VALIDATION_AUXILIARY_SHAPES_V1.transactionFieldItem,
      )
    ) {
      return base;
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
  materialRoute,
}: {
  readonly oneStepArgument: ValidationOneStepSubmissionArgumentV1;
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  /** Required by, and only by, the CEK execution-selection resolver (11/1). */
  readonly materialRoute?: ValidationCekMaterialRouteV1;
}): Buffer => {
  if (inputIndex < 0n || outputIndex < 0n) {
    throw new Error(
      "Validation semantic redeemer indexes must be non-negative",
    );
  }
  // Option B (#620): the item-semantic `Verify` is transition-only — no
  // carriage field and no retired `VerifyReference` arm — so the CanonicalDecode
  // complete item flows through the generic `semanticActionFieldsV1` shape like
  // every other semantic action.
  const staged = requireStagedOneStepArgumentV1(oneStepArgument);
  const fields = semanticActionFieldsV1({
    resolverIndex: oneStepArgument.resolverIndex,
    semanticResolverIndex: staged.semanticResolverIndex,
    inputIndex,
    outputIndex,
    transition: staged.transitionData,
    auxiliary: staged.auxiliary,
    ...(materialRoute === undefined
      ? {}
      : { materialRoute: validationCekMaterialRouteDataV1(materialRoute) }),
  });
  return Buffer.from(
    Data.to(new Constr(1, [new Constr(0, [...fields])])),
    "hex",
  );
};

/**
 * The semantic-resolution spend layout: the thread input, the award output
 * and — for the CEK execution selection only — the canonical indices of the
 * CEK program-material reference inputs, in the supplied (root) order.
 */
type SemanticResolutionLayout = ContinueLayout & {
  readonly materialReferenceInputIndices: readonly bigint[];
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
  materialReferenceUtxos = [],
  materialRoute,
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
  /** CEK program-material UTxOs the route names, in root order. */
  readonly materialReferenceUtxos?: readonly UTxO[];
  /** Builds the CEK material route once the reference-input indices are known. */
  readonly materialRoute?: (
    layout: SemanticResolutionLayout,
  ) => ValidationCekMaterialRouteV1;
  readonly onLayout: (layout: SemanticResolutionLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "validation dispute semantic resolution",
    );
    const layout: SemanticResolutionLayout = {
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
      materialReferenceInputIndices: materialReferenceUtxos.map((utxo) =>
        requireReferenceInputIndex(
          ctx,
          utxo,
          "validation dispute semantic resolution CEK material",
        ),
      ),
    };
    onLayout(layout);
    // Option B (#620): the item-semantic `Verify` is transition-only, so the
    // CanonicalDecode complete item takes the generic shape below and the
    // retired proof-item reference route has no arm to target.
    const fields = semanticActionFieldsV1({
      resolverIndex,
      semanticResolverIndex,
      inputIndex: layout.inputIndex,
      outputIndex: layout.outputIndex,
      transition,
      auxiliary,
      ...(materialRoute === undefined
        ? {}
        : {
            materialRoute: validationCekMaterialRouteDataV1(
              materialRoute(layout),
            ),
          }),
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
  // Option B (#620): the prepare-selected redeemer never carries the auxiliary
  // — the canonical-decode validator computes the transition-only evidence hash
  // itself — so no preimage bytes ride in this transaction on any tier and the
  // retired by-hash escape (#597's envelope-pressure valve) has nothing left to
  // relieve.
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
  // validator from the published reference script (#617 follow-up to #597
  // ruling a). Option B removed the tier-1 preimage from this redeemer, but
  // the ~5.6 KiB applied validator body still must not ride inside the
  // 16,384-byte L1 envelope.
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
        resolverIndex,
        semanticResolverIndex: staged.semanticResolverIndex,
        transition: staged.transition,
        auxiliary: staged.auxiliaryWitness,
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

export type SubmitValidationDisputeSemanticResolutionResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly proofItemCarriage: "direct" | "reference";
  /**
   * How the semantic *validator itself* rode on the single resolution
   * transaction: attached inline, or supplied by a published reference script
   * (the CEK entries, and since #634 the ValueAndMint ones). Absent on the
   * staged-chain routes, which have no single semantic-validator attachment.
   */
  readonly semanticValidatorCarriage?: "inline" | "reference";
  readonly proofItemReferenceOutRef?: string;
  /**
   * Present when an inline observe build was refused pre-sign for exceeding
   * the L1 proof envelope and the builder fell back to the reference route
   * (#621). The refused transaction was never signed or submitted.
   */
  readonly proofItemInlineEnvelopeRefusal?: {
    readonly projectedSignedBytes: number;
    readonly maxTransactionBytes: number;
  };
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
    /**
     * The pre-sign envelope projection this stage was admitted under, when
     * the inline delivery route projected it (#621). Signed bytes equal to
     * the projection are the projection's own correctness pin.
     */
    readonly projectedSignedBytes?: number;
  }[];
  /**
   * Present for a CEK execution selection (resolver 11, semantic resolver 1):
   * the program-material route the submitted transaction carried, the
   * material reference inputs it named (root order, canonical indices), and
   * every local route attempt refused pre-sign for a deterministic fit
   * failure before the selected route fit.
   */
  readonly cekRoute?: ValidationCekSelectedRouteV1;
  readonly cekMaterialReferenceInputOutRefs?: readonly string[];
  readonly cekMaterialReferenceInputIndices?: readonly number[];
  readonly cekRejectedLocalRouteAttempts?: readonly ValidationCekRejectedLocalRouteAttemptV1[];
};

export type ValidationCekSelectedRouteV1 =
  | "noCekMaterial"
  | "directProof"
  | "completeSinglePublicationReference"
  | "minimumMultiOutputReconstruction";

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
  proofItemDelivery,
  carriageMaterial,
  cekProgramMaterialReferenceOutRefs,
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
  /**
   * Already-confirmed CEK program-material publications for the
   * execution-selection route ladder (resolver 11, semantic resolver 1):
   * consulted only after the direct-proof route is refused for size.
   */
  readonly cekProgramMaterialReferenceOutRefs?: ValidationCekProgramMaterialReferenceOutRefsV1;
  readonly proofItemReferenceOutRef?: string;
  /**
   * Tier-1 complete-item delivery preference (#621): "inline" carries the
   * §5.1 preimage in the observe redeemer, "reference" routes it through a
   * §8 proof-item publication. Omitted, the builder routes by
   * {@link selectValidationCompleteItemCarriageV1}'s measured cost heuristic
   * (a supplied `proofItemReferenceOutRef` implies "reference"). A
   * preference steers cost, never liveness: an inline build over the L1
   * envelope is refused pre-sign and falls back to the reference route.
   */
  readonly proofItemDelivery?: ValidationProofItemDeliveryV1;
  /** Required when the staged carriage is tier 2 or tier 3 (#600). */
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
  // The CEK execution-selection, context-step and core-step semantic bodies
  // can never fit the L1 proof envelope, so their resolutions consume the
  // published reference script instead of attaching the validator. Resolved
  // up front so a missing deployment entry fails fast.
  const cekSemanticReferenceScriptUtxo =
    resolverIndex === 11 &&
    validationCekSemanticReferenceScriptDeploymentEntryV1(
      staged.semanticResolverIndex,
    ) !== undefined
      ? await requireValidationCekSemanticReferenceScriptUtxo({
          lucid,
          deploymentInfo: parsedDeploymentInfo,
          semanticResolverIndex: staged.semanticResolverIndex,
          expectedScriptHash: semanticContract.spendingScriptHash,
        })
      : undefined;
  // #634. The ValueAndMint semantics get the same reference-script deployment
  // role, but their route is chosen by what the deployment info carries rather
  // than by a frozen sub-roster: eight of the eleven applied bodies are over
  // the envelope today and three are not, and which is which moves with every
  // regeneration. So a published entry is consumed by reference, an absent one
  // attaches inline as before — except when the applied body alone already
  // exceeds the envelope, where no redeemer can make the transaction fit and
  // the honest failure is a precise "publish it" instead of Lucid's
  // "Max transaction size of 16384 exceeded" from deep inside `complete()`.
  const valueAndMintSemanticReferenceEntryName =
    resolverIndex === VALIDATION_VALUE_AND_MINT_RESOLVER_INDEX_V1
      ? validationValueAndMintSemanticReferenceScriptDeploymentEntryV1(
          staged.semanticResolverIndex,
        )
      : undefined;
  if (
    valueAndMintSemanticReferenceEntryName !== undefined &&
    parsedDeploymentInfo[valueAndMintSemanticReferenceEntryName] === undefined &&
    semanticContract.spendingScript.script.length / 2 >
      MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES
  ) {
    throw new Error(
      `Applied ValueAndMint semantic resolver ${staged.semanticResolverIndex.toString()} is ${(
        semanticContract.spendingScript.script.length / 2
      ).toString()} bytes and cannot ride inline inside the ${MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES.toString()}-byte L1 proof envelope; publish it as "${valueAndMintSemanticReferenceEntryName}" and regenerate deployment info before submitting this semantic resolution`,
    );
  }
  const valueAndMintSemanticReferenceScriptUtxo =
    valueAndMintSemanticReferenceEntryName !== undefined &&
    parsedDeploymentInfo[valueAndMintSemanticReferenceEntryName] !== undefined
      ? await requireValidationValueAndMintSemanticReferenceScriptUtxo({
          lucid,
          deploymentInfo: parsedDeploymentInfo,
          semanticResolverIndex: staged.semanticResolverIndex,
          expectedScriptHash: semanticContract.spendingScriptHash,
        })
      : undefined;
  // At most one of the two can be set: the two rosters are keyed by disjoint
  // resolver indices (CEK 11, ValueAndMint 12).
  const semanticValidatorReferenceScriptUtxo =
    cekSemanticReferenceScriptUtxo ?? valueAndMintSemanticReferenceScriptUtxo;
  const isCekExecutionSelection =
    resolverIndex === 11 && staged.semanticResolverIndex === 1;
  if (
    !isCekExecutionSelection &&
    cekProgramMaterialReferenceOutRefs !== undefined
  ) {
    throw new Error(
      "CEK program-material publication outrefs are permitted only for the CEK execution-selection semantic resolver",
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
  const stagedItemCarriage = isCompleteCanonicalItem
    ? midgardFieldCarriageFromDataV1(
        staged.auxiliary.fields[0]!,
        "Validation complete proof-item §8 carriage",
      )
    : undefined;
  const completeItemCarriageMaterial = (():
    | ValidationFieldCarriageMaterialV1
    | undefined => {
    if (
      stagedItemCarriage === undefined ||
      stagedItemCarriage.carriage === "Inline"
    ) {
      return undefined;
    }
    if (carriageMaterial === undefined) {
      throw new Error(
        `Validation complete proof-item carriage is tier-${
          stagedItemCarriage.carriage === "RawUtxo" ? "2" : "3"
        } \`${stagedItemCarriage.carriage}\`, which names reference inputs and carries no ` +
          "bytes, so the submission must supply the §8 carriage material those indices name (#600)",
      );
    }
    if (carriageMaterial.plan.tier !== stagedItemCarriage.carriage) {
      throw new Error(
        `Validation complete proof-item carriage material plans tier \`${carriageMaterial.plan.tier}\` ` +
          `while the staged auxiliary names \`${stagedItemCarriage.carriage}\``,
      );
    }
    return carriageMaterial;
  })();
  const completeFieldPreimage = ((): string | undefined => {
    if (stagedItemCarriage === undefined) {
      return undefined;
    }
    if (stagedItemCarriage.carriage === "Inline") {
      return stagedItemCarriage.preimage.toString("hex");
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
  // #619/#621: how a tier-1 complete item's preimage reaches the §8.8 door is
  // decided here, at build time — explicit request, then a supplied
  // publication out-ref, then the measured cost heuristic. The committed
  // evidence is transition-only, so this is a routing decision and nothing
  // staged on chain can disagree with it.
  const proofItemDeliveryRoute = resolveValidationProofItemDeliveryRouteV1({
    requestedDelivery: proofItemDelivery,
    hasProofItemReferenceOutRef: proofItemReferenceOutRef !== undefined,
    committedCarriage: stagedItemCarriage?.carriage,
    ...(stagedItemCarriage?.carriage === "Inline"
      ? {
          preimageByteLength: Buffer.from(
            completeFieldPreimage as string,
            "hex",
          ).length,
        }
      : {}),
  });
  let resolvedProofItemReferenceOutRef = proofItemReferenceOutRef;
  let proofItemReferenceUtxo: UTxO | undefined;
  if (proofItemReferenceOutRef !== undefined) {
    try {
      proofItemReferenceUtxo = await fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(
          proofItemReferenceOutRef,
          "--proof-item-reference-out-ref",
        ),
        label: "validation complete proof-item reference UTxO",
      });
    } catch (cause) {
      const detail = cause instanceof Error ? cause.message : String(cause);
      // A spent or missing publication is a routing setback, never a loss of
      // the dispute: the §8 publication is content-addressed (§8.7), so any
      // fresh copy of the same bytes serves, and tier-1 bytes also fit the
      // observe redeemer directly. Refuse here, before any stage transaction
      // exists, and name both recoveries (#621).
      throw new Error(
        `Validation complete proof-item publication ${proofItemReferenceOutRef} is spent or ` +
          `missing on chain (${detail}). The publication is content-addressed, so recover by ` +
          "either (a) re-publishing: omit `proofItemReferenceOutRef` and the builder publishes " +
          "a fresh publication and routes by reference, or (b) inline delivery: pass " +
          '`proofItemDelivery: "inline"` to carry the preimage in the observe redeemer when ' +
          "it fits the L1 envelope.",
      );
    }
  }
  let proofItemPublication:
    | SubmitValidationDisputeSemanticResolutionResult["proofItemPublication"]
    | undefined;
  // The publication route reconstructs tier-1 `Inline` from a datum, so it
  // exists only inside tier 1 — as a redeemer-size optimisation, never as a
  // fourth rung (#600 Ruling 1, Q4). Above the cap the carriage already names
  // reference inputs of its own and a second one would be an unbound copy of
  // the same bytes. Factored into a function because two call sites route
  // through it: the up-front reference route, and the observe stage's
  // pre-sign fallback when an inline build outgrows the L1 envelope (#621).
  const publishProofItemPublication = async (): Promise<UTxO> => {
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
    const publishedUtxo = await fetchUtxoByOutRef({
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
      lovelace: publishedUtxo.assets.lovelace ?? 0n,
      awaitedConfirmation: true,
    };
    return publishedUtxo;
  };
  if (
    proofItemReferenceUtxo === undefined &&
    proofItemDeliveryRoute === "reference"
  ) {
    proofItemReferenceUtxo = await publishProofItemPublication();
  }
  if (proofItemReferenceUtxo !== undefined) {
    if (!isCompleteCanonicalItem) {
      throw new Error(
        "Validation complete proof-item reference is only valid for a CanonicalDecode complete item",
      );
    }
    if (stagedItemCarriage?.carriage !== "Inline") {
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
      readonly projectedSignedBytes?: number;
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
      projectEnvelopePreSign = false,
      encode,
    }: {
      readonly inputUtxo: UTxO;
      readonly inputContract: StageContract;
      readonly outputContract: StageContract;
      readonly stageOutputDatum: string;
      readonly label: string;
      readonly proofReference?: UTxO;
      readonly scriptReference?: UTxO;
      /** §8 tiers 2-3: the carriage UTxOs the resolved indices name. */
      readonly carriageReferences?: readonly UTxO[];
      readonly awaitStage: boolean;
      /**
       * Inline delivery's pre-sign envelope gate (#621): project the signed
       * byte length before signing and throw
       * {@link ValidationInlineDeliveryEnvelopeRefusedErrorV1} — signing and
       * submitting nothing — when it exceeds the L1 proof envelope, so the
       * caller can fall back to the reference route.
       */
      readonly projectEnvelopePreSign?: boolean;
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
        // On any chain whose protocol `maxTxSize` sits at or under the
        // Midgard envelope — today's L1 parameters exactly — an
        // over-envelope inline build never reaches the dummy-witness
        // projection below: CML's fee-calculation build refuses it first,
        // with its own size message (the same signature midgard-node's
        // reference-script publisher pins). That is still a pre-sign
        // refusal of the same build for the same reason, so on the
        // envelope-projection path it converts to the routing refusal and
        // the caller's publication fallback, not a hard failure (#621).
        const builderCeiling = projectEnvelopePreSign
          ? /Max transaction size of (\d+) exceeded\. Found: (\d+)/iu.exec(
              detail,
            )
          : null;
        if (builderCeiling !== null) {
          throw new ValidationInlineDeliveryEnvelopeRefusedErrorV1({
            label,
            projectedSignedBytes: Number(builderCeiling[2]),
            maxTransactionBytes: Number(builderCeiling[1]),
          });
        }
        throw new Error(`${label} local evaluation failed: ${detail}`);
      }
      if (stageLayout === undefined) {
        throw new Error(`BuildTxWithRedeemer did not resolve ${label} layout`);
      }
      const resolvedLayout = stageLayout as ContinueLayout;
      let projectedSignedBytes: number | undefined;
      if (projectEnvelopePreSign) {
        projectedSignedBytes = projectSignedL1ProofTransactionBytesV1(
          unsigned.toCBOR(),
        );
        if (projectedSignedBytes > MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES) {
          throw new ValidationInlineDeliveryEnvelopeRefusedErrorV1({
            label,
            projectedSignedBytes,
            maxTransactionBytes: MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES,
          });
        }
      }
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
        ...(projectedSignedBytes === undefined ? {} : { projectedSignedBytes }),
      };
    };
    const stages = contracts.validationTraceDispute.canonicalDecodeItemStages;
    // Build-time carriage resolution (#619/#621, re-scoping #600 ruling
    // D3-A's committed-index re-check). Since Option B no index is frozen
    // into `evidence_hash`, so there is nothing committed left to re-check:
    // tiers 2-3 resolve the producer's plan by content (§8.7) against the
    // complete reference-input set the door transaction will read — the
    // published observe validator rides that same canonically-sorted list —
    // and the resolved carriage is what goes on the observe wire. Tier 1
    // carries the delivered preimage itself. Either way, material whose
    // content the door will not see refuses here, before any stage
    // transaction exists, where re-staging is still free.
    const observeCarriageReferences =
      completeItemCarriageMaterial === undefined
        ? undefined
        : completeItemCarriageMaterial.referenceUtxos;
    const observeCarriageData: PlutusDataValue =
      completeItemCarriageMaterial !== undefined
        ? midgardFieldCarriageToDataV1(
            resolveMidgardFieldCarriageAgainstReferenceInputsV1({
              plan: completeItemCarriageMaterial.plan,
              referenceInputs: [
                ...(observeReferenceScriptUtxo === undefined
                  ? []
                  : [observeReferenceScriptUtxo]),
                ...completeItemCarriageMaterial.referenceUtxos,
              ],
              ...(completeItemCarriageMaterial.certificatePolicyId === undefined
                ? {}
                : {
                    certificatePolicyId:
                      completeItemCarriageMaterial.certificatePolicyId,
                  }),
            }),
          )
        : new Constr(0, [completeFieldPreimage as string]);
    const authenticate = await submitStage({
      inputUtxo: threadUtxo,
      inputContract: semanticContract,
      outputContract: stages.source,
      stageOutputDatum: authenticatedDatum,
      label: "Validation canonical item authentication",
      scriptReference: semanticReferenceScriptUtxo,
      awaitStage: true,
      // Option B (#620): `Verify` is `(input_index, output_index, transition)`.
      // The stage re-checks the transition-only commitment — the carriage is
      // neither forwarded nor referenced here; it is dereferenced once, at the
      // observe stage's §8.8 door, whichever route delivers it there.
      encode: ({ inputIndex, outputIndex }) =>
        Data.to(
          new Constr(1, [
            new Constr(0, [inputIndex, outputIndex, staged.transitionData]),
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
    // **The §8.8 door's own transaction (#600 ruling D3-A, #619/#621).** The
    // observe stage is the one stage that dereferences the carriage — every
    // earlier stage only re-checks the transition-only commitment — so it is
    // the one that reads the carriage UTxOs, and the sole content gate on
    // this path. The carriage on its wire is the one resolved at build time
    // above, never a replay of the staged auxiliary; on the tier-1 inline
    // route the redeemer carries the preimage itself, and a build whose
    // projected signed bytes outgrow the L1 envelope is refused pre-sign and
    // falls back to the §8 publication route — no routing input can strand
    // the staged thread (#621).
    let proofItemInlineEnvelopeRefusal:
      | SubmitValidationDisputeSemanticResolutionResult["proofItemInlineEnvelopeRefusal"]
      | undefined;
    const observeByReference = (
      proofReference: UTxO,
    ): Promise<SubmittedStage> =>
      submitStage({
        inputUtxo: source.nextThreadUtxo!,
        inputContract: stages.observe,
        outputContract: stages.proof,
        stageOutputDatum: observedDatum,
        label: "Validation canonical item observation",
        proofReference,
        scriptReference: observeReferenceScriptUtxo,
        awaitStage: true,
        encode: ({ inputIndex, outputIndex, referenceInputIndex }) =>
          Data.to(
            new Constr(1, [
              new Constr(1, [inputIndex, outputIndex, referenceInputIndex!]),
            ]),
          ),
      });
    let observe: SubmittedStage;
    if (proofItemReferenceUtxo !== undefined) {
      observe = await observeByReference(proofItemReferenceUtxo);
    } else {
      try {
        observe = await submitStage({
          inputUtxo: source.nextThreadUtxo!,
          inputContract: stages.observe,
          outputContract: stages.proof,
          stageOutputDatum: observedDatum,
          label: "Validation canonical item observation",
          scriptReference: observeReferenceScriptUtxo,
          ...(observeCarriageReferences === undefined
            ? {}
            : { carriageReferences: observeCarriageReferences }),
          awaitStage: true,
          projectEnvelopePreSign: proofItemDeliveryRoute === "inline",
          // #592's `Observe` is `(input_index, output_index, carriage)`.
          encode: ({ inputIndex, outputIndex }) =>
            Data.to(
              new Constr(1, [
                new Constr(0, [inputIndex, outputIndex, observeCarriageData]),
              ]),
            ),
        });
      } catch (cause) {
        if (
          !(cause instanceof ValidationInlineDeliveryEnvelopeRefusedErrorV1)
        ) {
          throw cause;
        }
        // The refused build was never signed; the same staged thread and
        // datums serve the reference route unchanged, because the route
        // decides only how the door's bytes travel (#621).
        proofItemInlineEnvelopeRefusal = {
          projectedSignedBytes: cause.projectedSignedBytes,
          maxTransactionBytes: cause.maxTransactionBytes,
        };
        proofItemReferenceUtxo = await publishProofItemPublication();
        observe = await observeByReference(proofItemReferenceUtxo);
      }
    }
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
    ].map(
      ({
        kind,
        txHash,
        nextThreadOutRef,
        completeSignedBytes,
        projectedSignedBytes,
      }) => ({
        kind,
        txHash,
        nextThreadOutRef,
        completeSignedBytes,
        ...(projectedSignedBytes === undefined ? {} : { projectedSignedBytes }),
      }),
    );
    return {
      txHash: settle.txHash,
      threadOutRef,
      nextThreadOutRef: settle.nextThreadOutRef,
      proofItemCarriage:
        proofItemReferenceUtxo === undefined ? "direct" : "reference",
      ...(resolvedProofItemReferenceOutRef === undefined
        ? {}
        : { proofItemReferenceOutRef: resolvedProofItemReferenceOutRef }),
      ...(proofItemInlineEnvelopeRefusal === undefined
        ? {}
        : { proofItemInlineEnvelopeRefusal }),
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
  // One semantic-resolution transaction, signed and envelope-checked but not
  // yet submitted. Factored so the CEK execution selection can ladder through
  // its program-material routes — each a complete build refused pre-sign on a
  // deterministic fit failure before the next is tried — while every other
  // resolver builds exactly once.
  const prepareSemanticResolution = async ({
    label,
    materialReferenceUtxos = [],
    materialRoute,
  }: {
    readonly label: string;
    readonly materialReferenceUtxos?: readonly UTxO[];
    readonly materialRoute?: (
      layout: SemanticResolutionLayout,
    ) => ValidationCekMaterialRouteV1;
  }): Promise<{
    readonly signed: TxSigned;
    readonly layout: SemanticResolutionLayout;
  }> => {
    let layout: SemanticResolutionLayout | undefined;
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
          materialReferenceUtxos,
          ...(materialRoute === undefined ? {} : { materialRoute }),
          onLayout: (resolvedLayout) => {
            layout = resolvedLayout;
          },
        }),
      );
    if (proofItemReferenceUtxo !== undefined) {
      tx = tx.readFrom([proofItemReferenceUtxo]);
    }
    if (materialReferenceUtxos.length > 0) {
      tx = tx.readFrom([...materialReferenceUtxos]);
    }
    tx = tx.pay
      .ToContract(
        contracts.validationTraceDispute.award.spendingScriptAddress,
        { kind: "inline", value: outputDatum },
        threadAssets(threadUtxo, token.unit),
      )
      .validFrom(range.validFrom)
      .validTo(range.validTo)
      .addSignerKey(signer.paymentKeyHash);
    // The published reference script supplies the oversized CEK (#617 R5) and
    // ValueAndMint (#634) semantic validators; every other semantic validator
    // rides inline.
    const readiedTx =
      semanticValidatorReferenceScriptUtxo === undefined
        ? tx.attach.SpendingValidator(semanticContract.spendingScript)
        : tx.readFrom([semanticValidatorReferenceScriptUtxo]);
    const unsigned = await readiedTx.complete({ localUPLCEval: true });
    if (layout === undefined) {
      throw new Error(
        "BuildTxWithRedeemer did not resolve validation semantic resolution layout",
      );
    }
    const signed = await unsigned.sign.withWallet().complete();
    requireL1ProofEnvelope(signed.toCBOR(), label);
    return { signed, layout };
  };
  const submitPreparedSemanticResolution = async (
    prepared: Awaited<ReturnType<typeof prepareSemanticResolution>>,
    cekRoute?: {
      readonly route: ValidationCekSelectedRouteV1;
      readonly materialReferenceUtxos: readonly UTxO[];
      readonly rejectedLocalRouteAttempts: readonly ValidationCekRejectedLocalRouteAttemptV1[];
    },
  ): Promise<SubmitValidationDisputeSemanticResolutionResult> => {
    const txHash = await prepared.signed.submit();
    if (awaitConfirmation) {
      await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
    }
    return {
      txHash,
      threadOutRef,
      nextThreadOutRef: `${txHash}#${prepared.layout.outputIndex.toString()}`,
      proofItemCarriage:
        proofItemReferenceUtxo === undefined ? "direct" : "reference",
      semanticValidatorCarriage:
        semanticValidatorReferenceScriptUtxo === undefined
          ? ("inline" as const)
          : ("reference" as const),
      ...(resolvedProofItemReferenceOutRef === undefined
        ? {}
        : { proofItemReferenceOutRef: resolvedProofItemReferenceOutRef }),
      ...(proofItemPublication === undefined ? {} : { proofItemPublication }),
      resolverIndex,
      semanticResolverIndex: staged.semanticResolverIndex,
      semanticResolverGlobalIndex: staged.semanticResolverGlobalIndex,
      inputIndex: Number(prepared.layout.inputIndex),
      outputIndex: Number(prepared.layout.outputIndex),
      awaitedConfirmation: awaitConfirmation,
      ...(cekRoute === undefined
        ? {}
        : {
            cekRoute: cekRoute.route,
            cekMaterialReferenceInputOutRefs:
              cekRoute.materialReferenceUtxos.map(outRefLabel),
            cekMaterialReferenceInputIndices:
              prepared.layout.materialReferenceInputIndices.map(Number),
            cekRejectedLocalRouteAttempts: cekRoute.rejectedLocalRouteAttempts,
          }),
    };
  };
  if (!isCekExecutionSelection) {
    return await submitPreparedSemanticResolution(
      await prepareSemanticResolution({
        label: "Validation semantic resolution",
      }),
    );
  }
  // CEK execution selection: `VerifyExecutionSelection` carries the
  // program-material route (`CekMaterialRouteV1`) beside the committed
  // evidence, and `verify_cek_route_v1` authenticates the material it names
  // against the immutable CEK program-material publications. A native-script
  // selection carries no material; a Plutus/Midgard selection ladders
  // direct proof → single publication → minimum multi-output, each refused
  // pre-sign on a deterministic fit failure, exactly as the retired direct
  // resolver did.
  const routeMaterial = staged.cekRouteMaterial;
  if (routeMaterial === undefined) {
    return await submitPreparedSemanticResolution(
      await prepareSemanticResolution({
        label: "Validation-dispute CEK execution selection (no material)",
        materialRoute: () => "NoCekMaterial",
      }),
      {
        route: "noCekMaterial",
        materialReferenceUtxos: [],
        rejectedLocalRouteAttempts: [],
      },
    );
  }
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
      layout: SemanticResolutionLayout,
    ) => ValidationCekMaterialRouteV1;
  }): Promise<
    Awaited<ReturnType<typeof prepareSemanticResolution>> | undefined
  > => {
    try {
      return await prepareSemanticResolution({
        label: `Validation-dispute CEK ${route}`,
        materialReferenceUtxos,
        materialRoute,
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
  const submitSelectedRoute = (
    prepared: Awaited<ReturnType<typeof prepareSemanticResolution>>,
    route: ValidationCekSelectedRouteV1,
    materialReferenceUtxos: readonly UTxO[],
  ): Promise<SubmitValidationDisputeSemanticResolutionResult> =>
    submitPreparedSemanticResolution(prepared, {
      route,
      materialReferenceUtxos,
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
    return await submitSelectedRoute(directPrepared, "directProof", []);
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
      [singleReferenceUtxo],
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
      multiReferenceUtxos,
    );
  }

  if (staged.cekIncrementalNecessityReceiptSet === undefined) {
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
  // fraud proof. Refuse here rather than construct a resolution that cannot
  // validate. The receipt-set machinery above and the ABI variant are retained
  // for the lease that adds the authenticated cross-transaction traversal
  // accumulator the sound route needs.
  throw new Error(
    "CEK incremental traversal is not verifiable on L1: the on-chain IncrementalCekMaterial route fails closed until an authenticated cross-transaction material-traversal accumulator is deployed. Publish the complete program material and resolve through the direct, single-publication, or minimum-multi-output route.",
  );
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

export const validationDisputeDescriptorData =
  validationTraceDescriptorDataFromCore;
