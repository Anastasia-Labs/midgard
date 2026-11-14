import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import type { RejectionReason } from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  claimedFieldItems,
  claimedIndex,
  claimedPlutusUndecodable,
} from "../evidence/claimed-coordinate.js";
import type { CanonicalViolationDetection } from "./classification.js";
import { forcedTransactionSubject } from "./detection-subject.js";

/**
 * A forced leaf rejected for a coordinate its transaction does not have. No
 * fraud-proof family is registered for it, so a block whose earliest finding
 * is this one classifies as an unprovable gap.
 */
export const FORCED_CLAIM_COORDINATE_ABSENT_VIOLATION_ID =
  "forced-claim-coordinate-absent" as const;

const FIELD_COUNT = 9;
const SPEND_INPUTS = 0;
const OUTPUTS = 2;
const OBSERVERS = 3;
const REQUIRED_SIGNERS = 4;
const MINT = 5;
const SCRIPT_WITNESSES = 6;
const ADDRESS_WITNESSES = 7;
const REDEEMERS = 8;

type FieldPreimages = readonly Uint8Array[];

/** `true` only when the claimed item is certainly not in the field. */
const itemAbsent = (
  fields: FieldPreimages,
  field: number,
  claimed: bigint,
): boolean => {
  const position = claimedIndex(claimed);
  if (position === undefined) return true;
  const items = claimedFieldItems(fields[field]!);
  return items !== undefined && items[position] === undefined;
};

/** A claimed field index outside the nine fields. */
const fieldAbsent = (claimed: bigint): boolean => {
  const field = claimedIndex(claimed);
  return field === undefined || field >= FIELD_COUNT;
};

/** A claimed input of a source kind no transaction has, or one it lacks. */
const inputAbsent = (
  fields: FieldPreimages,
  sourceKind: bigint,
  inputIndex: bigint,
): boolean =>
  (sourceKind !== 0n && sourceKind !== 1n) ||
  itemAbsent(fields, Number(sourceKind), inputIndex);

/**
 * Whether `reason` names a coordinate `fields` certainly lack. Arms whose
 * coordinates this cannot judge from the transaction alone answer `false`.
 */
const claimsAbsentCoordinate = (
  reason: Exclude<RejectionReason, string>,
  fields: FieldPreimages,
): boolean => {
  if ("FieldPreimageLengthMismatch" in reason)
    return fieldAbsent(reason.FieldPreimageLengthMismatch.field_index);
  if ("FieldItemWidthIllegal" in reason) {
    const { field_index, item_index } = reason.FieldItemWidthIllegal;
    return (
      fieldAbsent(field_index) ||
      itemAbsent(fields, Number(field_index), item_index)
    );
  }
  if ("DuplicateInput" in reason) {
    const pair = reason.DuplicateInput;
    return (
      inputAbsent(fields, pair.first_field_index, pair.first_item_index) ||
      inputAbsent(fields, pair.second_field_index, pair.second_item_index) ||
      (pair.first_field_index === pair.second_field_index &&
        pair.first_item_index === pair.second_item_index)
    );
  }
  for (const payload of [
    "InputNotFound" in reason ? reason.InputNotFound : undefined,
    "InputSpentOutputNonCanonical" in reason
      ? reason.InputSpentOutputNonCanonical
      : undefined,
    "ResolvedReferenceScriptMalformed" in reason
      ? reason.ResolvedReferenceScriptMalformed
      : undefined,
    "ResolvedReferenceScriptNodeLimit" in reason
      ? reason.ResolvedReferenceScriptNodeLimit
      : undefined,
    "ResolvedReferenceScriptDepthLimit" in reason
      ? reason.ResolvedReferenceScriptDepthLimit
      : undefined,
  ])
    if (payload !== undefined)
      return inputAbsent(fields, payload.source_kind, payload.input_index);
  if ("SpendInputSignerMissing" in reason)
    return itemAbsent(
      fields,
      SPEND_INPUTS,
      reason.SpendInputSignerMissing.input_index,
    );
  if ("InputAssetAccumulationLimit" in reason)
    return itemAbsent(
      fields,
      SPEND_INPUTS,
      reason.InputAssetAccumulationLimit.input_index,
    );
  if ("AddressWitnessSignatureInvalid" in reason)
    return itemAbsent(
      fields,
      ADDRESS_WITNESSES,
      reason.AddressWitnessSignatureInvalid.witness_index,
    );
  if ("RequiredSignerUnsigned" in reason)
    return itemAbsent(
      fields,
      REQUIRED_SIGNERS,
      reason.RequiredSignerUnsigned.signer_index,
    );
  if ("ObserverOrderInvalid" in reason)
    return itemAbsent(
      fields,
      OBSERVERS,
      reason.ObserverOrderInvalid.observer_index,
    );
  for (const payload of [
    "WitnessScriptHeaderMalformed" in reason
      ? reason.WitnessScriptHeaderMalformed
      : undefined,
    "WitnessNativeScriptMalformed" in reason
      ? reason.WitnessNativeScriptMalformed
      : undefined,
    "WitnessNativeScriptNodeLimit" in reason
      ? reason.WitnessNativeScriptNodeLimit
      : undefined,
    "WitnessNativeScriptDepthLimit" in reason
      ? reason.WitnessNativeScriptDepthLimit
      : undefined,
    "WitnessNativeScriptFalse" in reason
      ? reason.WitnessNativeScriptFalse
      : undefined,
    "UnusedScriptWitness" in reason ? reason.UnusedScriptWitness : undefined,
  ])
    if (payload !== undefined)
      return itemAbsent(fields, SCRIPT_WITNESSES, payload.script_index);
  for (const payload of [
    "RedeemerMalformed" in reason ? reason.RedeemerMalformed : undefined,
    "UnusedRedeemer" in reason ? reason.UnusedRedeemer : undefined,
  ])
    if (payload !== undefined)
      return itemAbsent(fields, REDEEMERS, payload.redeemer_index);
  // A Plutus execution needs a redeemer, so a transaction with none has no
  // execution that could fail.
  if ("PlutusExecutionFailed" in reason)
    return claimedFieldItems(fields[REDEEMERS]!)?.length === 0;
  for (const payload of [
    "OutputNonCanonical" in reason ? reason.OutputNonCanonical : undefined,
    "OutputReferenceScriptMalformed" in reason
      ? reason.OutputReferenceScriptMalformed
      : undefined,
    "OutputReferenceScriptNodeLimit" in reason
      ? reason.OutputReferenceScriptNodeLimit
      : undefined,
    "OutputReferenceScriptDepthLimit" in reason
      ? reason.OutputReferenceScriptDepthLimit
      : undefined,
    "ProtectedOutputSignerMissing" in reason
      ? reason.ProtectedOutputSignerMissing
      : undefined,
    "OutputBelowMinAda" in reason ? reason.OutputBelowMinAda : undefined,
    "OutputAssetAccumulationLimit" in reason
      ? reason.OutputAssetAccumulationLimit
      : undefined,
  ])
    if (payload !== undefined)
      return itemAbsent(fields, OUTPUTS, payload.output_index);
  for (const payload of [
    "MintDeclaredAssetLimit" in reason
      ? reason.MintDeclaredAssetLimit
      : undefined,
    "MintItemMalformed" in reason ? reason.MintItemMalformed : undefined,
  ])
    if (payload !== undefined)
      return itemAbsent(fields, MINT, payload.policy_index);
  if ("PlutusScriptUndecodable" in reason) {
    const claimed = claimedPlutusUndecodable(reason.PlutusScriptUndecodable);
    return (
      claimed === undefined ||
      itemAbsent(
        fields,
        claimed.itemKind === 0 ? SCRIPT_WITNESSES : OUTPUTS,
        reason.PlutusScriptUndecodable.item_index,
      )
    );
  }
  return false;
};

/**
 * One detection per forced leaf whose rejection names a coordinate its
 * transaction does not have. The header classifier adds these only when the
 * complete replay found nothing provable, so they never displace a finding a
 * family can prove; they keep such a block from reading as healthy.
 */
export const absentForcedClaimDetections = (
  evidence: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] =>
  evidence.reconstruction.forcedTransactions.flatMap(
    (transaction, forcedIndex): CanonicalViolationDetection[] => {
      const verdict = transaction.value.verdict;
      if (verdict === "ForcedTxValid") return [];
      const reason = verdict.ForcedTxInvalid.reason;
      if (typeof reason === "string") return [];
      const { fieldPreimages } = deriveMidgardForcedTxFaultEvidenceMaterial(
        transaction.fullTransactionCbor,
      );
      if (!claimsAbsentCoordinate(reason, fieldPreimages)) return [];
      const transactionId = transaction.value.tx_id;
      return [
        Object.freeze({
          ...forcedTransactionSubject(transaction.key),
          detectionId: `${FORCED_CLAIM_COORDINATE_ABSENT_VIOLATION_ID}:forced:${forcedIndex.toString()}:${transactionId}`,
          headerHash: evidence.headerHash,
          violationId: FORCED_CLAIM_COORDINATE_ABSENT_VIOLATION_ID,
          position: BigInt(forcedIndex),
          diagnostic: `forced transaction ${transactionId} was rejected for a coordinate it does not have`,
        }),
      ];
    },
  );
