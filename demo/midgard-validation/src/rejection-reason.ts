import {
  MIDGARD_REJECTION_REASON_V1_ARMS,
  type MidgardRejectionReasonV1,
  type MidgardRejectionReasonV1ArmName,
  midgardRejectionReasonV1Parts,
} from "@al-ft/midgard-core/validation-trace";

import type { RejectSubject } from "./reject-subject.js";
import { type RejectCode, RejectCodes, type RejectedTx } from "./types.js";

/**
 * The coordinate the validation machine writes where its state carries no
 * exact ordinal yet: every `DuplicateInput` coordinate, the `input_index` of
 * the input-resolution arms (`InputNotFound` through
 * `SpendInputSignerMissing`) and `InputAssetAccumulationLimit.input_index`.
 * Mirrors Aiken `validation_machine/shared.unset_reason_coordinate`, so a
 * reason built here hashes to the one the machine's terminal state binds.
 *
 * Interim: it holds only while the families that reopen those coordinates
 * against the trace stay excluded from the deployment. A forced leaf never
 * carries it: the operator's forced verdict names the exact subject
 * coordinates, which the forced-leaf families reopen by field position.
 */
export const UNSET_REASON_COORDINATE = -1n;

/**
 * Whose coordinates a reason carries. `machine`: the validation machine's
 * terminal reason, with {@link UNSET_REASON_COORDINATE} where its state has no
 * exact ordinal yet. `subject`: every coordinate exactly as the failing rule
 * recorded it, as a forced leaf's verdict names it.
 */
export type RejectionReasonCoordinates = "machine" | "subject";

/**
 * The `RejectionReasonV1` for the rule that recorded `subject`, coordinates
 * included, under the given coordinate convention.
 */
export const rejectionReasonOfSubject = (
  subject: RejectSubject,
  coordinates: RejectionReasonCoordinates,
): MidgardRejectionReasonV1 => {
  const interim = (exact: bigint): bigint => {
    switch (coordinates) {
      case "machine":
        return UNSET_REASON_COORDINATE;
      case "subject":
        return exact;
      default: {
        const unreachable: never = coordinates;
        throw new Error(
          `rejectionReasonOfSubject: unknown coordinate convention ${String(unreachable)}`,
        );
      }
    }
  };
  switch (subject.arm) {
    case "FieldItemWidthIllegal":
      return {
        FieldItemWidthIllegal: {
          field_index: subject.fieldIndex,
          item_index: subject.itemIndex,
        },
      };
    case "DuplicateInput":
      return {
        DuplicateInput: {
          first_field_index: interim(subject.first.fieldIndex),
          first_item_index: interim(subject.first.itemIndex),
          second_field_index: interim(subject.second.fieldIndex),
          second_item_index: interim(subject.second.itemIndex),
        },
      };
    case "AddressWitnessSignatureInvalid":
      return {
        AddressWitnessSignatureInvalid: { witness_index: subject.index },
      };
    case "RequiredSignerUnsigned":
      return { RequiredSignerUnsigned: { signer_index: subject.index } };
    case "WitnessNativeScriptFalse":
      return { WitnessNativeScriptFalse: { script_index: subject.index } };
    case "WitnessNativeScriptMalformed":
      return { WitnessNativeScriptMalformed: { script_index: subject.index } };
    case "WitnessScriptHeaderMalformed":
      return { WitnessScriptHeaderMalformed: { script_index: subject.index } };
    case "ObserverOrderInvalid":
      return { ObserverOrderInvalid: { observer_index: subject.index } };
    case "ScriptIntegrityHashMissing":
    case "ObserversForbiddenOnUntaggedNetwork":
    case "ScriptIntegrityHashMismatch":
      return subject.arm;
    case "InputNotFound":
      return {
        InputNotFound: {
          source_kind: subject.sourceKind,
          input_index: interim(subject.index),
        },
      };
    case "InputSpentOutputNonCanonical":
      return {
        InputSpentOutputNonCanonical: {
          source_kind: subject.sourceKind,
          input_index: interim(subject.index),
        },
      };
    case "SpendInputSignerMissing":
      return {
        SpendInputSignerMissing: { input_index: interim(subject.index) },
      };
    case "ProtectedOutputSignerMissing":
      return { ProtectedOutputSignerMissing: { output_index: subject.index } };
    case "ScriptSourceMissing":
      return {
        ScriptSourceMissing: {
          purpose_kind: subject.purposeKind,
          purpose_index: subject.purposeIndex,
        },
      };
    case "RedeemerMissing":
      return {
        RedeemerMissing: {
          purpose_kind: subject.purposeKind,
          purpose_index: subject.purposeIndex,
        },
      };
    case "RedeemerMalformed":
      return { RedeemerMalformed: { redeemer_index: subject.index } };
    case "UnusedRedeemer":
      return { UnusedRedeemer: { redeemer_index: subject.index } };
    case "UnusedScriptWitness":
      return { UnusedScriptWitness: { script_index: subject.index } };
    case "ExecutionNativeScriptFalse":
      return {
        ExecutionNativeScriptFalse: {
          purpose_kind: subject.purposeKind,
          purpose_index: subject.purposeIndex,
        },
      };
    case "ReceivePurposePlutusV3Forbidden":
      return {
        ReceivePurposePlutusV3Forbidden: {
          purpose_kind: subject.purposeKind,
          purpose_index: subject.purposeIndex,
        },
      };
    case "PlutusExecutionFailed":
      return {
        PlutusExecutionFailed: {
          purpose_kind: subject.purposeKind,
          purpose_index: subject.purposeIndex,
        },
      };
    case "OutputNonCanonical":
      return { OutputNonCanonical: { output_index: subject.index } };
    case "OutputBelowMinAda":
      return { OutputBelowMinAda: { output_index: subject.index } };
    case "InputAssetAccumulationLimit":
      return {
        InputAssetAccumulationLimit: {
          input_index: interim(subject.index),
          asset_index: subject.assetIndex,
        },
      };
    case "OutputAssetAccumulationLimit":
      return {
        OutputAssetAccumulationLimit: {
          output_index: subject.index,
          asset_index: subject.assetIndex,
        },
      };
    case "MintAssetAccumulationLimit":
      return { MintAssetAccumulationLimit: { mint_index: subject.index } };
    case "PlutusScriptUndecodable":
      return {
        PlutusScriptUndecodable: {
          item_kind: subject.itemKind,
          item_index: subject.index,
          class: subject.refusalClass,
        },
      };
    default: {
      const unreachable: never = subject;
      throw new Error(
        `rejectionReasonOfSubject: unknown subject ${JSON.stringify(unreachable)}`,
      );
    }
  }
};

/**
 * The machine arms each machine-provable rejection code can end in. A code
 * absent here is a node pre-screen or infrastructure failure that no
 * `RejectionReasonV1` arm proves.
 */
export const REJECTION_REASON_ARMS_BY_CODE: ReadonlyMap<
  RejectCode,
  readonly MidgardRejectionReasonV1ArmName[]
> = new Map<RejectCode, readonly MidgardRejectionReasonV1ArmName[]>([
  [RejectCodes.EmptyInputs, ["EmptyInputs"]],
  [RejectCodes.InvalidValidityIntervalFormat, ["ValidityIntervalMalformed"]],
  [RejectCodes.ValidityIntervalMismatch, ["ValidityIntervalExcludesBlockSlot"]],
  [RejectCodes.MinFee, ["FeeBelowMinimum"]],
  [RejectCodes.ValueNotPreserved, ["ValueNotPreserved"]],
  [RejectCodes.NetworkIdMismatch, ["NetworkIdMismatch"]],
  [RejectCodes.FieldPreimageSize, ["FieldPreimageLengthMismatch"]],
  [
    RejectCodes.InvalidFieldType,
    [
      "FieldItemWidthIllegal",
      "WitnessScriptHeaderMalformed",
      "WitnessNativeScriptMalformed",
      "ScriptIntegrityHashMissing",
      "ObserversForbiddenOnUntaggedNetwork",
      "ObserverOrderInvalid",
      "ResolvedReferenceScriptMalformed",
      "RedeemerMalformed",
      "OutputReferenceScriptMalformed",
      "UnusedScriptWitness",
      "UnusedRedeemer",
      "ExecutionNativeScriptMalformed",
      "ScriptIntegrityHashMismatch",
      "MintItemMalformed",
    ],
  ],
  [RejectCodes.DuplicateInputInTx, ["DuplicateInput"]],
  [RejectCodes.InvalidSignature, ["AddressWitnessSignatureInvalid"]],
  [
    RejectCodes.MissingRequiredWitness,
    [
      "RequiredSignerUnsigned",
      "SpendInputSignerMissing",
      "ProtectedOutputSignerMissing",
      "ScriptSourceMissing",
      "RedeemerMissing",
    ],
  ],
  [
    RejectCodes.NativeScriptNodeCount,
    [
      "WitnessNativeScriptNodeLimit",
      "ResolvedReferenceScriptNodeLimit",
      "OutputReferenceScriptNodeLimit",
      "ExecutionNativeScriptNodeLimit",
    ],
  ],
  [
    RejectCodes.NativeScriptDepth,
    [
      "WitnessNativeScriptDepthLimit",
      "ResolvedReferenceScriptDepthLimit",
      "OutputReferenceScriptDepthLimit",
      "ExecutionNativeScriptDepthLimit",
    ],
  ],
  [
    RejectCodes.NativeScriptInvalid,
    ["WitnessNativeScriptFalse", "ExecutionNativeScriptFalse"],
  ],
  [
    RejectCodes.PlutusScriptInvalid,
    ["ReceivePurposePlutusV3Forbidden", "PlutusExecutionFailed"],
  ],
  [RejectCodes.ScriptProgramEncoding, ["PlutusScriptUndecodable"]],
  [RejectCodes.InputNotFound, ["InputNotFound"]],
  [
    RejectCodes.InvalidOutput,
    ["InputSpentOutputNonCanonical", "OutputNonCanonical"],
  ],
  [
    RejectCodes.AssetCount,
    [
      "MintDeclaredAssetLimit",
      "InputAssetAccumulationLimit",
      "OutputAssetAccumulationLimit",
      "MintAssetAccumulationLimit",
    ],
  ],
  [RejectCodes.MinAda, ["OutputBelowMinAda"]],
]);

/** The arm name of `reason`. */
export const rejectionReasonArmName = (
  reason: MidgardRejectionReasonV1,
): MidgardRejectionReasonV1ArmName =>
  (typeof reason === "string"
    ? reason
    : Object.keys(reason)[0]) as MidgardRejectionReasonV1ArmName;

/** Whether the machine can end a `code` rejection in `reason`'s arm. */
export const rejectionReasonFitsCode = (
  code: RejectCode,
  reason: MidgardRejectionReasonV1,
): boolean => {
  midgardRejectionReasonV1Parts(reason);
  return (
    REJECTION_REASON_ARMS_BY_CODE.get(code)?.includes(
      rejectionReasonArmName(reason),
    ) ?? false
  );
};

export type RejectionReasonResolution =
  | { readonly kind: "reason"; readonly reason: MidgardRejectionReasonV1 }
  /** No machine arm proves this pre-screen or infrastructure failure. */
  | { readonly kind: "unsupported" }
  /** The code has coordinate-carrying arms and no subject of the code names one. */
  | { readonly kind: "subjectMissing" };

/**
 * The exact `RejectionReasonV1` for a Phase A/B rejection: a field-less
 * code's single arm, otherwise the arm and coordinates of the subject the
 * failing rule recorded, under `coordinates` (the machine's terminal reason,
 * or the forced leaf's). A subject whose arm the code cannot end in is never
 * written.
 */
export const resolveRejectionReason = (
  rejection: Pick<RejectedTx, "code" | "subject">,
  coordinates: RejectionReasonCoordinates,
): RejectionReasonResolution => {
  const arms = REJECTION_REASON_ARMS_BY_CODE.get(rejection.code);
  if (arms === undefined) return { kind: "unsupported" };
  const [onlyArm] = arms;
  if (
    arms.length === 1 &&
    MIDGARD_REJECTION_REASON_V1_ARMS.some(
      (entry) => entry.name === onlyArm && entry.fields.length === 0,
    )
  )
    return { kind: "reason", reason: onlyArm as MidgardRejectionReasonV1 };
  if (rejection.subject === undefined) return { kind: "subjectMissing" };
  const reason = rejectionReasonOfSubject(rejection.subject, coordinates);
  return arms.includes(rejectionReasonArmName(reason))
    ? { kind: "reason", reason }
    : { kind: "subjectMissing" };
};
