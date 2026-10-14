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
 * against the trace stay excluded from the deployment.
 */
export const UNSET_REASON_COORDINATE = -1n;

/**
 * The `RejectionReasonV1` the validation machine emits for the rule that
 * recorded `subject`, coordinates included. Coordinates the machine does not
 * carry yet are written as {@link UNSET_REASON_COORDINATE}.
 */
export const machineRejectionReasonOfSubject = (
  subject: RejectSubject,
): MidgardRejectionReasonV1 => {
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
          first_field_index: UNSET_REASON_COORDINATE,
          first_item_index: UNSET_REASON_COORDINATE,
          second_field_index: UNSET_REASON_COORDINATE,
          second_item_index: UNSET_REASON_COORDINATE,
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
          input_index: UNSET_REASON_COORDINATE,
        },
      };
    case "InputSpentOutputNonCanonical":
      return {
        InputSpentOutputNonCanonical: {
          source_kind: subject.sourceKind,
          input_index: UNSET_REASON_COORDINATE,
        },
      };
    case "SpendInputSignerMissing":
      return {
        SpendInputSignerMissing: { input_index: UNSET_REASON_COORDINATE },
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
          input_index: UNSET_REASON_COORDINATE,
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
    default: {
      const unreachable: never = subject;
      throw new Error(
        `machineRejectionReasonOfSubject: unknown subject ${JSON.stringify(unreachable)}`,
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
 * The exact `RejectionReasonV1` the validation machine emits for a Phase A/B
 * rejection: a field-less code's single arm, otherwise the arm and
 * coordinates of the subject the failing rule recorded. A subject whose arm
 * the code cannot end in is never written.
 */
export const resolveRejectionReason = (
  rejection: Pick<RejectedTx, "code" | "subject">,
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
  const reason = machineRejectionReasonOfSubject(rejection.subject);
  return arms.includes(rejectionReasonArmName(reason))
    ? { kind: "reason", reason }
    : { kind: "subjectMissing" };
};
