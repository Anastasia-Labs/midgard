import * as SDK from "@al-ft/midgard-sdk";
import {
  type RejectCode,
  RejectCodes,
  type RejectedTx,
  type RejectSubject,
} from "@al-ft/midgard-validation";

/** The validation phase whose rejection the forced verdict records. */
export type ForcedRejectionPhase = "phaseA" | "phaseB";

/** What the writer reads from a Phase A/B rejection. */
export type ForcedRejection = Pick<RejectedTx, "code" | "subject">;

/**
 * Rejection codes no `RejectionReasonV1` arm covers. Each is raised by a node
 * pre-screen (consensus guardrail, admission binding, or block-graph rule)
 * whose code is not one of the 19 descriptor codes the fault proofs bind.
 */
export const FORCED_REJECTION_NO_ARM_CODES = Object.freeze([
  RejectCodes.CborDeserialization,
  RejectCodes.TxHashMismatch,
  RejectCodes.UnsupportedFieldNonEmpty,
  RejectCodes.DoubleSpend,
  RejectCodes.DependencyCycle,
  RejectCodes.DependsOnRejectedTx,
  RejectCodes.PlutusEvaluationUnavailable,
  RejectCodes.IsValidFalseForbidden,
  RejectCodes.AuxDataForbidden,
  RejectCodes.CertificatesForbidden,
  RejectCodes.NonZeroWithdrawal,
  RejectCodes.TxVersion,
  RejectCodes.TxSize,
  RejectCodes.ValueSize,
  RejectCodes.InputCount,
  RejectCodes.ReferenceInputCount,
  RejectCodes.OutputCount,
  RejectCodes.AddressWitnessCount,
  RejectCodes.RequiredSignerCount,
  RejectCodes.ScriptExecutionCount,
  RejectCodes.ObserverCount,
  RejectCodes.LedgerOutputSize,
  RejectCodes.DatumSize,
  RejectCodes.ScriptProgramSize,
  RejectCodes.ScriptProgramEncoding,
  RejectCodes.ScriptProgramAggregateSize,
  RejectCodes.RedeemerSize,
  RejectCodes.MintForbidden,
  RejectCodes.ReferenceInputForbidden,
  RejectCodes.ScriptFeatureForbidden,
  RejectCodes.CekProgramMaterial,
] as const);

/** The `RejectionReasonV1` value a recorded subject names, coordinates included. */
export const rejectionReasonOfSubject = (
  subject: RejectSubject,
): SDK.RejectionReason => {
  switch (subject.arm) {
    case "DuplicateInput":
      return {
        DuplicateInput: {
          first_field_index: subject.first.fieldIndex,
          first_item_index: subject.first.itemIndex,
          second_field_index: subject.second.fieldIndex,
          second_item_index: subject.second.itemIndex,
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
          input_index: subject.index,
        },
      };
    case "InputSpentOutputNonCanonical":
      return {
        InputSpentOutputNonCanonical: {
          source_kind: subject.sourceKind,
          input_index: subject.index,
        },
      };
    case "SpendInputSignerMissing":
      return { SpendInputSignerMissing: { input_index: subject.index } };
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
    case "UnusedRedeemer":
      return { UnusedRedeemer: { redeemer_index: subject.index } };
    case "UnusedScriptWitness":
      return { UnusedScriptWitness: { script_index: subject.index } };
    case "ExecutionNativeScriptFalse":
      return { ExecutionNativeScriptFalse: { execution_index: subject.index } };
    case "ReceivePurposePlutusV3Forbidden":
      return {
        ReceivePurposePlutusV3Forbidden: { execution_index: subject.index },
      };
    case "PlutusExecutionFailed":
      return { PlutusExecutionFailed: { execution_index: subject.index } };
    case "OutputNonCanonical":
      return { OutputNonCanonical: { output_index: subject.index } };
    case "OutputBelowMinAda":
      return { OutputBelowMinAda: { output_index: subject.index } };
    default: {
      const unreachable: never = subject;
      throw new Error(
        `rejectionReasonOfSubject: unknown subject ${JSON.stringify(unreachable)}`,
      );
    }
  }
};

type CodeDisposition =
  | {
      readonly kind: "arm";
      readonly code: SDK.RejectionCodeLabel;
      /** The reason written when the rejection recorded no usable subject. */
      readonly unlocated: SDK.RejectionReason;
    }
  | { readonly kind: "noArm"; readonly reason: SDK.RejectionReason };

const arm = (
  code: SDK.RejectionCodeLabel,
  unlocated: SDK.RejectionReason,
): CodeDisposition => ({ kind: "arm", code, unlocated });

const dispositionOf = (
  code: RejectCode,
  phase: ForcedRejectionPhase,
): CodeDisposition => {
  switch (code) {
    case RejectCodes.EmptyInputs:
      return arm("E_EMPTY_INPUTS", "EmptyInputs");
    case RejectCodes.DuplicateInputInTx:
      return arm("E_DUPLICATE_INPUT_IN_TX", {
        DuplicateInput: {
          first_field_index: 0n,
          first_item_index: 0n,
          second_field_index: 0n,
          second_item_index: 0n,
        },
      });
    case RejectCodes.InvalidOutput:
      return arm("E_INVALID_OUTPUT", {
        OutputNonCanonical: { output_index: 0n },
      });
    case RejectCodes.InvalidFieldType:
      return arm("E_INVALID_FIELD_TYPE", {
        FieldItemWidthIllegal: { field_index: 0n, item_index: 0n },
      });
    case RejectCodes.InputNotFound:
      return arm("E_INPUT_NOT_FOUND", {
        InputNotFound: { source_kind: 0n, input_index: 0n },
      });
    case RejectCodes.InvalidValidityIntervalFormat:
      return arm(
        "E_INVALID_VALIDITY_INTERVAL_FORMAT",
        "ValidityIntervalMalformed",
      );
    case RejectCodes.ValidityIntervalMismatch:
      return arm(
        "E_VALIDITY_INTERVAL_MISMATCH",
        "ValidityIntervalExcludesBlockSlot",
      );
    case RejectCodes.MinFee:
      return arm("E_MIN_FEE", "FeeBelowMinimum");
    case RejectCodes.MinAda:
      return arm("E_MIN_ADA", { OutputBelowMinAda: { output_index: 0n } });
    case RejectCodes.ValueNotPreserved:
      return arm("E_VALUE_NOT_PRESERVED", "ValueNotPreserved");
    case RejectCodes.MissingRequiredWitness:
      return arm("E_MISSING_REQUIRED_WITNESS", {
        RequiredSignerUnsigned: { signer_index: 0n },
      });
    case RejectCodes.InvalidSignature:
      return arm("E_INVALID_SIGNATURE", {
        AddressWitnessSignatureInvalid: { witness_index: 0n },
      });
    case RejectCodes.NativeScriptInvalid:
      // Phase A raises it only for witness-set natives, Phase B only for
      // execution natives; both arms bridge to this one code.
      return arm(
        "E_NATIVE_SCRIPT_INVALID",
        phase === "phaseA"
          ? { WitnessNativeScriptFalse: { script_index: 0n } }
          : { ExecutionNativeScriptFalse: { execution_index: 0n } },
      );
    case RejectCodes.PlutusScriptInvalid:
      return arm("E_PLUTUS_SCRIPT_INVALID", {
        PlutusExecutionFailed: { execution_index: 0n },
      });
    case RejectCodes.NetworkIdMismatch:
      return arm("E_NETWORK_ID_MISMATCH", "NetworkIdMismatch");
    case RejectCodes.FieldPreimageSize:
      return arm("E_FIELD_PREIMAGE_SIZE", {
        FieldPreimageLengthMismatch: { field_index: 0n },
      });
    case RejectCodes.NativeScriptDepth:
      return arm("E_NATIVE_SCRIPT_DEPTH", {
        WitnessNativeScriptDepthLimit: { script_index: 0n },
      });
    case RejectCodes.NativeScriptNodeCount:
      return arm("E_NATIVE_SCRIPT_NODE_COUNT", {
        WitnessNativeScriptNodeLimit: { script_index: 0n },
      });
    case RejectCodes.AssetCount:
      return arm("E_ASSET_COUNT", {
        MintAssetAccumulationLimit: { mint_index: 0n },
      });
    // Open question: no fault proof can prove these rejections, so each
    // needs either an on-chain RejectionReasonV1 arm or removal of the node
    // pre-screen that raises it. Until then they keep the verdict the node
    // has always written for them.
    case RejectCodes.PlutusEvaluationUnavailable:
      return {
        kind: "noArm",
        reason: { PlutusExecutionFailed: { execution_index: 0n } },
      };
    case RejectCodes.CborDeserialization:
    case RejectCodes.TxHashMismatch:
    case RejectCodes.UnsupportedFieldNonEmpty:
    case RejectCodes.DoubleSpend:
    case RejectCodes.DependencyCycle:
    case RejectCodes.DependsOnRejectedTx:
    case RejectCodes.IsValidFalseForbidden:
    case RejectCodes.AuxDataForbidden:
    case RejectCodes.CertificatesForbidden:
    case RejectCodes.NonZeroWithdrawal:
    case RejectCodes.TxVersion:
    case RejectCodes.TxSize:
    case RejectCodes.ValueSize:
    case RejectCodes.InputCount:
    case RejectCodes.ReferenceInputCount:
    case RejectCodes.OutputCount:
    case RejectCodes.AddressWitnessCount:
    case RejectCodes.RequiredSignerCount:
    case RejectCodes.ScriptExecutionCount:
    case RejectCodes.ObserverCount:
    case RejectCodes.LedgerOutputSize:
    case RejectCodes.DatumSize:
    case RejectCodes.ScriptProgramSize:
    case RejectCodes.ScriptProgramEncoding:
    case RejectCodes.ScriptProgramAggregateSize:
    case RejectCodes.RedeemerSize:
    case RejectCodes.MintForbidden:
    case RejectCodes.ReferenceInputForbidden:
    case RejectCodes.ScriptFeatureForbidden:
    case RejectCodes.CekProgramMaterial:
      return { kind: "noArm", reason: "ValueNotPreserved" };
    default: {
      const unreachable: never = code;
      throw new Error(
        `forcedRejectionReason: unknown code ${String(unreachable)}`,
      );
    }
  }
};

/**
 * The exact `RejectionReasonV1` a forced leaf records for a Phase A/B
 * rejection: the arm and coordinates of the rule that failed.
 *
 * The chain binds the verdict to the replayed rejection through its code
 * (`rejection_code_of`), so a reason whose code differs from the replay's
 * is disproved outright. The families behind each arm reopen the subject
 * the reason names, so the coordinates must be the ones the failing rule
 * found. The subject is used only when its arm bridges to the rejection's
 * own code; otherwise the code's arm is written at ordinal zero, which
 * keeps the code binding exact.
 */
export const forcedRejectionReason = (
  rejection: ForcedRejection,
  phase: ForcedRejectionPhase,
): SDK.RejectionReason => {
  const disposition = dispositionOf(rejection.code, phase);
  if (disposition.kind === "noArm") return disposition.reason;
  if (rejection.subject === undefined) return disposition.unlocated;
  const located = rejectionReasonOfSubject(rejection.subject);
  return SDK.rejectionCodeOf(located) === SDK.RejectionCodes[disposition.code]
    ? located
    : disposition.unlocated;
};

/** The forced leaf's operator verdict for a Phase A/B rejection. */
export const forcedVerdictForRejection = (
  rejection: ForcedRejection,
  phase: ForcedRejectionPhase,
): SDK.OperatorVerdict => ({
  ForcedTxInvalid: { reason: forcedRejectionReason(rejection, phase) },
});
