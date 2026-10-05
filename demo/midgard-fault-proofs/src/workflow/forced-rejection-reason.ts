import * as SDK from "@al-ft/midgard-sdk";
import {
  type RejectCode,
  RejectCodes,
  type RejectedTx,
  type RejectSubject,
} from "@al-ft/midgard-validation";

/** What the writer reads from a Phase A/B rejection. */
export type ForcedRejection = Pick<
  RejectedTx,
  "code" | "subject" | "consensusPhase"
>;

/** A local inability to record a provable verdict; this is never a verdict. */
export class ForcedRejectionStopped extends Error {
  readonly _tag = "ForcedRejectionStopped";
  readonly code: RejectCode;
  readonly consensusPhase: ForcedRejection["consensusPhase"];
  readonly retryable: boolean;

  constructor(rejection: ForcedRejection, message: string) {
    super(message);
    this.name = "ForcedRejectionStopped";
    this.code = rejection.code;
    this.consensusPhase = rejection.consensusPhase;
    this.retryable = rejection.code === RejectCodes.PlutusEvaluationUnavailable;
  }
}

/** The machine did not supply the subject required to cite its arm exactly. */
export class ForcedRejectionSubjectMissing extends ForcedRejectionStopped {
  constructor(rejection: ForcedRejection) {
    super(
      rejection,
      `forced rejection ${rejection.code} at ${rejection.consensusPhase} records no subject of its code${
        rejection.subject === undefined
          ? ""
          : ` (it names ${rejection.subject.arm})`
      }`,
    );
    this.name = "ForcedRejectionSubjectMissing";
  }
}

/** No deployed machine arm proves this pre-screen or evaluation failure. */
export class ForcedRejectionUnsupported extends ForcedRejectionStopped {
  constructor(rejection: ForcedRejection) {
    super(
      rejection,
      `forced rejection ${rejection.code} at ${rejection.consensusPhase} has no deployed machine arm`,
    );
    this.name = "ForcedRejectionUnsupported";
  }
}

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
    case "WitnessNativeScriptMalformed":
      return { WitnessNativeScriptMalformed: { script_index: subject.index } };
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
    case "RedeemerMalformed":
      return { RedeemerMalformed: { redeemer_index: subject.index } };
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
    case "InputAssetAccumulationLimit":
      return {
        InputAssetAccumulationLimit: {
          input_index: subject.index,
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
        `rejectionReasonOfSubject: unknown subject ${JSON.stringify(unreachable)}`,
      );
    }
  }
};

type CodeDisposition =
  /** The code's only arm carries no coordinate. */
  | { readonly kind: "fixed"; readonly reason: SDK.RejectionReason }
  /** The code has coordinate-carrying arms: the subject names the one. */
  | { readonly kind: "located"; readonly code: SDK.RejectionCodeLabel }
  | { readonly kind: "stop" };

const fixed = (reason: SDK.RejectionReason): CodeDisposition => ({
  kind: "fixed",
  reason,
});

const located = (code: SDK.RejectionCodeLabel): CodeDisposition => ({
  kind: "located",
  code,
});

const dispositionOf = (code: RejectCode): CodeDisposition => {
  switch (code) {
    case RejectCodes.EmptyInputs:
      return fixed("EmptyInputs");
    case RejectCodes.InvalidValidityIntervalFormat:
      return fixed("ValidityIntervalMalformed");
    case RejectCodes.ValidityIntervalMismatch:
      return fixed("ValidityIntervalExcludesBlockSlot");
    case RejectCodes.MinFee:
      return fixed("FeeBelowMinimum");
    case RejectCodes.ValueNotPreserved:
      return fixed("ValueNotPreserved");
    case RejectCodes.NetworkIdMismatch:
      return fixed("NetworkIdMismatch");
    case RejectCodes.DuplicateInputInTx:
      return located("E_DUPLICATE_INPUT_IN_TX");
    case RejectCodes.InvalidOutput:
      return located("E_INVALID_OUTPUT");
    case RejectCodes.InvalidFieldType:
      return located("E_INVALID_FIELD_TYPE");
    case RejectCodes.InputNotFound:
      return located("E_INPUT_NOT_FOUND");
    case RejectCodes.MinAda:
      return located("E_MIN_ADA");
    case RejectCodes.MissingRequiredWitness:
      return located("E_MISSING_REQUIRED_WITNESS");
    case RejectCodes.InvalidSignature:
      return located("E_INVALID_SIGNATURE");
    case RejectCodes.NativeScriptInvalid:
      return located("E_NATIVE_SCRIPT_INVALID");
    case RejectCodes.PlutusScriptInvalid:
      return located("E_PLUTUS_SCRIPT_INVALID");
    case RejectCodes.FieldPreimageSize:
      return located("E_FIELD_PREIMAGE_SIZE");
    case RejectCodes.NativeScriptDepth:
      return located("E_NATIVE_SCRIPT_DEPTH");
    case RejectCodes.NativeScriptNodeCount:
      return located("E_NATIVE_SCRIPT_NODE_COUNT");
    case RejectCodes.AssetCount:
      return located("E_ASSET_COUNT");
    // Pre-screen and infrastructure failures are not machine verdicts. Keep
    // them as explicit stops until the authoritative machine can judge them.
    case RejectCodes.PlutusEvaluationUnavailable:
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
      return { kind: "stop" };
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
 * found: a code with coordinate-carrying arms is written only from a subject
 * of that code, and a rejection without one throws
 * {@link ForcedRejectionSubjectMissing}.
 */
export const forcedRejectionReason = (
  rejection: ForcedRejection,
): SDK.RejectionReason => {
  const disposition = dispositionOf(rejection.code);
  if (disposition.kind === "stop")
    throw new ForcedRejectionUnsupported(rejection);
  if (disposition.kind === "fixed") return disposition.reason;
  if (rejection.subject !== undefined) {
    const reason = rejectionReasonOfSubject(rejection.subject);
    if (SDK.rejectionCodeOf(reason) === SDK.RejectionCodes[disposition.code])
      return reason;
    throw new ForcedRejectionSubjectMissing(rejection);
  }
  throw new ForcedRejectionSubjectMissing(rejection);
};

/** The forced leaf's operator verdict for a Phase A/B rejection. */
export const forcedVerdictForRejection = (
  rejection: ForcedRejection,
): SDK.OperatorVerdict => ({
  ForcedTxInvalid: { reason: forcedRejectionReason(rejection) },
});
