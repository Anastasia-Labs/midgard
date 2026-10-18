/** Traversal precedence twin for the direct CanonicalDecode and ResolveInputs families. */
import type { RejectionReason } from "../rejection-reason.js";
import {
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "./proof-thread-substrate.js";

type ReasonArm<R = RejectionReason> = R extends string
  ? R
  : R extends object
    ? keyof R
    : never;

type PhaseRule = number | ((reason: RejectionReason) => number);

/**
 * A Plutus item is decoded where its native siblings are: a witness script at
 * PhaseANativeScripts, an output reference script at ScriptSources. No other
 * item kind exists.
 */
const scriptItemPhase = (reason: RejectionReason): number => {
  const kind =
    typeof reason === "string" || !("PlutusScriptUndecodable" in reason)
      ? undefined
      : reason.PlutusScriptUndecodable.item_kind;
  if (kind === 0n) return 5;
  if (kind === 1n) return 8;
  throw new Error("first-fault: unknown script item kind");
};

const PHASE = {
  FieldPreimageLengthMismatch: 0,
  FieldItemWidthIllegal: 0,
  NetworkIdMismatch: 2,
  FeeBelowMinimum: 2,
  EmptyInputs: 3,
  DuplicateInput: 3,
  ValidityIntervalMalformed: 3,
  AddressWitnessSignatureInvalid: 4,
  RequiredSignerUnsigned: 4,
  WitnessScriptHeaderMalformed: 5,
  WitnessNativeScriptMalformed: 5,
  WitnessNativeScriptNodeLimit: 5,
  WitnessNativeScriptDepthLimit: 5,
  WitnessNativeScriptFalse: 5,
  ScriptIntegrityHashMissing: 6,
  ObserversForbiddenOnUntaggedNetwork: 6,
  ObserverOrderInvalid: 6,
  ValidityIntervalExcludesBlockSlot: 7,
  InputNotFound: 7,
  InputSpentOutputNonCanonical: 7,
  ResolvedReferenceScriptMalformed: 7,
  ResolvedReferenceScriptNodeLimit: 7,
  ResolvedReferenceScriptDepthLimit: 7,
  SpendInputSignerMissing: 7,
  RedeemerMalformed: 8,
  OutputNonCanonical: 8,
  OutputReferenceScriptMalformed: 8,
  OutputReferenceScriptNodeLimit: 8,
  OutputReferenceScriptDepthLimit: 8,
  ProtectedOutputSignerMissing: 8,
  MintDeclaredAssetLimit: 8,
  ScriptSourceMissing: 8,
  RedeemerMissing: 8,
  UnusedScriptWitness: 8,
  UnusedRedeemer: 8,
  MintItemMalformed: 8,
  ExecutionNativeScriptMalformed: 9,
  ExecutionNativeScriptNodeLimit: 9,
  ExecutionNativeScriptDepthLimit: 9,
  ExecutionNativeScriptFalse: 9,
  ScriptIntegrityHashMismatch: 10,
  ReceivePurposePlutusV3Forbidden: 11,
  PlutusExecutionFailed: 11,
  InputAssetAccumulationLimit: 12,
  OutputAssetAccumulationLimit: 12,
  MintAssetAccumulationLimit: 12,
  OutputBelowMinAda: 12,
  ValueNotPreserved: 12,
  PlutusScriptUndecodable: scriptItemPhase,
} satisfies Record<ReasonArm, PhaseRule>;
const PHASE_BY_ARM: ReadonlyMap<string, PhaseRule> = new Map(
  Object.entries(PHASE),
);

export const rejectionReasonPhase = (reason: RejectionReason): number => {
  const arm = typeof reason === "string" ? reason : Object.keys(reason)[0];
  const phase = arm === undefined ? undefined : PHASE_BY_ARM.get(arm);
  if (phase === undefined)
    throw new Error("first-fault: unknown rejection arm");
  return typeof phase === "function" ? phase(reason) : phase;
};

export type CanonicalFaultPosition =
  | Readonly<{ kind: "item"; fieldIndex: bigint; itemIndex: bigint }>
  | Readonly<{ kind: "fieldEnd"; fieldIndex: bigint }>;

export type ResolutionFaultPosition = Readonly<{
  sourceKind: bigint;
  inputIndex: bigint;
  outRef: Uint8Array;
  subcheck: number;
}>;

export type FaultPosition =
  | Readonly<{ phase: "canonical"; position: CanonicalFaultPosition }>
  | Readonly<{ phase: "resolve"; position: ResolutionFaultPosition }>;

export const resolutionCoordinateOfReason = (
  reason: RejectionReason,
): Readonly<{
  sourceKind: bigint;
  inputIndex: bigint;
  subcheck: number;
}> | null => {
  if (typeof reason === "string") return null;
  if ("InputNotFound" in reason)
    return {
      sourceKind: reason.InputNotFound.source_kind,
      inputIndex: reason.InputNotFound.input_index,
      subcheck: 0,
    };
  if ("InputSpentOutputNonCanonical" in reason)
    return {
      sourceKind: reason.InputSpentOutputNonCanonical.source_kind,
      inputIndex: reason.InputSpentOutputNonCanonical.input_index,
      subcheck: 1,
    };
  if ("ResolvedReferenceScriptMalformed" in reason)
    return {
      sourceKind: reason.ResolvedReferenceScriptMalformed.source_kind,
      inputIndex: reason.ResolvedReferenceScriptMalformed.input_index,
      subcheck: 2,
    };
  if ("ResolvedReferenceScriptNodeLimit" in reason)
    return {
      sourceKind: reason.ResolvedReferenceScriptNodeLimit.source_kind,
      inputIndex: reason.ResolvedReferenceScriptNodeLimit.input_index,
      subcheck: 3,
    };
  if ("ResolvedReferenceScriptDepthLimit" in reason)
    return {
      sourceKind: reason.ResolvedReferenceScriptDepthLimit.source_kind,
      inputIndex: reason.ResolvedReferenceScriptDepthLimit.input_index,
      subcheck: 4,
    };
  if ("SpendInputSignerMissing" in reason)
    return {
      sourceKind: 0n,
      inputIndex: reason.SpendInputSignerMissing.input_index,
      subcheck: 5,
    };
  return null;
};

export const canonicalPositionPrecedesOrEquals = (
  left: CanonicalFaultPosition,
  right: CanonicalFaultPosition,
): boolean => {
  if (left.fieldIndex !== right.fieldIndex)
    return left.fieldIndex < right.fieldIndex;
  if (left.kind === "fieldEnd") return right.kind === "fieldEnd";
  return right.kind === "fieldEnd" || left.itemIndex <= right.itemIndex;
};

export const resolutionPositionPrecedesOrEquals = (
  left: ResolutionFaultPosition,
  right: ResolutionFaultPosition,
): boolean => {
  for (const point of [left, right]) {
    if (
      (point.sourceKind !== 0n && point.sourceKind !== 1n) ||
      point.inputIndex < 0n ||
      point.outRef.length !== 38 ||
      !Number.isInteger(point.subcheck) ||
      point.subcheck < 0 ||
      point.subcheck > 5
    ) {
      throw new Error("first-fault: invalid authenticated resolution position");
    }
  }
  const compared = Buffer.compare(left.outRef, right.outRef);
  if (compared !== 0) return compared < 0;
  if (
    left.sourceKind !== right.sourceKind ||
    left.inputIndex !== right.inputIndex
  )
    throw new Error("first-fault: duplicate outrefs survived InputSets");
  return left.subcheck <= right.subcheck;
};

/** Claimed resolution bytes are reopened under the same source at the exact original reason coordinate. */
export const leafAdmitsFault = (
  subject: VerdictSubject,
  observed: FaultPosition,
  claimedResolution: ResolutionFaultPosition | null = null,
): boolean => {
  if (!verdictSubjectIsCanonical(subject))
    throw new Error("first-fault: noncanonical subject");
  const reason = subject.rejection_reason;
  if (observed.phase === "canonical") {
    if (claimedResolution !== null)
      throw new Error("first-fault: unused resolution claim");
    if (
      reason === null ||
      rejectionReasonPhase(reason) !== 0 ||
      typeof reason === "string"
    )
      return false;
    if ("FieldItemWidthIllegal" in reason)
      return canonicalPositionPrecedesOrEquals(
        {
          kind: "item",
          fieldIndex: reason.FieldItemWidthIllegal.field_index,
          itemIndex: reason.FieldItemWidthIllegal.item_index,
        },
        observed.position,
      );
    if ("FieldPreimageLengthMismatch" in reason)
      return canonicalPositionPrecedesOrEquals(
        {
          kind: "fieldEnd",
          fieldIndex: reason.FieldPreimageLengthMismatch.field_index,
        },
        observed.position,
      );
    throw new Error("first-fault: unmapped canonical arm");
  }
  const phase = reason === null ? null : rejectionReasonPhase(reason);
  const coordinate =
    reason === null ? null : resolutionCoordinateOfReason(reason);
  if (coordinate === null) {
    if (claimedResolution !== null)
      throw new Error("first-fault: unused resolution claim");
    return (
      phase !== null &&
      (phase < 7 || reason === "ValidityIntervalExcludesBlockSlot")
    );
  }
  if (
    claimedResolution === null ||
    claimedResolution.sourceKind !== coordinate.sourceKind ||
    claimedResolution.inputIndex !== coordinate.inputIndex ||
    claimedResolution.subcheck !== coordinate.subcheck
  )
    throw new Error(
      "first-fault: claimed original coordinate differs from leaf reason",
    );
  return resolutionPositionPrecedesOrEquals(
    claimedResolution,
    observed.position,
  );
};
