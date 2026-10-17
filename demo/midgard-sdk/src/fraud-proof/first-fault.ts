/** Traversal precedence for canonical, resolution and direct-purpose faults. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import type { Header } from "../ledger-state.js";
import type { RejectionReason } from "../rejection-reason.js";
import type { NativeTxBodyCompact } from "./native.js";
import {
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "./proof-thread-substrate.js";

type ReasonArm<R = RejectionReason> = R extends string
  ? R
  : R extends object
    ? keyof R
    : never;

const PHASE = {
  FieldPreimageLengthMismatch: 0,
  FieldItemWidthIllegal: 0,
  NetworkIdMismatch: "network",
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
} satisfies Record<ReasonArm, number | "network">;
const PHASE_BY_ARM: ReadonlyMap<string, number | "network"> = new Map(
  Object.entries(PHASE),
);

export const NetworkOrderContextSchema = Data.Object({
  body_network_mismatch: Data.Boolean(),
});
export type NetworkOrderContext = Data.Static<typeof NetworkOrderContextSchema>;
export const NetworkOrderContext = asDataType<NetworkOrderContext>(
  NetworkOrderContextSchema,
);

export const PurposeCoordinateSchema = Data.Object({
  kind: Data.Integer(),
  index: Data.Integer(),
});
export type PurposeCoordinate = Data.Static<typeof PurposeCoordinateSchema>;
export const PurposeCoordinate = asDataType<PurposeCoordinate>(
  PurposeCoordinateSchema,
);

/** The owning CT route proves existence/nonexistence under the same source.
 * ExistingPurpose does not establish that the leaf's failure predicate is true. */
export const ClaimedPurposeOrderSchema = Data.Enum([
  Data.Object({
    ExistingPurpose: Data.Object({ coordinate: PurposeCoordinateSchema }),
  }),
  Data.Object({
    ImpossiblePurpose: Data.Object({ coordinate: PurposeCoordinateSchema }),
  }),
]);
export type ClaimedPurposeOrder = Data.Static<typeof ClaimedPurposeOrderSchema>;
export const ClaimedPurposeOrder = asDataType<ClaimedPurposeOrder>(
  ClaimedPurposeOrderSchema,
);

const hasExactKeys = (value: unknown, keys: readonly string[]): boolean =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  Object.keys(value).length === keys.length &&
  keys.every((key) => Object.prototype.hasOwnProperty.call(value, key));

const requireContext = (
  context: NetworkOrderContext | null,
): NetworkOrderContext => {
  if (
    !hasExactKeys(context, ["body_network_mismatch"]) ||
    typeof context?.body_network_mismatch !== "boolean"
  )
    throw new Error("first-fault: missing or malformed network context");
  return context;
};

/** Both projections must already be authenticated under the same source/header. */
export const networkOrderContext = (
  compact: Readonly<{ body: NativeTxBodyCompact }>,
  header: Header,
): NetworkOrderContext => ({
  body_network_mismatch:
    compact.body.network_id !== 255n &&
    compact.body.network_id !== header.expectedNetworkId,
});

const reasonArm = (reason: RejectionReason): string => {
  const arm =
    typeof reason === "string"
      ? reason
      : typeof reason === "object" &&
          reason !== null &&
          !Array.isArray(reason) &&
          Object.keys(reason).length === 1
        ? Object.keys(reason)[0]
        : undefined;
  if (arm === undefined || !PHASE_BY_ARM.has(arm))
    throw new Error("first-fault: unknown rejection arm");
  return arm;
};

export const rejectionReasonPhase = (
  reason: RejectionReason,
  context: NetworkOrderContext,
): number => {
  const checked = requireContext(context);
  const phase = PHASE_BY_ARM.get(reasonArm(reason));
  if (phase === undefined)
    throw new Error("first-fault: unknown rejection arm");
  return phase === "network" ? (checked.body_network_mismatch ? 2 : 8) : phase;
};

export type PurposeSubcheck = "SourceRequired" | "RedeemerRequired";
export type PurposeFaultPosition = Readonly<{
  coordinate: PurposeCoordinate;
  subcheck: PurposeSubcheck;
}>;
export type ClaimedFaultOrder =
  | "NotRequired"
  | Readonly<{ ResolutionReopened: ResolutionFaultPosition }>
  | Readonly<{ PurposeReopened: ClaimedPurposeOrder }>;

export const purposeCoordinateOfReason = (
  reason: RejectionReason,
): PurposeCoordinate | null => {
  reasonArm(reason);
  if (typeof reason === "string") return null;
  const payload =
    "ScriptSourceMissing" in reason
      ? reason.ScriptSourceMissing
      : "RedeemerMissing" in reason
        ? reason.RedeemerMissing
        : null;
  return payload === null
    ? null
    : { kind: payload.purpose_kind, index: payload.purpose_index };
};
export const purposeSubcheckOfReason = (
  reason: RejectionReason,
): PurposeSubcheck => {
  switch (reasonArm(reason)) {
    case "ScriptSourceMissing":
      return "SourceRequired";
    case "RedeemerMissing":
      return "RedeemerRequired";
    default:
      throw new Error("first-fault: not a direct-purpose reason");
  }
};
export const purposeCoordinateIsValid = (
  coordinate: PurposeCoordinate,
): boolean =>
  hasExactKeys(coordinate, ["kind", "index"]) &&
  typeof coordinate.kind === "bigint" &&
  typeof coordinate.index === "bigint" &&
  coordinate.kind >= 0n &&
  coordinate.kind <= 3n &&
  coordinate.index >= 0n;

const requirePurposePosition = (position: PurposeFaultPosition): void => {
  if (
    !purposeCoordinateIsValid(position.coordinate) ||
    (position.subcheck !== "SourceRequired" &&
      position.subcheck !== "RedeemerRequired")
  )
    throw new Error("first-fault: invalid authenticated purpose position");
};
export const purposePositionPrecedesOrEquals = (
  left: PurposeFaultPosition,
  right: PurposeFaultPosition,
): boolean => {
  requirePurposePosition(left);
  requirePurposePosition(right);
  if (left.coordinate.kind !== right.coordinate.kind)
    return left.coordinate.kind < right.coordinate.kind;
  if (left.coordinate.index !== right.coordinate.index)
    return left.coordinate.index < right.coordinate.index;
  return (
    left.subcheck === "SourceRequired" || right.subcheck === "RedeemerRequired"
  );
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
  | Readonly<{ phase: "resolve"; position: ResolutionFaultPosition }>
  | Readonly<{ phase: "purpose"; position: PurposeFaultPosition }>;

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

const requireCanonicalPosition = (position: CanonicalFaultPosition): void => {
  if (
    !hasExactKeys(
      position,
      position.kind === "item"
        ? ["kind", "fieldIndex", "itemIndex"]
        : ["kind", "fieldIndex"],
    ) ||
    (position.kind !== "item" && position.kind !== "fieldEnd") ||
    typeof position.fieldIndex !== "bigint" ||
    (position.kind === "item" && typeof position.itemIndex !== "bigint")
  )
    throw new Error("first-fault: invalid canonical position");
};

export const canonicalPositionPrecedesOrEquals = (
  left: CanonicalFaultPosition,
  right: CanonicalFaultPosition,
): boolean => {
  requireCanonicalPosition(left);
  requireCanonicalPosition(right);
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

const requireNotRequired = (claimed: ClaimedFaultOrder): void => {
  if (claimed !== "NotRequired")
    throw new Error("first-fault: unused claimed receipt");
};

/** Receipts must be reopened under the observed proof's authenticated source. */
export const leafAdmitsFault = (
  subject: VerdictSubject,
  context: NetworkOrderContext | null,
  observed: FaultPosition,
  claimed: ClaimedFaultOrder,
): boolean => {
  if (!verdictSubjectIsCanonical(subject))
    throw new Error("first-fault: noncanonical subject");
  const reason = subject.rejection_reason;
  if (reason !== null) reasonArm(reason);
  if (observed.phase === "canonical") {
    if (context !== null)
      throw new Error("first-fault: unused network context");
    requireNotRequired(claimed);
    requireCanonicalPosition(observed.position);
    if (reason === null || typeof reason === "string") return false;
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
    return false;
  }
  const checked = requireContext(context);
  const phase = reason === null ? null : rejectionReasonPhase(reason, checked);
  if (observed.phase === "resolve") {
    const coordinate =
      reason === null ? null : resolutionCoordinateOfReason(reason);
    if (coordinate === null) {
      requireNotRequired(claimed);
      return (
        phase !== null &&
        (phase < 7 || reason === "ValidityIntervalExcludesBlockSlot")
      );
    }
    if (
      !hasExactKeys(claimed, ["ResolutionReopened"]) ||
      typeof claimed === "string" ||
      !("ResolutionReopened" in claimed)
    )
      throw new Error("first-fault: missing claimed resolution receipt");
    const point = claimed.ResolutionReopened;
    if (
      point.sourceKind !== coordinate.sourceKind ||
      point.inputIndex !== coordinate.inputIndex ||
      point.subcheck !== coordinate.subcheck
    )
      throw new Error(
        "first-fault: claimed original coordinate differs from leaf reason",
      );
    return resolutionPositionPrecedesOrEquals(point, observed.position);
  }
  if (observed.phase !== "purpose")
    throw new Error("first-fault: unknown observed phase");
  requirePurposePosition(observed.position);
  if (phase !== 8 || reason === null) {
    requireNotRequired(claimed);
    return phase !== null && phase < 8;
  }
  switch (reasonArm(reason)) {
    case "RedeemerMalformed":
    case "OutputNonCanonical":
    case "OutputReferenceScriptMalformed":
    case "OutputReferenceScriptNodeLimit":
    case "OutputReferenceScriptDepthLimit":
    case "ProtectedOutputSignerMissing":
    case "MintDeclaredAssetLimit":
    case "MintItemMalformed":
    case "NetworkIdMismatch":
      requireNotRequired(claimed);
      return true;
    case "UnusedScriptWitness":
    case "UnusedRedeemer":
      requireNotRequired(claimed);
      return false;
    case "ScriptSourceMissing":
    case "RedeemerMissing": {
      if (
        !hasExactKeys(claimed, ["PurposeReopened"]) ||
        typeof claimed === "string" ||
        !("PurposeReopened" in claimed)
      )
        throw new Error("first-fault: missing claimed purpose receipt");
      const receipt = claimed.PurposeReopened;
      const existing = hasExactKeys(receipt, ["ExistingPurpose"]);
      if (!existing && !hasExactKeys(receipt, ["ImpossiblePurpose"]))
        throw new Error("first-fault: unknown claimed purpose receipt");
      const evidence =
        "ExistingPurpose" in receipt
          ? receipt.ExistingPurpose
          : receipt.ImpossiblePurpose;
      const coordinate = purposeCoordinateOfReason(reason);
      if (
        !hasExactKeys(evidence, ["coordinate"]) ||
        !hasExactKeys(evidence.coordinate, ["kind", "index"]) ||
        typeof evidence.coordinate.kind !== "bigint" ||
        typeof evidence.coordinate.index !== "bigint" ||
        coordinate === null ||
        evidence.coordinate.kind !== coordinate.kind ||
        evidence.coordinate.index !== coordinate.index
      )
        throw new Error(
          "first-fault: claimed purpose coordinate differs from leaf reason",
        );
      if (!existing) return false;
      return purposePositionPrecedesOrEquals(
        { coordinate, subcheck: purposeSubcheckOfReason(reason) },
        observed.position,
      );
    }
    default:
      throw new Error("first-fault: unmapped purpose-phase reason");
  }
};
