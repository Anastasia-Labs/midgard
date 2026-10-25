/**
 * Traversal precedence for the faults after the direct purposes: the
 * ScriptSources unused audits and the NativeScripts, ScriptIntegrity, Cek and
 * ValueAndMint phases. Twin of the later-fault half of first-fault-v1.ak.
 *
 * Each key is (phase, stage, coordinate, subcheck), the machine's order:
 * - ScriptSources audits inline witnesses by field-6 ordinal, then redeemers
 *   by field-8 ordinal; both follow every purpose;
 * - NativeScripts and Cek visit executions in purpose-cursor order,
 *   (kind, index); one execution's token scan precedes its evaluation, and its
 *   receive-language selection precedes its run;
 * - ScriptIntegrity is transaction-global;
 * - ValueAndMint folds inputs, then each output (its minimum-Ada floor before
 *   its assets), then mint assets, then checks preservation.
 * A later position needs no claimed receipt: inside a phase the leaf's own
 * coordinate orders it directly.
 */
import type { RejectionReason } from "../rejection-reason.js";
import {
  type NetworkOrderContext,
  type PurposeCoordinate,
  purposeCoordinateIsValid,
  rejectionReasonPhase,
} from "./first-fault.js";
import {
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "./proof-thread-substrate.js";

export type UnusedAuditPosition =
  | Readonly<{ kind: "witness"; scriptIndex: bigint }>
  | Readonly<{ kind: "redeemer"; redeemerIndex: bigint }>;

/** The token scan's three arms share one rank. */
export type ExecutionSubcheck = "NativeScriptScan" | "NativeScriptEvaluation";
export type ExecutionFaultPosition = Readonly<{
  coordinate: PurposeCoordinate;
  subcheck: ExecutionSubcheck;
}>;

export type CekSubcheck = "ReceiveLanguage" | "ScriptRun";
export type CekFaultPosition = Readonly<{
  coordinate: PurposeCoordinate;
  subcheck: CekSubcheck;
}>;

export type ValueFaultPosition =
  | Readonly<{ kind: "input"; inputIndex: bigint; assetIndex: bigint }>
  | Readonly<{ kind: "output"; outputIndex: bigint; assetIndex: bigint }>
  | Readonly<{ kind: "mint"; mintIndex: bigint }>;

export type LaterFaultPosition =
  | Readonly<{ phase: "unusedAudit"; position: UnusedAuditPosition }>
  | Readonly<{ phase: "execution"; position: ExecutionFaultPosition }>
  | Readonly<{ phase: "scriptIntegrity" }>
  | Readonly<{ phase: "cek"; position: CekFaultPosition }>
  | Readonly<{ phase: "value"; position: ValueFaultPosition }>;

const fail = (message: string): never => {
  throw new Error(`first-fault: ${message}`);
};

const index = (value: unknown): bigint =>
  typeof value === "bigint" && value >= 0n
    ? value
    : fail("invalid later-fault index");

const coordinate = (value: PurposeCoordinate): PurposeCoordinate =>
  purposeCoordinateIsValid(value) ? value : fail("invalid purpose coordinate");

/** Payload of a single-arm reason object, or null for another arm. */
const payload = <T>(reason: RejectionReason, arm: string): T | null =>
  typeof reason === "object" && arm in reason
    ? ((reason as Record<string, unknown>)[arm] as T)
    : null;

type PurposePayload = Readonly<{ purpose_kind: bigint; purpose_index: bigint }>;

export const laterFaultPhase = (position: LaterFaultPosition): number => {
  switch (position.phase) {
    case "unusedAudit":
      return 8;
    case "execution":
      return 9;
    case "scriptIntegrity":
      return 10;
    case "cek":
      return 11;
    case "value":
      return 12;
    default:
      return fail("unknown later-fault phase");
  }
};

export const unusedAuditPositionOfReason = (
  reason: RejectionReason,
): UnusedAuditPosition | null => {
  const witness = payload<{ script_index: bigint }>(
    reason,
    "UnusedScriptWitness",
  );
  if (witness !== null)
    return { kind: "witness", scriptIndex: witness.script_index };
  const redeemer = payload<{ redeemer_index: bigint }>(
    reason,
    "UnusedRedeemer",
  );
  return redeemer === null
    ? null
    : { kind: "redeemer", redeemerIndex: redeemer.redeemer_index };
};

const unusedAuditKey = (
  position: UnusedAuditPosition,
): readonly [number, bigint] => {
  switch (position.kind) {
    case "witness":
      return [0, index(position.scriptIndex)];
    case "redeemer":
      return [1, index(position.redeemerIndex)];
    default:
      return fail("unknown unused audit");
  }
};

export const unusedAuditPositionPrecedesOrEquals = (
  left: UnusedAuditPosition,
  right: UnusedAuditPosition,
): boolean => {
  const [leftStage, leftIndex] = unusedAuditKey(left);
  const [rightStage, rightIndex] = unusedAuditKey(right);
  return leftStage !== rightStage
    ? leftStage < rightStage
    : leftIndex <= rightIndex;
};

const purposeOf = (value: PurposePayload): PurposeCoordinate => ({
  kind: value.purpose_kind,
  index: value.purpose_index,
});

export const executionPositionOfReason = (
  reason: RejectionReason,
): ExecutionFaultPosition | null => {
  for (const arm of [
    "ExecutionNativeScriptMalformed",
    "ExecutionNativeScriptNodeLimit",
    "ExecutionNativeScriptDepthLimit",
  ]) {
    const scan = payload<PurposePayload>(reason, arm);
    if (scan !== null)
      return { coordinate: purposeOf(scan), subcheck: "NativeScriptScan" };
  }
  const evaluation = payload<PurposePayload>(
    reason,
    "ExecutionNativeScriptFalse",
  );
  return evaluation === null
    ? null
    : {
        coordinate: purposeOf(evaluation),
        subcheck: "NativeScriptEvaluation",
      };
};

/** (kind, index); at one coordinate the subcheck verdict decides. */
const coordinatePrecedesOrEquals = (
  left: PurposeCoordinate,
  right: PurposeCoordinate,
  subcheckPrecedesOrEquals: boolean,
): boolean => {
  const l = coordinate(left);
  const r = coordinate(right);
  if (l.kind !== r.kind) return l.kind < r.kind;
  if (l.index !== r.index) return l.index < r.index;
  return subcheckPrecedesOrEquals;
};

const requireSubcheck = <S extends string>(
  value: S,
  allowed: readonly [S, S],
): S => (allowed.includes(value) ? value : fail("unknown later subcheck"));

export const executionPositionPrecedesOrEquals = (
  left: ExecutionFaultPosition,
  right: ExecutionFaultPosition,
): boolean => {
  const order = ["NativeScriptScan", "NativeScriptEvaluation"] as const;
  const l = requireSubcheck(left.subcheck, order);
  const r = requireSubcheck(right.subcheck, order);
  return coordinatePrecedesOrEquals(
    left.coordinate,
    right.coordinate,
    l === "NativeScriptScan" || r === "NativeScriptEvaluation",
  );
};

export const cekPositionOfReason = (
  reason: RejectionReason,
): CekFaultPosition | null => {
  const selection = payload<PurposePayload>(
    reason,
    "ReceivePurposePlutusV3Forbidden",
  );
  if (selection !== null)
    return { coordinate: purposeOf(selection), subcheck: "ReceiveLanguage" };
  const run = payload<PurposePayload>(reason, "PlutusExecutionFailed");
  return run === null
    ? null
    : { coordinate: purposeOf(run), subcheck: "ScriptRun" };
};

export const cekPositionPrecedesOrEquals = (
  left: CekFaultPosition,
  right: CekFaultPosition,
): boolean => {
  const order = ["ReceiveLanguage", "ScriptRun"] as const;
  const l = requireSubcheck(left.subcheck, order);
  const r = requireSubcheck(right.subcheck, order);
  return coordinatePrecedesOrEquals(
    left.coordinate,
    right.coordinate,
    l === "ReceiveLanguage" || r === "ScriptRun",
  );
};

export const valuePositionOfReason = (
  reason: RejectionReason,
): ValueFaultPosition | null => {
  const input = payload<{ input_index: bigint; asset_index: bigint }>(
    reason,
    "InputAssetAccumulationLimit",
  );
  if (input !== null)
    return {
      kind: "input",
      inputIndex: input.input_index,
      assetIndex: input.asset_index,
    };
  const output = payload<{ output_index: bigint; asset_index: bigint }>(
    reason,
    "OutputAssetAccumulationLimit",
  );
  if (output !== null)
    return {
      kind: "output",
      outputIndex: output.output_index,
      assetIndex: output.asset_index,
    };
  const mint = payload<{ mint_index: bigint }>(
    reason,
    "MintAssetAccumulationLimit",
  );
  return mint === null ? null : { kind: "mint", mintIndex: mint.mint_index };
};

type ValueKey = readonly [number, bigint, bigint];

const valueKeyOfPosition = (position: ValueFaultPosition): ValueKey => {
  switch (position.kind) {
    case "input":
      return [0, index(position.inputIndex), index(position.assetIndex)];
    case "output":
      return [1, index(position.outputIndex), index(position.assetIndex)];
    case "mint":
      return [2, index(position.mintIndex), 0n];
    default:
      return fail("unknown value position");
  }
};

/** Inside one output the minimum-Ada floor (minor -1) precedes its assets. */
const valueKeyOfReason = (reason: RejectionReason): ValueKey => {
  if (reason === "ValueNotPreserved") return [3, 0n, 0n];
  const floor = payload<{ output_index: bigint }>(reason, "OutputBelowMinAda");
  if (floor !== null) return [1, index(floor.output_index), -1n];
  const position = valuePositionOfReason(reason);
  return position === null
    ? fail("not a value reason")
    : valueKeyOfPosition(position);
};

/** `reason` must be a ValueAndMint (phase 12) reason. */
export const valueReasonPrecedesOrEquals = (
  reason: RejectionReason,
  observed: ValueFaultPosition,
): boolean => {
  const [ls, lm, ln] = valueKeyOfReason(reason);
  const [rs, rm, rn] = valueKeyOfPosition(observed);
  if (ls !== rs) return ls < rs;
  if (lm !== rm) return lm < rm;
  return ln <= rn;
};

const recorded = <T>(position: T | null): T =>
  position === null ? fail("reason has no position in its phase") : position;

/**
 * Whether `reason` is at or before `observed`. A ScriptSources reason other
 * than an unused audit belongs to an earlier stage than either audit.
 */
export const reasonPrecedesOrEqualsLaterFault = (
  reason: RejectionReason,
  context: NetworkOrderContext,
  observed: LaterFaultPosition,
): boolean => {
  const phase = rejectionReasonPhase(reason, context);
  const observedPhase = laterFaultPhase(observed);
  if (phase !== observedPhase) return phase < observedPhase;
  switch (observed.phase) {
    case "unusedAudit": {
      const audit = unusedAuditPositionOfReason(reason);
      return audit === null
        ? true
        : unusedAuditPositionPrecedesOrEquals(audit, observed.position);
    }
    case "execution":
      return executionPositionPrecedesOrEquals(
        recorded(executionPositionOfReason(reason)),
        observed.position,
      );
    case "scriptIntegrity":
      return true;
    case "cek":
      return cekPositionPrecedesOrEquals(
        recorded(cekPositionOfReason(reason)),
        observed.position,
      );
    case "value":
      return valueReasonPrecedesOrEquals(reason, observed.position);
    default:
      return fail("unknown later-fault phase");
  }
};

/**
 * Twin of `leaf_admits_later_fault_v1`. Terminal callers negate it: an
 * accepted leaf admits nothing, and a malformed position on either side
 * throws.
 */
export const leafAdmitsLaterFault = (
  subject: VerdictSubject,
  context: NetworkOrderContext,
  observed: LaterFaultPosition,
): boolean => {
  if (!verdictSubjectIsCanonical(subject)) fail("noncanonical subject");
  if (typeof context?.body_network_mismatch !== "boolean")
    fail("missing or malformed network context");
  const reason = subject.rejection_reason;
  if (reason === null) {
    laterFaultPhase(observed);
    return false;
  }
  return reasonPrecedesOrEqualsLaterFault(reason, context, observed);
};
