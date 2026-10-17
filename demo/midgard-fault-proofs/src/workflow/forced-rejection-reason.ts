import * as SDK from "@al-ft/midgard-sdk";
import {
  type RejectCode,
  RejectCodes,
  type RejectedTx,
  resolveRejectionReason,
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
  RejectCodes.TxExUnits,
] as const);

/**
 * The exact `RejectionReasonV1` a forced leaf records for a Phase A/B
 * rejection: the arm the validation machine ends in for the rule that failed,
 * with every coordinate exactly as that rule recorded it
 * (`resolveRejectionReason` under the `subject` convention).
 *
 * The forced-leaf families reopen those coordinates by field position, so a
 * reason with another arm or other coordinates is disproved outright. Where
 * the machine's terminal reason does not carry an exact ordinal yet
 * (`UNSET_REASON_COORDINATE`), the leaf still names the exact one. A code
 * with coordinate-carrying arms is written only from a subject of that code,
 * and a rejection without one throws {@link ForcedRejectionSubjectMissing}.
 */
export const forcedRejectionReason = (
  rejection: ForcedRejection,
): SDK.RejectionReason => {
  const resolved = resolveRejectionReason(rejection, "subject");
  if (resolved.kind === "unsupported")
    throw new ForcedRejectionUnsupported(rejection);
  if (resolved.kind === "subjectMissing")
    throw new ForcedRejectionSubjectMissing(rejection);
  return resolved.reason;
};

/** The forced leaf's operator verdict for a Phase A/B rejection. */
export const forcedVerdictForRejection = (
  rejection: ForcedRejection,
): SDK.OperatorVerdict => ({
  ForcedTxInvalid: { reason: forcedRejectionReason(rejection) },
});
