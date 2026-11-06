/** The evidence each phase semantic resolver takes beside the transition,
 * as the phase planner (one-step-phase.ts) reads it from a step: the
 * auxiliary witness it carries, with the inline limit of a field preimage
 * it carries, or the evidence derived from the step's ledger-output proof
 * control. */
import { requiredObserverAt } from "./one-step-carriage.js";
import { bytes, constr, integer } from "./one-step-data.js";
import {
  MEMBERSHIP_FINALIZE_YIELDS,
  MEMBERSHIP_STEP_YIELDS,
  membershipFinalizeEvidence,
  membershipStepEvidence,
  scriptSourcesOutputProofFinalizeEvidence,
  scriptSourcesOutputProofStepEvidence,
} from "./one-step-ledger-output-proof.js";
import type { ValidationTracePhaseHolder } from "./one-step-phase.js";
import {
  PHASE_A_NATIVE_ITEM_YIELDS,
  SCRIPT_SOURCES_OBSERVER_YIELDS,
  type ValidationTracePhaseYield,
} from "./one-step-phase-yields.js";

/** An inline limit as measured at one step: the longest field preimage
 * its resolution carries inline there, and the bytes its moves carry beside
 * the field there: its transition and auxiliary evidence (the field read by
 * reference), and the indices of the states its thread stands between
 * (`stepIndexBytes`). A move carries the field beside them, so a step whose
 * moves carry more beside the field (its control's peak lists grow with the
 * transaction's other fields, its indices with the trace) carries that much
 * less of it inline. */
export type ValidationTraceInlineLimitAt = Readonly<{
  maxPreimageBytes: number;
  besideBytes: number;
}>;

/** The longest field preimage a step whose transition and auxiliary
 * evidence carry `besideBytes` beside the field carries inline under
 * `limit`: a byte less for each byte more than where it was measured, and
 * never more than the limit. */
export const inlineMaxPreimageBytesBeside = (
  { maxPreimageBytes, besideBytes: measured }: ValidationTraceInlineLimitAt,
  besideBytes: number,
): number => maxPreimageBytes - Math.max(0, besideBytes - measured);

/** The bytes a Data integer of `value` takes. */
const integerBytes = (value: number) =>
  value < 24
    ? 1
    : value < 256
      ? 2
      : value < 65_536
        ? 3
        : value < 2 ** 32
          ? 5
          : 9;

/** The bytes the thread a move of the step from state `stateIndex` carries
 * takes to name the states it stands between. */
export const stepIndexBytes = (stateIndex: number): number =>
  integerBytes(stateIndex) + integerBytes(stateIndex + 1);

/** The limit `maxPreimageBytes` measured at the step from state
 * `stateIndex`, whose transition and auxiliary evidence carried
 * `evidenceBytes` beside the field. */
export const measuredAt = (
  maxPreimageBytes: number,
  evidenceBytes: number,
  stateIndex: number,
): ValidationTraceInlineLimitAt =>
  Object.freeze({
    maxPreimageBytes,
    besideBytes: evidenceBytes + stepIndexBytes(stateIndex),
  });

/** The longest field preimage whose `Inline` carriage an input-sets item
 * step's resolution carries: the phase resolver's move and the semantic
 * resolver's each carry it whole, and the longer of the two lands within
 * the transaction size limit less its 512-byte reserve even when our wallet
 * pays it
 * (tests/validation-trace-dispute-one-step-input-sets-inline-cap.test.ts).
 * An input-sets field is a whole number of 40-byte out-refs, and this is
 * the longest such field that fits. */
export const VALIDATION_TRACE_INPUT_SETS_INLINE_MAX_PREIMAGE_BYTES = 13_643;
/** That limit, with what its step carried beside the field. */
export const VALIDATION_TRACE_INPUT_SETS_INLINE_LIMIT = measuredAt(
  VALIDATION_TRACE_INPUT_SETS_INLINE_MAX_PREIMAGE_BYTES,
  855,
  352,
);

/** The same for a signatures step over an address-witness item, a field
 * of 103-byte witnesses
 * (the validation-trace dispute one-step signatures-address
 * inline-cap test). */
export const VALIDATION_TRACE_SIGNATURES_ADDRESS_INLINE_MAX_PREIMAGE_BYTES = 13_701;
/** That limit, with what its step carried beside the field. */
export const VALIDATION_TRACE_SIGNATURES_ADDRESS_INLINE_LIMIT = measuredAt(
  VALIDATION_TRACE_SIGNATURES_ADDRESS_INLINE_MAX_PREIMAGE_BYTES,
  814,
  144,
);

/** The same for a signatures step over a required-signer item, a field of
 * 30-byte key hashes, which may repeat one signer
 * (the validation-trace dispute one-step signatures-required
 * inline-cap test). */
export const VALIDATION_TRACE_SIGNATURES_REQUIRED_INLINE_MAX_PREIMAGE_BYTES = 13_623;
/** That limit, with what its step carried beside the field. */
export const VALIDATION_TRACE_SIGNATURES_REQUIRED_INLINE_LIMIT = measuredAt(
  VALIDATION_TRACE_SIGNATURES_REQUIRED_INLINE_MAX_PREIMAGE_BYTES,
  899,
  466,
);

/** The same for a phase-A native scripts step over a witness script, a
 * field of versioned scripts of any length, so measured to the byte
 * (the validation-trace dispute one-step phase-a-native-item
 * inline-cap test). */
export const VALIDATION_TRACE_PHASE_A_NATIVE_ITEM_INLINE_MAX_PREIMAGE_BYTES = 13_659;
/** That limit, with what its step carried beside the field. */
export const VALIDATION_TRACE_PHASE_A_NATIVE_ITEM_INLINE_LIMIT = measuredAt(
  VALIDATION_TRACE_PHASE_A_NATIVE_ITEM_INLINE_MAX_PREIMAGE_BYTES,
  875,
  15,
);

/** The same for a script-sources step over a witness script, also
 * measured to the byte; its moves carry more beside the field than a
 * phase-A native scripts step's do
 * (the validation-trace dispute one-step script-sources-witness-script
 * inline-cap test). */
export const VALIDATION_TRACE_SCRIPT_SOURCES_WITNESS_SCRIPT_INLINE_MAX_PREIMAGE_BYTES = 13_612;
/** That limit, with what its step carried beside the field. */
export const VALIDATION_TRACE_SCRIPT_SOURCES_WITNESS_SCRIPT_INLINE_LIMIT =
  measuredAt(
    VALIDATION_TRACE_SCRIPT_SOURCES_WITNESS_SCRIPT_INLINE_MAX_PREIMAGE_BYTES,
    922,
    31,
  );

/** The same for a script-sources step over the redeemer field, a field of
 * redeemers of any length, so measured to the byte
 * (the validation-trace dispute one-step script-sources-redeemer
 * inline-cap test). */
export const VALIDATION_TRACE_SCRIPT_SOURCES_REDEEMER_INLINE_MAX_PREIMAGE_BYTES = 13_576;
/** That limit, with what its step carried beside the field. */
export const VALIDATION_TRACE_SCRIPT_SOURCES_REDEEMER_INLINE_LIMIT = measuredAt(
  VALIDATION_TRACE_SCRIPT_SOURCES_REDEEMER_INLINE_MAX_PREIMAGE_BYTES,
  959,
  48,
);

/** The same for a phase-A script preconditions step over a required
 * observer, a field of 30-byte script hashes
 * (the validation-trace dispute one-step phase-a-script-preconditions
 * observer inline-cap test). */
export const VALIDATION_TRACE_REQUIRED_OBSERVER_INLINE_MAX_PREIMAGE_BYTES = 13_683;
/** That limit, with what its step carried beside the field. */
export const VALIDATION_TRACE_REQUIRED_OBSERVER_INLINE_LIMIT = measuredAt(
  VALIDATION_TRACE_REQUIRED_OBSERVER_INLINE_MAX_PREIMAGE_BYTES,
  849,
  4_122,
);

/** The same for a script-sources step over a required observer, whose
 * moves carry more beside the field than a phase-A script preconditions
 * step's do
 * (the validation-trace dispute one-step script-sources-observer
 * inline-cap test). */
export const VALIDATION_TRACE_SCRIPT_SOURCES_OBSERVER_INLINE_MAX_PREIMAGE_BYTES = 13_203;
/** That limit, with what its step carried beside the field. */
export const VALIDATION_TRACE_SCRIPT_SOURCES_OBSERVER_INLINE_LIMIT = measuredAt(
  VALIDATION_TRACE_SCRIPT_SOURCES_OBSERVER_INLINE_MAX_PREIMAGE_BYTES,
  1_337,
  11_516,
);

/** The auxiliary evidence a semantic resolver that takes any verifies the
 * transition beside: its constructor among the auxiliary witnesses, the
 * position among its fields of the carriage that delivers the field
 * preimage the item is read from, and the inline limit of the field
 * preimage the step's resolution carries; past it, the step's evidence
 * names our publications of the field instead. */
export type PhaseSemanticEvidence = Readonly<{
  auxiliary: number;
  /** The yields it invokes by the item's kind; the item's index is the
   * evidence's field 1. */
  itemYields?: readonly ValidationTracePhaseYield[];
  /** The fields of the auxiliary witness it fixes itself, by position,
   * at the Data CBOR it fixes each to: it takes the others alone. */
  fixes?: Readonly<Record<number, string>>;
  /** What it claims of the item beside the evidence, as Data CBOR, from
   * the field's preimage and the item's index. */
  claims?: (preimage: string, itemIndex: number) => readonly string[];
  /** The yields it invokes beside the evidence, whose reference-input
   * indices follow it, each a field of its own. */
  yieldFields?: readonly ValidationTracePhaseYield[];
}> &
  (
    | Readonly<{ carriage: number; inlineLimit: ValidationTraceInlineLimitAt }>
    | Readonly<{
        carriage?: undefined;
        inlineLimit?: undefined;
      }>
  );

/** `NoSignerSetProof`, which the native-script token semantic resolvers
 * other than the signature payloads fix in the token witness they verify;
 * and the native language tag, which the stage-nine native match fixes in
 * the source scan witness. */
const NO_SIGNER_SET_PROOF_CBOR = constr(0, []);
const NATIVE_LANGUAGE_TAG_CBOR = integer(0);

/** `NativeScriptTokenWitness { chunk_proof, next_chunk_proof }` beside
 * `NoSignerSetProof`. */
const NATIVE_TOKEN = Object.freeze({
  auxiliary: 3,
  fixes: Object.freeze({ 2: NO_SIGNER_SET_PROOF_CBOR }),
});

/** `NativeScriptTokenWitness { chunk_proof, next_chunk_proof,
 * signer_proof }`. */
const NATIVE_SIGNATURE = Object.freeze({ auxiliary: 3 });

/** The required observer a stage-seven observer step reads, and how many
 * the transaction requires: the item's bytes and the field's item count. */
const observerClaims = (preimage: string, itemIndex: number) => {
  const { hash, count } = requiredObserverAt(
    Buffer.from(preimage, "hex"),
    itemIndex,
  );
  return [bytes(hash), integer(count)];
};

/** The auxiliary evidence each semantic resolver that takes any verifies
 * the transition beside; every other semantic resolver takes
 * `NoAuxiliaryWitness`. */
export const PHASE_SEMANTIC_EVIDENCE: Readonly<
  Partial<Record<ValidationTracePhaseHolder, PhaseSemanticEvidence>>
> = Object.freeze({
  // `TransactionFieldItemWitness { carriage }`: the control fixes the item.
  inputSetsItemSemantic: {
    auxiliary: 28,
    carriage: 0,
    inlineLimit: VALIDATION_TRACE_INPUT_SETS_INLINE_LIMIT,
  },
  signaturesAddressItemSemantic: {
    auxiliary: 28,
    carriage: 0,
    inlineLimit: VALIDATION_TRACE_SIGNATURES_ADDRESS_INLINE_LIMIT,
  },
  // `RequiredSignerItemWitness { carriage, signer_proof }`.
  signaturesRequiredItemSemantic: {
    auxiliary: 2,
    carriage: 0,
    inlineLimit: VALIDATION_TRACE_SIGNATURES_REQUIRED_INLINE_LIMIT,
  },
  phaseANativeScriptsItemSemantic: {
    auxiliary: 1,
    carriage: 2,
    inlineLimit: VALIDATION_TRACE_PHASE_A_NATIVE_ITEM_INLINE_LIMIT,
    itemYields: PHASE_A_NATIVE_ITEM_YIELDS,
  },
  phaseANativeScriptsTokenHeadSemantic: NATIVE_TOKEN,
  phaseANativeScriptsAllOrAnyContainerFrameSemantic: NATIVE_TOKEN,
  phaseANativeScriptsAllOrAnyEmptySemantic: NATIVE_TOKEN,
  phaseANativeScriptsAtLeastContainerFrameSemantic: NATIVE_TOKEN,
  phaseANativeScriptsAtLeastEmptySemantic: NATIVE_TOKEN,
  phaseANativeScriptsTimelockSemantic: NATIVE_TOKEN,
  phaseANativeScriptsSignatureMembershipSemantic: NATIVE_SIGNATURE,
  phaseANativeScriptsSignatureEmptySemantic: NATIVE_SIGNATURE,
  phaseANativeScriptsSignatureBelowFirstSemantic: NATIVE_SIGNATURE,
  phaseANativeScriptsSignatureAboveLastSemantic: NATIVE_SIGNATURE,
  phaseANativeScriptsSignatureBetweenSemantic: NATIVE_SIGNATURE,
  // `NativeScriptFrameWitness { frame }`.
  phaseANativeScriptsFrameSemantic: { auxiliary: 4 },
  // `TransactionFieldChunkWitness`, over a required observer.
  phaseAScriptPreconditionsItemSemantic: {
    auxiliary: 1,
    carriage: 2,
    inlineLimit: VALIDATION_TRACE_REQUIRED_OBSERVER_INLINE_LIMIT,
  },
  // `ScheduledLedgerMembershipWitness`, whose fields carry no field
  // preimage.
  resolveInputsMembershipBeginSemantic: { auxiliary: 5 },
  // `LedgerOutputProofBeginWitness`.
  scriptSourcesOutputProofBeginSemantic: { auxiliary: 29 },
  scriptSourcesStageZeroBeginSemantic: {
    auxiliary: 1,
    carriage: 2,
    inlineLimit: VALIDATION_TRACE_SCRIPT_SOURCES_WITNESS_SCRIPT_INLINE_LIMIT,
  },
  // `ScriptSourceHashBlockWitness { chunk_proof, next_chunk_proof }`.
  scriptSourcesStageZeroHashBlockSemantic: { auxiliary: 32 },
  // `ScriptSourceScanWitness`.
  scriptSourcesStageNineMismatchSemantic: { auxiliary: 9 },
  scriptSourcesStageNineNativeMatchSemantic: {
    auxiliary: 9,
    fixes: Object.freeze({ 3: NATIVE_LANGUAGE_TAG_CBOR }),
  },
  scriptSourcesStageNineEffectfulMatchSemantic: { auxiliary: 9 },
  scriptSourcesStageElevenSourceSemantic: { auxiliary: 9 },
  // `TransactionRedeemerItemBeginWitness { carriage }`.
  scriptSourcesStageOneRedeemerSemantic: {
    auxiliary: 27,
    carriage: 0,
    inlineLimit: VALIDATION_TRACE_SCRIPT_SOURCES_REDEEMER_INLINE_LIMIT,
  },
  // `ScriptPurposeScanWitness`.
  scriptSourcesStageEightPurposeSemantic: { auxiliary: 8 },
  scriptSourcesStageSevenReceiveSemantic: { auxiliary: 8 },
  // `TransactionFieldChunkWitness`, over a required observer, beside the
  // observer it reads and how many the transaction requires, which the
  // observer yields verify.
  scriptSourcesStageSevenObserverSemantic: {
    auxiliary: 1,
    carriage: 2,
    inlineLimit: VALIDATION_TRACE_SCRIPT_SOURCES_OBSERVER_INLINE_LIMIT,
    claims: observerClaims,
    yieldFields: SCRIPT_SOURCES_OBSERVER_YIELDS,
  },
});

/** The semantic resolvers whose evidence is derived from the step's
 * ledger-output proof control, with every yield each may invoke. */
export const LEDGER_OUTPUT_PROOF_SEMANTICS: Readonly<
  Partial<
    Record<
      ValidationTracePhaseHolder,
      Readonly<{
        evidence: typeof membershipStepEvidence;
        yields: readonly ValidationTracePhaseYield[];
      }>
    >
  >
> = Object.freeze({
  resolveInputsMembershipStepSemantic: {
    evidence: membershipStepEvidence,
    yields: MEMBERSHIP_STEP_YIELDS,
  },
  resolveInputsMembershipFinalizeSemantic: {
    evidence: membershipFinalizeEvidence,
    yields: MEMBERSHIP_FINALIZE_YIELDS,
  },
  scriptSourcesOutputProofStepSemantic: {
    evidence: scriptSourcesOutputProofStepEvidence,
    yields: MEMBERSHIP_STEP_YIELDS,
  },
  scriptSourcesOutputProofFinalizeSemantic: {
    evidence: scriptSourcesOutputProofFinalizeEvidence,
    yields: MEMBERSHIP_FINALIZE_YIELDS,
  },
});
