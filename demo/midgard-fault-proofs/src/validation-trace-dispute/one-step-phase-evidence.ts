/** The evidence each phase semantic resolver takes beside the transition,
 * as the phase planner (one-step-phase.ts) reads it from a step: the
 * auxiliary witness it carries, with the inline limit of a field preimage
 * it carries, or the evidence derived from the step's ledger-output proof
 * control. */
import {
  MEMBERSHIP_FINALIZE_YIELDS,
  MEMBERSHIP_STEP_YIELDS,
  membershipFinalizeEvidence,
  membershipStepEvidence,
} from "./one-step-ledger-output-proof.js";
import type { ValidationTracePhaseHolder } from "./one-step-phase.js";
import {
  PHASE_A_NATIVE_ITEM_YIELDS,
  type ValidationTracePhaseYield,
} from "./one-step-phase-yields.js";

/** The longest field preimage whose `Inline` carriage an input-sets item
 * step's resolution carries: the phase resolver's move and the semantic
 * resolver's each carry it whole, and the longer of the two lands within
 * the transaction size limit less its 512-byte reserve even when our wallet
 * pays it
 * (tests/validation-trace-dispute-one-step-input-sets-inline-cap.test.ts).
 * An input-sets field is a whole number of 40-byte out-refs, and this is
 * the longest such field that fits. */
export const VALIDATION_TRACE_INPUT_SETS_INLINE_MAX_PREIMAGE_BYTES = 13_683;

/** The same for a signatures step over an address-witness item, a field
 * of 103-byte witnesses
 * (tests/validation-trace-dispute-one-step-signatures-address-inline-cap.test.ts). */
export const VALIDATION_TRACE_SIGNATURES_ADDRESS_INLINE_MAX_PREIMAGE_BYTES = 13_701;

/** The same for a signatures step over a required-signer item, a field of
 * 30-byte key hashes, which may repeat one signer
 * (tests/validation-trace-dispute-one-step-signatures-required-inline-cap.test.ts). */
export const VALIDATION_TRACE_SIGNATURES_REQUIRED_INLINE_MAX_PREIMAGE_BYTES = 13_623;

/** The same for a phase-A native scripts step over a witness script, a
 * field of versioned scripts of any length, so measured to the byte
 * (tests/validation-trace-dispute-one-step-phase-a-native-item-inline-cap.test.ts). */
export const VALIDATION_TRACE_PHASE_A_NATIVE_ITEM_INLINE_MAX_PREIMAGE_BYTES = 13_659;

/** The auxiliary evidence each semantic resolver that takes any verifies
 * the transition beside: its constructor among the auxiliary witnesses, the
 * position among its fields of the carriage that delivers the field
 * preimage the item is read from, and the longest preimage the step's
 * resolution carries inline. A step over a longer field is not driven: it
 * waits under the one-step-required alarm. Every other semantic resolver
 * takes `NoAuxiliaryWitness`. */
export const PHASE_SEMANTIC_EVIDENCE: Readonly<
  Partial<
    Record<
      ValidationTracePhaseHolder,
      Readonly<{
        auxiliary: number;
        /** The yields it invokes by the item's kind; the item's index is
         * the evidence's field 1. */
        itemYields?: readonly ValidationTracePhaseYield[];
      }> &
        (
          | Readonly<{ carriage: number; inlineMaxPreimageBytes: number }>
          | Readonly<{
              carriage?: undefined;
              inlineMaxPreimageBytes?: undefined;
            }>
        )
    >
  >
> = Object.freeze({
  // `TransactionFieldChunkWitness { field_index, item_index, carriage }`.
  inputSetsItemSemantic: {
    auxiliary: 1,
    carriage: 2,
    inlineMaxPreimageBytes:
      VALIDATION_TRACE_INPUT_SETS_INLINE_MAX_PREIMAGE_BYTES,
  },
  signaturesAddressItemSemantic: {
    auxiliary: 1,
    carriage: 2,
    inlineMaxPreimageBytes:
      VALIDATION_TRACE_SIGNATURES_ADDRESS_INLINE_MAX_PREIMAGE_BYTES,
  },
  // `RequiredSignerItemWitness { carriage, signer_proof }`.
  signaturesRequiredItemSemantic: {
    auxiliary: 2,
    carriage: 0,
    inlineMaxPreimageBytes:
      VALIDATION_TRACE_SIGNATURES_REQUIRED_INLINE_MAX_PREIMAGE_BYTES,
  },
  phaseANativeScriptsItemSemantic: {
    auxiliary: 1,
    carriage: 2,
    inlineMaxPreimageBytes:
      VALIDATION_TRACE_PHASE_A_NATIVE_ITEM_INLINE_MAX_PREIMAGE_BYTES,
    itemYields: PHASE_A_NATIVE_ITEM_YIELDS,
  },
  // `ScheduledLedgerMembershipWitness`, whose fields carry no field
  // preimage.
  resolveInputsMembershipBeginSemantic: { auxiliary: 5 },
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
});
