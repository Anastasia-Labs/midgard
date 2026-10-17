/**
 * Re-derived onto the flat field commitments by #604 (the #575 off-chain builder
 * remediation): thread state carries the §2.5 anchor rather than a per-field
 * collection commitment, and a step redeemer carries a `FieldOpeningV1` rather
 * than a reproduced `..._preimage: List<…>`. The rebind is explained once in
 * `docs/fault-proofs/decisions/0001-reference-input-field-evidence.md`.
 */

import { MIDGARD_EMPTY_FIELD_COMMITMENT } from "@al-ft/midgard-core";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { OutputReferenceSchema } from "../common.js";
import { ForcedInclusionTxV1Schema, HeaderSchema } from "../ledger-state.js";
import { RejectionReasonSchema } from "../rejection-reason.js";
import { rootMembershipProofSchema } from "../transition-trace.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxBodyCompact,
  type NativeTxBodyCompact as NativeTxBodyCompactData,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";

export const ZeroInputStep01DatumSchema = faultProofStepDatumSchema(Data.Any());
export type ZeroInputStep01Datum = Data.Static<
  typeof ZeroInputStep01DatumSchema
>;
export const ZeroInputStep01Datum = asDataType<ZeroInputStep01Datum>(
  ZeroInputStep01DatumSchema,
);

/**
 * Mirrors `midgard/fraud_proofs/zero_input/step_01.SourceV1`: the accepted
 * direction carries the native inclusion; the forced direction carries the
 * forced-inclusion leaf and its polarity.
 */
export const ZeroInputStep01SourceSchema = Data.Enum([
  Data.Object({
    AcceptedSource: Data.Object({ inclusion: NativeTxInclusionCarriageSchema }),
  }),
  Data.Object({
    ForcedSource: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      header: HeaderSchema,
      membership: rootMembershipProofSchema(
        OutputReferenceSchema,
        ForcedInclusionTxV1Schema,
      ),
      direction: Data.Integer(),
    }),
  }),
]);
export type ZeroInputStep01Source = Data.Static<
  typeof ZeroInputStep01SourceSchema
>;

/** Mirrors `midgard/fraud_proofs/zero_input/step_01.Args`. */
export const ZeroInputStep01SpendRedeemerSchema = faultProofStepRedeemerSchema(
  Data.Object({ source: ZeroInputStep01SourceSchema }),
);
export type ZeroInputStep01SpendRedeemer = Data.Static<
  typeof ZeroInputStep01SpendRedeemerSchema
>;
export const ZeroInputStep01SpendRedeemer =
  asDataType<ZeroInputStep01SpendRedeemer>(ZeroInputStep01SpendRedeemerSchema);

export const ZeroInputVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});

/**
 * Mirrors `midgard/fraud_proofs/zero_input/rule.StateV1`. The thread carries
 * the proof-thread substrate's verdict subject, whose `transaction_id` is the
 * §2.5 anchor step-02 opens field 0 against; the retired
 * `bad_tx_spend_inputs_hash` was compared against `empty_field_commitment`
 * directly, and the step now reads the door's authenticated item count instead.
 */
export const ZeroInputStep02StateSchema = Data.Object({
  subject: ZeroInputVerdictSubjectSchema,
});
export type ZeroInputStep02State = Data.Static<
  typeof ZeroInputStep02StateSchema
>;
export const ZeroInputStep02State = asDataType<ZeroInputStep02State>(
  ZeroInputStep02StateSchema,
);

export const ZeroInputStep02DatumSchema = faultProofStepDatumSchema(
  ZeroInputStep02StateSchema,
);
export type ZeroInputStep02Datum = Data.Static<
  typeof ZeroInputStep02DatumSchema
>;
export const ZeroInputStep02Datum = asDataType<ZeroInputStep02Datum>(
  ZeroInputStep02DatumSchema,
);

/**
 * Mirrors `midgard/fraud_proofs/zero_input/step_02.Args`. The opening is new:
 * the step no longer compares a carried hash against the empty commitment, it
 * opens the field. For a genuinely empty field the §5.1 preimage is the single
 * byte `80`, so tier 1 always carries it.
 */
export const ZeroInputStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  spend_inputs_opening: FieldOpeningSchema,
});
export type ZeroInputStep02Args = Data.Static<typeof ZeroInputStep02ArgsSchema>;
export const ZeroInputStep02Args = asDataType<ZeroInputStep02Args>(
  ZeroInputStep02ArgsSchema,
);

export const ZeroInputStep02SpendRedeemerSchema = faultProofStepRedeemerSchema(
  ZeroInputStep02ArgsSchema,
);
export type ZeroInputStep02SpendRedeemer = Data.Static<
  typeof ZeroInputStep02SpendRedeemerSchema
>;
export const ZeroInputStep02SpendRedeemer =
  asDataType<ZeroInputStep02SpendRedeemer>(ZeroInputStep02SpendRedeemerSchema);

export {
  FaultProofStepCancel as ZeroInputStepCancel,
  FaultProofStepCancelSchema as ZeroInputStepCancelSchema,
  NativeTxInclusionArgs as ZeroInputTxInclusionArgs,
  NativeTxInclusionArgsSchema as ZeroInputTxInclusionArgsSchema,
};

/**
 * The `spend_inputs_hash` a native transaction body carries when it spends
 * nothing: `docs/spec/midgard-tx.md` §4's flat commitment of the empty §5.1
 * field, i.e. `blake2b_256(#"80")`.
 *
 * Taken from `midgard-core`'s `MIDGARD_EMPTY_FIELD_COMMITMENT` rather than
 * written out as a literal, so it cannot drift from the hashes the codec,
 * fixtures and builders in this repository actually produce. Its Aiken twin is
 * `native_tx_field_access_v1.empty_field_commitment`, which is what
 * `fraud_proofs/zero_input/step_02` compares against; the cross-language golden
 * vector proves the two equal.
 *
 * §4's plain hashing carries no field index, so this value is the empty
 * commitment of **all nine** fields, not of field 0 alone. Field identity is
 * positional, and the aliasing is safe because the §4 positional-identity
 * invariant makes every consumer take its expected hash from the committed
 * compact structure in view — here, `txBody.spend_inputs_hash`.
 */
export const EMPTY_SPEND_INPUTS_HASH: string =
  MIDGARD_EMPTY_FIELD_COMMITMENT.toString("hex");

/**
 * A transaction violates the "at least one input" ledger rule when its body
 * commits to an empty spend-inputs list.
 */
export const nativeTxBodyHasZeroInputViolation = ({
  txBody,
}: {
  readonly txBody: NativeTxBodyCompactData;
}): boolean => {
  const roundTripped = Data.from(
    Data.to(txBody, NativeTxBodyCompact),
    NativeTxBodyCompact,
  );
  return roundTripped.spend_inputs_hash === EMPTY_SPEND_INPUTS_HASH;
};
