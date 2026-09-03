/**
 * Input-set-uniqueness fault-proof family (pre-registration): schema twins of
 * `midgard/fraud_proofs/input_set_uniqueness/step_01` / `step_02`.
 *
 * The family convicts an operator-ACCEPTED committed transaction whose
 * intra-transaction input sets violate uniqueness/disjointness — the
 * single-party conversion of the validation machine's InputSets rule
 * (`reject_duplicate_input`). Step-01 binds the leaf through the counted
 * `transactions_root` and refuses any leaf whose embedded validity scalar is
 * not `TxIsValid`; step-02 opens §2.5 fields 0/1 through the §8.8 door against
 * the carried anchor and concludes on byte equality of two door-authenticated
 * §5.3 items.
 *
 * Deliberately absent from `catalogue.ts` / the category order: like
 * native-script-decoding, this family predates its catalogue registration.
 */

import { Data } from "@lucid-evolution/lucid";

import { H32Schema, OutputReferenceSchema } from "../common.js";
import { ForcedInclusionTxSchema, HeaderSchema } from "../ledger-state.js";
import { FieldCarriageSchema } from "../native-tx-field-access-v1.js";
import { RejectionReasonSchema } from "../rejection-reason-v1.js";
import { rootMembershipProofSchema } from "../transition-trace.js";
import { FieldOpeningSchema } from "./field-opening-v1.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";

export const InputSetUniquenessStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type InputSetUniquenessStep01Datum = Data.Static<
  typeof InputSetUniquenessStep01DatumSchema
>;
export const InputSetUniquenessStep01Datum =
  InputSetUniquenessStep01DatumSchema as unknown as InputSetUniquenessStep01Datum;

export const InputSetUniquenessStep01SourceSchema = Data.Enum([
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
        ForcedInclusionTxSchema,
      ),
    }),
  }),
]);
export const InputSetUniquenessStep01ArgsSchema = Data.Object({
  source: InputSetUniquenessStep01SourceSchema,
});
export const InputSetUniquenessStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(InputSetUniquenessStep01ArgsSchema);
export type InputSetUniquenessStep01SpendRedeemer = Data.Static<
  typeof InputSetUniquenessStep01SpendRedeemerSchema
>;
export const InputSetUniquenessStep01SpendRedeemer =
  InputSetUniquenessStep01SpendRedeemerSchema as unknown as InputSetUniquenessStep01SpendRedeemer;

/**
 * Mirrors `midgard/fraud_proofs/input_set_uniqueness/step_02.State`: the
 * thread carries the disputed transaction's §2.5 anchor and nothing else.
 */
export const InputSetUniquenessStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
});
export type InputSetUniquenessStep02State = Data.Static<
  typeof InputSetUniquenessStep02StateSchema
>;
export const InputSetUniquenessStep02State =
  InputSetUniquenessStep02StateSchema as unknown as InputSetUniquenessStep02State;

export const InputSetUniquenessStep02DatumSchema = faultProofStepDatumSchema(
  InputSetUniquenessStep02StateSchema,
);
export type InputSetUniquenessStep02Datum = Data.Static<
  typeof InputSetUniquenessStep02DatumSchema
>;
export const InputSetUniquenessStep02Datum =
  InputSetUniquenessStep02DatumSchema as unknown as InputSetUniquenessStep02Datum;

/**
 * Mirrors `midgard/fraud_proofs/input_set_uniqueness/step_02.Args`.
 * Constructor order is wire format: `DuplicateSpendInputs` 0,
 * `DuplicateReferenceInputs` 1, `SpendReferenceOverlap` 2.
 *
 * All indices name §5.3 **items**, never byte offsets: the door derives item
 * positions arithmetically from the fixed stride and refuses anything outside
 * `0 <= index < count`.
 */
export const InputSetUniquenessStep02ArgsSchema = Data.Enum([
  Data.Object({
    DuplicateSpendInputs: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
      first_index: Data.Integer(),
      second_index: Data.Integer(),
      spend_inputs_opening: FieldOpeningSchema,
    }),
  }),
  Data.Object({
    DuplicateReferenceInputs: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
      first_index: Data.Integer(),
      second_index: Data.Integer(),
      reference_inputs_opening: FieldOpeningSchema,
    }),
  }),
  Data.Object({
    SpendReferenceOverlap: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
      spend_index: Data.Integer(),
      reference_index: Data.Integer(),
      native_tx_compact_cbor: Data.Bytes(),
      spend_inputs_carriage: FieldCarriageSchema,
      reference_inputs_carriage: FieldCarriageSchema,
    }),
  }),
]);
export type InputSetUniquenessStep02Args = Data.Static<
  typeof InputSetUniquenessStep02ArgsSchema
>;
export const InputSetUniquenessStep02Args =
  InputSetUniquenessStep02ArgsSchema as unknown as InputSetUniquenessStep02Args;

export const InputSetUniquenessStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(InputSetUniquenessStep02ArgsSchema);
export type InputSetUniquenessStep02SpendRedeemer = Data.Static<
  typeof InputSetUniquenessStep02SpendRedeemerSchema
>;
export const InputSetUniquenessStep02SpendRedeemer =
  InputSetUniquenessStep02SpendRedeemerSchema as unknown as InputSetUniquenessStep02SpendRedeemer;

export const InputSetUniquenessVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: H32Schema,
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});

export const InputSetUniquenessBoundDuplicateInputSchema = Data.Object({
  subject: InputSetUniquenessVerdictSubjectSchema,
  first_field_index: Data.Integer(),
  first_item_index: Data.Integer(),
  second_field_index: Data.Integer(),
  second_item_index: Data.Integer(),
});
export type InputSetUniquenessBoundDuplicateInput = Data.Static<
  typeof InputSetUniquenessBoundDuplicateInputSchema
>;

export const InputSetUniquenessStep03StateSchema = Data.Object({
  bound: InputSetUniquenessBoundDuplicateInputSchema,
});
export type InputSetUniquenessStep03State = Data.Static<
  typeof InputSetUniquenessStep03StateSchema
>;
export const InputSetUniquenessStep03DatumSchema = faultProofStepDatumSchema(
  InputSetUniquenessStep03StateSchema,
);
export const InputSetUniquenessStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  native_tx_compact_cbor: Data.Bytes(),
  spend_inputs_carriage: FieldCarriageSchema,
  reference_inputs_carriage: FieldCarriageSchema,
});
export const InputSetUniquenessStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(InputSetUniquenessStep03ArgsSchema);
export type InputSetUniquenessStep03SpendRedeemer = Data.Static<
  typeof InputSetUniquenessStep03SpendRedeemerSchema
>;

export const InputSetUniquenessUniqueScanStateSchema = Data.Object({
  bound: InputSetUniquenessBoundDuplicateInputSchema,
  spend_count: Data.Integer(),
  reference_count: Data.Integer(),
  cursor: Data.Integer(),
  previous_item: Data.Bytes(),
  next_expected_script_hash: Data.Bytes({ minLength: 28, maxLength: 28 }),
  checkpoint_hash: H32Schema,
});
export type InputSetUniquenessUniqueScanState = Data.Static<
  typeof InputSetUniquenessUniqueScanStateSchema
>;
export const InputSetUniquenessStep04DatumSchema = faultProofStepDatumSchema(
  InputSetUniquenessUniqueScanStateSchema,
);
export const InputSetUniquenessStep04ArgsSchema = Data.Enum([
  Data.Object({
    Advance: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      field_opening: FieldOpeningSchema,
    }),
  }),
  Data.Object({
    Finalize: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
    }),
  }),
]);
export const InputSetUniquenessStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(InputSetUniquenessStep04ArgsSchema);
export type InputSetUniquenessStep04SpendRedeemer = Data.Static<
  typeof InputSetUniquenessStep04SpendRedeemerSchema
>;
