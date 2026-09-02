/**
 * `reference-input-no-idx` fault-proof family (Goal task `Q31`) — the
 * reference-input mirror of `input-no-idx`.
 *
 * **Rule.** Every *reference* input of a committed transaction must name an
 * output that its producing transaction actually created: for a reference input
 * `(tx_id, output_index)` whose producer `tx_id` is itself committed in the same
 * block, `output_index` must be strictly less than the number of outputs that
 * producer commits.
 *
 * **Violation.** A committed transaction reads
 * `(producing_tx_id, output_index)` where `producing_tx_id` *is* committed in
 * the same block — so the preimage of the transaction id exists — yet
 * `output_index >= |producer.outputs|`. `no-reference-input` cannot convict it:
 * that family proves exclusion from the previous block's ledger, which says
 * nothing about an output index of a transaction produced inside this block.
 *
 * The proof is a four-step computation thread:
 *
 * 1. bind the bad transaction to the block's counted `transactions_root` and
 *    forward its canonical `reference_inputs_hash`;
 * 2. open that commitment with the complete reference-inputs preimage and
 *    forward the challenged `(tx_id, output_index)`;
 * 3. bind the producing transaction to the *same* block and forward its
 *    canonical `outputs_hash` alongside the challenged index; and
 * 4. open the outputs commitment with the complete outputs preimage and require
 *    `output_index >= |outputs|`.
 *
 * Steps 01 and 02 genuinely differ from `input-no-idx`: step 01 commits the
 * reference-inputs hash, and step 02 opens consensus field 1 with a flat `Args`
 * rather than the spend side's Complete/Published/Fold enum. Steps 03 and 04
 * differ only in record field *names*, which PlutusData erases, so they compile
 * to the same UPLC and the two chains share those two applied scripts — exactly
 * like the `no_input`/`no_reference_input` pair. The threads stay
 * distinguishable because the computation-thread token asset name binds each
 * thread to its own category and block.
 *
 * This module is the strict TypeScript twin of
 * `onchain/aiken/lib/midgard/fraud-proofs/reference-input-no-idx/step-0{1..4}.ak`.
 * Field order in every `Data.Object` mirrors the aiken record declarations 1:1 —
 * the PlutusData encoding is positional, so re-ordering here would silently
 * produce redeemers the validators reject.
 */
import { midgardFieldCommitmentFromItemsV1 } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "../common.js";
import { FieldOpeningV1Schema } from "./field-opening-v1.js";
import {
  encodeMidgardTxInputCanonicalV1,
  encodeMidgardTxOutputCanonicalV1,
  type MidgardTxOutput,
} from "./input-no-idx.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  type MidgardTxInput as MidgardTxInputData,
  NativeTxInclusionCarriage,
  NativeTxInclusionCarriageSchema,
} from "./native.js";

/** Catalogue violation identifier adjudicated by this family. */
export const REFERENCE_INPUT_NO_IDX_VIOLATION_ID_V1 =
  "reference-input-no-idx" as const;

/** Canonical reference-inputs field index of a native V1 transaction body. */
export const REFERENCE_INPUT_NO_IDX_REFERENCE_INPUTS_FIELD_INDEX_V1 = 1;

/** Canonical outputs field index of a native V1 transaction body. */
export const REFERENCE_INPUT_NO_IDX_OUTPUTS_FIELD_INDEX_V1 = 2;

// ## Rule

/**
 * The adjudicated violation predicate, over evidence that has already been
 * authenticated against the block header. This is exactly
 * `bad_reference_input_output_index >= list.length(outputs_preimage)` in
 * `validators/fraud-proofs/reference-input-no-idx/step-04.ak`.
 */
export const isReferenceInputNoIdxViolationV1 = ({
  badReferenceInputOutputIndex,
  producingTxOutputCount,
}: {
  readonly badReferenceInputOutputIndex: bigint;
  readonly producingTxOutputCount: number;
}): boolean => badReferenceInputOutputIndex >= BigInt(producingTxOutputCount);

/**
 * The `reference_inputs_hash` a native transaction body commits: §4's flat
 * `blake2b_256` over the §5.1 preimage the items assemble into.
 *
 * Reference inputs and spend inputs share the per-item encoder, and under §4 they
 * also share the commitment — the field index is not in the hash input, so an
 * identical spend-inputs list commits to exactly this value. The retired counted
 * scheme salted each item with its field index and so separated them. What keeps
 * the two apart now is positional: the caller reads
 * `body.reference_inputs_hash` out of the committed compact structure.
 */
export const referenceInputNoIdxReferenceInputsCommitmentV1 = (
  inputs: readonly MidgardTxInputData[],
): string =>
  midgardFieldCommitmentFromItemsV1(
    inputs.map(encodeMidgardTxInputCanonicalV1),
  ).toString("hex");

/**
 * The `outputs_hash` a native transaction body commits for `outputs`: §4's flat
 * `blake2b_256` over the §5.1 preimage the items assemble into.
 */
export const referenceInputNoIdxOutputsCommitmentV1 = (
  outputs: readonly MidgardTxOutput[],
): string =>
  midgardFieldCommitmentFromItemsV1(
    outputs.map(encodeMidgardTxOutputCanonicalV1),
  ).toString("hex");

// ## Shared step aliases

export const ReferenceInputNoIdxTxInclusionArgsSchema =
  NativeTxInclusionCarriageSchema;
export type ReferenceInputNoIdxTxInclusionArgs = NativeTxInclusionCarriage;
export const ReferenceInputNoIdxTxInclusionArgs =
  NativeTxInclusionCarriage as unknown as ReferenceInputNoIdxTxInclusionArgs;

export const ReferenceInputNoIdxStepCancelSchema = FaultProofStepCancelSchema;
export type ReferenceInputNoIdxStepCancel = FaultProofStepCancel;
export const ReferenceInputNoIdxStepCancel =
  FaultProofStepCancel as unknown as ReferenceInputNoIdxStepCancel;

// ## Step 01 — verify inclusion of the bad transaction
//
// The step-01 UTxO is the initialized fraud proof (its `data` is `None`), so it
// is read with the generic computation-thread step datum. Spending it requires
// only the native-tx inclusion redeemer; the produced UTxO carries step-02.

export const ReferenceInputNoIdxStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type ReferenceInputNoIdxStep01Datum = Data.Static<
  typeof ReferenceInputNoIdxStep01DatumSchema
>;
export const ReferenceInputNoIdxStep01Datum =
  ReferenceInputNoIdxStep01DatumSchema as unknown as ReferenceInputNoIdxStep01Datum;

export const ReferenceInputNoIdxStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ReferenceInputNoIdxTxInclusionArgsSchema);
export type ReferenceInputNoIdxStep01SpendRedeemer = Data.Static<
  typeof ReferenceInputNoIdxStep01SpendRedeemerSchema
>;
export const ReferenceInputNoIdxStep01SpendRedeemer =
  ReferenceInputNoIdxStep01SpendRedeemerSchema as unknown as ReferenceInputNoIdxStep01SpendRedeemer;

// ## Step 02 — open the reference-inputs preimage and select the bad input

/**
 * Mirrors `midgard/fraud_proofs/reference_input_no_idx/step_02.State`.
 *
 * The thread carries the authenticated transaction id required to open the
 * reference-input field through the shared field-opening door. See
 * `docs/fault-proofs/offchain-builder-staleness-575.md` §2.
 */
export const ReferenceInputNoIdxStep02StateSchema = Data.Object({
  verified_tx_id: H32Schema,
});
export type ReferenceInputNoIdxStep02State = Data.Static<
  typeof ReferenceInputNoIdxStep02StateSchema
>;
export const ReferenceInputNoIdxStep02State =
  ReferenceInputNoIdxStep02StateSchema as unknown as ReferenceInputNoIdxStep02State;

export const ReferenceInputNoIdxStep02DatumSchema = faultProofStepDatumSchema(
  ReferenceInputNoIdxStep02StateSchema,
);
export type ReferenceInputNoIdxStep02Datum = Data.Static<
  typeof ReferenceInputNoIdxStep02DatumSchema
>;
export const ReferenceInputNoIdxStep02Datum =
  ReferenceInputNoIdxStep02DatumSchema as unknown as ReferenceInputNoIdxStep02Datum;

/** Mirrors `midgard/fraud_proofs/reference_input_no_idx/step_02.Args`. */
export const ReferenceInputNoIdxStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  reference_inputs_opening: FieldOpeningV1Schema,
  bad_reference_input_index: Data.Integer(),
});
export type ReferenceInputNoIdxStep02Args = Data.Static<
  typeof ReferenceInputNoIdxStep02ArgsSchema
>;
export const ReferenceInputNoIdxStep02Args =
  ReferenceInputNoIdxStep02ArgsSchema as unknown as ReferenceInputNoIdxStep02Args;

export const ReferenceInputNoIdxStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ReferenceInputNoIdxStep02ArgsSchema);
export type ReferenceInputNoIdxStep02SpendRedeemer = Data.Static<
  typeof ReferenceInputNoIdxStep02SpendRedeemerSchema
>;
export const ReferenceInputNoIdxStep02SpendRedeemer =
  ReferenceInputNoIdxStep02SpendRedeemerSchema as unknown as ReferenceInputNoIdxStep02SpendRedeemer;

// ## Step 03 — verify inclusion of the producing transaction
//
// The producing transaction is the one whose id equals the bad reference input's
// `tx_id`. It is bound to the same block via the native inclusion redeemer, and
// its native `outputs_hash` is threaded to step-04.

export const ReferenceInputNoIdxStep03StateSchema = Data.Object({
  bad_reference_input_tx_id: H32Schema,
  bad_reference_input_output_index: Data.Integer(),
});
export type ReferenceInputNoIdxStep03State = Data.Static<
  typeof ReferenceInputNoIdxStep03StateSchema
>;
export const ReferenceInputNoIdxStep03State =
  ReferenceInputNoIdxStep03StateSchema as unknown as ReferenceInputNoIdxStep03State;

export const ReferenceInputNoIdxStep03DatumSchema = faultProofStepDatumSchema(
  ReferenceInputNoIdxStep03StateSchema,
);
export type ReferenceInputNoIdxStep03Datum = Data.Static<
  typeof ReferenceInputNoIdxStep03DatumSchema
>;
export const ReferenceInputNoIdxStep03Datum =
  ReferenceInputNoIdxStep03DatumSchema as unknown as ReferenceInputNoIdxStep03Datum;

export const ReferenceInputNoIdxStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ReferenceInputNoIdxTxInclusionArgsSchema);
export type ReferenceInputNoIdxStep03SpendRedeemer = Data.Static<
  typeof ReferenceInputNoIdxStep03SpendRedeemerSchema
>;
export const ReferenceInputNoIdxStep03SpendRedeemer =
  ReferenceInputNoIdxStep03SpendRedeemerSchema as unknown as ReferenceInputNoIdxStep03SpendRedeemer;

// ## Step 04 — verify the producing tx outputs and the out-of-range index
//
// `outputs_preimage` is the producing transaction's complete outputs list as
// structured `MidgardTxOutput` PlutusData. On-chain it is re-encoded with
// `encode_midgard_tx_output` and re-committed with
// `bounded_collection_v1.from_items(2, ...)`; matching the committed
// `outputs_hash` therefore also fixes the exact output count, which is what
// makes the out-of-range check sound.

/** Mirrors `midgard/fraud_proofs/reference_input_no_idx/step_04.State`. */
export const ReferenceInputNoIdxStep04StateSchema = Data.Object({
  producing_tx_id: H32Schema,
  bad_reference_input_output_index: Data.Integer(),
});
export type ReferenceInputNoIdxStep04State = Data.Static<
  typeof ReferenceInputNoIdxStep04StateSchema
>;
export const ReferenceInputNoIdxStep04State =
  ReferenceInputNoIdxStep04StateSchema as unknown as ReferenceInputNoIdxStep04State;

export const ReferenceInputNoIdxStep04DatumSchema = faultProofStepDatumSchema(
  ReferenceInputNoIdxStep04StateSchema,
);
export type ReferenceInputNoIdxStep04Datum = Data.Static<
  typeof ReferenceInputNoIdxStep04DatumSchema
>;
export const ReferenceInputNoIdxStep04Datum =
  ReferenceInputNoIdxStep04DatumSchema as unknown as ReferenceInputNoIdxStep04Datum;

/** Mirrors `midgard/fraud_proofs/reference_input_no_idx/step_04.Args`. */
export const ReferenceInputNoIdxStep04ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  outputs_opening: FieldOpeningV1Schema,
});
export type ReferenceInputNoIdxStep04Args = Data.Static<
  typeof ReferenceInputNoIdxStep04ArgsSchema
>;
export const ReferenceInputNoIdxStep04Args =
  ReferenceInputNoIdxStep04ArgsSchema as unknown as ReferenceInputNoIdxStep04Args;

export const ReferenceInputNoIdxStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(ReferenceInputNoIdxStep04ArgsSchema);
export type ReferenceInputNoIdxStep04SpendRedeemer = Data.Static<
  typeof ReferenceInputNoIdxStep04SpendRedeemerSchema
>;
export const ReferenceInputNoIdxStep04SpendRedeemer =
  ReferenceInputNoIdxStep04SpendRedeemerSchema as unknown as ReferenceInputNoIdxStep04SpendRedeemer;

// ## Step-state builders (twins of the on-chain forwarding rules)

/** Exactly the state `step-01` writes for `step-02`. */
export const referenceInputNoIdxStep02StateFromBadTxV1 = (
  badTxId: string,
): ReferenceInputNoIdxStep02State => ({
  verified_tx_id: badTxId.toLowerCase(),
});

/** Exactly the state `step-02` writes for `step-03`. */
export const referenceInputNoIdxStep03StateFromBadInputV1 = (
  badReferenceInput: MidgardTxInputData,
): ReferenceInputNoIdxStep03State => ({
  bad_reference_input_tx_id: badReferenceInput.tx_id.toLowerCase(),
  bad_reference_input_output_index: badReferenceInput.output_index,
});

/** Exactly the state `step-03` writes for `step-04`. */
export const referenceInputNoIdxStep04StateFromBadInputV1 = ({
  badReferenceInput,
  producingTxId,
}: {
  readonly badReferenceInput: MidgardTxInputData;
  readonly producingTxId: string;
}): ReferenceInputNoIdxStep04State => ({
  producing_tx_id: producingTxId.toLowerCase(),
  bad_reference_input_output_index: badReferenceInput.output_index,
});
