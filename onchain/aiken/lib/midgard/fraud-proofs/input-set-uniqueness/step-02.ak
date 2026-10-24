use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId}
use midgard/native_tx_field_access_v1.{FieldCarriageV1}

/// The thread carries the disputed transaction's **id**. Step-02 re-opens the
/// input-set field(s) through the §8.8 door against that anchor and concludes
/// on byte equality of two door-authenticated §5.3 items, so neither a field
/// commitment nor any block root needs to be threaded forward.
pub type State {
  bad_tx_id: MidgardTxId,
}

pub type Datum =
  ct.StepDatum<State>

/// One constructor per sub-variant of the intra-transaction input-set rule
/// (§2.5 fields 0 and 1 share the §5.3 out-ref item encoding, so item-payload
/// byte equality *is* out-ref equality in every arm).
///
/// Constructor order is wire format: `DuplicateSpendInputs` is Constr 0,
/// `DuplicateReferenceInputs` Constr 1, `SpendReferenceOverlap` Constr 2.
///
/// Indices name **items**, never byte offsets: the door derives each item's
/// position arithmetically from §5.3's fixed stride and refuses anything out
/// of `0 <= index < count`, so a prover chooses which items to compare and
/// nothing about where they live.
pub type Args {
  /// Field 0 names one out-ref twice: `first_index < second_index` and the
  /// two items are byte-equal.
  DuplicateSpendInputs {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
    first_index: Int,
    second_index: Int,
    spend_inputs_opening: FieldOpeningV1,
  }
  /// Field 1 names one out-ref twice, same shape.
  DuplicateReferenceInputs {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
    first_index: Int,
    second_index: Int,
    reference_inputs_opening: FieldOpeningV1,
  }
  /// Fields 0 and 1 both name one out-ref. Two fields of one transaction, so
  /// the anchor is paid once: the compact bytes ride here and each field
  /// names its own §8 carriage (`anchored_native_tx` + `anchored_field_view`,
  /// the multi-field door route).
  SpendReferenceOverlap {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
    spend_index: Int,
    reference_index: Int,
    native_tx_compact_cbor: ByteArray,
    spend_inputs_carriage: FieldCarriageV1,
    reference_inputs_carriage: FieldCarriageV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
