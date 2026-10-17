use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId}

/// One state, one route — the reference-input mirror of `input-no-idx` step-02.
///
/// This step used to carry the disputed transaction's field-1 *hash* forward
/// and re-hash a prover-reproduced `List<MidgardTxInput>` against it with
/// `native_tx_field_access_v1.field_commitment_from_items`. Under §4's flat
/// scheme the thread carries the transaction *id* instead (§8.8), and the door
/// authenticates field 1's preimage once and reads item `n` by arithmetic at
/// §5.3's fixed 38-byte stride — the same O(1) access field 0 gets, since
/// reference and spend inputs share that stride (§10.5). The reproduced item
/// list and the free-standing hash it was checked against are both gone.
pub type State {
  verified_tx_id: MidgardTxId,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  /// The prover's chosen §8 carriage for field 1's preimage (the disputed
  /// transaction's reference inputs).
  reference_inputs_opening: FieldOpeningV1,
  bad_reference_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
