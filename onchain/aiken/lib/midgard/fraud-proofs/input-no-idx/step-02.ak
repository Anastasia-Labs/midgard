use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId}

/// One state, one route. The door hashes field 0's preimage once and reads
/// item `n` by arithmetic, so nothing is streamed; tier-2 carriage is the
/// door's `RawUtxo` arm.
pub type State {
  verified_tx_id: MidgardTxId,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  /// The prover's chosen §8 carriage for field 0's preimage.
  spend_inputs_opening: FieldOpeningV1,
  bad_inputs_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
