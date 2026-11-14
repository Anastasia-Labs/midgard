use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/network_id/step_01.{NetworkIdFaultV1}
use midgard/ledger_state.{MidgardTxId}

/// Everything the final step trusts was written from the authenticated
/// transactions-root leaf by step-01. `expected_network_id` is a parameter of
/// that registered first step, rather than a prover-selected redeemer value.
pub type State {
  bad_tx_id: MidgardTxId,
  committed_tx_network_id: Int,
  expected_network_id: Int,
  fault: NetworkIdFaultV1,
  /// Canonical CBOR of the authenticated forced root key. `None` marks a
  /// transaction bound from the transactions root; `Some` marks a forced
  /// leaf: accepted under `TransactionNetwork`/`OutputNetwork` (its outputs
  /// open under the forced source), rejected under `ForcedNetworkIdMismatch`.
  forced_source_key: Option<ByteArray>,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  /// Present exactly for `OutputNetwork`; opens native-V1 field 2 through the
  /// complete §8.8 reference-input set supplied by the proof transaction.
  outputs_opening: Option<FieldOpeningV1>,
  /// `OutputNetwork` only: `None` reads the output by walking field 2;
  /// `Some(i)` reads it through the field grammar certificate at reference
  /// input `i`, so the step's cost no longer grows with the field's item
  /// count. Every other claim opens no field and carries `None`.
  grammar_ref_input_index: Option<Int>,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
