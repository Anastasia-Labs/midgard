use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/ledger_state.{MidgardTxId}

/// One state, one route.
///
/// This step used to carry a two-constructor state and a four-constructor
/// redeemer: a direct arm, a tier-2 "complete publication" arm, and a
/// `FoldStart`/`FoldNext` pair that streamed the spend-input collection one
/// counted `bounded_collection_v1.verify_item` opening at a time. All of that
/// existed for one reason — the whole collection had to be reproduced in the
/// step to re-hash it, and at real cardinalities it did not fit (Q1X-F6,
/// #551). Under the flat scheme the door hashes the preimage once and reads
/// item `n` by arithmetic, so there is nothing left to stream: the fold arms,
/// the self-loop state, and the bespoke `PublishedSpendInputsV1` publication
/// type are gone, and tier-2 carriage is now the door's `RawUtxo` arm rather
/// than a type this family maintains.
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
