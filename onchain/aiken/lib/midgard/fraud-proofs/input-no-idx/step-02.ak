use aiken/crypto.{VerificationKeyHash}
use cardano/assets.{AssetName, PolicyId}
use midgard/bounded_collection_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/ledger_state.{MidgardInputsHash}

pub type State {
  Direct { verified_tx_inputs_hash: MidgardInputsHash }
  Folding {
    verified_tx_inputs_hash: MidgardInputsHash,
    item_count: Int,
    next_item_index: Int,
    bad_inputs_index: Int,
    selected_input: Option<MidgardTxInput>,
  }
}

pub type Datum =
  ct.StepDatum<State>

/// Tier-2 complete publication. It is bound to one computation thread and
/// prover so an Ada-only public-data UTxO cannot be replayed across disputes.
pub type PublishedSpendInputsV1 {
  version: Int,
  computation_thread_policy_id: PolicyId,
  computation_thread_asset_name: AssetName,
  fraud_prover: VerificationKeyHash,
  verified_tx_inputs_hash: MidgardInputsHash,
  item_count: Int,
  inputs: List<MidgardTxInput>,
}

/// Bounded fallback for a complete public preimage that exceeds the §3.3
/// direct-carriage reserve. The prover API still receives the whole list; the
/// builder derives this authenticated opening internally.
pub type Args {
  Complete {
    input_index: Int,
    output_index: Int,
    inputs_preimage: List<MidgardTxInput>,
    bad_inputs_index: Int,
  }
  CompletePublished {
    input_index: Int,
    output_index: Int,
    publication_reference_input_index: Int,
    bad_inputs_index: Int,
  }
  FoldStart {
    input_index: Int,
    output_index: Int,
    bad_inputs_index: Int,
    input_cbor: ByteArray,
    collection_proof: bounded_collection_v1.ItemProofV1,
  }
  FoldNext {
    input_index: Int,
    output_index: Int,
    input_cbor: ByteArray,
    collection_proof: bounded_collection_v1.ItemProofV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
