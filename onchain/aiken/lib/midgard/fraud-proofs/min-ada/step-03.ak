//// Q27 min-Ada UTxO predicate step. The preceding step authenticated the
//// exact post-root descriptor; this step applies the compiled production
//// min-Ada predicate without carrying another MPF opening.

use midgard/computation_thread as ct

pub type State {
  MinAdaTxDescriptor { total_length: Int, lovelace: Int }
  MinAdaUtxoDescriptor {
    descriptor_cbor: ByteArray,
    out_ref_key: ByteArray,
    prev_utxos_root: ByteArray,
  }
}

pub type Args {
  input_index: Int,
  output_index: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
