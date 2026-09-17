//// Q27 min-Ada UTxO predicate step. The preceding step authenticated the
//// exact post-root descriptor; this step applies the compiled production
//// min-Ada predicate without carrying another MPF opening.

use midgard/computation_thread as ct
use midgard/fraud_proofs/transaction_output_non_canonical/rule.{
  OutputScanStateV1,
}

pub type State {
  MinAdaTxScan { direction: Int, scan: OutputScanStateV1 }
  MinAdaTxDescriptor { direction: Int, total_length: Int, lovelace: Int }
  MinAdaUtxoDescriptor {
    descriptor_cbor: ByteArray,
    out_ref_key: ByteArray,
    prev_utxos_root: ByteArray,
  }
}

pub type Args {
  window: ByteArray,
  input_index: Int,
  output_index: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
