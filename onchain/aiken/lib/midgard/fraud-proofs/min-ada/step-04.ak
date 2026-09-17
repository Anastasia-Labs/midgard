//// Q27 min-Ada UTxO culpability step. The preceding steps authenticated the
//// exact post-root descriptor and production predicate; this step proves that
//// the same out-ref was absent from the predecessor root.

use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NonMembershipCarriage}

pub type State {
  out_ref_key: ByteArray,
  prev_utxos_root: ByteArray,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  predecessor_non_membership: NonMembershipCarriage,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
