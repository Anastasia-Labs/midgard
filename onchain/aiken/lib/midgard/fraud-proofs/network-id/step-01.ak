//// Q35 `network-id`: bind one operator-accepted native-V1 transaction and
//// freeze the exact network claim that the final step will adjudicate.
////
//// `TransactionNetwork` follows Cardano's optional transaction-body field:
//// compact scalar 255 means absent and is not a mismatch. `OutputNetwork`
//// names one item of field 2; output addresses have no absent-network form.

use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionCarriage}

pub type NetworkIdFaultV1 {
  TransactionNetwork
  OutputNetwork { output_index: Int }
  /// The exact authenticated forced leaf says `NetworkIdMismatch`.  The
  /// terminal step proves the complete network-id predicate false.
  ForcedNetworkIdMismatch
}

pub type ForcedDispatchV1 {
  input_index: Int,
  output_index: Int,
}

pub type Args {
  tx_inclusion: Option<NativeTxInclusionCarriage>,
  forced_source: Option<ForcedDispatchV1>,
  fault: NetworkIdFaultV1,
}

pub type Datum =
  ct.StepDatum<Data>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
