use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{NativeTxInclusionArgs}
use midgard/fraud_proofs/native_tx/types.{NativeTxWitnessSetCompact}

pub type Datum =
  ct.StepDatum<Data>

pub type Args {
  tx_inclusion_args: NativeTxInclusionArgs,
  /// Preimage of the bad transaction's committed `witness_set_hash`, revealing
  /// the canonical per-collection witness hashes.
  bad_tx_witness_set_compact: NativeTxWitnessSetCompact,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
