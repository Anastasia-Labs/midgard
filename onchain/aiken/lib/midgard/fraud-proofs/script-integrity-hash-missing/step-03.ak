use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/native_tx/types.{NativeTxWitnessSetCompact}
use midgard/fraud_proofs/script_integrity_hash_missing/state.{SubjectStateV1}
use midgard/native_tx_field_access_v1.{FieldCarriageV1}

pub type State =
  SubjectStateV1

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  Direct {
    input_index: Int,
    output_index: Int,
    native_tx_compact_cbor: ByteArray,
    witness_set: NativeTxWitnessSetCompact,
    script_witnesses: FieldCarriageV1,
    redeemers: FieldCarriageV1,
  }
  StartStaged {
    input_index: Int,
    output_index: Int,
    script_witnesses_opening: FieldOpeningV1,
    item_budget: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
