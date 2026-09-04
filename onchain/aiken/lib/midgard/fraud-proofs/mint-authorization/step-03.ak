//// Datum and redeemer wire shapes for the mint-authorization thread's third
//// step — the direction dispatch. Direction A proves the inline half of the
//// source-absence claim (field-6 walk) and routes into the reference-input
//// scan; direction B pins the policy's native script by hash and refutes it
//// against the committed signer set and validity interval.

use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type State {
  /// The claimed policy id, read off the committed field-5 item by step-02.
  policy_id: ByteArray,
  direction: Int,
  bad_tx_id: ByteArray,
  bad_tx_witness_set_hash: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
  /// The transition step's `pre_utxos_root` — the ledger the machine
  /// resolved this transaction's reference inputs against.
  prior_ledger_root: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  /// Direction A: walk the whole committed field 6 and prove no inline
  /// script witness (of any language) hashes to the claimed policy id, then
  /// route into step-04's reference-input scan at cursor 0.
  WitnessAbsence {
    input_index: Int,
    output_index: Int,
    script_tx_wits_opening: FieldOpeningV1,
  }
  /// Direction B: `script_bytes` is the policy's native payload, pinned by
  /// `blake2b_224(0x00 ‖ script_bytes) == policy_id`; the committed signer
  /// set is read off field 7 and the machine-twin evaluator must yield
  /// `satisfied: False`. Closes straight to step-05.
  EvaluateUnsatisfied {
    input_index: Int,
    output_index: Int,
    script_bytes: ByteArray,
    addr_tx_wits_opening: FieldOpeningV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
