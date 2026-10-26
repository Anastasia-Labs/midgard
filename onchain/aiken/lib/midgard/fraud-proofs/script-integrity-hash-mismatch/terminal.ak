//// Terminal decision for `scriptIntegrityHashMismatch` (arm 39, phase 10, no
//// coordinate). The expected hash is computed from the language set of every
//// execution, as the completed all-purposes walk folded it, and from the
//// committed redeemer witness hash.

use aiken/crypto.{blake2b_256}
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/field_opening_v1 as opening
use midgard/fraud_proofs/first_fault_v1.{ScriptIntegrityFault}
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_witness_set_compact, decode_tx_compact_view_v1,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{ScriptIntegrityHashMismatch}
use midgard/script_language_views_v1

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  witness_set_compact_cbor: ByteArray,
}

pub type Datum =
  ct.StepDatum<scan.StateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

/// Whether the transaction's committed script integrity hash differs from the
/// one its executions require. The redeemer witness hash is the field-8
/// commitment of the witness set the bound compact commits to: the hash every
/// field-8 opening is authenticated against.
pub fn mismatch_v1(
  result: scan.ResultV1,
  witness_set_compact_cbor: ByteArray,
) -> Bool {
  let compact = decode_tx_compact_view_v1(
    opening.source_kind_of_anchor(result.bound.subject.source_kind),
    result.bound.compact_cbor,
  )
  expect blake2b_256(witness_set_compact_cbor) == compact.witness_set_hash
  let witness_set = decode_native_tx_witness_set_compact(
    witness_set_compact_cbor,
  )
  compact.body.script_integrity_hash != script_language_views_v1.expected_script_integrity_hash(
    witness_set.redeemer_tx_wits_hash,
    result.facts.language_bitmap,
  )
}

/// A wrongful acceptance convicts on a mismatch. A wrongful rejection convicts
/// when it records this arm and there is no mismatch, or when there is a
/// mismatch and the recorded reason ranks after phase 10.
pub fn convicts_v1(
  state: scan.StateV1,
  witness_set_compact_cbor: ByteArray,
) -> Bool {
  let result = scan.result_v1(state)
  let subject = result.bound.subject
  let mismatch = mismatch_v1(result, witness_set_compact_cbor)
  substrate.terminal_contradiction_v1(
    subject,
    when subject.rejection_reason is {
      None -> mismatch
      Some(ScriptIntegrityHashMismatch) -> mismatch
      Some(_) ->
        !mismatch || first_fault_v1.leaf_admits_later_fault_v1(
          subject,
          result.bound.network_order_context,
          ScriptIntegrityFault,
        )
    },
  )
}
