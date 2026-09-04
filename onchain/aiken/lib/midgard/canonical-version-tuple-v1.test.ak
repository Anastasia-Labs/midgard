use midgard/blake2b_224_trace_v1
use midgard/blake2b_256_trace_v1
use midgard/bounded_blob_v1
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/cek_blob_frontier_v1
use midgard/cek_data_bytes_v1
use midgard/cek_data_integer_v1
use midgard/cek_proof_v1
use midgard/cek_source_blob_v1
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_proof_v1
use midgard/ledger_output_scan_v1
use midgard/ledger_output_value_v1
use midgard/ledger_state
use midgard/mpf_proof_fold_v1
use midgard/native_script_scan_v1
use midgard/validation_claim_v1
use midgard/validation_dispute_v1
use midgard/validation_resolution_v1
use midgard/validation_trace_v1

test canonical_v1_version_tuple_matches_typescript_profile() {
  and {
    ledger_state.protocol_version_v1 == 1,
    ledger_state.transition_step_schema_version_v1 == 1,
    native_types.native_tx_version_v1 == 1,
    validation_trace_v1.machine_version == 1,
    validation_trace_v1.descriptor_version == 1,
    validation_dispute_v1.dispute_version == 1,
    validation_claim_v1.claim_version == 1,
    validation_resolution_v1.resolution_version == 1,
    validation_resolution_v1.prepared_resolution_version == 1,
    validation_resolution_v1.winning_resolution_version == 1,
    cek_proof_v1.program_envelope_version == 1,
    bounded_blob_v1.version == 1,
    bounded_collection_v1.version == 1,
    bounded_item_v1.version == 1,
    cek_blob_frontier_v1.version == 1,
    cek_data_bytes_v1.version == 1,
    cek_data_integer_v1.version == 1,
    cek_source_blob_v1.version == 1,
    ledger_output_commitment_v1.version == 1,
    ledger_output_proof_v1.version == 1,
    ledger_output_scan_v1.version == 1,
    ledger_output_value_v1.version == 1,
    native_script_scan_v1.version == 1,
    mpf_proof_fold_v1.proof_fold_version == 1,
    blake2b_224_trace_v1.version == 1,
    blake2b_256_trace_v1.version == 1,
  }
}
