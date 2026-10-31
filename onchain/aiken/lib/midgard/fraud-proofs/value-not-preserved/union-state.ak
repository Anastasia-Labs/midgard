use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/fraud_proofs/value_not_preserved/union.{ConservationClaim}
use midgard/ledger_output_scan_v1.{LedgerOutputScanControlV1}
use midgard/ledger_state.{EventKey}

pub type SourceState {
  transaction_id: ByteArray,
  claim: ConservationClaim,
  fee: Int,
  event_key: EventKey,
  event_root: ByteArray,
  event_count: Int,
  trace_root: ByteArray,
  trace_count: Int,
}

pub type EventState {
  transaction_id: ByteArray,
  claim: ConservationClaim,
  fee: Int,
  event_key: EventKey,
  trace_root: ByteArray,
  trace_count: Int,
  step_index: Int,
}

pub type Inputs {
  cursor: Int,
}

pub type SelectedInput {
  input: MidgardTxInput,
  cursor: Int,
  selector_hash: ByteArray,
}

pub type AssetCursor {
  count: Int,
  frontier_commitment: ByteArray,
  cursor: Int,
  quantity_sign: Int,
  next_script_hash: ByteArray,
  next_continuation: Data,
}

pub type FieldGrammar {
  field_index: Int,
  checkpoint_hash: Option<ByteArray>,
}

pub type FieldCursor {
  field_index: Int,
  checkpoint_hash: ByteArray,
  grammar_script_hash: ByteArray,
}

pub type OutputItem {
  field_total_length: Int,
  field_chunk_hashes: List<ByteArray>,
  index: Int,
  offset: Int,
  length: Int,
  checkpoint_hash: ByteArray,
  next_checkpoint_hash: ByteArray,
  selector_hash: ByteArray,
  grammar_script_hash: ByteArray,
}

pub type OutputScan {
  item: OutputItem,
  control: LedgerOutputScanControlV1,
}

pub type MintPolicy {
  offset: Int,
  length: Int,
  next_checkpoint_hash: ByteArray,
  policy_id: ByteArray,
  cursor: Int,
  remaining: Int,
  previous_asset: Option<ByteArray>,
}

pub type MintCursor {
  field: FieldCursor,
  previous_policy: Option<ByteArray>,
  active: Option<MintPolicy>,
}

pub type Complete {
  Complete
}
