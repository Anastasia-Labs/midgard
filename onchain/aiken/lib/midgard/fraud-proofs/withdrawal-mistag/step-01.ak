use aiken/builtin
use aiken/primitive/bytearray
use cardano/assets.{AssetName}
use midgard/common/types.{H32}
use midgard/computation_thread as ct
use midgard/fraud_proof_catalogue
use midgard/ledger_state
use midgard/transition_trace

/// Reserved only by the pre-registration Q41 harness. Production registration
/// must allocate again from the append-only catalogue.
pub const withdrawal_mistag_test_category_id_v1: ByteArray = #"00000014"

pub fn category_is_withdrawal_mistag_v1(asset_name: AssetName) -> Bool {
  and {
    bytearray.length(asset_name) == fraud_proof_catalogue.id_byte_count + 28,
    bytearray.take(asset_name, fraud_proof_catalogue.id_byte_count) == withdrawal_mistag_test_category_id_v1,
  }
}

pub fn challenged_header_hash_of_v1(
  asset_name: AssetName,
) -> ledger_state.HeaderHash {
  bytearray.drop(asset_name, fraud_proof_catalogue.id_byte_count)
}

pub fn computation_thread_asset_name_v1(
  header_hash: ledger_state.HeaderHash,
) -> AssetName {
  withdrawal_mistag_test_category_id_v1
    |> builtin.append_bytearray(header_hash)
}

pub type WithdrawalInfoHash =
  H32<ledger_state.WithdrawalInfo>

pub type State {
  challenged_header_hash: ledger_state.HeaderHash,
  withdrawal_id: ledger_state.WithdrawalId,
  withdrawal_info_hash: WithdrawalInfoHash,
  claimed_valid: Bool,
  event_to_step_root: ledger_state.EventToStepRoot,
  total_event_count: Int,
  transition_trace_root: ledger_state.TransitionTraceRoot,
  transition_step_count: Int,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  state_queue_node_ref_input_index: Int,
  committed_withdrawal: transition_trace.RootMembershipProof<
    ledger_state.WithdrawalId,
    ledger_state.WithdrawalInfo,
  >,
}

pub type Datum =
  ct.StepDatum<Data>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

test reserved_test_category_is_exact() {
  withdrawal_mistag_test_category_id_v1 == #"00000014"
}

test accepts_own_category_and_header_suffix() {
  let header_hash = #"14141414141414141414141414141414141414141414141414141414"
  let asset_name = computation_thread_asset_name_v1(header_hash)
  and {
    category_is_withdrawal_mistag_v1(asset_name),
    challenged_header_hash_of_v1(asset_name) == header_hash,
  }
}

test rejects_wrong_category() {
  !category_is_withdrawal_mistag_v1(
    #"0000000414141414141414141414141414141414141414141414141414141414",
  )
}

test rejects_wrong_category_length() {
  !category_is_withdrawal_mistag_v1(#"00000014")
}
