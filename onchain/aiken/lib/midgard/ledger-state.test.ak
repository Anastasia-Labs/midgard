use aiken/cbor
use aiken/crypto
use cardano/transaction.{OutputReference}
use midgard/ledger_state
use midgard/state_queue

const ordinary_header_hash =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const non_genesis_root =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

fn sample_header_v1() -> ledger_state.HeaderV1 {
  ledger_state.HeaderV1 {
    prev_utxos_root: #"0101010101010101010101010101010101010101010101010101010101010101",
    utxos_root: #"0202020202020202020202020202020202020202020202020202020202020202",
    withdrawals_root: ledger_state.genesis_utxo_root,
    forced_transactions_root: ledger_state.genesis_utxo_root,
    transactions_root: #"0303030303030303030303030303030303030303030303030303030303030303",
    deposits_root: ledger_state.genesis_utxo_root,
    transition_trace_root: #"0404040404040404040404040404040404040404040404040404040404040404",
    event_to_step_root: #"0505050505050505050505050505050505050505050505050505050505050505",
    validation_traces_root: #"0808080808080808080808080808080808080808080808080808080808080808",
    withdrawal_count: 0,
    forced_transaction_count: 0,
    l2_transaction_count: 1,
    deposit_count: 0,
    total_event_count: 1,
    transition_step_count: 1,
    validation_trace_count: 1,
    start_time: 1,
    end_time: 2,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: #"06060606060606060606060606060606060606060606060606060606",
    operator_vkey: #"07070707070707070707070707070707070707070707070707070707",
    protocol_version: ledger_state.protocol_version_v1,
  }
}

test l01_header_v1_aiken_cbor_and_hash_vector_is_exact() {
  let encoded = cbor.serialise(sample_header_v1())
  and {
    encoded == #"d8799f582001010101010101010101010101010101010101010101010101010101010101015820020202020202020202020202020202020202020202020202020202020202020258200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a85820030303030303030303030303030303030303030303030303030303030303030358200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200404040404040404040404040404040404040404040404040404040404040404582005050505050505050505050505050505050505050505050505050505050505055820080808080808080808080808080808080808080808080808080808080808080800000100010101010200000000581c06060606060606060606060606060606060606060606060606060606581c0707070707070707070707070707070707070707070707070707070701ff",
    crypto.blake2b_224(encoded) == #"68e507eaad2278934d696204c01ffa64ca7381e989823e5aed19afbc",
    ledger_state.header_v1_is_valid(sample_header_v1()),
  }
}

test l02_header_transition_commitments_v1_are_complete_and_exact() {
  let commitments =
    ledger_state.header_transition_commitments_v1(sample_header_v1())
  and {
    cbor.serialise(commitments) == #"d8799f58200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200404040404040404040404040404040404040404040404040404040404040404582005050505050505050505050505050505050505050505050505050505050505055820080808080808080808080808080808080808080808080808080808080808080800000100010101ff",
    ledger_state.header_transition_commitments_v1_are_valid(commitments),
    !ledger_state.header_transition_commitments_v1_are_valid(
      ledger_state.HeaderTransitionCommitmentsV1 {
        ..commitments,
        forced_transaction_count: 1,
      },
    ),
    !ledger_state.header_transition_commitments_v1_are_valid(
      ledger_state.HeaderTransitionCommitmentsV1 {
        ..commitments,
        transition_step_count: 0,
      },
    ),
    !ledger_state.header_v1_is_valid(
      ledger_state.HeaderV1 { ..sample_header_v1(), protocol_version: 2 },
    ),
  }
}

test l03_state_queue_node_v1_aiken_topology_and_datum_vector_is_exact() {
  let node: state_queue.StateQueueNodeV1 =
    state_queue.StateQueueNode {
      header: sample_header_v1(),
      da_attestation: #"aa",
    }
  cbor.serialise(node) == #"d8799fd8799f582001010101010101010101010101010101010101010101010101010101010101015820020202020202020202020202020202020202020202020202020202020202020258200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a85820030303030303030303030303030303030303030303030303030303030303030358200e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a858200404040404040404040404040404040404040404040404040404040404040404582005050505050505050505050505050505050505050505050505050505050505055820080808080808080808080808080808080808080808080808080808080808080800000100010101010200000000581c06060606060606060606060606060606060606060606060606060606581c0707070707070707070707070707070707070707070707070707070701ff41aaff"
}

test l04_init_v1_and_merge_to_confirmed_state_v1_aiken_vectors_are_exact() {
  let init: state_queue.MintRedeemer = state_queue.InitV1 { output_index: 2 }
  let merge: state_queue.MintRedeemer =
    state_queue.MergeToConfirmedStateV1 {
      header_node_key: #"11111111111111111111111111111111111111111111111111111111",
      confirmed_state_input_outref: OutputReference {
        transaction_id: #"4444444444444444444444444444444444444444444444444444444444444444",
        output_index: 0,
      },
      confirmed_state_output_index: 0,
      m_settlement_redeemer_index: Some(1),
      merged_block_withdrawals_root: #"2121212121212121212121212121212121212121212121212121212121212121",
      merged_block_forced_transactions_root: #"2222222222222222222222222222222222222222222222222222222222222222",
      merged_block_transactions_root: #"2323232323232323232323232323232323232323232323232323232323232323",
      merged_block_deposits_root: #"2424242424242424242424242424242424242424242424242424242424242424",
      merged_block_transition_trace_root: #"2525252525252525252525252525252525252525252525252525252525252525",
      merged_block_event_to_step_root: #"2626262626262626262626262626262626262626262626262626262626262626",
      merged_block_validation_traces_root: #"2727272727272727272727272727272727272727272727272727272727272727",
      merged_block_withdrawal_count: 1,
      merged_block_forced_transaction_count: 2,
      merged_block_l2_transaction_count: 3,
      merged_block_deposit_count: 4,
      merged_block_total_event_count: 10,
      merged_block_transition_step_count: 10,
      merged_block_validation_trace_count: 5,
    }
  and {
    cbor.serialise(init) == #"d8799f02ff",
    cbor.serialise(merge) == #"d87d9f581c11111111111111111111111111111111111111111111111111111111d8799f5820444444444444444444444444444444444444444444444444444444444444444400ff00d8799f01ff58202121212121212121212121212121212121212121212121212121212121212121582022222222222222222222222222222222222222222222222222222222222222225820232323232323232323232323232323232323232323232323232323232323232358202424242424242424242424242424242424242424242424242424242424242424582025252525252525252525252525252525252525252525252525252525252525255820262626262626262626262626262626262626262626262626262626262626262658202727272727272727272727272727272727272727272727272727272727272727010203040a0a05ff",
  }
}

fn confirmed_state(
  header_hash: ByteArray,
  prev_header_hash: ByteArray,
  utxo_root: ByteArray,
  start_time: Int,
  end_time: Int,
  protocol_version: Int,
) -> ledger_state.ConfirmedState {
  ledger_state.ConfirmedState {
    header_hash,
    prev_header_hash,
    utxo_root,
    start_time,
    end_time,
    protocol_version,
  }
}

test genesis_protocol_sentinel_and_first_ordinary_v1_are_distinct() {
  let exact_genesis =
    confirmed_state(
      ledger_state.genesis_header_hash,
      ledger_state.genesis_header_hash,
      ledger_state.genesis_utxo_root,
      10,
      10,
      ledger_state.genesis_protocol_version,
    )
  let ordinary_v1 =
    confirmed_state(
      ordinary_header_hash,
      ledger_state.genesis_header_hash,
      non_genesis_root,
      10,
      11,
      ledger_state.protocol_version_v1,
    )
  and {
    ledger_state.genesis_confirmed_state_v1(10) == Some(exact_genesis),
    ledger_state.genesis_confirmed_state_v1(-1) == None,
    ledger_state.confirmed_state_next_header_protocol_version_v1(exact_genesis) == Some(
      ledger_state.protocol_version_v1,
    ),
    ledger_state.confirmed_state_next_header_protocol_version_v1(ordinary_v1) == Some(
      ledger_state.protocol_version_v1,
    ),
    ledger_state.confirmed_state_next_header_protocol_version_v1(
      confirmed_state(
        ledger_state.genesis_header_hash,
        ledger_state.genesis_header_hash,
        ledger_state.genesis_utxo_root,
        10,
        10,
        ledger_state.protocol_version_v1,
      ),
    ) == None,
    ledger_state.confirmed_state_next_header_protocol_version_v1(
      confirmed_state(
        ordinary_header_hash,
        ledger_state.genesis_header_hash,
        ledger_state.genesis_utxo_root,
        10,
        10,
        ledger_state.genesis_protocol_version,
      ),
    ) == None,
    ledger_state.confirmed_state_next_header_protocol_version_v1(
      confirmed_state(
        ledger_state.genesis_header_hash,
        ordinary_header_hash,
        ledger_state.genesis_utxo_root,
        10,
        10,
        ledger_state.genesis_protocol_version,
      ),
    ) == None,
    ledger_state.confirmed_state_next_header_protocol_version_v1(
      confirmed_state(
        ledger_state.genesis_header_hash,
        ledger_state.genesis_header_hash,
        non_genesis_root,
        10,
        10,
        ledger_state.genesis_protocol_version,
      ),
    ) == None,
    ledger_state.confirmed_state_next_header_protocol_version_v1(
      confirmed_state(
        ledger_state.genesis_header_hash,
        ledger_state.genesis_header_hash,
        ledger_state.genesis_utxo_root,
        10,
        11,
        ledger_state.genesis_protocol_version,
      ),
    ) == None,
    ledger_state.confirmed_state_next_header_protocol_version_v1(
      confirmed_state(
        ordinary_header_hash,
        ledger_state.genesis_header_hash,
        non_genesis_root,
        11,
        10,
        ledger_state.protocol_version_v1,
      ),
    ) == None,
    ledger_state.confirmed_state_next_header_protocol_version_v1(
      confirmed_state(
        ordinary_header_hash,
        ledger_state.genesis_header_hash,
        non_genesis_root,
        10,
        11,
        2,
      ),
    ) == None,
  }
}
