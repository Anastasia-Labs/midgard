use midgard/ledger_state

const ordinary_header_hash =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const non_genesis_root =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

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
  let exact_genesis = confirmed_state(
    ledger_state.genesis_header_hash,
    ledger_state.genesis_header_hash,
    ledger_state.genesis_utxo_root,
    10,
    10,
    ledger_state.genesis_protocol_version,
  )
  let ordinary_v1 = confirmed_state(
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
