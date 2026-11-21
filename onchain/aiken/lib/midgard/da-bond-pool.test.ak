//// Unit coverage for `lib/midgard/da-bond-pool.ak`: the backing clamp, the
//// authenticated pool readers (the reference input #688's Apply reads, the
//// spent input #693's Timeout slashes), and the two cross-module facts the
//// pool's Slash arm reads cheaply instead of decoding in full.
////
//// GUARD -> TEST MAP
////
//// lovelace_backing / pool_backing
////   `max(0, lovelace - floor)`
////     -> da_bond_pool_backing_clamps_at_zero
////
//// get_authentic_pool
////   honest -> da_bond_pool_reader_accepts_authentic_pool
////   payment credential `Script(pool_policy_id)`
////     -> da_bond_pool_reader_rejects_other_credential
////   NFT quantity 1
////     -> da_bond_pool_reader_rejects_missing_nft
////   inline datum
////     -> da_bond_pool_reader_rejects_datum_hash
////
//// get_authentic_pool_input
////   honest, both states
////     -> da_bond_pool_input_reader_accepts_authentic_pool_in_both_states
////   payment credential `Script(pool_policy_id)`
////     -> da_bond_pool_input_reader_rejects_other_credential
////   NFT quantity 1
////     -> da_bond_pool_input_reader_rejects_missing_nft
////   inline datum
////     -> da_bond_pool_input_reader_rejects_datum_hash
////   input present at the index
////     -> da_bond_pool_input_reader_rejects_missing_input
////
//// remove_unavailable_block_after_timeout_constructor_index (hazard T3)
////   -> da_bond_pool_pins_remove_unavailable_constructor_index
////
//// get_state_queue_policy (hazard T4)
////   honest -> da_bond_pool_partial_hub_read_matches_full_decode
////   hub authenticity (`utils.get_authentic_input_of`)
////     -> da_bond_pool_partial_hub_read_rejects_unauthentic_hub

use aiken/builtin
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{DatumHash, InlineDatum, Input, Output, OutputReference}
use midgard/availability_challenge as availability
use midgard/da_bond_pool as pool
use midgard/hub_oracle as hub
use midgard/state_queue

const pool_policy: PolicyId =
  #"d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0"

const other_policy: PolicyId =
  #"d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1"

const hub_policy: PolicyId =
  #"e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0"

const pool_floor: Int = 5_000_000

fn parameters() -> availability.ParametersV1 {
  availability.ParametersV1 {
    response_geometry: availability.ResponseGeometryV1 {
      chunk_byte_length: 4_096,
      tranche_byte_length: 4 * 1024 * 1024,
      max_tranche_count: 16,
    },
    da_bond_lovelace: 500_000_000,
    challenger_bond_lovelace: 10_000_000_000,
    max_open_fee_lovelace: 1,
    max_publication_fee_lovelace: 1,
    max_settlement_fee_lovelace: 1,
    max_close_fee_lovelace: 1,
    max_timeout_fee_lovelace: 1,
    da_slash_penalty_lovelace: 100_000_000,
    da_bond_min_top_up_lovelace: 5_000_000,
    da_bond_pool_floor_lovelace: pool_floor,
    challenge_record_lovelace: 27_000_000,
  }
}

fn outref(index: Int) -> OutputReference {
  OutputReference {
    transaction_id: #"4242424242424242424242424242424242424242424242424242424242424242",
    output_index: index,
  }
}

fn pool_output(lovelace: Int) -> Output {
  let datum: Data = pool.Withdrawing { unlock_at: 123 }
  Output {
    address: address.from_script(pool_policy),
    value: assets.from_lovelace(lovelace)
      |> assets.add(pool_policy, pool.da_bond_pool_asset_name, 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn read_pool(output: Output) -> (pool.DaBondPoolDatum, Int) {
  pool.get_authentic_pool(
    [
      Input {
        output_reference: outref(0),
        output: Output {
          ..output,
          address: address.from_script(other_policy),
          value: assets.from_lovelace(2_000_000),
        },
      },
      Input { output_reference: outref(1), output },
    ],
    pool_policy,
    1,
  )
}

// ---------------------------------------------------------------------------
// Backing
// ---------------------------------------------------------------------------

test da_bond_pool_backing_clamps_at_zero() {
  and {
    pool.pool_backing(assets.from_lovelace(0), parameters()) == 0,
    pool.pool_backing(assets.from_lovelace(pool_floor - 1), parameters()) == 0,
    pool.pool_backing(assets.from_lovelace(pool_floor), parameters()) == 0,
    pool.pool_backing(assets.from_lovelace(pool_floor + 7), parameters()) == 7,
    pool.pool_backing(
      assets.from_lovelace(pool_floor + 7)
        |> assets.add(pool_policy, pool.da_bond_pool_asset_name, 1),
      parameters(),
    ) == 7,
    pool.lovelace_backing(pool_floor - 1, parameters()) == 0,
  }
}

// ---------------------------------------------------------------------------
// Authenticated reader
// ---------------------------------------------------------------------------

test da_bond_pool_reader_accepts_authentic_pool() {
  read_pool(pool_output(505_000_000)) == (
    pool.Withdrawing { unlock_at: 123 },
    505_000_000,
  )
}

test da_bond_pool_reader_rejects_other_credential() fail {
  let honest = pool_output(505_000_000)
  read_pool(
    Output {
      ..honest,
      address: Address {
        payment_credential: Script(other_policy),
        stake_credential: None,
      },
    },
  ).2nd > 0
}

test da_bond_pool_reader_rejects_missing_nft() fail {
  let honest = pool_output(505_000_000)
  read_pool(Output { ..honest, value: assets.from_lovelace(505_000_000) }).2nd > 0
}

test da_bond_pool_reader_rejects_datum_hash() fail {
  let honest = pool_output(505_000_000)
  read_pool(
    Output {
      ..honest,
      datum: DatumHash(
        #"abababababababababababababababababababababababababababababababab",
      ),
    },
  ).2nd > 0
}

// ---------------------------------------------------------------------------
// Authenticated spent pool (the Timeout yield's pool input)
// ---------------------------------------------------------------------------

fn spent_pool(output: Output) -> Output {
  pool.get_authentic_pool_input(
    [
      Input {
        output_reference: outref(0),
        output: Output {
          ..output,
          address: address.from_script(other_policy),
          value: assets.from_lovelace(2_000_000),
        },
      },
      Input { output_reference: outref(1), output },
    ],
    pool_policy,
    1,
  )
}

/// Either datum state passes and the output comes back unchanged.
test da_bond_pool_input_reader_accepts_authentic_pool_in_both_states() {
  let withdrawing = pool_output(505_000_000)
  let bonded_datum: Data = pool.Bonded
  let bonded = Output { ..withdrawing, datum: InlineDatum(bonded_datum) }
  and {
    spent_pool(withdrawing) == withdrawing,
    spent_pool(bonded) == bonded,
  }
}

/// Refused by the credential pin (`Script(pool_policy_id)`). The body is True
/// whenever the reader returns, so only the pin's refusal keeps it green.
test da_bond_pool_input_reader_rejects_other_credential() fail {
  let honest = pool_output(505_000_000)
  let mutated = Output { ..honest, address: address.from_script(other_policy) }
  spent_pool(mutated) == mutated
}

/// Refused by the NFT quantity pin. The body is True whenever the reader
/// returns, so only the pin's refusal keeps it green.
test da_bond_pool_input_reader_rejects_missing_nft() fail {
  let honest = pool_output(505_000_000)
  let mutated = Output { ..honest, value: assets.from_lovelace(505_000_000) }
  spent_pool(mutated) == mutated
}

/// Refused by the inline-datum pin. The body is True whenever the reader
/// returns, so only the pin's refusal keeps it green.
test da_bond_pool_input_reader_rejects_datum_hash() fail {
  let honest = pool_output(505_000_000)
  let mutated = Output {
    ..honest,
    datum: DatumHash(
      #"abababababababababababababababababababababababababababababababab",
    ),
  }
  spent_pool(mutated) == mutated
}

/// Refused: no input at the index.
test da_bond_pool_input_reader_rejects_missing_input() fail {
  pool.get_authentic_pool_input([], pool_policy, 0) == pool_output(505_000_000)
}

// ---------------------------------------------------------------------------
// Hazard T3: Slash reads only the constructor index of the state-queue redeemer
// ---------------------------------------------------------------------------

test da_bond_pool_pins_remove_unavailable_constructor_index() {
  let redeemer: state_queue.MintRedeemer = state_queue.RemoveUnavailableBlockAfterTimeout {
    yield_to_ref_input_index: 0,
    unavailable_header_hash: #"0101010101010101010101010101010101010101010101010101010101010101",
    challenge_asset_name: #"4441434802020202020202020202020202020202020202020202020202020202",
    removal_approach: state_queue.RemoveTimedOutHead {
      confirmed_state_input_outref: outref(3),
      confirmed_state_output_index: 0,
    },
  }
  let redeemer_data: Data = redeemer
  let constructor_index = builtin.un_constr_data(redeemer_data).1st
  and {
    constructor_index == 5,
    constructor_index == pool.remove_unavailable_block_after_timeout_constructor_index,
  }
}

// ---------------------------------------------------------------------------
// Hazard T4: the partial hub read is field 4 of the real hub datum
// ---------------------------------------------------------------------------

/// Every policy and address field distinct, so field 4 is the only one that
/// can equal `state_queue`.
fn hub_datum() -> hub.Datum {
  hub.Datum {
    registered_operators: #"a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0",
    active_operators: #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1",
    retired_operators: #"a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2",
    scheduler: #"a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3",
    state_queue: #"a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4",
    fraud_proof_catalogue: #"a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5",
    fraud_proof: #"a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6",
    deposit: #"a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7",
    withdrawal: #"a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8",
    tx_order: #"a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9",
    settlement: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    payout: #"abababababababababababababababababababababababababababab",
    registered_operators_addr: address.from_script(
      #"b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0",
    ),
    active_operators_addr: address.from_script(
      #"b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1",
    ),
    retired_operators_addr: address.from_script(
      #"b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2",
    ),
    scheduler_addr: address.from_script(
      #"b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3b3",
    ),
    state_queue_addr: address.from_script(
      #"b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4",
    ),
    fraud_proof_catalogue_addr: address.from_script(
      #"b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5b5",
    ),
    fraud_proof_addr: address.from_script(
      #"b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6",
    ),
    deposit_addr: address.from_script(
      #"b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7b7",
    ),
    withdrawal_addr: address.from_script(
      #"b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8b8",
    ),
    tx_order_addr: address.from_script(
      #"b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9b9",
    ),
    settlement_addr: address.from_script(
      #"babababababababababababababababababababababababababababa",
    ),
    reserve_addr: address.from_script(
      #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
    ),
    payout_addr: address.from_script(
      #"bcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbcbc",
    ),
    reserve_observer: #"bdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbdbd",
  }
}

fn hub_reference_inputs(hub_nft_policy: PolicyId) -> List<Input> {
  [
    Input {
      output_reference: outref(0),
      output: Output {
        address: address.from_script(other_policy),
        value: assets.from_lovelace(2_000_000),
        datum: InlineDatum(0),
        reference_script: None,
      },
    },
    Input {
      output_reference: outref(1),
      output: Output {
        address: address.from_script(hub_policy),
        value: assets.from_lovelace(2_000_000)
          |> assets.add(hub_nft_policy, hub.asset_name, 1),
        datum: InlineDatum(hub_datum()),
        reference_script: None,
      },
    },
  ]
}

test da_bond_pool_partial_hub_read_matches_full_decode() {
  let reference_inputs = hub_reference_inputs(hub_policy)
  let partial = pool.get_state_queue_policy(reference_inputs, hub_policy, 1)
  and {
    partial == hub.get_datum(reference_inputs, hub_policy, 1).state_queue,
    partial == hub_datum().state_queue,
  }
}

test da_bond_pool_partial_hub_read_rejects_unauthentic_hub() fail {
  pool.get_state_queue_policy(hub_reference_inputs(other_policy), hub_policy, 1) == hub_datum().state_queue
}
