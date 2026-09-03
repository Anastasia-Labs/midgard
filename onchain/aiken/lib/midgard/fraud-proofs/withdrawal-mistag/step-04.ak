use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use cardano/address.{Address, Inline, Pointer, VerificationKey}
use cardano/assets
use cardano/transaction.{DatumHash, InlineDatum, NoDatum, OutputReference}
use env
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/withdrawal_mistag/step_03
use midgard/ledger_output_commitment_v1
use midgard/ledger_state
use midgard/validation_machine/shared as vm_shared

pub type State {
  challenged_header_hash: ledger_state.HeaderHash,
  withdrawal_id: ledger_state.WithdrawalId,
  claimed_valid: Bool,
  actual_valid: Bool,
  exact_output_bytes: Int,
  required_lovelace: Int,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  withdrawal_body: ledger_state.WithdrawalBody,
}

pub type Datum =
  ct.StepDatum<step_03.State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

fn cbor_bytes_head_length(length: Int) -> Int {
  if length < 24 {
    1
  } else if length <= 255 {
    2
  } else if length <= 65535 {
    3
  } else if length <= 4294967295 {
    5
  } else {
    9
  }
}

fn pointer_word_length(value: Int) -> Int {
  expect value >= 0
  if value < 128 {
    1
  } else {
    1 + pointer_word_length(value / 128)
  }
}

pub fn address_payload_length_v1(target_address: address.Address) -> Int {
  29 + when target_address.stake_credential is {
    None -> 0
    Some(Inline(_)) -> 28
    Some(Pointer { slot_number, transaction_index, certificate_index }) ->
      pointer_word_length(slot_number) + pointer_word_length(transaction_index) + pointer_word_length(
        certificate_index,
      )
  }
}

fn datum_option_length(datum: transaction.Datum) -> Int {
  when datum is {
    NoDatum -> 0
    DatumHash(hash) -> {
      expect bytearray.length(hash) == 32
      // key 2 + [0, bytes(32)]
      1 + 1 + 1 + 2 + 32
    }
    InlineDatum(data) -> {
      let data_bytes = cbor.serialise(data)
      let data_length = bytearray.length(data_bytes)
      // key 2 + [1, tag(24), bytes(serialiseData)]
      1 + 1 + 1 + 2 + cbor_bytes_head_length(data_length) + data_length
    }
  }
}

pub fn exact_payout_output_bytes_v1(
  body: ledger_state.WithdrawalBody,
  cardano_value_size: Int,
) -> Int {
  let address_length = address_payload_length_v1(body.l1_address)
  // map head + key 0 + address bytes + key 1 + exact Cardano value bytes.
  1 + 1 + cbor_bytes_head_length(address_length) + address_length + 1 + cardano_value_size + datum_option_length(
    body.l1_datum,
  )
}

pub fn payout_is_exactly_payable_with_rate_v1(
  body: ledger_state.WithdrawalBody,
  cardano_value_size: Int,
  coins_per_utxo_byte: Int,
) -> Bool {
  let lovelace = assets.lovelace_of(assets.from_asset_list(body.l2_value))
  and {
    cardano_value_size > 0,
    cardano_value_size <= ledger_output_commitment_v1.max_cardano_value_cbor_bytes,
    vm_shared.output_meets_min_ada_v1(
      coins_per_utxo_byte,
      exact_payout_output_bytes_v1(body, cardano_value_size),
      lovelace,
    ),
  }
}

pub fn payout_is_exactly_payable_v1(
  body: ledger_state.WithdrawalBody,
  cardano_value_size: Int,
) -> Bool {
  payout_is_exactly_payable_with_rate_v1(
    body,
    cardano_value_size,
    env.coins_per_utxo_byte,
  )
}

pub fn establish_mistag_v1(
  input_state: step_03.State,
  withdrawal_body: ledger_state.WithdrawalBody,
) -> State {
  expect
    utils.serialise_and_hash_32(withdrawal_body) == input_state.withdrawal_body_hash
  let claimed_valid = input_state.claimed_valid
  let payable = payout_is_exactly_payable_v1(
    withdrawal_body,
    input_state.cardano_value_size,
  )
  let actual_valid = and {
    input_state.output_present,
    input_state.core_valid,
    payable,
  }
  expect claimed_valid != actual_valid
  let exact_output_bytes = exact_payout_output_bytes_v1(
    withdrawal_body,
    input_state.cardano_value_size,
  )
  State {
    challenged_header_hash: input_state.challenged_header_hash,
    withdrawal_id: input_state.withdrawal_id,
    claimed_valid,
    actual_valid,
    exact_output_bytes,
    required_lovelace: vm_shared.min_ada_lovelace_v1(
      env.coins_per_utxo_byte,
      exact_output_bytes,
    ),
  }
}

pub fn fixture_body_v1(lovelace: Int) -> ledger_state.WithdrawalBody {
  ledger_state.WithdrawalBody {
    l2_outref: OutputReference { transaction_id: #"01", output_index: 0 },
    l2_owner: #"11111111111111111111111111111111111111111111111111111111",
    l2_value: [
      Pair(assets.ada_policy_id, [Pair(assets.ada_asset_name, lovelace)]),
    ],
    l1_address: address.from_verification_key(
      #"11111111111111111111111111111111111111111111111111111111",
    ),
    l1_datum: NoDatum,
  }
}

test binds_parameterized_payability() {
  let body = fixture_body_v1(1_000_000)
  !payout_is_exactly_payable_with_rate_v1(body, 5, 1_000_000)
}

test accepts_unpayable_mistag() {
  !payout_is_exactly_payable_v1(fixture_body_v1(1), 5)
}

test accepts_exact_minimum() {
  let body = fixture_body_v1(1)
  let bytes = exact_payout_output_bytes_v1(body, 5)
  let minimum = vm_shared.min_ada_lovelace_v1(env.coins_per_utxo_byte, bytes)
  payout_is_exactly_payable_v1(fixture_body_v1(minimum), 5)
}

test rejects_one_lovelace_below_exact_minimum() {
  let body = fixture_body_v1(1)
  let bytes = exact_payout_output_bytes_v1(body, 5)
  let minimum = vm_shared.min_ada_lovelace_v1(env.coins_per_utxo_byte, bytes)
  !payout_is_exactly_payable_v1(fixture_body_v1(minimum - 1), 5)
}

test rejects_parameter_mutation() {
  let body = fixture_body_v1(1_000_000)
  payout_is_exactly_payable_with_rate_v1(body, 5, 1) != payout_is_exactly_payable_with_rate_v1(
    body,
    5,
    1_000_000,
  )
}

test counts_inline_stake_and_datum_hash_bytes_exactly() {
  let body = fixture_body_v1(1_000_000)
  let staked = ledger_state.WithdrawalBody {
    ..body,
    l1_address: Address {
      ..body.l1_address,
      stake_credential: Some(
        Inline(
          VerificationKey(
            #"22222222222222222222222222222222222222222222222222222222",
          ),
        ),
      ),
    },
    l1_datum: DatumHash(
      #"3333333333333333333333333333333333333333333333333333333333333333",
    ),
  }
  exact_payout_output_bytes_v1(staked, 5) == 104
}

test counts_pointer_address_words_exactly() {
  let body = fixture_body_v1(1_000_000)
  let pointed = ledger_state.WithdrawalBody {
    ..body,
    l1_address: Address {
      ..body.l1_address,
      stake_credential: Some(
        Pointer {
          slot_number: 127,
          transaction_index: 128,
          certificate_index: 16384,
        },
      ),
    },
  }
  exact_payout_output_bytes_v1(pointed, 5) == 45
}

test counts_inline_datum_tag_and_bytes_exactly() {
  let body = fixture_body_v1(1_000_000)
  exact_payout_output_bytes_v1(
    ledger_state.WithdrawalBody { ..body, l1_datum: InlineDatum(42) },
    5,
  ) == 47
}

test counts_canonical_inline_map_datum_exactly() {
  let body = fixture_body_v1(1_000_000)
  exact_payout_output_bytes_v1(
    ledger_state.WithdrawalBody {
      ..body,
      l1_datum: InlineDatum(
        builtin.map_data([Pair(builtin.i_data(1), builtin.i_data(2))]),
      ),
    },
    5,
  ) == 48
}

fn fixture_step_03_state(
  body: ledger_state.WithdrawalBody,
  claimed_valid: Bool,
  output_present: Bool,
  core_valid: Bool,
  cardano_value_size: Int,
) -> step_03.State {
  step_03.State {
    challenged_header_hash: #"14141414141414141414141414141414141414141414141414141414",
    withdrawal_id: body.l2_outref,
    withdrawal_body_hash: utils.serialise_and_hash_32(body),
    claimed_valid,
    output_present,
    core_valid,
    cardano_value_size,
  }
}

test accepts_valid_withdrawal_marked_invalid() {
  let body = fixture_body_v1(1_000_000)
  let result = establish_mistag_v1(
    fixture_step_03_state(body, False, True, True, 5),
    body,
  )
  and {
    !result.claimed_valid,
    result.actual_valid,
  }
}

test accepts_unpayable_withdrawal_marked_valid() {
  let body = fixture_body_v1(1)
  let result = establish_mistag_v1(
    fixture_step_03_state(body, True, True, True, 5),
    body,
  )
  and {
    result.claimed_valid,
    !result.actual_valid,
  }
}

test refuses_honestly_tagged_valid_withdrawal() fail {
  let body = fixture_body_v1(1_000_000)
  establish_mistag_v1(fixture_step_03_state(body, True, True, True, 5), body).actual_valid
}

test refuses_honestly_tagged_unpayable_withdrawal() fail {
  let body = fixture_body_v1(1)
  establish_mistag_v1(fixture_step_03_state(body, False, True, True, 5), body).actual_valid
}

test refuses_substituted_body_opening() fail {
  let authentic = fixture_body_v1(1_000_000)
  establish_mistag_v1(
    fixture_step_03_state(authentic, False, True, True, 5),
    fixture_body_v1(999_999),
  ).actual_valid
}
