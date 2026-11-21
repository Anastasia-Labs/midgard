//// `step_03.serialise_withdrawal_info_v1` against `cbor.serialise` on the
//// whole `WithdrawalInfo`: the same bytes for every info, including infos read
//// back from Data the way a carried payload is.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/fuzz
use aiken/primitive/bytearray
use cardano/address
use cardano/transaction.{DatumHash, InlineDatum, NoDatum, OutputReference}
use midgard/fraud_proofs/withdrawal_mistag/step_03
use midgard/ledger_state

fn bytes_fuzzer() -> Fuzzer<ByteArray> {
  fuzz.either3(
    fuzz.bytearray_between(0, 4),
    fuzz.bytearray_between(28, 33),
    fuzz.bytearray_between(60, 140),
  )
}

fn quantity_fuzzer() -> Fuzzer<Int> {
  fuzz.either3(
    fuzz.int_between(-30, 30),
    fuzz.int_between(-4294967296, 4294967296),
    fuzz.map(
      fuzz.int_between(0, 4),
      fn(i) { 18446744073709551616 * ( i + 1 ) - 2 + i },
    ),
  )
}

fn value_fuzzer() -> Fuzzer<ledger_state.MidgardValue> {
  fuzz.list_between(
    fuzz.both(
      bytes_fuzzer(),
      fuzz.list_between(fuzz.both(bytes_fuzzer(), quantity_fuzzer()), 0, 4),
    )
      |> fuzz.map(
          fn(policy) {
            let (policy_id, tokens) = policy
            Pair(policy_id, list.map(tokens, fn(t) { Pair(t.1st, t.2nd) }))
          },
        ),
    0,
    3,
  )
}

fn datum_fuzzer() -> Fuzzer<transaction.Datum> {
  let kind <- fuzz.and_then(fuzz.int_between(0, 3))
  if kind == 0 {
    fuzz.constant(NoDatum)
  } else if kind == 1 {
    fuzz.map(bytes_fuzzer(), DatumHash)
  } else if kind == 2 {
    fuzz.map(bytes_fuzzer(), fn(b) { InlineDatum(builtin.b_data(b)) })
  } else {
    fuzz.map(
      fuzz.list_between(quantity_fuzzer(), 0, 5),
      fn(xs) { InlineDatum(builtin.list_data(list.map(xs, builtin.i_data))) },
    )
  }
}

fn validity_fuzzer() -> Fuzzer<ledger_state.WithdrawalValidity> {
  let kind <- fuzz.and_then(fuzz.int_between(0, 2))
  if kind == 0 {
    fuzz.constant(ledger_state.WithdrawalIsValid)
  } else if kind == 1 {
    fuzz.constant(ledger_state.NonExistentWithdrawalUtxo)
  } else {
    fuzz.map(
      bytes_fuzzer(),
      fn(l2_tx_id) { ledger_state.SpentWithdrawalUtxo { l2_tx_id } },
    )
  }
}

fn info_fuzzer() -> Fuzzer<ledger_state.WithdrawalInfo> {
  let transaction_id <- fuzz.and_then(bytes_fuzzer())
  let output_index <- fuzz.and_then(fuzz.int_between(0, 70000))
  let l2_owner <- fuzz.and_then(bytes_fuzzer())
  let l2_value <- fuzz.and_then(value_fuzzer())
  let key <- fuzz.and_then(bytes_fuzzer())
  let script <- fuzz.and_then(fuzz.bool())
  let l1_datum <- fuzz.and_then(datum_fuzzer())
  let verification_key <- fuzz.and_then(bytes_fuzzer())
  let signature <- fuzz.and_then(bytes_fuzzer())
  let validity <- fuzz.and_then(validity_fuzzer())
  fuzz.constant(
    ledger_state.WithdrawalInfo {
      body: ledger_state.WithdrawalBody {
        l2_outref: OutputReference { transaction_id, output_index },
        l2_owner,
        l2_value,
        l1_address: if script {
          address.from_script(key)
        } else {
          address.from_verification_key(key)
        },
        l1_datum,
      },
      signature: Pair(verification_key, signature),
      validity,
    },
  )
}

fn agrees(info: ledger_state.WithdrawalInfo) -> Bool {
  step_03.serialise_withdrawal_info_v1(info, cbor.serialise(info.body)) == cbor.serialise(
    info,
  )
}

test serialise_withdrawal_info_matches_the_whole_serialisation(
  info via info_fuzzer(),
) {
  agrees(info)
}

test serialise_withdrawal_info_matches_after_a_data_round_trip(
  info via info_fuzzer(),
) {
  let data: Data = info
  expect read_back: ledger_state.WithdrawalInfo = builtin.constr_data(
    0,
    builtin.unconstr_fields(data),
  )
  agrees(read_back)
}

fn maximum_value_name(i: Int) -> ByteArray {
  if i == 0 {
    #""
  } else if i <= 256 {
    bytearray.push(#"", i - 1)
  } else {
    bytearray.from_int_big_endian(i - 257, 2)
  }
}

test serialise_withdrawal_info_matches_at_the_maximum_value() {
  let tokens = list.map(
    list.range(0, 1303),
    fn(i) {
      Pair(
        maximum_value_name(i),
        if i == 1303 {
          256
        } else {
          1
        },
      )
    },
  )
  let info = ledger_state.WithdrawalInfo {
    body: ledger_state.WithdrawalBody {
      l2_outref: OutputReference {
        transaction_id: #"4242424242424242424242424242424242424242424242424242424242424242",
        output_index: 0,
      },
      l2_owner: #"11111111111111111111111111111111111111111111111111111111",
      l2_value: [
        Pair(#"", [Pair(#"", 100000000)]),
        Pair(
          #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
          tokens,
        ),
      ],
      l1_address: address.from_verification_key(
        #"11111111111111111111111111111111111111111111111111111111",
      ),
      l1_datum: NoDatum,
    },
    signature: Pair(
      #"0000000000000000000000000000000000000000000000000000000000000000",
      #"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    ),
    validity: ledger_state.SpentWithdrawalUtxo {
      l2_tx_id: #"0101010101010101010101010101010101010101010101010101010101010101",
    },
  }
  agrees(info)
}
