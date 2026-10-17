// Differential regression for the source-final publication optimization. The
// reference is the predecessor's total canonicality screen and Data decoder.
use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/transaction.{OutputReference}
use midgard/canonical_plutus_data_v1
use midgard/fraud_proofs/transition_trace/proof
use midgard/ledger_state

fn previous_key_decoder(bytes: ByteArray) -> Pair<Int, ByteArray> {
  if !canonical_plutus_data_v1.is_materialisable_plutus_data_v1(bytes) {
    Pair(0, #"")
  } else {
    when cbor.deserialise(bytes) is {
      None -> Pair(0, #"")
      Some(data) ->
        if builtin.choose_data(data, True, False, False, False, False) {
          let Pair(tag, fields) = builtin.un_constr_data(data)
          when fields is {
            [item] ->
              if tag == 2 && builtin.choose_data(
                item,
                False,
                False,
                False,
                False,
                True,
              ) {
                Pair(2, builtin.un_b_data(item))
              } else if tag == 1 && builtin.choose_data(
                item,
                True,
                False,
                False,
                False,
                False,
              ) {
                let Pair(inner_tag, inner_fields) = builtin.un_constr_data(item)
                when inner_fields is {
                  [txid, index] ->
                    if and {
                      inner_tag == 0,
                      builtin.choose_data(
                        txid,
                        False,
                        False,
                        False,
                        False,
                        True,
                      ),
                      builtin.choose_data(
                        index,
                        False,
                        False,
                        False,
                        True,
                        False,
                      ),
                    } {
                      Pair(1, cbor.serialise(item))
                    } else {
                      Pair(0, #"")
                    }
                  _ -> Pair(0, #"")
                }
              } else {
                Pair(0, #"")
              }
            _ -> Pair(0, #"")
          }
        } else {
          Pair(0, #"")
        }
    }
  }
}

fn payload(length: Int) -> ByteArray {
  if length <= 0 {
    #""
  } else {
    bytearray.push(payload(length - 1), 170)
  }
}

fn l2_key(bytes: ByteArray) -> ByteArray {
  cbor.serialise(ledger_state.L2TransactionEventKey { tx_id: bytes })
}

fn forced_key(bytes: ByteArray, index: Int) -> ByteArray {
  cbor.serialise(
    ledger_state.ForcedTransactionEventKey {
      tx_order_id: OutputReference {
        transaction_id: bytes,
        output_index: index,
      },
    },
  )
}

test transaction_key_decoder_keeps_byte_string_chunk_boundaries() {
  list.all(
    [0, 1, 23, 24, 63, 64, 65, 127, 128, 129],
    fn(length) {
      let bytes = payload(length)
      let l2 = l2_key(bytes)
      let forced = forced_key(bytes, 0)
      and {
        proof.validation_run_source_key(l2) == Pair(2, bytes),
        proof.validation_run_source_key(l2) == previous_key_decoder(l2),
        proof.validation_run_source_key(forced) == previous_key_decoder(forced),
      }
    },
  )
}

test transaction_key_decoder_keeps_signed_integer_widths() {
  list.all(
    [
      0,
      1,
      23,
      24,
      255,
      256,
      65535,
      65536,
      4294967295,
      4294967296,
      18446744073709551615,
      -1,
      -24,
      -25,
      -256,
      -257,
      -65536,
      -65537,
      -4294967296,
      -4294967297,
      -18446744073709551616,
    ],
    fn(index) {
      let bytes = forced_key(payload(32), index)
      let expected = cbor.serialise(
        OutputReference { transaction_id: payload(32), output_index: index },
      )
      and {
        proof.validation_run_source_key(bytes) == Pair(1, expected),
        proof.validation_run_source_key(bytes) == previous_key_decoder(bytes),
      }
    },
  )
}

test transaction_key_decoder_keeps_foreign_and_malformed_classification() {
  let cases = [
    #"",
    #"00",
    #"ff",
    #"c24100",
    #"c249010000000000000000",
    #"d87b9f40ffff",
    #"d87b9f4000ff",
    #"d87b8140",
    #"d87b9f5800ff",
    #"d87b9f5f4040ffff",
    #"d87b9f5f4141ffff",
    #"d87b9f5f5840",
    #"d87a9fd8799f401800ffff",
    #"d87a9fd8799f4040ffff",
    #"d87a9fd8799f40c249010000000000000000ffff",
    #"d87a9fd8799f40d86682188080ffff",
    #"d87a9fd8799f400000ffff",
    #"d8799fd8799f4000ffff",
    #"d87c9f40ff",
    #"d87b80",
    builtin.append_bytearray(l2_key(payload(32)), #"00"),
    builtin.append_bytearray(
      #"d87b9f5841",
      builtin.append_bytearray(payload(65), #"ff"),
    ),
    builtin.append_bytearray(
      #"d87b9f5f5840",
      builtin.append_bytearray(payload(64), #"ffff"),
    ),
  ]
  list.all(
    cases,
    fn(bytes) {
      and {
        proof.validation_run_source_key(bytes) == Pair(0, #""),
        proof.validation_run_source_key(bytes) == previous_key_decoder(bytes),
      }
    },
  )
}
