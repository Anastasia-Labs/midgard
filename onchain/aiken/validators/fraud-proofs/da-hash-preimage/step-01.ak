//// ## DA Hash/Preimage Fraud Proof — step 01 (GOAL_SPEC.md Q44)
////
//// Binds one raw `(key, value)` leaf of the fraudulent block's committed
//// `transactions_root` and forwards the total verdict step 02 adjudicates.
//// Unlike every normal native family this step must not run the source-codec
//// precondition: malformed source bytes and mismatched ids are precisely the
//// properties under dispute.

use aiken/cbor
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, pass_committed_transactions_leaf_to_next_step,
}
use midgard/fraud_proofs/da_hash_preimage/rule
use midgard/fraud_proofs/da_hash_preimage/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/da_hash_preimage/step_02
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/fraud_proofs/native_tx/compact as native_compact
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/ledger_state

validator main(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(tx_inclusion_args) -> {
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            _m_input_state_data,
            output_script_hash,
            output_state_data,
            _header,
            committed_tx_id,
            committed_leaf_value
          <- pass_committed_transactions_leaf_to_next_step(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          tx_inclusion_args,
          own_out_ref,
          tx,
        )

        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          verdict: rule.adjudicate_committed_source_leaf_v1(
            committed_tx_id,
            committed_leaf_value,
          ),
        }
        expect output_state_data == expected_output_state
        True
      }
    }
  }

  else(_) {
    fail
  }
}

const foreign_committed_key =
  #"9999999999999999999999999999999999999999999999999999999999999999"

fn source_cbor(
  tx_id: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
) -> ByteArray {
  cbor.serialise(
    ledger_state.L2TransactionSourceV1 {
      tx_id,
      source: ledger_state.NativeTxProofSourceV1 {
        compact_cbor,
        witness_set_compact_cbor,
        field_preimage_lengths_cbor,
      },
    },
  )
}

/// Alternate, semantically equivalent definite-list spelling of the source
/// Data. `cbor.deserialise` accepts it, while exact source re-encoding rejects
/// it because the consensus wire is the Aiken record encoding above.
fn noncanonical_source_cbor(
  tx_id: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
) -> ByteArray {
  #"d87982"
    |> bytearray.concat(encode_definite_bytes(tx_id))
    |> bytearray.concat(#"d87983")
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
}

fn block_with_source(
  base: fixture.NativeBlockFixtureV1,
  committed_key: ByteArray,
  embedded_tx_id: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
) -> fixture.NativeBlockFixtureV1 {
  fixture.committed_leaf_block_fixture_v1(
    base,
    committed_key,
    source_cbor(
      embedded_tx_id,
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ),
  )
}

fn expected_state(block: fixture.NativeBlockFixtureV1) -> Data {
  let state: step_02.State = step_02.State {
    verdict: rule.adjudicate_committed_source_leaf_v1(
      block.tx_id,
      block.source_cbor,
    ),
  }
  let state_data: Data = state
  state_data
}

fn run_step_01(block: fixture.NativeBlockFixtureV1, output_state: Data) -> Bool {
  let l1_tx = fixture.continue_step_l1_tx_v1(
    block,
    output_state,
    block.transactions_phas_root,
    block.tx_id,
    block.source_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(fixture.native_inclusion_args_v1(block)),
    fixture.own_out_ref(),
    l1_tx,
  )
}

test da_hash_preimage_step_01_binds_a_miskeyed_source_leaf() {
  let block = fixture.miskeyed_block_fixture_v1(
    fixture.valid_native_tx_v1(),
    foreign_committed_key,
  )
  and {
    rule.adjudicate_committed_source_leaf_v1(block.tx_id, block.source_cbor) == rule.KeyMismatch,
    run_step_01(block, expected_state(block)),
  }
}

test da_hash_preimage_step_01_binds_a_malformed_source_leaf() {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let block = fixture.committed_leaf_block_fixture_v1(
    base,
    foreign_committed_key,
    #"deadbeef",
  )
  and {
    rule.adjudicate_committed_source_leaf_v1(block.tx_id, block.source_cbor) == rule.MalformedSource,
    run_step_01(block, expected_state(block)),
  }
}

test da_hash_preimage_step_01_binds_a_noncanonical_source_leaf() {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  expect Some(source) = rule.inspect_source_envelope_v1(base.source_cbor)
  let block = fixture.committed_leaf_block_fixture_v1(
    base,
    base.tx_id,
    noncanonical_source_cbor(
      base.tx_id,
      base.compact_cbor,
      source.witness_set_compact_cbor,
      source.field_preimage_lengths_cbor,
    ),
  )
  and {
    rule.adjudicate_committed_source_leaf_v1(block.tx_id, block.source_cbor) == rule.MalformedSource,
    run_step_01(block, expected_state(block)),
  }
}

test da_hash_preimage_step_01_binds_a_malformed_proof_source_leaf() {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  expect Some(source) = rule.inspect_source_envelope_v1(base.source_cbor)
  let block = block_with_source(
    base,
    base.tx_id,
    base.tx_id,
    base.compact_cbor,
    #"deadbeef",
    source.field_preimage_lengths_cbor,
  )
  and {
    rule.adjudicate_committed_source_leaf_v1(block.tx_id, block.source_cbor) == rule.MalformedProofSource,
    run_step_01(block, expected_state(block)),
  }
}

test da_hash_preimage_step_01_binds_a_forged_witness_set_hash() {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  expect Some(source) = rule.inspect_source_envelope_v1(base.source_cbor)
  let forged_witness = native_compact.encode_native_tx_witness_set_compact(
    native_types.NativeTxWitnessSetCompact {
      addr_tx_wits_hash: foreign_committed_key,
      script_tx_wits_hash: foreign_committed_key,
      redeemer_tx_wits_hash: foreign_committed_key,
    },
  )
  let block = block_with_source(
    base,
    base.tx_id,
    base.tx_id,
    base.compact_cbor,
    forged_witness,
    source.field_preimage_lengths_cbor,
  )
  and {
    rule.adjudicate_committed_source_leaf_v1(block.tx_id, block.source_cbor) == rule.MalformedProofSource,
    run_step_01(block, expected_state(block)),
  }
}

test da_hash_preimage_step_01_binds_malformed_field_lengths() {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  expect Some(source) = rule.inspect_source_envelope_v1(base.source_cbor)
  let block = block_with_source(
    base,
    base.tx_id,
    base.tx_id,
    base.compact_cbor,
    source.witness_set_compact_cbor,
    #"deadbeef",
  )
  and {
    rule.adjudicate_committed_source_leaf_v1(block.tx_id, block.source_cbor) == rule.MalformedProofSource,
    run_step_01(block, expected_state(block)),
  }
}

test da_hash_preimage_step_01_binds_a_derived_id_mismatch() {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  expect Some(source) = rule.inspect_source_envelope_v1(base.source_cbor)
  let block = block_with_source(
    base,
    foreign_committed_key,
    foreign_committed_key,
    base.compact_cbor,
    source.witness_set_compact_cbor,
    source.field_preimage_lengths_cbor,
  )
  and {
    rule.adjudicate_committed_source_leaf_v1(block.tx_id, block.source_cbor) == rule.DerivedIdMismatch,
    run_step_01(block, expected_state(block)),
  }
}

test da_hash_preimage_step_01_binds_a_valid_leaf_without_convicting() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  and {
    rule.adjudicate_committed_source_leaf_v1(block.tx_id, block.source_cbor) == rule.NoViolation,
    run_step_01(block, expected_state(block)),
  }
}

// Exact signed-cardano maximum fixture generated by
// `fixtures:ordered-collection-boundary-aiken:sync`. Dynamic field bytes do not
// travel in the source leaf; their exact lengths and commitments do.
const maximum_cardano_transaction_id =
  #"7b4e4657e0083544359f4398fb092c482766220cd53ad99b598239297d1e9813"

const maximum_cardano_compact_cbor =
  #"84018c58202d56d604247c43792618a75b77864f8a6c6d35b9b5a66d25b944476d6930588e582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820095c12f5790acc50dbbf52c0b47fe4ebd1dfd9ab308b14701543d6d4d78a06ae1a000d59492020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820e2d5bb3b4c4475d516516e5396ec041b553ada06379a53f665b47e1485e0451f582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820ad12ff89400f2f7975c77231241032e6a7bf49d0f2ab388425b3de0cefef003000"

const maximum_cardano_witness_set_compact_cbor =
  #"835820689afcab7a4406fa8da9a4f97b325f34458bd2114d6b2ae9eb357e681acc0e97582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"

const maximum_cardano_field_preimage_lengths_cbor =
  #"89182901183001190e8a01011931e601"

test da_hash_preimage_maximum_compact_is_canonical() {
  rule.inspect_compact_v1(maximum_cardano_compact_cbor) != None
}

test da_hash_preimage_maximum_witness_set_is_canonical() {
  rule.witness_set_is_canonical_v1(maximum_cardano_witness_set_compact_cbor)
}

test da_hash_preimage_maximum_lengths_are_canonical() {
  rule.field_lengths_are_canonical_v1(
    maximum_cardano_field_preimage_lengths_cbor,
  )
}

test da_hash_preimage_step_01_accepts_the_maximum_valid_source_leaf() {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let block = block_with_source(
    base,
    maximum_cardano_transaction_id,
    maximum_cardano_transaction_id,
    maximum_cardano_compact_cbor,
    maximum_cardano_witness_set_compact_cbor,
    maximum_cardano_field_preimage_lengths_cbor,
  )
  and {
    rule.adjudicate_committed_source_leaf_v1(block.tx_id, block.source_cbor) == rule.NoViolation,
    run_step_01(block, expected_state(block)),
  }
}

test da_hash_preimage_step_01_rejects_a_fabricated_verdict() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let forged: step_02.State = step_02.State { verdict: rule.KeyMismatch }
  let forged_data: Data = forged
  run_step_01(block, forged_data)
}

test da_hash_preimage_step_01_rejects_a_forged_transactions_root() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged_block = fixture.miskeyed_block_fixture_v1(
    fixture.valid_native_tx_v1(),
    foreign_committed_key,
  )
  let claimed_block = fixture.NativeBlockFixtureV1 {
    ..forged_block,
    header: valid_block.header,
    header_hash: valid_block.header_hash,
  }
  run_step_01(claimed_block, expected_state(claimed_block))
}
