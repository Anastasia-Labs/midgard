//// ## DA Hash/Preimage Fraud Proof — step 01 (GOAL_SPEC.md Q44)
////
//// Binds one raw `(key, value)` leaf of the fraudulent block's committed
//// `transactions_root` and forwards the evidence triple step 02 adjudicates:
//// the committed key, the canonical native-V1 transaction id the leaf value
//// itself commits to, and the leaf's byte length.
////
//// Unlike every other native family this step must **not** run the native
//// codec precondition (`verify_native_tx_compact_cbor_v1`, which requires
//// `derived_id == key`): that precondition is precisely the property under
//// dispute. See `lib/midgard/fraud-proofs/da-hash-preimage/rule.ak` for the
//// rule, the violation and the decoder-free soundness argument.

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
use midgard/fraud_proofs/native_tx/compact as native_compact
use midgard/fraud_proofs/native_tx/types.{native_tx_version_v1}

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
        // 1. The claimed leaf must be a genuine leaf of this block's counted
        //    `transactions_root`, under the block registered in the state
        //    queue for this computation thread.
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

        // 2. The UTxO must be produced at the next step's script hash carrying
        //    the exact evidence triple. Every component is derived here from
        //    authenticated bytes, so step 02 adjudicates committed values only.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          committed_tx_id,
          derived_tx_id: rule.derive_committed_leaf_tx_id_v1(
            committed_leaf_value,
          ),
          committed_leaf_byte_count: bytearray.length(committed_leaf_value),
        }
        expect output_state_data == expected_output_state

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

// ## Framing constants pinned against the real canonical encoder
//
// The decoder-free derivation is sound only if an honest leaf's body preimage
// is exactly `slice(value, head, len - head - tail)`. These selectors pin both
// constants against `encode_native_tx_compact_v1` itself.

/// The canonical encoder frames every compact transaction with exactly
/// `rule.compact_v1_frame_byte_count` bytes around its body preimage.
test da_hash_preimage_rule_frame_matches_the_canonical_encoder() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let body_cbor = native_compact.encode_native_tx_body_compact(
    block.compact.body,
  )
  bytearray.length(block.compact_cbor) == bytearray.length(body_cbor) + rule.compact_v1_frame_byte_count
}

/// The framed slice of an honest leaf is byte-identical to the encoder's body
/// preimage, so the derived id is the genuine transaction id.
test da_hash_preimage_rule_recovers_the_committed_body_preimage() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  and {
    rule.committed_leaf_body_cbor_v1(block.compact_cbor) == native_compact.encode_native_tx_body_compact(
      block.compact.body,
    ),
    rule.derive_committed_leaf_tx_id_v1(block.compact_cbor) == block.tx_id,
  }
}

/// Maximum/adversarial proof-fit: the largest canonical compact transaction
/// the Cardano boundary admits (296 spend redeemers, derived by
/// `demo/midgard-validation/tests/ordered-collection-redeemer-boundary-v1.test.ts`
/// and pinned in `lib/midgard/fraud-proofs/native-tx.max-redeemers.test.ak`).
/// The framing derivation must recover its transaction id unchanged.
const maximum_cardano_compact_cbor =
  #"84018c5820ecd24706a07b755bbdf8e6f0e55bc6ba08f18408528bb3dc00a4af3e1a88c2225820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f58205581fd909e08e4dea9336a8928f0d2731f2f31e1ce31a16cb1f5b2ebc2c9dccf1a000de2ec20205820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d5820e650a24c14c0e6a48877805b4185f8ff2ee711e964e6aa63ce05c29ddeb1bd26582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820723dfb187dd11e5d8b44a3ebc9b44da9037807f5ff794e07f962798509df1f6100"

const maximum_cardano_transaction_id =
  #"bb9eefcba3b233b08c3969d7e72dd6911353f5ae4fb0f78dd4fa94bec151e203"

test da_hash_preimage_rule_holds_at_the_maximum_cardano_transaction() {
  rule.derive_committed_leaf_tx_id_v1(maximum_cardano_compact_cbor) == maximum_cardano_transaction_id
}

/// The version byte the framing assumes is the single-byte canonical V1
/// encoding, so the head constant cannot silently drift.
test da_hash_preimage_rule_head_covers_the_canonical_version_byte() {
  and {
    native_tx_version_v1 <= 23,
    rule.compact_v1_head_byte_count == 2,
  }
}

// ## Step-01 binding selectors

const foreign_committed_key =
  #"9999999999999999999999999999999999999999999999999999999999999999"

fn expected_state(block: fixture.NativeBlockFixtureV1) -> Data {
  let state: step_02.State = step_02.State {
    committed_tx_id: block.tx_id,
    derived_tx_id: rule.derive_committed_leaf_tx_id_v1(block.compact_cbor),
    committed_leaf_byte_count: bytearray.length(block.compact_cbor),
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
    block.compact_cbor,
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

/// Positive binding selector: a leaf committed under a key that is not its own
/// transaction id binds to the block's counted `transactions_root` and
/// forwards the mismatching evidence triple.
test da_hash_preimage_step_01_binds_a_miskeyed_committed_leaf() {
  let block = fixture.miskeyed_block_fixture_v1(
    fixture.valid_native_tx_v1(),
    foreign_committed_key,
  )
  run_step_01(block, expected_state(block))
}

/// Positive binding selector: an arbitrary non-transaction byte string
/// committed as a leaf binds just as well — the step never decodes it.
test da_hash_preimage_step_01_binds_a_non_transaction_committed_leaf() {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let block = fixture.committed_leaf_block_fixture_v1(
    base,
    foreign_committed_key,
    #"deadbeef",
  )
  run_step_01(block, expected_state(block))
}

/// A well-formed, correctly keyed leaf of a valid block also binds — step 01
/// is evidence carriage only. The forwarded triple then makes step 02
/// unfinalizable (see `da_hash_preimage_step_02_rejects_valid_block_challenge`).
test da_hash_preimage_step_01_binds_a_valid_block_leaf_without_convicting() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_step_01(block, expected_state(block))
}

/// A prover cannot forward a fabricated derived id: the output state is
/// recomputed from the authenticated leaf bytes.
test da_hash_preimage_step_01_rejects_a_fabricated_derived_id() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let forged: step_02.State = step_02.State {
    committed_tx_id: block.tx_id,
    derived_tx_id: foreign_committed_key,
    committed_leaf_byte_count: bytearray.length(block.compact_cbor),
  }
  let forged_data: Data = forged
  run_step_01(block, forged_data)
}

/// A prover cannot forward a fabricated leaf byte count either.
test da_hash_preimage_step_01_rejects_a_fabricated_leaf_byte_count() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let forged: step_02.State = step_02.State {
    committed_tx_id: block.tx_id,
    derived_tx_id: rule.derive_committed_leaf_tx_id_v1(block.compact_cbor),
    committed_leaf_byte_count: 0,
  }
  let forged_data: Data = forged
  run_step_01(block, forged_data)
}

/// Valid-block negative selector: evidence manufactured under a raw
/// transactions root the header never committed cannot bind, so a valid block
/// cannot be challenged with a fabricated leaf.
test da_hash_preimage_step_01_rejects_a_forged_transactions_root() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged_block = fixture.miskeyed_block_fixture_v1(
    fixture.valid_native_tx_v1(),
    foreign_committed_key,
  )
  // The prover claims the fabricated miskeyed leaf under the valid block's
  // header by supplying the fabricated raw root; the counted-root unwrap must
  // reject it.
  let claimed_block = fixture.NativeBlockFixtureV1 {
    ..forged_block,
    header: valid_block.header,
    header_hash: valid_block.header_hash,
  }
  run_step_01(claimed_block, expected_state(claimed_block))
}
