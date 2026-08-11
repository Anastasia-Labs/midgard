//// ## DA Hash/Preimage Fraud Proof — step 02 (GOAL_SPEC.md Q44)
////
//// Concludes the proof that the leaf carried through step 01 breaks
//// hash/preimage correspondence: the key the block committed it under is not
//// the canonical native-V1 transaction id of the leaf value itself.

use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/da_hash_preimage/rule
use midgard/fraud_proofs/da_hash_preimage/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  computation_thread_token_policy_id: PolicyId,
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
      ct.Continue(Args {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced at the
        //    fraud proof's spending address.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data
          <- finalize(
          computation_thread_token_policy_id,
          fraud_proof_token_policy_id,
          fraud_proof_token_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_out_ref,
          inputs,
          outputs,
          redeemers,
        )

        expect
            Some(State {
              committed_tx_id,
              derived_tx_id,
              committed_leaf_byte_count,
            })
          = m_input_state_data

        // 2. The committed leaf must violate hash/preimage correspondence:
        //    either it is too short to carry the canonical frame at all, or
        //    the id its own body preimage hashes to is not the key the block
        //    committed it under.
        expect
          rule.is_da_hash_preimage_violation_v1(
            committed_tx_id,
            derived_tx_id,
            committed_leaf_byte_count,
          )

        // Done.
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

/// Largest canonical compact transaction the Cardano boundary admits (296
/// spend redeemers). GENERATED from the same boundary vector as
/// `lib/midgard/fraud-proofs/native-tx.max-redeemers.test.ak`, not copied out of
/// it:
///
///   producer: pnpm --dir demo/midgard-validation run fixtures:ordered-collection-boundary-aiken:sync
const maximum_cardano_compact_cbor =
  #"84018c5820d2de13bbfffe80e6db054d1577456ea75b28dea66b6aaed899d3dd4c54faecab582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820a7774afe7dd2957973cf90d95c0f9d3a56bb72356a4d2e3cd57e11654505c3801a000de2ec2020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820e650a24c14c0e6a48877805b4185f8ff2ee711e964e6aa63ce05c29ddeb1bd26582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820fe9cbd2aa7dde86cac94b8047c6ba073e1ea7ffbcbf6a939f83b6eb8466c922600"

const maximum_cardano_transaction_id =
  #"f5f996b9433cd5b58e08f0f6a65de8ab93b93c7d71a9d965166871deb0f72605"

fn finalize_datum(state: State) -> Datum {
  ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(state),
  }
}

fn run_finalize(state: State) -> Bool {
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(finalize_datum(state)),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

/// State exactly as step 01 derives it for the given committed pair.
fn state_for(committed_key: ByteArray, leaf_value: ByteArray) -> State {
  State {
    committed_tx_id: committed_key,
    derived_tx_id: rule.derive_committed_leaf_tx_id_v1(leaf_value),
    committed_leaf_byte_count: bytearray.length(leaf_value),
  }
}

/// Positive selector: a canonical transaction committed under a foreign key
/// finalizes the proof.
test da_hash_preimage_step_02_convicts_a_miskeyed_committed_leaf() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_finalize(state_for(foreign_committed_key, block.compact_cbor))
}

/// Positive selector: a committed leaf that is not a transaction at all — and
/// that no other family could ever decode — is convicted here.
test da_hash_preimage_step_02_convicts_a_non_transaction_committed_leaf() {
  run_finalize(state_for(foreign_committed_key, #"deadbeef"))
}

/// Positive selector: a leaf shorter than the canonical frame is convicted on
/// the underframed branch even if its clamped derivation were to collide.
test da_hash_preimage_step_02_convicts_an_underframed_committed_leaf() {
  let underframed = #"deadbeef"
  run_finalize(
    State {
      committed_tx_id: rule.derive_committed_leaf_tx_id_v1(underframed),
      derived_tx_id: rule.derive_committed_leaf_tx_id_v1(underframed),
      committed_leaf_byte_count: bytearray.length(underframed),
    },
  )
}

/// Valid-block negative selector: the honest committed pair of a valid block
/// derives the very key it is committed under, so the challenge cannot
/// finalize.
test da_hash_preimage_step_02_rejects_valid_block_challenge() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  run_finalize(state_for(valid_block.tx_id, valid_block.compact_cbor))
}

/// Maximum/adversarial proof-fit negative: the largest canonical compact
/// transaction the Cardano boundary admits is honestly keyed, so it cannot be
/// convicted either.
test da_hash_preimage_step_02_rejects_the_maximum_valid_transaction() fail {
  run_finalize(
    state_for(maximum_cardano_transaction_id, maximum_cardano_compact_cbor),
  )
}

/// Maximum/adversarial proof-fit positive: the same maximum transaction
/// committed under a foreign key is convicted, so the family's proof fits at
/// the largest committed leaf the boundary admits.
test da_hash_preimage_step_02_convicts_the_maximum_transaction_miskeyed() {
  run_finalize(state_for(foreign_committed_key, maximum_cardano_compact_cbor))
}

/// The rule predicate itself: equal ids at a framed length are never a
/// violation, unequal ids always are.
test da_hash_preimage_rule_predicate_is_exact() {
  let a = #"1111111111111111111111111111111111111111111111111111111111111111"
  let b = #"2222222222222222222222222222222222222222222222222222222222222222"
  and {
    !rule.is_da_hash_preimage_violation_v1(
      a,
      a,
      rule.compact_v1_frame_byte_count,
    ),
    rule.is_da_hash_preimage_violation_v1(
      a,
      b,
      rule.compact_v1_frame_byte_count,
    ),
    rule.is_da_hash_preimage_violation_v1(
      a,
      a,
      rule.compact_v1_frame_byte_count - 1,
    ),
  }
}
