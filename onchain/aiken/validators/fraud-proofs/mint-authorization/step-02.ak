//// ## Mint-Authorization Fault Proof — Step 02 (W-C14)
////
//// Binds the disputed block's header to the thread NFT, opens the committed
//// claim (the transition step's `pre_utxos_root`, via the same
//// `verify_committed_pre_state_v1` the decoding family uses), and reads the
//// claimed policy id off the committed field-5 item at the accused ordinal.
////
//// The policy id is READ, never asserted: the §5.6 item walk enforces the
//// 28-byte id, a non-empty asset map and non-zero quantities, so a zero-net
//// entry or an empty mint cannot even name a claim — exactly the machine's
//// `script_sources_begin_mint_policy` purpose domain (one purpose per
//// canonical field-5 item, burns included).

use aiken/builtin
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, mint_field_index, opened_field_view,
}
use midgard/fraud_proofs/mint_authorization/engine
use midgard/fraud_proofs/mint_authorization/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/mint_authorization/step_03
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/engine as decoding_engine
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/preimages
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/ledger_state
use midgard/native_tx_field_access_v1.{
  encode_field_preimage, field_item_at, field_item_count,
}

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
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
        header,
        event_to_step_membership,
        transition_step_membership,
        policy_index,
        direction,
        mint_opening,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Generic thread-advancement validations must pass.
        let
            _own_script_hash,
            computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data,
            output_script_hash,
            output_state_data
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )

        expect
            Some(State {
              bad_tx_id,
              bad_tx_witness_set_hash,
              validity_interval_start,
              validity_interval_end,
            })
          = m_input_state_data

        // 2. The header is the thread's: the NFT asset name is
        //    `category_id ‖ header_hash`, so nothing but the disputed
        //    block's header can pass. Every root opened below is this
        //    header's.
        let header_data: Data = header
        expect
          utils.serialise_and_hash_28(header_data) == bytearray.drop(
            computation_thread_token_asset_name,
            fraud_proof_catalogue.id_byte_count,
          )

        // 3. Open the committed claim: the transition step for this very
        //    transaction event, cross-checked under the header's counted
        //    roots. Its `pre_utxos_root` is the ledger the machine resolved
        //    this transaction's reference inputs against — direction A's
        //    step-04 consumes it.
        let event_key = ledger_state.L2TransactionEventKey { tx_id: bad_tx_id }
        let prior_ledger_root = decoding_engine.verify_committed_pre_state_v1(
          header,
          event_key,
          event_to_step_membership,
          transition_step_membership,
        )

        // 4. The claimed policy id is read off the committed field-5 item at
        //    the accused ordinal, through the §8.8 door. The §5.6 walk
        //    enforces canonicality — including non-zero quantities, so a
        //    zero-net entry can never seed a claim, while a burn (negative
        //    quantity) is a mint purpose like any other.
        let mint_view = opened_field_view(
          mint_opening,
          BodyAnchor { tx_id: bad_tx_id },
          mint_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        expect and {
            policy_index >= 0,
            policy_index < field_item_count(mint_view),
          }
        let policy_id = engine.policy_id_of_mint_item_v1(
          field_item_at(mint_view, policy_index),
        )

        // 5. The direction is fixed now, in this family's two-value domain.
        expect or {
            direction == engine.direction_script_absent,
            direction == engine.direction_script_unsatisfied,
          }

        // 6. The UTxO must be produced at next step's script hash with the
        //    bound claim.
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data = step_03.State {
          policy_id,
          direction,
          bad_tx_id,
          bad_tx_witness_set_hash,
          validity_interval_start,
          validity_interval_end,
          prior_ledger_root,
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

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)

/// A 28-byte policy id with no relation to any fixture script — the
/// direction-A accusation shape.
const accused_policy_id: ByteArray =
  #"42424242424242424242424242424242424242424242424242424242"

fn mint_of(policy_id: ByteArray) -> Data {
  let asset_pairs = [Pair(builtin.b_data(#"544f4b454e"), builtin.i_data(5))]
  builtin.map_data(
    [Pair(builtin.b_data(policy_id), builtin.map_data(asset_pairs))],
  )
}

fn mint_bad_tx(policy_id: ByteArray) -> native_types.MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  native_types.MidgardTransaction {
    ..base,
    body: native_types.MidgardTransactionBody {
      ..base.body,
      mint: mint_of(policy_id),
    },
  }
}

fn run_step_02_against(
  block: fixture.NativeBlockFixtureV1,
  carried_items: List<ByteArray>,
  policy_index: Int,
  direction: Int,
  expected_policy_id: ByteArray,
) -> Bool {
  let prior_root = tf.h32_c
  let claim = tf.thread_claim_v1(
    ledger_state.L2TransactionEventKey { tx_id: block.tx_id },
    ledger_state.L2Transaction,
    prior_root,
    None,
  )
  let asset_name = tf.header_asset_name_v1(claim.header)
  let expected_output_state: Data = step_03.State {
    policy_id: expected_policy_id,
    direction,
    bad_tx_id: block.tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
    validity_interval_start: block.compact.body.validity_interval_start,
    validity_interval_end: block.compact.body.validity_interval_end,
    prior_ledger_root: prior_root,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            bad_tx_id: block.tx_id,
            bad_tx_witness_set_hash: block.compact.witness_set_hash,
            validity_interval_start: block.compact.body.validity_interval_start,
            validity_interval_end: block.compact.body.validity_interval_end,
          },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        header: claim.header,
        event_to_step_membership: claim.event_to_step_membership,
        transition_step_membership: claim.transition_step_membership,
        policy_index,
        direction,
        mint_opening: fixture.inline_body_opening(
          block.compact_cbor,
          carried_items,
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

fn run_step_02(policy_index: Int, direction: Int) -> Bool {
  let tx = mint_bad_tx(accused_policy_id)
  let block = fixture.native_block_fixture_v1(tx)
  run_step_02_against(
    block,
    preimages.mint_policy_items(tx.body.mint),
    policy_index,
    direction,
    accused_policy_id,
  )
}

/// Positive selector, direction A: the committed policy id is read off the
/// committed mint item and forwarded with the claim.
test mint_authorization_step_02_reads_the_committed_policy_direction_a() {
  run_step_02(0, engine.direction_script_absent)
}

/// Positive selector, direction B: same read, other direction.
test mint_authorization_step_02_reads_the_committed_policy_direction_b() {
  run_step_02(0, engine.direction_script_unsatisfied)
}

/// The direction domain is exactly {0, 1}.
test mint_authorization_step_02_rejects_an_out_of_domain_direction() fail {
  run_step_02(0, 2)
}

/// A mint item the transaction never committed fails the door's
/// authenticate-once check — the prover cannot choose the policy id.
test mint_authorization_step_02_rejects_a_substituted_mint_preimage() fail {
  let tx = mint_bad_tx(accused_policy_id)
  let block = fixture.native_block_fixture_v1(tx)
  let foreign_policy = tf.h28_y
  run_step_02_against(
    block,
    preimages.mint_policy_items(mint_of(foreign_policy)),
    0,
    engine.direction_script_absent,
    foreign_policy,
  )
}

/// A transaction that mints nothing has no field-5 item to accuse: the
/// authenticated item count refuses the ordinal.
test mint_authorization_step_02_rejects_an_empty_mint() fail {
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(tx)
  run_step_02_against(
    block,
    [],
    0,
    engine.direction_script_absent,
    accused_policy_id,
  )
}

/// A committed zero-quantity entry is outside the machine's purpose domain
/// (§5.6 walks reject quantity 0), so the item-level walk refuses it — the
/// zero-net corner cannot seed a claim.
test mint_authorization_step_02_rejects_a_zero_quantity_entry() fail {
  // `[policy, {"A": 0}]` — canonical everywhere except the zero quantity.
  let bad_item = #"82"
    |> bytearray.concat(#"581c")
    |> bytearray.concat(accused_policy_id)
    |> bytearray.concat(#"a1414100")
  let preimage = encode_field_preimage([bad_item])
  let base = fixture.native_block_fixture_v1(mint_bad_tx(accused_policy_id))
  let block = fixture.committed_field_preimage_block_fixture_v1(
    base,
    5,
    preimage,
  )
  run_step_02_against(
    block,
    [bad_item],
    0,
    engine.direction_script_absent,
    accused_policy_id,
  )
}

/// A header other than the thread's fails the NFT asset-name binding.
test mint_authorization_step_02_rejects_a_foreign_header() fail {
  let tx = mint_bad_tx(accused_policy_id)
  let block = fixture.native_block_fixture_v1(tx)
  let prior_root = tf.h32_c
  let claim = tf.thread_claim_v1(
    ledger_state.L2TransactionEventKey { tx_id: block.tx_id },
    ledger_state.L2Transaction,
    prior_root,
    None,
  )
  let other_claim = tf.thread_claim_v1(
    ledger_state.L2TransactionEventKey { tx_id: block.tx_id },
    ledger_state.L2Transaction,
    tf.h32_d,
    None,
  )
  let asset_name = tf.header_asset_name_v1(claim.header)
  let expected_output_state: Data = step_03.State {
    policy_id: accused_policy_id,
    direction: engine.direction_script_absent,
    bad_tx_id: block.tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
    validity_interval_start: block.compact.body.validity_interval_start,
    validity_interval_end: block.compact.body.validity_interval_end,
    prior_ledger_root: tf.h32_d,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            bad_tx_id: block.tx_id,
            bad_tx_witness_set_hash: block.compact.witness_set_hash,
            validity_interval_start: block.compact.body.validity_interval_start,
            validity_interval_end: block.compact.body.validity_interval_end,
          },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        header: other_claim.header,
        event_to_step_membership: other_claim.event_to_step_membership,
        transition_step_membership: other_claim.transition_step_membership,
        policy_index: 0,
        direction: engine.direction_script_absent,
        mint_opening: fixture.inline_body_opening(
          block.compact_cbor,
          preimages.mint_policy_items(mint_of(accused_policy_id)),
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}
