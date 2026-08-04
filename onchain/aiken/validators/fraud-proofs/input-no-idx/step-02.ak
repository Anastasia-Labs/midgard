use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/bounded_collection_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/input_no_idx/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/input_no_idx/step_03
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTxInput,
}

validator main(
  step_03_validator_script_hash: ScriptHash,
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
        inputs_preimage,
        bad_inputs_index,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
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

        expect Some(State { verified_tx_inputs_hash }) = m_input_state_data

        // 2. Provided inputs preimage must correspond correctly to the passed
        //    along hash. Each item is the node's canonical `TransactionInput`
        //    CBOR under the consensus-ordered spend-inputs field (0), not
        //    `cbor.serialise` of an Aiken `OutputReference` list.
        expect
          bounded_collection_v1.from_items(
            0,
            list.map(inputs_preimage, encode_midgard_tx_input),
          ) == verified_tx_inputs_hash

        let bad_input = inputs_preimage |> list.expect_at(bad_inputs_index)

        // 3. The fetched input's tx id and output index must be passed to the
        //    next step so that its producing transaction can be verified
        //    against the included hash.
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data = step_03.State {
          bad_input_tx_id: bad_input.tx_id,
          bad_input_output_index: bad_input.output_index,
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

// ## Selectors
//
// The step-02 obligation is a preimage opening: the prover must reveal the
// exact spend-inputs preimage the bad transaction committed under the block's
// counted `transactions_root`, and may forward only an input that preimage
// actually contains.

/// A transaction spending an out-of-range output index of `producing_tx_id`.
fn input_no_idx_bad_tx(producing_tx_id: ByteArray) -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: producing_tx_id, output_index: 7 }],
    },
  }
}

fn run_step_02(
  block: fixture.NativeBlockFixtureV1,
  verified_tx_inputs_hash: ByteArray,
  inputs_preimage: List<MidgardTxInput>,
  bad_inputs_index: Int,
  forwarded: MidgardTxInput,
) -> Bool {
  let expected_output_state: Data = step_03.State {
    bad_input_tx_id: forwarded.tx_id,
    bad_input_output_index: forwarded.output_index,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(State { verified_tx_inputs_hash }),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        inputs_preimage,
        bad_inputs_index,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(
      fixture.ct_token_asset_name(block),
      expected_output_state,
    ),
  )
}

/// Positive selector: the canonical spend-inputs preimage of the committed bad
/// transaction opens the hash carried from step-01, and the out-of-range input
/// it names is forwarded verbatim to step-03.
test input_no_idx_step_02_forwards_the_committed_out_of_range_input() {
  let producing = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let block = fixture.native_block_fixture_v1(
    input_no_idx_bad_tx(producing.tx_id),
  )
  run_step_02(
    block,
    block.compact.body.spend_inputs_hash,
    block.tx.body.inputs,
    0,
    MidgardTxInput { tx_id: producing.tx_id, output_index: 7 },
  )
}

/// Valid-block negative selector: every transaction of a valid block spends an
/// input its producer really has, so a prover who substitutes a fabricated
/// out-of-range input for the committed preimage cannot open the committed
/// spend-inputs commitment.
test input_no_idx_step_02_rejects_a_forged_inputs_preimage() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged = MidgardTxInput { tx_id: valid_block.tx_id, output_index: 7 }
  run_step_02(
    valid_block,
    valid_block.compact.body.spend_inputs_hash,
    [forged],
    0,
    forged,
  )
}

// ### Maximum/adversarial proof-fit (GOAL_SPEC.md §3.2 / §3.3)
//
// Measured against `docs/consensus-profile-v1.md` §10
// (`minSupportedTransactionExecutionMemoryUnits` 16_500_000,
// `minSupportedTransactionExecutionCpuUnits` 10_000_000_000) with the §3.3
// 20% reserve, i.e. 13_200_000 mem / 8_000_000_000 cpu:
//
// | spend inputs | mem         | cpu            | verdict                     |
// | ------------ | ----------- | -------------- | --------------------------- |
// | 16           |  11_080_218 |  4_938_245_579 | fits with reserve           |
// | 19           |  12_718_812 |  5_654_474_206 | fits with reserve (largest) |
// | 20           |  13_240_624 |  5_875_285_273 | under raw limit, over 20%   |
// | 296          | 184_636_976 | 77_629_999_849 | 11.2x over the raw limit    |
//
// 296 is the Cardano spend boundary pinned by
// `lib/midgard/fraud-proofs/native-tx.max-redeemers.test.ak`, and the
// consensus profile admits `maxSpendInputCount` 16384, so the complete-item
// direct redeemer of §3.2 tier 1 does **not** reach the family maximum. The
// tier-1 path stays enabled for every transaction it fits (selector below);
// anything larger owes the §3.2 necessity artifact before a bounded fallback
// may be introduced. Nothing here chunks the preimage.

const largest_fitting_spend_input_count = 19

const proof_fit_spend_input_tx_id =
  #"2020202020202020202020202020202020202020202020202020202020202020"

fn proof_fit_spend_inputs_from(index: Int) -> List<MidgardTxInput> {
  if index >= largest_fitting_spend_input_count {
    []
  } else {
    [
      MidgardTxInput { tx_id: proof_fit_spend_input_tx_id, output_index: index },
      ..proof_fit_spend_inputs_from(index + 1)
    ]
  }
}

/// The largest complete spend-inputs preimage that clears both §3.3 execution
/// reserves in a single step-02 redeemer, carried directly with no chunking.
test input_no_idx_step_02_carries_the_largest_fitting_spend_inputs_preimage() {
  let inputs_preimage = proof_fit_spend_inputs_from(0)
  let verified_tx_inputs_hash = bounded_collection_v1.from_items(
    0,
    list.map(inputs_preimage, encode_midgard_tx_input),
  )
  let last_index = largest_fitting_spend_input_count - 1
  run_step_02(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    verified_tx_inputs_hash,
    inputs_preimage,
    last_index,
    MidgardTxInput {
      tx_id: proof_fit_spend_input_tx_id,
      output_index: last_index,
    },
  )
}
