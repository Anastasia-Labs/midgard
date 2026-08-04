//// ## Invalid Range Fraud Proof (Section 5.1.2)
////
//// Proves a given transaction, which has a validity range with one or both
//// ends bounded, is included in a Midgard block whose validity range does not
//// cover the transaction's validity range. Or if the transaction has an
//// invalid time range (i.e. reversed interval).

use aiken/crypto.{ScriptHash}
use aiken_design_patterns/validity_range_normalization as vrn
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use env
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step}
use midgard/fraud_proofs/invalid_range/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/invalid_range/step_02
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, NativeTxBodyCompact,
}
use midgard/ledger_state.{HeaderV1}

fn normalize_native_validity_range(
  body: NativeTxBodyCompact,
) -> vrn.NormalizedTimeRange {
  let
      NativeTxBodyCompact {
        validity_interval_start: lower,
        validity_interval_end: exclusive_upper,
        ..
      }
    = body

  when
    (lower == env.posix_time_none, exclusive_upper == env.posix_time_none)
  is {
    (True, True) -> vrn.Always
    (True, False) -> vrn.FromNegInf { upper: exclusive_upper - 1 }
    (False, True) -> vrn.ToPosInf { lower }
    (False, False) -> {
      let upper = exclusive_upper - 1
      if lower > upper {
        vrn.InvalidRange
      } else {
        vrn.ClosedRange { lower, upper }
      }
    }
  }
}

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
        // 1. Generic validations for passing the provided bad tx to the next
        //    step must pass.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data,
            output_script_hash,
            output_state_data,
            header,
            _bad_tx_id,
            bad_tx_view
          <- pass_native_tx_to_next_step(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          tx_inclusion_args,
          own_out_ref,
          tx,
        )

        expect None = m_input_state_data

        let
            HeaderV1 {
              start_time: block_valid_from,
              end_time: block_valid_to,
              ..
            }
          = header

        let bad_tx_normalized_validity_range = normalize_native_validity_range(
          bad_tx_view.tx_compact.body,
        )

        // 2. The UTxO must be produced at next step's script hash with validity
        //    range of the bad tx, and block's time range provided.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          block_valid_from,
          block_valid_to,
          bad_tx_normalized_validity_range,
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

fn test_native_body(lower: Int, exclusive_upper: Int) -> NativeTxBodyCompact {
  NativeTxBodyCompact {
    spend_inputs_hash: "",
    reference_inputs_hash: "",
    outputs_hash: "",
    fee: 0,
    validity_interval_start: lower,
    validity_interval_end: exclusive_upper,
    required_observers_hash: "",
    required_signers_hash: "",
    mint_hash: "",
    script_integrity_hash: "",
    auxiliary_data_hash: "",
    network_id: 0,
  }
}

test normalize_native_validity_range_preserves_one_tick_interval() {
  normalize_native_validity_range(test_native_body(10, 11)) == vrn.ClosedRange {
    lower: 10,
    upper: 10,
  }
}

test normalize_native_validity_range_rejects_empty_interval() {
  normalize_native_validity_range(test_native_body(10, 10)) == vrn.InvalidRange
}

test normalize_native_validity_range_detects_unbounded_interval() {
  normalize_native_validity_range(
    test_native_body(env.posix_time_none, env.posix_time_none),
  ) == vrn.Always
}

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)
//
// The three controls above are range-normalization unit cases. The two
// selectors below are the family's positive and valid-block-negative pair: the
// positive drives the deployed `main.spend` handler on a genuine native-V1
// block whose header commits the challenged transaction under the counted
// `transactions_root`; the negative proves fabricated evidence against a valid
// block cannot bind.

/// A native-V1 transaction whose closed validity interval `[5, 9)` starts
/// before the fixture header's `start_time` of 10 — the `invalid-range`
/// violation.
fn out_of_range_bad_tx() -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      validity_interval_start: 5,
      validity_interval_end: 9,
    },
  }
}

/// A native-V1 transaction whose validity interval is inverted (`start` 9 above
/// the exclusive `end` 5, normalizing to `InvalidRange`). This is the malformed
/// interval concern the coverage matrix routes to `invalid-range` (F21 physical
/// structural row L302, Q49-L302).
fn inverted_interval_bad_tx() -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      validity_interval_start: 9,
      validity_interval_end: 5,
    },
  }
}

fn run_step_01(
  claimed: fixture.NativeBlockFixtureV1,
  header: ledger_state.HeaderV1,
  expected_range: vrn.NormalizedTimeRange,
) -> Bool {
  let expected_output_state: Data = step_02.State {
    block_valid_from: header.start_time,
    block_valid_to: header.end_time,
    bad_tx_normalized_validity_range: expected_range,
  }
  let l1_tx = fixture.continue_step_l1_tx_v1(
    claimed,
    expected_output_state,
    claimed.transactions_phas_root,
    claimed.tx_id,
    claimed.compact_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(fixture.native_inclusion_args_v1(claimed)),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Positive selector: both malformed-interval shapes the family adjudicates —
/// an interval the block does not cover, and an inverted interval — are
/// genuinely committed under the block's counted `transactions_root`, so step-01
/// forwards the block's exact time range together with the normalized
/// transaction interval.
test invalid_range_step_01_binds_native_v1_block_fixture() {
  let uncovered = fixture.native_block_fixture_v1(out_of_range_bad_tx())
  let inverted = fixture.native_block_fixture_v1(inverted_interval_bad_tx())
  and {
    run_step_01(
      uncovered,
      uncovered.header,
      vrn.ClosedRange { lower: 5, upper: 8 },
    ),
    run_step_01(inverted, inverted.header, vrn.InvalidRange),
  }
}

/// Valid-block negative selector: a raw transactions root that is not the one
/// committed by the header's counted `transactions_root` cannot bind, so an
/// out-of-range transaction fabricated against a valid block's header is
/// rejected at step one.
test invalid_range_step_01_rejects_forged_transactions_root() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged_block = fixture.native_block_fixture_v1(out_of_range_bad_tx())
  run_step_01(
    fixture.NativeBlockFixtureV1 {
      ..forged_block,
      header: valid_block.header,
      header_hash: valid_block.header_hash,
    },
    valid_block.header,
    vrn.ClosedRange { lower: 5, upper: 8 },
  )
}
