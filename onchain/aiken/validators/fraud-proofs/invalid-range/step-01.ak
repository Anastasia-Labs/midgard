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
use midgard/common/constants.{posix_time_none}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step}
use midgard/fraud_proofs/invalid_range/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/invalid_range/step_02
use midgard/fraud_proofs/native_tx/types.{NativeTxBodyCompact}
use midgard/ledger_state.{Header}

fn normalize_native_validity_range(
  body: NativeTxBodyCompact,
) -> vrn.NormalizedTimeRange {
  let NativeTxBodyCompact {
    validity_interval_start: lower,
    validity_interval_end: exclusive_upper,
    ..
  } = body

  when (lower == posix_time_none, exclusive_upper == posix_time_none) is {
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
          bad_tx_view,
        <-
          pass_native_tx_to_next_step(
            computation_thread_token_policy_id,
            hub_oracle,
            datum,
            tx_inclusion_args,
            own_out_ref,
            tx,
          )

        expect None = m_input_state_data

        let Header {
          start_time: block_valid_from,
          end_time: block_valid_to,
          ..
        } = header

        let bad_tx_normalized_validity_range =
          normalize_native_validity_range(bad_tx_view.tx_compact.body)

        // 2. The UTxO must be produced at next step's script hash with validity
        //    range of the bad tx, and block's time range provided.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data =
          step_02.State {
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
    test_native_body(posix_time_none, posix_time_none),
  ) == vrn.Always
}
