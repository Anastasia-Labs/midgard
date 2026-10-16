use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/validation_case_thread_v1.{ThreadDatum}

pub type Datum =
  ThreadDatum

pub type SpendRedeemer {
  /// Closes the thread into its case with the winner of its timed-out turn.
  CloseTimedOutThread { input_index: Int, case_input_index: Int }
  /// The case can no longer take the thread (`validation_case_thread_v1.expires`).
  ExpireTimedOutThread { input_index: Int }
}

/// A case thread whose side to move ran out of clock. The thread closes into
/// its case with that side's loss, read from the same turn the game validator
/// timed out; the case spend checks the rest of the close.
validator main(computation_thread_token_policy_id: PolicyId) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(step_datum) = datum
    expect Some(thread) = step_datum.data
    when redeemer is {
      CloseTimedOutThread { input_index, case_input_index } ->
        validation_case_thread_v1.closes_into_case(
          computation_thread_token_policy_id,
          input_index,
          case_input_index,
          own_out_ref,
          validation_case_thread_v1.timeout_winner(
            thread,
            utils.get_inclusive_lower_bound_of_interval(tx.validity_range),
          ),
          tx,
        )
      ExpireTimedOutThread { input_index } ->
        validation_case_thread_v1.expires(
          computation_thread_token_policy_id,
          thread,
          input_index,
          own_out_ref,
          tx,
        )
    }
  }

  else(_) {
    fail
  }
}
