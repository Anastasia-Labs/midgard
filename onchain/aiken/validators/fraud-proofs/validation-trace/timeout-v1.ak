use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/validation_case_thread_v1.{ThreadDatum}
use midgard/validation_thread_expiry_v1

pub type Datum =
  ThreadDatum

pub type SpendRedeemer {
  /// Closes the thread into its case with the winner of its timed-out turn.
  CloseTimedOutThread { input_index: Int, case_input_index: Int }
}

/// A case thread whose side to move ran out of clock. The thread closes into
/// its case with that side's loss, read from the same turn the game validator
/// timed out; the case spend checks the rest of the close. A thread its case
/// can no longer take is released to the thread expiry script.
validator main(
  computation_thread_token_policy_id: PolicyId,
  thread_expiry_script_hash: ScriptHash,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    or {
      validation_thread_expiry_v1.holder_releases_to_expiry(
        computation_thread_token_policy_id,
        thread_expiry_script_hash,
        own_out_ref,
        tx,
      ),
      {
        expect Some(step_datum) = datum
        expect Some(thread) = step_datum.data
        let CloseTimedOutThread { input_index, case_input_index } = redeemer
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
      },
    }
  }

  else(_) {
    fail
  }
}
