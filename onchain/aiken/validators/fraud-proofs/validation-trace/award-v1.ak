use aiken/collection/list
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/validation_award_v1.{
  CloseAwardedThread, Datum, MintReceipt, ReceiptRedeemerV1, SpendRedeemer,
}
use midgard/validation_case_thread_v1
use midgard/validation_dispute_v1
use midgard/validation_resolution_v1

/// A case thread whose one-step resolution certified the challenger's step,
/// and the policy of the lost-defense receipts its close mints.
validator main(computation_thread_token_policy_id: PolicyId) {
  mint(redeemer: ReceiptRedeemerV1, _policy_id: PolicyId, tx: Transaction) {
    when redeemer is {
      // The case spend fixes the whole mint, receipt included.
      MintReceipt { case_input_index } -> {
        expect Some(case_input) = list.at(tx.inputs, case_input_index)
        case_input.output.address.payment_credential == Script(
          computation_thread_token_policy_id,
        )
      }
    }
  }

  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      CloseAwardedThread { input_index, case_input_index } -> {
        expect Some(step_datum) = datum
        expect Some(state) = step_datum.data
        and {
          validation_resolution_v1.winning_resolution_is_well_formed(state),
          validation_case_thread_v1.closes_into_case(
            computation_thread_token_policy_id,
            input_index,
            case_input_index,
            own_out_ref,
            validation_dispute_v1.ChallengerWins,
            tx,
          ),
        }
      }
    }
  }

  else(_) {
    fail
  }
}
