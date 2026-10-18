use cardano/address.{Credential}
use cardano/assets.{PolicyId}
use cardano/transaction.{Transaction}
use midgard/validation_thread_expiry_v1

/// The expiry of a validation case thread its case can no longer take
/// (`validation_thread_expiry_v1.expires`), as a withdraw-zero script. Its
/// parameters name no thread holder: every holder releases its thread input
/// to this script's withdrawal, and the case mint burns the thread token
/// beside it.
validator main(
  computation_thread_token_policy_id: PolicyId,
  case_duration_milliseconds: Int,
) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    validation_thread_expiry_v1.expires(
      computation_thread_token_policy_id,
      case_duration_milliseconds,
      tx,
    )
  }

  else(_) {
    fail
  }
}
