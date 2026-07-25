use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{Transaction}
use midgard/user_events/tx_field_receipt_v1.{MintRedeemer, validate}

validator mint(
  field_preimage_script_hash: ScriptHash,
  receipt_script_hash: ScriptHash,
) {
  mint(redeemer: MintRedeemer, own_policy: PolicyId, tx: Transaction) {
    validate(
      field_preimage_script_hash,
      receipt_script_hash,
      redeemer,
      own_policy,
      tx,
    )
  }

  else(_) {
    fail
  }
}
