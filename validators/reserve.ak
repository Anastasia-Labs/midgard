use aiken/crypto.{ScriptHash}
use cardano/transaction.{OutputReference, Transaction}
use midgard/reserve.{Datum, SpendRedeemer}

validator spend(
  hub_oracle: ScriptHash,
  plutarch_phas_validator_hash: ScriptHash,
) {
  spend(
    m_datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    todo
  }
  else(_) {
    False
  }
}
