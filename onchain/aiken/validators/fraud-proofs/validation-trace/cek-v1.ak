use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/validation_resolver_v1.{CekSpendRedeemer, Datum, validate_cek}

validator main(
  computation_thread_policy_id: PolicyId,
  fraud_proof_policy_id: PolicyId,
  fraud_proof_address: Address,
  cek_program_material_script_hash: ScriptHash,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: CekSpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    validate_cek(
      cek_program_material_script_hash,
      computation_thread_policy_id,
      fraud_proof_policy_id,
      fraud_proof_address,
      field_preimage_certificate_policy_id,
      datum,
      redeemer,
      own_out_ref,
      tx,
    )
  }

  else(_) {
    fail
  }
}
