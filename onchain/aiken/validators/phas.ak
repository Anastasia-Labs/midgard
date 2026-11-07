use aiken/builtin
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use midgard/mpf_proof_v1 as canonical_mpf

validator membership {
  withdraw(redeemer: Data, _own_credential: Credential, _tx: Transaction) {
    expect [root_data,
          key_data, value_data, proof_data] = builtin.un_list_data(redeemer)
    expect root: ByteArray = root_data
    expect key: ByteArray = key_data
    expect value: ByteArray = value_data
    expect proof: mpf.Proof = proof_data

    canonical_mpf.has(root, key, value, proof)
  }

  else(_) {
    fail
  }
}
