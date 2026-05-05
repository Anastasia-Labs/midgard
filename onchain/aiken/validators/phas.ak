use aiken/builtin
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Credential}
use cardano/transaction.{Transaction}

validator membership {
  withdraw(redeemer: Data, _own_credential: Credential, _tx: Transaction) {
    expect [root_data,
      key_data, value_data, proof_data] = builtin.un_list_data(redeemer)
    expect root: ByteArray = root_data
    expect key: ByteArray = key_data
    expect value: ByteArray = value_data
    expect proof: mpf.Proof = proof_data

    mpf.has(mpf.from_root(root), key, value, proof)
  }

  else(_) {
    fail
  }
}
