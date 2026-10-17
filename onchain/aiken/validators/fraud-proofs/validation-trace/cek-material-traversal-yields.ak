use aiken/crypto.{ScriptHash}
use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use midgard/cek_material_traversal as material
use midgard/cek_proof_v1 as proof

validator program(dispatcher_script_hash: ScriptHash) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (_, action) = material.unique_dispatch(
      dispatcher_script_hash,
      tx.inputs,
      tx.redeemers,
    )
    expect proof.program_partition_root_matches(action.entry)
    expect Some(children) = proof.program_partition_children(
      action.task,
      action.entry,
    )
    action.next_pending_root == if action.already_seen {
      action.tail_root
    } else {
      material.push_children(children, action.tail_root)
    }
  }

  else(_) {
    fail
  }
}

validator data(dispatcher_script_hash: ScriptHash) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (_, action) = material.unique_dispatch(
      dispatcher_script_hash,
      tx.inputs,
      tx.redeemers,
    )
    expect proof.data_partition_root_matches(action.entry)
    expect Some(children) = proof.data_partition_children(
      action.task,
      action.entry,
    )
    action.next_pending_root == if action.already_seen {
      action.tail_root
    } else {
      material.push_children(children, action.tail_root)
    }
  }

  else(_) {
    fail
  }
}
