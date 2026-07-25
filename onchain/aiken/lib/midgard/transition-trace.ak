use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use env
use midgard/common/types.{MerkleRoot, Proof}
use midgard/ledger_state

pub type RootDomain {
  WithdrawalsRootDomain
  ForcedTransactionsV1RootDomain
  TransactionsV1RootDomain
  DepositsRootDomain
  TransitionTraceRootDomain
  EventToStepRootDomain
  ValidationTracesRootDomain
}

pub type RootCountProof {
  domain: RootDomain,
  root: ByteArray,
  phas_root: ByteArray,
  count: Int,
}

pub type RootMembershipProof<key, value> {
  domain: RootDomain,
  root: ByteArray,
  phas_root: MerkleRoot<key, value>,
  count: Int,
  key: key,
  value: value,
  proof: Proof,
}

pub type RootNonMembershipProof<key> {
  domain: RootDomain,
  root: ByteArray,
  phas_root: ByteArray,
  count: Int,
  key: key,
  proof: Proof,
}

pub type IndexedTraceProof =
  RootMembershipProof<Int, ledger_state.TransitionStep>

pub type AdjacentTraceProof {
  lower: IndexedTraceProof,
  upper: IndexedTraceProof,
}

pub type EventToStepProof {
  EventToStepMembership {
    membership: RootMembershipProof<
      ledger_state.EventKey,
      ledger_state.EventToStepValue,
    >,
  }
  EventToStepNonMembership {
    non_membership: RootNonMembershipProof<ledger_state.EventKey>,
  }
}

const counted_root_tag = #"4d696467617264526f6f74436f756e745631"

pub fn commit_counted_root(
  domain: RootDomain,
  phas_root: ByteArray,
  count: Int,
) -> ByteArray {
  if count == 0 && phas_root == env.empty_merkle_tree_root {
    env.empty_merkle_tree_root
  } else {
    counted_root_tag
      |> bytearray.concat(cbor.serialise(domain))
      |> bytearray.concat(phas_root)
      |> bytearray.concat(cbor.serialise(count))
      |> blake2b_256
  }
}

fn phas_root_count_is_consistent(root: ByteArray, count: Int) -> Bool {
  and {
    count >= 0,
    if root == env.empty_merkle_tree_root {
      count == 0
    } else {
      count > 0
    },
  }
}

fn counted_root_is_consistent(
  domain: RootDomain,
  root: ByteArray,
  phas_root: ByteArray,
  count: Int,
) -> Bool {
  and {
    phas_root_count_is_consistent(phas_root, count),
    if count == 0 {
      root == env.empty_merkle_tree_root
    } else {
      and {
        root != env.empty_merkle_tree_root,
        root == commit_counted_root(domain, phas_root, count),
      }
    },
  }
}

fn mpf_from_midgard_root(root: ByteArray) {
  if root == env.empty_merkle_tree_root {
    mpf.empty
  } else {
    mpf.from_root(root)
  }
}

pub fn verify_root_count_proof(
  proof: RootCountProof,
  expected_domain: RootDomain,
  expected_root: ByteArray,
  expected_count: Int,
) -> Bool {
  and {
    proof.domain == expected_domain,
    proof.root == expected_root,
    proof.count == expected_count,
    counted_root_is_consistent(
      proof.domain,
      proof.root,
      proof.phas_root,
      proof.count,
    ),
  }
}

pub fn verify_root_membership_raw(
  root: ByteArray,
  count: Int,
  key_bytes: ByteArray,
  value_bytes: ByteArray,
  proof: Proof,
) -> Bool {
  and {
    phas_root_count_is_consistent(root, count),
    count > 0,
    mpf.has(mpf_from_midgard_root(root), key_bytes, value_bytes, proof),
  }
}

pub fn verify_root_non_membership_raw(
  root: ByteArray,
  count: Int,
  key_bytes: ByteArray,
  proof: Proof,
) -> Bool {
  expect phas_root_count_is_consistent(root, count)
  expect _inserted = mpf.insert(
    mpf_from_midgard_root(root),
    key_bytes,
    env.empty,
    proof,
  )
  True
}

pub fn verify_root_membership_with_bytes(
  witness: RootMembershipProof<key, value>,
  expected_domain: RootDomain,
  expected_root: ByteArray,
  expected_count: Int,
  key_bytes: ByteArray,
  value_bytes: ByteArray,
) -> Bool {
  and {
    witness.domain == expected_domain,
    witness.root == expected_root,
    witness.count == expected_count,
    counted_root_is_consistent(
      witness.domain,
      witness.root,
      witness.phas_root,
      witness.count,
    ),
    verify_root_membership_raw(
      witness.phas_root,
      witness.count,
      key_bytes,
      value_bytes,
      witness.proof,
    ),
  }
}

pub fn verify_root_non_membership_with_key_bytes(
  witness: RootNonMembershipProof<key>,
  expected_domain: RootDomain,
  expected_root: ByteArray,
  expected_count: Int,
  key_bytes: ByteArray,
) -> Bool {
  and {
    witness.domain == expected_domain,
    witness.root == expected_root,
    witness.count == expected_count,
    counted_root_is_consistent(
      witness.domain,
      witness.root,
      witness.phas_root,
      witness.count,
    ),
    verify_root_non_membership_raw(
      witness.phas_root,
      witness.count,
      key_bytes,
      witness.proof,
    ),
  }
}

pub fn verify_indexed_trace_proof(
  witness: IndexedTraceProof,
  expected_root: ledger_state.TransitionTraceRoot,
  expected_count: Int,
) -> Bool {
  and {
    verify_root_membership_with_bytes(
      witness,
      TransitionTraceRootDomain,
      expected_root,
      expected_count,
      cbor.serialise(witness.key),
      cbor.serialise(witness.value),
    ),
    witness.key == witness.value.step_index,
    witness.value.schema_version == ledger_state.transition_step_schema_version_v1,
    witness.key >= 0,
    witness.key < witness.count,
  }
}

pub fn verify_adjacent_trace_proof(
  witness: AdjacentTraceProof,
  expected_root: ledger_state.TransitionTraceRoot,
  expected_count: Int,
) -> Bool {
  and {
    verify_indexed_trace_proof(witness.lower, expected_root, expected_count),
    verify_indexed_trace_proof(witness.upper, expected_root, expected_count),
    witness.lower.root == witness.upper.root,
    witness.lower.count == witness.upper.count,
    witness.upper.key == witness.lower.key + 1,
  }
}

pub fn verify_event_to_step_proof(
  witness: EventToStepProof,
  expected_root: ledger_state.EventToStepRoot,
  expected_count: Int,
) -> Bool {
  when witness is {
    EventToStepMembership { membership } ->
      verify_root_membership_with_bytes(
        membership,
        EventToStepRootDomain,
        expected_root,
        expected_count,
        cbor.serialise(membership.key),
        cbor.serialise(membership.value),
      )
    EventToStepNonMembership { non_membership } ->
      verify_root_non_membership_with_key_bytes(
        non_membership,
        EventToStepRootDomain,
        expected_root,
        expected_count,
        cbor.serialise(non_membership.key),
      )
  }
}
