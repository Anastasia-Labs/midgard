use aiken/cbor
use aiken/crypto
use aiken/merkle_patricia_forestry.{Leaf, Proof}
use env
use midgard/ledger_state
use midgard/transition_trace

const single_raw_root =
  #"8e6a655e86132c76e2ee2dfba167949b82854be7a7657567eaf4cb86daf53588"

const single_typed_root =
  #"0fe4e134a5df52ba919e462ade1ba22090792b63f1c99a747deadea952abf1d4"

const empty_proof: Proof = []

const raw_non_membership_proof: Proof =
  [
    Leaf {
      skip: 0,
      key: #"ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25",
      value: #"55951e629cad560ea5f8be280c35d8788ee84324b842fee1b41c546efb62d2d5",
    },
  ]

fn sample_transition_step_v1(
  schema_version: Int,
) -> ledger_state.TransitionStepV1 {
  ledger_state.TransitionStep {
    schema_version,
    step_index: 2,
    event_key: ledger_state.L2TransactionEventKey {
      tx_id: #"3333333333333333333333333333333333333333333333333333333333333333",
    },
    phase: ledger_state.L2Transaction,
    pre_utxos_root: #"4444444444444444444444444444444444444444444444444444444444444444",
    post_utxos_root: #"5555555555555555555555555555555555555555555555555555555555555555",
  }
}

test l06_transition_step_v1_aiken_cbor_and_hash_vector_is_exact() {
  let step =
    sample_transition_step_v1(ledger_state.transition_step_schema_version_v1)
  let encoded = cbor.serialise(step)
  and {
    encoded == #"d8799f0102d87b9f58203333333333333333333333333333333333333333333333333333333333333333ffd87b805820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555ff",
    crypto.blake2b_256(encoded) == #"c7931b31b050e3d59769ee6df1556585c608f1af0c74decc08026ecafbc234c4",
    ledger_state.transition_step_v1_is_valid(step),
  }
}

test l06_transition_step_v1_rejects_adjacent_schema_versions() {
  and {
    !ledger_state.transition_step_v1_is_valid(sample_transition_step_v1(0)),
    !ledger_state.transition_step_v1_is_valid(sample_transition_step_v1(2)),
  }
}

test accepts_typescript_raw_membership_fixture() {
  transition_trace.verify_root_membership_raw(
    single_raw_root,
    1,
    #"01",
    #"aa",
    empty_proof,
  )
}

test accepts_typescript_raw_non_membership_fixture() {
  transition_trace.verify_root_non_membership_raw(
    single_raw_root,
    1,
    #"02",
    raw_non_membership_proof,
  )
}

test accepts_empty_midgard_root_non_membership_fixture() {
  transition_trace.verify_root_non_membership_raw(
    env.empty_merkle_tree_root,
    0,
    #"01",
    empty_proof,
  )
}

test accepts_typed_membership_witness_fixture() {
  let counted_root =
    transition_trace.commit_counted_root(
      transition_trace.DepositsRootDomain,
      single_typed_root,
      1,
    )
  let witness =
    transition_trace.RootMembershipProof {
      domain: transition_trace.DepositsRootDomain,
      root: counted_root,
      phas_root: single_typed_root,
      count: 1,
      key: 0,
      value: #"aa",
      proof: empty_proof,
    }

  transition_trace.verify_root_membership_with_bytes(
    witness,
    transition_trace.DepositsRootDomain,
    counted_root,
    1,
    cbor.serialise(0),
    cbor.serialise(#"aa"),
  )
}
