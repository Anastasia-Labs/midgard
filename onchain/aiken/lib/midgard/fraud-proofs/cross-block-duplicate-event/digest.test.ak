use aiken/builtin
use aiken/cbor
use midgard/fraud_proofs/cross_block_duplicate_event/step_01
use midgard/transition_trace

fn digest_opening() -> step_01.CommittedDuplicateEventProofV1 {
  let block = step_01.duplicate_deposit_block_v1()
  expect step_01.CommittedDuplicateDepositV1 { membership } = block.committed_event
  step_01.CommittedDuplicateEventDigestV1 {
    event_kind: step_01.DuplicateDepositV1,
    membership: transition_trace.RootMembershipProof {
      domain: membership.domain,
      root: membership.root,
      phas_root: membership.phas_root,
      count: membership.count,
      key: membership.key,
      proof: membership.proof,
      value: builtin.blake2b_256(cbor.serialise(membership.value)),
    },
  }
}

test digest_opens_identical_committed_leaf() {
  step_01.verify_committed_event_membership_v1(
    step_01.duplicate_deposit_block_v1().block.header,
    digest_opening(),
  )
}

test digest_refuses_substituted_value_hash() {
  expect step_01.CommittedDuplicateEventDigestV1 { event_kind, membership } = digest_opening()
  !step_01.verify_committed_event_membership_v1(
    step_01.duplicate_deposit_block_v1().block.header,
    step_01.CommittedDuplicateEventDigestV1 {
      event_kind,
      membership: transition_trace.RootMembershipProof {
        ..membership,
        value: #"1111111111111111111111111111111111111111111111111111111111111111",
      },
    },
  )
}

test digest_refuses_wrong_domain_and_count() {
  expect step_01.CommittedDuplicateEventDigestV1 { membership, .. } = digest_opening()
  let header = step_01.duplicate_deposit_block_v1().block.header
  and {
    !step_01.verify_committed_event_membership_v1(
      header,
      step_01.CommittedDuplicateEventDigestV1 {
        event_kind: step_01.DuplicateWithdrawalV1,
        membership,
      },
    ),
    !step_01.verify_committed_event_membership_v1(
      header,
      step_01.CommittedDuplicateEventDigestV1 {
        event_kind: step_01.DuplicateDepositV1,
        membership: transition_trace.RootMembershipProof {
          ..membership,
          count: 2,
        },
      },
    ),
  }
}

test digest_refuses_wrong_key_and_digest_width() {
  expect step_01.CommittedDuplicateEventDigestV1 { event_kind, membership } = digest_opening()
  let header = step_01.duplicate_deposit_block_v1().block.header
  and {
    !step_01.verify_committed_event_membership_v1(
      header,
      step_01.CommittedDuplicateEventDigestV1 {
        event_kind,
        membership: transition_trace.RootMembershipProof {
          ..membership,
          key: step_01.foreign_event_id_v1(),
        },
      },
    ),
    !step_01.verify_committed_event_membership_v1(
      header,
      step_01.CommittedDuplicateEventDigestV1 {
        event_kind,
        membership: transition_trace.RootMembershipProof {
          ..membership,
          value: #"11",
        },
      },
    ),
  }
}
