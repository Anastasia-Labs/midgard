//// One counted opening, using the existing single-claim direct/published
//// membership verifier. Count/domain/key/value authority never comes from an
//// empty inline proof or a publication label.

use cardano/transaction.{Input, Redeemer, ScriptPurpose}
use env
use midgard/fraud_proofs/common.{MembershipCarriage, verify_membership_carried}
use midgard/transition_trace.{RootDomain, commit_counted_root}

pub type CarriedCountedMembershipV1<key, value> {
  domain: RootDomain,
  root: ByteArray,
  phas_root: ByteArray,
  count: Int,
  key: key,
  value: value,
  carriage: MembershipCarriage,
}

/// At most one call per L1 transaction: the existing merkelized withdrawal
/// must carry this exact claim. Separate physical event and transition steps
/// preserve that authority and the maximum proof-carriage envelope.
pub fn verify_carried_counted_membership_v1(
  witness: CarriedCountedMembershipV1<key, value>,
  expected_domain: RootDomain,
  expected_root: ByteArray,
  expected_count: Int,
  key_bytes: ByteArray,
  value_bytes: ByteArray,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  let
      CarriedCountedMembershipV1 {
        domain,
        root,
        phas_root,
        count,
        carriage,
        ..
      }
    = witness
  and {
    expected_count > 0,
    domain == expected_domain,
    root == expected_root,
    count == expected_count,
    // `expected_count > 0` with `count == expected_count` fixes a positive
    // count, so of `transition_trace.counted_root_is_consistent` only the
    // non-empty branch can hold: both roots are non-empty and the root commits
    // to this count.
    phas_root != env.empty_merkle_tree_root,
    root != env.empty_merkle_tree_root,
    root == commit_counted_root(domain, phas_root, count),
    verify_membership_carried(
      carriage,
      phas_root,
      key_bytes,
      value_bytes,
      reference_inputs,
      redeemers,
    ),
  }
}
