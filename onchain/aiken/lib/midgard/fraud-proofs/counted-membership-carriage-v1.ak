//// One counted opening, using the existing single-claim direct/published
//// membership verifier. Count/domain/key/value authority never comes from an
//// empty inline proof or a publication label.

use cardano/transaction.{Input, Redeemer, ScriptPurpose}
use midgard/fraud_proofs/common.{MembershipCarriage, verify_membership_carried}
use midgard/transition_trace.{
  RootCountProof, RootDomain, verify_root_count_proof,
}

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
  and {
    expected_count > 0,
    verify_root_count_proof(
      RootCountProof {
        domain: witness.domain,
        root: witness.root,
        phas_root: witness.phas_root,
        count: witness.count,
      },
      expected_domain,
      expected_root,
      expected_count,
    ),
    verify_membership_carried(
      witness.carriage,
      witness.phas_root,
      key_bytes,
      value_bytes,
      reference_inputs,
      redeemers,
    ),
  }
}
