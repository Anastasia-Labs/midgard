//// Test-only direct carriage; this creates no source authority.

use midgard/fraud_proofs/common.{RedeemerCarriedMembership}
use midgard/fraud_proofs/counted_membership_carriage_v1.{
  CarriedCountedMembershipV1,
}
use midgard/fraud_proofs/native_tx/types.{NativeTxCompact, TxCompactView}
use midgard/transition_trace.{RootMembershipProof}

pub fn direct_carried_membership_v1(
  witness: RootMembershipProof<key, value>,
  withdrawal_index: Int,
) -> CarriedCountedMembershipV1<key, value> {
  CarriedCountedMembershipV1 {
    domain: witness.domain,
    root: witness.root,
    phas_root: witness.phas_root,
    count: witness.count,
    key: witness.key,
    value: witness.value,
    carriage: RedeemerCarriedMembership {
      membership_proof: witness.proof,
      membership_proof_script_redeemer_index: withdrawal_index,
    },
  }
}

pub fn compact_view_v1(compact: NativeTxCompact) -> TxCompactView {
  TxCompactView {
    body: compact.body,
    witness_set_hash: compact.witness_set_hash,
    validity_code: Some(compact.validity_code),
  }
}
