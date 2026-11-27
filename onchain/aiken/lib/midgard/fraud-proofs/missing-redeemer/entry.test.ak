use midgard/fraud_proofs/direct_script_derivation/claim.{
  ClaimContextV1, LeafPurpose,
}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/first_fault_v1.{NetworkOrderContextV1}
use midgard/fraud_proofs/missing_redeemer/entry
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/rejection_reason_v1 as reasons

fn with_leaf_reason(reason: Option<reasons.RejectionReasonV1>) -> BoundIndexV1 {
  BoundIndexV1 {
    bound: BoundV1 {
      subject: VerdictSubjectV1 {
        version: 1,
        direction: 1,
        source_kind: 1,
        transaction_id: #"11",
        source_key: #"",
        rejection_reason: reason,
      },
      compact_cbor: #"22",
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: #"33",
    },
    source_index: source_index.StateV1 { root: #"44", count: 7 },
  }
}

test claimed_only_route_admits_a_redeemer_missing_leaf() {
  let bound = with_leaf_reason(
    Some(reasons.RedeemerMissing { purpose_kind: 0, purpose_index: 7 }),
  )
  entry.claim_context_v1(bound) == ClaimContextV1 {
    bound,
    observed: None,
    target: LeafPurpose,
  }
}

/// The claimed-only route opens only on a RedeemerMissing leaf.
test claimed_only_route_refuses_a_leaf_of_another_reason_constructor() fail {
  let bound = with_leaf_reason(
    Some(reasons.UnusedRedeemer { redeemer_index: 7 }),
  )
  entry.claim_context_v1(bound).observed == None
}

test claimed_only_route_refuses_an_accepted_leaf() fail {
  entry.claim_context_v1(with_leaf_reason(None)).observed == None
}
