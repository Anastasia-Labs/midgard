use midgard/fraud_proofs/mint_declared_asset_limit/rule.{BoundPolicyV1}

pub type AuthenticationStateV1 {
  Bound { bound: BoundPolicyV1 }
  Grammar { bound: BoundPolicyV1, checkpoint_hash: ByteArray }
}
