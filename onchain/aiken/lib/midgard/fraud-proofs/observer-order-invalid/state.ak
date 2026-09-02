use midgard/fraud_proofs/observer_order_invalid/rule.{BoundObserverV1}

pub type AuthenticationStateV1 {
  Bound { bound: BoundObserverV1 }
  Reserved { bound: BoundObserverV1 }
}
