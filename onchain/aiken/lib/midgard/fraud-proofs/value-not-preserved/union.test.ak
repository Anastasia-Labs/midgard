use midgard/fraud_proofs/value_not_preserved/step_01.{
  AdaAsset, ClaimedAssetDeflated, ClaimedAssetInflated, TokenAsset,
}
use midgard/fraud_proofs/value_not_preserved/union.{
  AcceptedImbalance, AssetDeltaWitness, BalanceState, ForcedConservation,
  apply_contribution, empty_delta_root, terminal_claim_holds,
}

const unit = #"11111111111111111111111111111111111111111111111111111111"

const hash = #"2222222222222222222222222222222222222222222222222222222222222222"

fn forced(root: ByteArray, ada: Int) -> BalanceState {
  BalanceState {
    transaction_id: hash,
    claim: ForcedConservation,
    pre_utxos_root: hash,
    lovelace_delta: ada,
    asset_delta_root: root,
  }
}

test universal_conservation_requires_empty_union_and_zero_ada() {
  let input = apply_contribution(empty_delta_root(), unit, 10, 0, [])
  let mint = apply_contribution(input, unit, 3, 10, [])
  let output = apply_contribution(mint, unit, -13, 13, [])
  and {
    !terminal_claim_holds(forced(input, 0), None),
    !terminal_claim_holds(forced(mint, 0), None),
    terminal_claim_holds(forced(output, 0), None),
    !terminal_claim_holds(forced(output, 1), None),
    !terminal_claim_holds(forced(output, -1), None),
  }
}

test burn_and_negative_intermediate_deltas_are_conserved() {
  let output = apply_contribution(empty_delta_root(), unit, -7, 0, [])
  let input = apply_contribution(output, unit, 10, -7, [])
  let burn = apply_contribution(input, unit, -3, 3, [])
  terminal_claim_holds(forced(burn, 0), None)
}

test omitted_or_adjacent_contribution_cannot_close_universal_claim() {
  let input = apply_contribution(empty_delta_root(), unit, 10, 0, [])
  let short = apply_contribution(input, unit, -9, 10, [])
  !terminal_claim_holds(forced(short, 0), None)
}

test forged_old_delta_is_refused() fail {
  let input = apply_contribution(empty_delta_root(), unit, 10, 0, [])
  apply_contribution(input, unit, -9, 9, []) == empty_delta_root()
}

test zero_contribution_is_refused() fail {
  apply_contribution(empty_delta_root(), unit, 0, 0, []) == empty_delta_root()
}

test accepted_ada_keeps_the_original_signed_predicate() {
  let state = BalanceState {
    ..forced(empty_delta_root(), -1),
    claim: AcceptedImbalance {
      asset: AdaAsset,
      direction: ClaimedAssetInflated,
    },
  }
  and {
    terminal_claim_holds(state, None),
    !terminal_claim_holds(BalanceState { ..state, lovelace_delta: 0 }, None),
    !terminal_claim_holds(BalanceState { ..state, lovelace_delta: 1 }, None),
  }
}

test accepted_token_keeps_the_original_authenticated_signed_predicate() {
  let root = apply_contribution(empty_delta_root(), unit, 1, 0, [])
  let state = BalanceState {
    ..forced(root, 0),
    claim: AcceptedImbalance {
      asset: TokenAsset { policy_id: unit, asset_name: #"" },
      direction: ClaimedAssetDeflated,
    },
  }
  terminal_claim_holds(state, Some(AssetDeltaWitness { delta: 1, proof: [] }))
}

test forced_terminal_refuses_a_prover_selected_delta_witness() fail {
  terminal_claim_holds(
    forced(empty_delta_root(), 0),
    Some(AssetDeltaWitness { delta: 0, proof: [] }),
  )
}
