use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine, nibble, suffix}
use aiken/primitive/bytearray
use midgard/fraud_proofs/value_not_preserved/step_01.{
  AdaAsset, ClaimedAssetDeflated, ClaimedAssetInflated, TokenAsset,
}
use midgard/fraud_proofs/value_not_preserved/union.{
  AcceptedImbalance, AssetDeltaWitness, BalanceState, ForcedConservation,
  apply_contribution, empty_delta_root, terminal_claim_holds,
}
use midgard/mpf_branch_opening_fixture as opening_fixture

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
  let input = apply_contribution(empty_delta_root(), unit, 10, 0, [], #"")
  let mint = apply_contribution(input, unit, 3, 10, [], #"")
  let output = apply_contribution(mint, unit, -13, 13, [], #"")
  and {
    !terminal_claim_holds(forced(input, 0), None),
    !terminal_claim_holds(forced(mint, 0), None),
    terminal_claim_holds(forced(output, 0), None),
    !terminal_claim_holds(forced(output, 1), None),
    !terminal_claim_holds(forced(output, -1), None),
  }
}

test burn_and_negative_intermediate_deltas_are_conserved() {
  let output = apply_contribution(empty_delta_root(), unit, -7, 0, [], #"")
  let input = apply_contribution(output, unit, 10, -7, [], #"")
  let burn = apply_contribution(input, unit, -3, 3, [], #"")
  terminal_claim_holds(forced(burn, 0), None)
}

test omitted_or_adjacent_contribution_cannot_close_universal_claim() {
  let input = apply_contribution(empty_delta_root(), unit, 10, 0, [], #"")
  let short = apply_contribution(input, unit, -9, 10, [], #"")
  !terminal_claim_holds(forced(short, 0), None)
}

test forged_old_delta_is_refused() fail {
  let input = apply_contribution(empty_delta_root(), unit, 10, 0, [], #"")
  apply_contribution(input, unit, -9, 9, [], #"") == empty_delta_root()
}

test zero_contribution_is_refused() fail {
  apply_contribution(empty_delta_root(), unit, 0, 0, [], #"") == empty_delta_root()
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
  let root = apply_contribution(empty_delta_root(), unit, 1, 0, [], #"")
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

// ## Removing a unit keeps the delta map canonical
//
// Units whose paths start with nibble 5 (`me`), 8, 9 and 12. The map is a
// root branch with one leaf per unit, each holding delta 1.

fn slot_unit(slot: Int) -> ByteArray {
  bytearray.concat(
    #"000000000000000000000000000000000000000000000000000000",
    bytearray.push(
      #"",
      when slot is {
        5 -> 0
        8 -> 1
        9 -> 19
        _ -> 18
      },
    ),
  )
}

fn delta_children(slots: List<Int>) -> List<ByteArray> {
  opening_fixture.children(
    list.map(
      slots,
      fn(slot) {
        Pair(
          slot,
          combine(
            suffix(blake2b_256(slot_unit(slot)), 1),
            blake2b_256(cbor.serialise(1)),
          ),
        )
      },
    ),
  )
}

fn delta_root(slots: List<Int>) -> ByteArray {
  combine(#"", opening_fixture.merkle(delta_children(slots)))
}

fn remove_unit_5(slots: List<Int>, opening: ByteArray) -> ByteArray {
  apply_contribution(
    delta_root(slots),
    slot_unit(5),
    -1,
    1,
    [
      mpf.Branch {
        skip: 0,
        neighbors: opening_fixture.neighbors(delta_children(slots), 5),
      },
    ],
    opening,
  )
}

test delta_units_sit_in_their_slots() {
  list.all(
    [5, 8, 9, 12],
    fn(slot) { nibble(blake2b_256(slot_unit(slot)), 0) == slot },
  )
}

test removal_opens_a_lone_neighbour_group() {
  let opening = opening_fixture.opening(delta_children([5, 8, 9]), 5)
  and {
    bytearray.length(opening) == 66,
    remove_unit_5([5, 8, 9], opening) == delta_root([8, 9]),
  }
}

// The lone other unit re-read as a terminal Branch: the map it would leave is
// a branch with one child, never the canonical single leaf.
test removal_refuses_a_terminal_branch_masquerade() fail {
  remove_unit_5([5, 12], #"") != empty_delta_root()
}
