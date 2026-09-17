use aiken/cbor
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use midgard/fraud_proofs/value_not_preserved/step_01.{
  AdaAsset, ClaimedAssetV1, ClaimedImbalanceDirectionV1, TokenAsset,
}
use midgard/fraud_proofs/value_not_preserved/step_04
use midgard/mpf_proof_v1

pub type ConservationClaim {
  AcceptedImbalance {
    asset: ClaimedAssetV1,
    direction: ClaimedImbalanceDirectionV1,
  }
  ForcedConservation
}

pub type BalanceState {
  transaction_id: ByteArray,
  claim: ConservationClaim,
  pre_utxos_root: ByteArray,
  lovelace_delta: Int,
  asset_delta_root: ByteArray,
}

pub type FoldState {
  balance: BalanceState,
  continuation: Data,
}

pub type PendingContribution {
  unit: ByteArray,
  quantity: Int,
  return_script_hash: ByteArray,
  next_continuation: Data,
}

pub type AssetDeltaWitness {
  delta: Int,
  proof: mpf.Proof,
}

pub fn empty_delta_root() -> ByteArray {
  mpf.root(mpf.empty)
}

/// The inductive map invariant is: only nonzero deltas are stored. Starting
/// empty, every canonical occurrence inserts, updates, or removes exactly its
/// unit. Empty at exact domain exhaustion proves the universal zero claim.
pub fn apply_contribution(
  root: ByteArray,
  unit: ByteArray,
  quantity: Int,
  old_delta: Int,
  proof: mpf.Proof,
) -> ByteArray {
  expect bytearray.length(unit) >= 28 && bytearray.length(unit) <= 60
  expect quantity != 0
  expect mpf_proof_v1.proof_has_at_most_steps(proof, 64)
  let next_delta = old_delta + quantity
  expect Some(next_root) = if old_delta == 0 {
    mpf_proof_v1.insert_root(root, unit, cbor.serialise(quantity), proof)
  } else if next_delta == 0 {
    mpf_proof_v1.delete_root_paired_fold(
      root,
      unit,
      cbor.serialise(old_delta),
      proof,
    )
  } else {
    mpf_proof_v1.update_root(
      root,
      unit,
      cbor.serialise(old_delta),
      cbor.serialise(next_delta),
      proof,
    )
  }
  next_root
}

pub fn terminal_claim_holds(
  state: BalanceState,
  witness: Option<AssetDeltaWitness>,
) -> Bool {
  when state.claim is {
    ForcedConservation -> {
      expect witness == None
      state.lovelace_delta == 0 && state.asset_delta_root == empty_delta_root()
    }
    AcceptedImbalance { asset, direction } -> {
      let final_delta = when asset is {
        AdaAsset -> {
          expect witness == None
          state.lovelace_delta
        }
        TokenAsset { policy_id, asset_name } -> {
          expect Some(AssetDeltaWitness { delta, proof }) = witness
          expect
            mpf_proof_v1.has(
              state.asset_delta_root,
              bytearray.concat(policy_id, asset_name),
              cbor.serialise(delta),
              proof,
            )
          delta
        }
      }
      step_04.value_not_preserved_fault_is_established_v1(
        step_04.State {
          bad_tx_id: state.transaction_id,
          claimed_asset: asset,
          claimed_direction: direction,
          final_delta,
        },
      )
    }
  }
}
