//// The pooled DA committee bond: one UTxO at `Script(pool policy)` holding the
//// committee's bond, identified by a one-shot NFT that is never burned.
//// `validators/da-bond-pool.ak` enforces its transitions; the attestation
//// contract reads it through `get_authentic_pool`.

use aiken/builtin
use aiken/collection/list
use aiken/math
use cardano/address.{Script}
use cardano/assets.{AssetName, PolicyId, Value}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/availability_challenge as availability
use midgard/common/types.{PosixTime}
use midgard/common/utils
use midgard/hub_oracle

pub const da_bond_pool_asset_name: AssetName = "MIDGARD_DA_BOND_POOL"

/// Constructor index of `state_queue.RemoveUnavailableBlockAfterTimeout` in
/// `state_queue.MintRedeemer`. Slash checks only this index; the state-queue
/// mint and its yield decode the redeemer in full in the same transaction.
/// `da-bond-pool.test.ak` pins it against the real type.
pub const remove_unavailable_block_after_timeout_constructor_index: Int = 5

/// `Bonded` backs attestations. `Withdrawing` refuses new attestations until the
/// quorum cancels the withdrawal or completes it at or after `unlock_at`.
pub type DaBondPoolDatum {
  Bonded
  Withdrawing { unlock_at: PosixTime }
}

pub type MintRedeemer {
  InitPool { output_index: Int }
}

/// Every arm continues the pool at `output_index`.
pub type SpendRedeemer {
  TopUp { output_index: Int }
  Slash {
    hub_oracle_ref_input_index: Int,
    state_queue_mint_redeemer_index: Int,
    correction_lock_input_index: Int,
    output_index: Int,
  }
  BeginWithdraw { da_params_ref_input_index: Int, output_index: Int }
  CancelWithdraw { da_params_ref_input_index: Int, output_index: Int }
  CompleteWithdraw {
    amount: Int,
    da_params_ref_input_index: Int,
    output_index: Int,
  }
}

/// Lovelace above the pool floor. The floor keeps the pool UTxO above its
/// min-UTxO and never counts as backing, so a pool at or below it backs 0.
pub fn lovelace_backing(
  lovelace: Int,
  parameters: availability.ParametersV1,
) -> Int {
  math.max(0, lovelace - parameters.da_bond_pool_floor_lovelace)
}

pub fn pool_backing(value: Value, parameters: availability.ParametersV1) -> Int {
  lovelace_backing(assets.lovelace_of(value), parameters)
}

/// The authentic pool read as a reference input: it sits at
/// `Script(pool_policy_id)`, holds the pool NFT once and carries an inline
/// datum. Returns the datum and the pool's lovelace (see `lovelace_backing`).
///
/// The NFT is minted once and every pool continuation keeps it at the pool
/// address with a lovelace-only value, so the credential and the NFT together
/// identify the one pool UTxO.
pub fn get_authentic_pool(
  reference_inputs: List<Input>,
  pool_policy_id: PolicyId,
  pool_ref_input_index: Int,
) -> (DaBondPoolDatum, Int) {
  expect
      Some(Input {
        output: Output { address, value, datum: InlineDatum(datum_data), .. },
        ..
      })
    = reference_inputs |> list.at(pool_ref_input_index)
  expect Script(pool_script_hash) = address.payment_credential
  expect pool_script_hash == pool_policy_id
  expect assets.quantity_of(value, pool_policy_id, da_bond_pool_asset_name) == 1
  expect datum: DaBondPoolDatum = datum_data
  (datum, assets.lovelace_of(value))
}

/// The authentic pool spent as an input: the input at `pool_input_index` sits
/// at `Script(pool_policy_id)`, holds the pool NFT once and carries an inline
/// datum. Returns its output. The datum is not decoded and either state
/// passes: a `Withdrawing` pool stays slashable, and the pool's own spend
/// validator decodes the datum in the same transaction.
pub fn get_authentic_pool_input(
  inputs: List<Input>,
  pool_policy_id: PolicyId,
  pool_input_index: Int,
) -> Output {
  expect Some(Input { output, .. }) = inputs |> list.at(pool_input_index)
  expect Output { address, value, datum: InlineDatum(_), .. } = output
  expect Script(pool_script_hash) = address.payment_credential
  expect pool_script_hash == pool_policy_id
  expect assets.quantity_of(value, pool_policy_id, da_bond_pool_asset_name) == 1
  output
}

/// The state-queue policy, hub datum field 4, read from the authentic hub
/// oracle reference input without decoding the rest of the datum.
/// `da-bond-pool.test.ak` pins it against the full `hub_oracle.get_datum` read.
pub fn get_state_queue_policy(
  reference_inputs: List<Input>,
  hub_oracle_policy_id: PolicyId,
  hub_oracle_ref_input_index: Int,
) -> PolicyId {
  expect Input { output: Output { datum: InlineDatum(hub_datum_data), .. }, .. }
    = utils.get_authentic_input_of(
    reference_inputs,
    hub_oracle_policy_id,
    hub_oracle.asset_name,
    hub_oracle_ref_input_index,
  )
  expect Pair(0, [_,
        _, _, _, state_queue_data, ..]) = builtin.un_constr_data(hub_datum_data)
  builtin.un_b_data(state_queue_data)
}
