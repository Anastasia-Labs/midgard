//// The pooled DA committee bond (spec #685 §2.1, §4.1; ticket #687).
////
//// One UTxO at `Address { Script(own policy), None }` holds the committee's
//// bond, identified by a one-shot NFT that is never burned. Anyone tops it up;
//// only an availability timeout slashes it; only the DA params owner quorum
//// withdraws from it, in two steps separated by `da_bond_withdraw_delay_ms_v1`.
////
//// Every continuation keeps the pool address (payment and stake), carries an
//// inline datum and no reference script, and holds exactly lovelace plus the
//// NFT. Exactly one input per transaction may sit at the pool's credential.

use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/math
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction,
}
use env
use midgard/availability_challenge as availability
use midgard/common/utils
use midgard/correction_lock
use midgard/da_bond_pool as pool
use midgard/da_params

/// The continuing pool output at `output_index`: the pool address, an inline
/// datum, no reference script, and lovelace plus the NFT only. Returns its
/// datum and lovelace.
fn continuing_pool_output(
  outputs: List<Output>,
  output_index: Int,
  pool_address: Address,
  own_policy: PolicyId,
) -> (Data, Int) {
  expect
      Some(Output {
        address,
        value,
        datum: InlineDatum(datum_data),
        reference_script: None,
      })
    = outputs |> list.at(output_index)
  expect address == pool_address
  expect
    assets.without_lovelace(value) == assets.from_asset(
      own_policy,
      pool.da_bond_pool_asset_name,
      1,
    )
  (datum_data, assets.lovelace_of(value))
}

/// The DA params owner quorum signed the transaction. The params UTxO is
/// authenticated by credential, NFT and inline datum (`da_params`), and the
/// count walks the owners, never the signers.
fn owner_quorum_signed(
  tx: Transaction,
  da_params_policy_id: PolicyId,
  da_params_ref_input_index: Int,
) -> Bool {
  let params = da_params.get_da_params(
    tx.reference_inputs,
    da_params_policy_id,
    da_params_ref_input_index,
  )
  da_params.owner_quorum_met(
    params.owners,
    tx.extra_signatories,
    params.update_threshold,
    0,
  )
}

validator da_bond_pool(
  init_ref: OutputReference,
  hub_oracle_policy_id: PolicyId,
  da_params_policy_id: PolicyId,
  parameters: availability.ParametersV1,
) {
  /// One-shot: spends `init_ref` and mints the one NFT to a `Bonded` pool at
  /// `Script(own policy)` with no stake credential. There is no burn arm, so
  /// every other mint or burn under this policy fails.
  mint(redeemer: pool.MintRedeemer, policy_id: PolicyId, tx: Transaction) {
    let pool.InitPool { output_index } = redeemer
    expect [Pair(asset_name, minted_quantity)] = tx.mint
      |> assets.tokens(policy_id)
      |> dict.to_pairs
    expect asset_name == pool.da_bond_pool_asset_name
    expect minted_quantity == 1

    expect list.any(tx.inputs, fn(input) { input.output_reference == init_ref })

    expect
        Some(Output {
          address,
          value,
          datum: InlineDatum(datum_data),
          reference_script: None,
        })
      = tx.outputs |> list.at(output_index)
    expect
      address == Address {
        payment_credential: Script(policy_id),
        stake_credential: None,
      }
    let bonded: Data = pool.Bonded
    expect datum_data == bonded
    expect
      assets.without_lovelace(value) == assets.from_asset(
        policy_id,
        pool.da_bond_pool_asset_name,
        1,
      )
    expect assets.lovelace_of(value) >= parameters.da_bond_pool_floor_lovelace

    True
  }

  spend(
    _datum: Option<Data>,
    redeemer: pool.SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, outputs, .. } = tx
    expect
        Some(Input {
          output: Output {
            address: pool_address,
            value: pool_in_value,
            datum: InlineDatum(pool_in_datum_data),
            ..
          },
          ..
        })
      = transaction.find_input(inputs, own_ref)
    expect Script(own_policy) = pool_address.payment_credential
    expect
      assets.quantity_of(
        pool_in_value,
        own_policy,
        pool.da_bond_pool_asset_name,
      ) == 1
    // A stray UTxO at the pool credential cannot ride along with a pool action.
    expect
      list.count(
        inputs,
        fn(input) {
          input.output.address.payment_credential == Script(own_policy)
        },
      ) == 1
    expect pool_in_datum: pool.DaBondPoolDatum = pool_in_datum_data
    let pool_in_lovelace = assets.lovelace_of(pool_in_value)

    when redeemer is {
      pool.TopUp { output_index } -> {
        let (pool_out_datum_data, pool_out_lovelace) = continuing_pool_output(
          outputs,
          output_index,
          pool_address,
          own_policy,
        )
        expect pool_out_datum_data == pool_in_datum_data
        expect
          pool_out_lovelace - pool_in_lovelace >= parameters.da_bond_min_top_up_lovelace
        True
      }

      // Slash binds to the first step of an unavailable-block removal, the
      // only step that runs the availability `TimeoutChallenge`:
      // - the tx's state-queue mint redeemer (policy read from the hub, field
      //   4) is `RemoveUnavailableBlockAfterTimeout` (constructor 5);
      // - the correction-lock singleton the removal spends is `Idle`. On an
      //   Idle lock that redeemer forces a DACH burn of -1 under the
      //   availability policy, and only `TimeoutChallenge` can make that burn
      //   while the node is removed; the resume steps run on a `Locked` lock
      //   with no availability redeemer at all.
      // Valid in both states; the datum, address and NFT are kept, and the pool
      // gives up exactly `min(da_bond, backing)`.
      pool.Slash {
        hub_oracle_ref_input_index,
        state_queue_mint_redeemer_index,
        correction_lock_input_index,
        output_index,
      } -> {
        let state_queue_policy = pool.get_state_queue_policy(
          tx.reference_inputs,
          hub_oracle_policy_id,
          hub_oracle_ref_input_index,
        )
        let state_queue_redeemer = utils.get_redeemer_at(
          tx.redeemers,
          Mint(state_queue_policy),
          state_queue_mint_redeemer_index,
        )
        expect
          builtin.un_constr_data(state_queue_redeemer).1st == pool.remove_unavailable_block_after_timeout_constructor_index

        expect Some(Input { output: lock_output, .. }) = inputs
          |> list.at(correction_lock_input_index)
        expect
          correction_lock.output_has_token(lock_output, hub_oracle_policy_id)
        expect correction_lock.decode_datum(lock_output) == correction_lock.Idle

        let taken = math.min(
          parameters.da_bond_lovelace,
          pool.lovelace_backing(pool_in_lovelace, parameters),
        )
        let (pool_out_datum_data, pool_out_lovelace) = continuing_pool_output(
          outputs,
          output_index,
          pool_address,
          own_policy,
        )
        expect pool_out_datum_data == pool_in_datum_data
        expect pool_out_lovelace == pool_in_lovelace - taken
        True
      }

      pool.BeginWithdraw { da_params_ref_input_index, output_index } -> {
        expect
          owner_quorum_signed(
            tx,
            da_params_policy_id,
            da_params_ref_input_index,
          )
        expect pool.Bonded = pool_in_datum
        // Anchored at the inclusive upper bound, as Open anchors `opened_at`:
        // the tx lands at or before it, so the delay is never shortened.
        let (_inclusive_lower_bound, inclusive_upper_bound) = utils.get_inclusive_bounds_of_a_short_validity_range(
          tx.validity_range,
        )
        let (pool_out_datum_data, pool_out_lovelace) = continuing_pool_output(
          outputs,
          output_index,
          pool_address,
          own_policy,
        )
        let withdrawing: Data = pool.Withdrawing {
          unlock_at: inclusive_upper_bound + env.da_bond_withdraw_delay_ms_v1,
        }
        expect pool_out_datum_data == withdrawing
        expect pool_out_lovelace == pool_in_lovelace
        True
      }

      pool.CancelWithdraw { da_params_ref_input_index, output_index } -> {
        expect
          owner_quorum_signed(
            tx,
            da_params_policy_id,
            da_params_ref_input_index,
          )
        expect pool.Withdrawing { .. } = pool_in_datum
        let (pool_out_datum_data, pool_out_lovelace) = continuing_pool_output(
          outputs,
          output_index,
          pool_address,
          own_policy,
        )
        let bonded: Data = pool.Bonded
        expect pool_out_datum_data == bonded
        expect pool_out_lovelace == pool_in_lovelace
        True
      }

      pool.CompleteWithdraw { amount, da_params_ref_input_index, output_index } -> {
        expect
          owner_quorum_signed(
            tx,
            da_params_policy_id,
            da_params_ref_input_index,
          )
        expect pool.Withdrawing { unlock_at } = pool_in_datum
        // Inclusive lower bound, as Timeout's `lower >= response_deadline`:
        // the tx lands no earlier than it, so never before `unlock_at`.
        expect
          utils.get_inclusive_lower_bound_of_interval(tx.validity_range) >= unlock_at
        expect amount > 0
        expect amount <= pool.lovelace_backing(pool_in_lovelace, parameters)
        let (pool_out_datum_data, pool_out_lovelace) = continuing_pool_output(
          outputs,
          output_index,
          pool_address,
          own_policy,
        )
        let bonded: Data = pool.Bonded
        expect pool_out_datum_data == bonded
        expect pool_out_lovelace == pool_in_lovelace - amount
        True
      }
    }
  }

  else(_) {
    fail
  }
}
