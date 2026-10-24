use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/transaction.{Input, Redeemer, ScriptPurpose, Spend}
use midgard/validation_machine/machine_types.{AssetFoldClaim}

pub const asset_fold_role = "V1VtVamAssetFoldYield"

/// One zero withdrawal may discharge exactly one of the three asset spends.
/// Its claim comes from that input's own Continue action, at field zero.
pub fn unique_asset_fold_dispatch(
  replay_hash: ScriptHash,
  output_hash: ScriptHash,
  mint_hash: ScriptHash,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> (ScriptHash, AssetFoldClaim) {
  expect [input] = inputs
    |> list.filter(
        fn(input) {
          when input.output.address.payment_credential is {
            Script(hash) ->
              hash == replay_hash || hash == output_hash || hash == mint_hash
            _ -> False
          }
        },
      )
  expect Script(hash) = input.output.address.payment_credential
  expect [Pair(_, redeemer)] = redeemers
    |> list.filter(fn(pair) { pair.1st == Spend(input.output_reference) })
  expect Pair(1, [action]) = builtin.un_constr_data(redeemer)
  expect Pair(0, [claim_data, ..]) = builtin.un_constr_data(action)
  expect claim: AssetFoldClaim = claim_data
  (hash, claim)
}
