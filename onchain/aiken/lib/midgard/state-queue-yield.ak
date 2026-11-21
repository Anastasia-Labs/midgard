use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Credential, Script, from_script}
use cardano/assets.{AssetName, Lovelace, PolicyId}
use cardano/transaction.{
  Input, NoDatum, Output, OutputReference, Redeemer, ScriptPurpose, Withdraw,
}
use midgard/common/utils

pub const commit_role: AssetName = "StateQueueCommitYield"

pub const unattested_timeout_role: AssetName = "StateQueueUnattestedYield"

pub const unavailable_timeout_role: AssetName = "StateQueueUnavailableYield"

pub const fraud_removal_role: AssetName = "StateQueueFraudRemovalYield"

pub const merge_role: AssetName = "StateQueueMergeYield"

/// Authenticate and invoke one arm-specific rewarding validator. The indexed
/// reference input must carry exactly the deployment role NFT and the script
/// hash invoked by an exact zero-valued withdrawal. Requiring its unique
/// withdraw redeemer completes the fail-closed ledger handshake.
pub fn require_authenticated_zero_yield(
  reference_inputs: List<Input>,
  withdrawals: Pairs<Credential, Lovelace>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  reference_script_auth_policy_id: PolicyId,
  role_asset_name: AssetName,
  yield_to_ref_input_index: Int,
) -> ScriptHash {
  expect
      Some(Input {
        output: Output { value, reference_script: Some(script_hash), .. },
        ..
      })
    = reference_inputs |> list.at(yield_to_ref_input_index)
  expect [Pair(candidate_role, 1)] = value
    |> assets.tokens(reference_script_auth_policy_id)
    |> dict.to_pairs
  expect candidate_role == role_asset_name
  expect [Pair(_, amount)] = withdrawals
    |> list.filter(
        fn(entry) {
          when entry.1st is {
            Script(candidate_hash) -> candidate_hash == script_hash
            _ -> False
          }
        },
      )
  expect amount == 0
  expect _redeemer: Data = utils.get_unique_withdraw_redeemer(
    redeemers,
    script_hash,
  )
  script_hash
}

const test_auth_policy =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const test_commit_hash =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const test_other_hash =
  #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

fn test_reference_input(role: AssetName, script_hash: ScriptHash) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
      output_index: 0,
    },
    output: Output {
      address: from_script(script_hash),
      value: assets.from_lovelace(2_000_000)
        |> assets.merge(assets.from_asset(test_auth_policy, role, 1)),
      datum: NoDatum,
      reference_script: Some(script_hash),
    },
  }
}

fn test_withdrawals(script_hash: ScriptHash) -> Pairs<Credential, Lovelace> {
  [Pair(Script(script_hash), 0)]
}

fn test_redeemers(script_hash: ScriptHash) -> Pairs<ScriptPurpose, Redeemer> {
  let redeemer: Data = 0
  [Pair(Withdraw(Script(script_hash)), redeemer)]
}

test authenticated_zero_yield_accepts_exact_role_script_and_withdrawal() {
  require_authenticated_zero_yield(
    [test_reference_input(commit_role, test_commit_hash)],
    test_withdrawals(test_commit_hash),
    test_redeemers(test_commit_hash),
    test_auth_policy,
    commit_role,
    0,
  ) == test_commit_hash
}

test authenticated_zero_yield_rejects_cross_arm_role_substitution() fail {
  require_authenticated_zero_yield(
    [test_reference_input(unavailable_timeout_role, test_commit_hash)],
    test_withdrawals(test_commit_hash),
    test_redeemers(test_commit_hash),
    test_auth_policy,
    commit_role,
    0,
  ) == test_commit_hash
}

test authenticated_zero_yield_rejects_withdrawal_script_substitution() fail {
  require_authenticated_zero_yield(
    [test_reference_input(commit_role, test_commit_hash)],
    test_withdrawals(test_other_hash),
    test_redeemers(test_other_hash),
    test_auth_policy,
    commit_role,
    0,
  ) == test_commit_hash
}

test authenticated_zero_yield_rejects_nonzero_reward_movement() fail {
  require_authenticated_zero_yield(
    [test_reference_input(commit_role, test_commit_hash)],
    [Pair(Script(test_commit_hash), 1)],
    test_redeemers(test_commit_hash),
    test_auth_policy,
    commit_role,
    0,
  ) == test_commit_hash
}
