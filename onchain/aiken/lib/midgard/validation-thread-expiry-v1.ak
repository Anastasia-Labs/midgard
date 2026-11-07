//// Thread expiry: a validation case thread its case can no longer take.
////
//// Past `started_at + D_case` the case may close with a thread still open, as
//// a defender win, so the thread's value returns to its defender. One
//// withdraw-zero script (`thread-expiry-v1`) checks the expiry, wherever the
//// thread sits: at the game, timeout, boundary or award validator, or at any
//// resolver of the one-step chain. It reads the thread's `ThreadHeaderV1`,
//// the first field of the state of every thread holder datum.
////
//// Each holder releases its own thread input to that script
//// (`holder_releases_to_expiry`), and the case mint burns the thread token
//// beside its withdrawal (`BurnThreadToken`).

use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/collection/pairs
use aiken/crypto.{ScriptHash}
use aiken/math
use cardano/address.{Script, from_verification_key}
use cardano/assets.{PolicyId, lovelace_of}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use env
use midgard/common/utils
use midgard/validation_case_v1.{ThreadHeaderV1}

/// The header of a thread holder datum: the datum is
/// `StepDatum { fraud_prover, data: Some(state) }`, and the header is the
/// first field of `state`.
pub fn thread_header_of(datum: Data) -> ThreadHeaderV1 {
  expect [_fraud_prover, data] = builtin.unconstr_fields(datum)
  expect [state] = builtin.unconstr_fields(data)
  expect [header, ..] = builtin.unconstr_fields(state)
  expect header: ThreadHeaderV1 = header
  header
}

/// The clause every thread holder adds to its spend: it releases its own
/// input when that input holds a thread token and the thread expiry script
/// withdraws in the same transaction. The expiry script checks the rest. A
/// UTxO at the holder without a thread token, such as a lost-defense receipt
/// at the award validator, is never released by this clause.
pub fn holder_releases_to_expiry(
  thread_token_policy_id: PolicyId,
  thread_expiry_script_hash: ScriptHash,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  and {
    pairs.has_key(tx.withdrawals, Script(thread_expiry_script_hash)),
    when transaction.find_input(tx.inputs, own_out_ref) is {
      Some(Input { output: Output { value, .. }, .. }) ->
        !dict.is_empty(assets.tokens(value, thread_token_policy_id))
      None -> False
    },
  }
}

/// The expiry of the one thread in `tx`:
/// - exactly one input holds a token of the thread policy, and no input sits
///   at the case validator (whose hash is the thread policy), so no
///   `ThreadClose` can count this burn as a close with another outcome;
/// - that input holds exactly one thread token, and the token is burned;
/// - the validity lower bound is past the header's `started_at + D_case`;
/// - the thread's lovelace goes to the header's defender, tagged with the
///   thread's out-ref, less at most `min(fee, cap)` when the thread is the
///   only input.
pub fn expires(
  thread_token_policy_id: PolicyId,
  case_duration_milliseconds: Int,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, mint, fee, validity_range, .. } = tx
  let thread_inputs = list.filter(
    inputs,
    fn(input) {
      expect
        input.output.address.payment_credential != Script(
          thread_token_policy_id,
        )
      !dict.is_empty(assets.tokens(input.output.value, thread_token_policy_id))
    },
  )
  expect
      [
        Input {
          output_reference,
          output: Output { value, datum: InlineDatum(datum), .. },
        },
      ]
    = thread_inputs
  expect [Pair(name, 1)] = dict.to_pairs(
    assets.tokens(value, thread_token_policy_id),
  )
  let header = thread_header_of(datum)
  let fee_allowance = when inputs is {
    [_] -> math.min(fee, env.max_dispute_move_fee_lovelace)
    _ -> 0
  }
  let defender = from_verification_key(header.defender_vkh)
  let tag: Data = output_reference
  let thread_lovelace = lovelace_of(value)
  and {
    utils.get_inclusive_lower_bound_of_interval(validity_range) > header.started_at + case_duration_milliseconds,
    assets.quantity_of(mint, thread_token_policy_id, name) == -1,
    list.any(
      outputs,
      fn(output) {
        and {
          output.address == defender,
          output.datum == InlineDatum(tag),
          lovelace_of(output.value) >= thread_lovelace - fee_allowance,
        }
      },
    ),
  }
}
