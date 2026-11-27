//// Retirement is a semantic list removal, not owner-authorized early refund.
//// Authority parameters must come from the deployment's authenticated hub.

use aiken/cbor
use aiken/collection/dict
use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, NoDatum, Transaction}
use midgard/event_history/admission.{Parameters}
use midgard/event_history/funding
use midgard/event_history/horizon
use midgard/event_history/mutation
use midgard/event_history/order_facts.{
  Deposit, DepositPayload, Withdrawal, WithdrawalPayload,
}
use midgard/ledger_state
use midgard/payout
use midgard/settlement
use midgard/state_queue
use midgard/transition_trace
use midgard/user_events

pub type Authority {
  state_queue_policy: PolicyId,
  settlement_policy: PolicyId,
  reserve_address: Address,
  payout_policy: PolicyId,
  payout_address: Address,
}

pub type Purpose {
  AbsorbDeposit
  InitializeWithdrawalPayout
  RefundInvalidWithdrawal { validity: ledger_state.WithdrawalValidity }
}

/// Root/domain and leaf bytes are derived from authenticated inputs. Only the
/// root count opening and trie path travel in the redeemer.
pub type Membership {
  phas_root: ByteArray,
  count: Int,
  proof: mpf.Proof,
}

pub type Args {
  hub_reference_index: Int,
  witness: Witness,
}

pub type Witness {
  predecessor_input_index: Int,
  order_input_index: Int,
  predecessor_output_index: Int,
  funds_output_index: Int,
  structural_refund_output_index: Option<Int>,
  confirmed_reference_index: Int,
  settlement_reference_index: Int,
  external_reference_index: Option<Int>,
  membership: Membership,
  purpose: Purpose,
}

fn valid_membership(
  domain: transition_trace.RootDomain,
  root: ByteArray,
  witness: Membership,
  key: Data,
  value: Data,
) -> Bool {
  and {
    witness.count > 0,
    transition_trace.verify_root_count_proof(
      transition_trace.RootCountProof {
        domain,
        root,
        phas_root: witness.phas_root,
        count: witness.count,
      },
      domain,
      root,
      witness.count,
    ),
    mpf.has(
      mpf.from_root(witness.phas_root),
      cbor.serialise(key),
      cbor.serialise(value),
      witness.proof,
    ),
  }
}

pub fn retire(
  tx: Transaction,
  params: Parameters,
  authority: Authority,
  witness: Witness,
) -> Bool {
  let node = mutation.remove(
    tx,
    params.policy,
    params.address,
    params.protection_duration,
    witness.predecessor_input_index,
    witness.order_input_index,
    witness.predecessor_output_index,
  )
  let facts = order_facts.from_node(node)
  let confirmed = state_queue.get_confirmed_state(
    tx.reference_inputs,
    authority.state_queue_policy,
    witness.confirmed_reference_index,
  )
  expect horizon.retirement_permitted(facts.inclusion_time, confirmed.end_time)
  let roots = settlement.get_datum(
    tx.reference_inputs,
    authority.settlement_policy,
    witness.settlement_reference_index,
  )
  let payload = order_facts.open_payload(
    facts,
    tx,
    params.retention_address,
    params.inline_limit,
    params.max_payload_bytes,
    params.max_payload_nodes,
    witness.external_reference_index,
  )
  expect order_facts.matches_kind(payload, params.kind)
  expect Some(input) = list.at(tx.inputs, witness.order_input_index)
  let key = user_events.out_ref_to_nonce(facts.event_id)
  let original_assets = funding.order_assets(
    input.output.value,
    params.policy,
    key,
    facts.structural_lovelace,
  )
  expect Some(output) = list.at(tx.outputs, witness.funds_output_index)
  expect witness.funds_output_index != witness.predecessor_output_index
  expect output.reference_script == None
  expect
    if facts.structural_lovelace == 0 {
      witness.structural_refund_output_index == None
    } else {
      expect Some(index) = witness.structural_refund_output_index
      expect Some(refund) = list.at(tx.outputs, index)
      and {
        index != witness.funds_output_index,
        index != witness.predecessor_output_index,
        refund.address == address.from_verification_key(
          facts.structural_refund_key,
        ),
        refund.datum == NoDatum,
        refund.reference_script == None,
        refund.value == assets.from_lovelace(assets.lovelace_of(refund.value)),
        // A third party may top up a small structural refund to ledger min-ADA.
        // Original event assets still go in full to the separate funds output.
        assets.lovelace_of(refund.value) >= facts.structural_lovelace,
      }
    }
  when witness.purpose is {
    AbsorbDeposit -> {
      expect params.kind == Deposit
      expect DepositPayload { event } = payload
      and {
        output.address == authority.reserve_address,
        output.datum == NoDatum,
        output.value == original_assets,
        valid_membership(
          transition_trace.DepositsRootDomain,
          roots.deposits_root,
          witness.membership,
          event.id,
          event.info,
        ),
      }
    }
    RefundInvalidWithdrawal { validity } -> {
      expect params.kind == Withdrawal
      expect WithdrawalPayload { event, refund_address, refund_datum } = payload
      expect validity != ledger_state.WithdrawalIsValid
      let info = ledger_state.WithdrawalInfo { ..event.info, validity }
      and {
        output.address == refund_address,
        output.datum == refund_datum,
        output.value == original_assets,
        valid_membership(
          transition_trace.WithdrawalsRootDomain,
          roots.withdrawals_root,
          witness.membership,
          event.id,
          info,
        ),
      }
    }
    InitializeWithdrawalPayout -> {
      expect params.kind == Withdrawal
      expect WithdrawalPayload { event, .. } = payload
      expect event.info.validity == ledger_state.WithdrawalIsValid
      expect InlineDatum(raw) = output.datum
      expect datum: payout.Datum = raw
      let body = event.info.body
      let remaining = assets.merge(
        assets.from_asset_list(body.l2_value),
        assets.negate(original_assets),
      )
      and {
        output.address == authority.payout_address,
        output.value == assets.add(
          original_assets,
          authority.payout_policy,
          key,
          1,
        ),
        ( tx.mint |> assets.tokens(authority.payout_policy) |> dict.to_pairs ) == [
          Pair(key, 1),
        ],
        datum == payout.Datum {
          l2_value: body.l2_value,
          l1_address: body.l1_address,
          l1_datum: body.l1_datum,
        },
        list.all(
          assets.flatten(remaining),
          fn(asset) {
            let (_, _, amount) = asset
            amount >= 0
          },
        ),
        valid_membership(
          transition_trace.WithdrawalsRootDomain,
          roots.withdrawals_root,
          witness.membership,
          event.id,
          event.info,
        ),
      }
    }
  }
}
