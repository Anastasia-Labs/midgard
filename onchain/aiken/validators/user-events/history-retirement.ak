use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Address, Credential, Script}
use cardano/transaction.{Transaction}
use midgard/event_history/admission
use midgard/event_history/order_facts.{Deposit, Kind, Withdrawal}
use midgard/event_history/retirement.{Args}
use midgard/hub_oracle as hub

/// Isolated to keep both reference scripts publishable. Membership is checked
/// directly against authenticated event bytes, so large externally stored
/// payloads are not copied into a generic PHAS withdrawal redeemer.
validator retirement_observer(
  hub_oracle: ScriptHash,
  kind: Kind,
  retention_address: Address,
  protection_duration: Int,
  inline_limit: Int,
  max_payload_bytes: Int,
  max_payload_nodes: Int,
) {
  withdraw(args: Args, own_credential: Credential, tx: Transaction) {
    expect list.has(tx.withdrawals, Pair(own_credential, 0))
    let authority = hub.get_datum(
      tx.reference_inputs,
      hub_oracle,
      args.hub_reference_index,
    )
    let (policy, address) = when kind is {
      Deposit -> (authority.deposit, authority.deposit_addr)
      Withdrawal -> (authority.withdrawal, authority.withdrawal_addr)
    }
    expect list.has(tx.withdrawals, Pair(Script(policy), 0))
    retirement.retire(
      tx,
      admission.Parameters {
        policy,
        address,
        retention_address,
        kind,
        protection_duration,
        inline_limit,
        max_payload_bytes,
        max_payload_nodes,
      },
      retirement.Authority {
        state_queue_policy: authority.state_queue,
        settlement_policy: authority.settlement,
        reserve_address: authority.reserve_addr,
        payout_policy: authority.payout,
        payout_address: authority.payout_addr,
      },
      args.witness,
    )
  }

  else(_) {
    fail
  }
}
