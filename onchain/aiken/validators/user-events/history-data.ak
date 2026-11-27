use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/transaction.{OutputReference, Transaction}
use midgard/event_history/node_role
use midgard/event_history/order_facts.{Deposit, Kind, Withdrawal}
use midgard/event_history/retained_data.{EventData}
use midgard/hub_oracle as hub

pub type Reclaim {
  absence_reference_index: Int,
  hub_reference_index: Int,
}

/// Apply this script to one deployment's event-kind-specific list. There is
/// no owner-only update/escape route and no combined retirement/reclaim arm.
validator retention(hub_oracle: ScriptHash, kind: Kind) {
  spend(
    datum: Option<EventData>,
    redeemer: Reclaim,
    _own_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(data) = datum
    let authority = hub.get_datum(
      tx.reference_inputs,
      hub_oracle,
      redeemer.hub_reference_index,
    )
    let (list_policy, list_address) = when kind is {
      Deposit -> (authority.deposit, authority.deposit_addr)
      Withdrawal -> (authority.withdrawal, authority.withdrawal_addr)
    }
    expect Some(witness) = list.at(
      tx.reference_inputs,
      redeemer.absence_reference_index,
    )
    expect
      !list.any(
        tx.inputs,
        fn(input) { input.output_reference == witness.output_reference },
      )
    and {
      retained_data.owner_authorized(
        data.reclaim_auth,
        tx.extra_signatories,
        tx.withdrawals,
      ),
      node_role.authenticated_absence(
        witness.output,
        list_policy,
        list_address,
        data.event_key,
      ),
    }
  }

  else(_) {
    fail
  }
}
