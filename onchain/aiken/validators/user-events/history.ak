use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Address, Credential, Script}
use cardano/assets.{PolicyId}
use cardano/certificate.{Certificate, RegisterCredential}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/event_history/actions.{Apply, Initialize, Observe}
use midgard/event_history/admission
use midgard/event_history/order_facts.{Deposit, Kind, Withdrawal}
use midgard/event_history/output_claims
use midgard/event_history/ordering
use midgard/event_history/retirement
use midgard/hub_oracle as hub

/// One multi-purpose script per kind. Spending and minting invoke exactly its
/// own zero-withdrawal observer. Each observer operation validates the complete
/// consumed list-input set and exact policy mint, so no per-event staking
/// registration or reusable per-input approval is needed.
validator history(
  hub_oracle: ScriptHash,
  kind: Kind,
  retention_address: Address,
  retirement_observer: ScriptHash,
  one_shot: OutputReference,
  protection_duration: Int,
  inline_limit: Int,
  max_payload_bytes: Int,
  max_payload_nodes: Int,
) {
  spend(
    _datum: Option<Data>,
    input_index: Int,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(input) = list.at(tx.inputs, input_index)
    expect input.output_reference == own_ref
    expect Script(own_policy) = input.output.address.payment_credential
    let node = ordering.authenticate(
      input.output,
      own_policy,
      address.from_script(own_policy),
    )
    and {
      assets.quantity_of(
        input.output.value,
        own_policy,
        ordering.token_name(node.position),
      ) == 1,
      list.has(tx.withdrawals, Pair(Script(own_policy), 0)),
    }
  }

  mint(_redeemer: Data, own_policy: PolicyId, tx: Transaction) {
    list.has(tx.withdrawals, Pair(Script(own_policy), 0))
  }

  withdraw(redeemer: Observe, own_credential: Credential, tx: Transaction) {
    expect Script(own_policy) = own_credential
    expect list.has(tx.withdrawals, Pair(own_credential, 0))
    let params = admission.Parameters {
      policy: own_policy,
      address: address.from_script(own_policy),
      retention_address,
      kind,
      protection_duration,
      inline_limit,
      max_payload_bytes,
      max_payload_nodes,
    }
    when redeemer is {
      Initialize { nonce_input_index, root_output_index } ->
        admission.initialize(
          tx,
          params,
          one_shot,
          nonce_input_index,
          root_output_index,
        )
      Apply { hub_reference_index, operation } -> {
        let authority = hub.get_datum(
          tx.reference_inputs,
          hub_oracle,
          hub_reference_index,
        )
        let (policy, list_address) = when kind is {
          Deposit -> (authority.deposit, authority.deposit_addr)
          Withdrawal -> (authority.withdrawal, authority.withdrawal_addr)
        }
        expect policy == own_policy
        expect list_address == params.address
        let other_policy = when kind is {
          Deposit -> authority.withdrawal
          Withdrawal -> authority.deposit
        }
        expect output_claims.exclusive(tx, own_policy, other_policy, redeemer)
        when operation is {
          actions.InsertFiller {
            predecessor_input_index,
            predecessor_output_index,
            filler_output_index,
          } ->
            admission.insert_filler(
              tx,
              params,
              predecessor_input_index,
              predecessor_output_index,
              filler_output_index,
            )
          actions.InsertOrder {
            predecessor_input_index,
            predecessor_output_index,
            order_output_index,
            nonce_input_index,
            external_reference_index,
          } ->
            admission.insert_order(
              tx,
              params,
              predecessor_input_index,
              predecessor_output_index,
              order_output_index,
              nonce_input_index,
              external_reference_index,
            )
          actions.PromoteFiller {
            filler_input_index,
            order_output_index,
            refund_output_index,
            nonce_input_index,
            external_reference_index,
          } ->
            admission.promote_filler(
              tx,
              params,
              filler_input_index,
              order_output_index,
              refund_output_index,
              nonce_input_index,
              external_reference_index,
            )
          actions.ReclaimFiller {
            predecessor_input_index,
            filler_input_index,
            predecessor_output_index,
            refund_output_index,
          } ->
            admission.reclaim_filler(
              tx,
              params,
              predecessor_input_index,
              filler_input_index,
              predecessor_output_index,
              refund_output_index,
            )
          actions.RetireOrder {
            predecessor_output_index,
            funds_output_index,
            structural_refund_output_index,
          } -> {
            expect
              list.has(tx.withdrawals, Pair(Script(retirement_observer), 0))
            expect delegated: retirement.Args = utils.get_unique_withdraw_redeemer(
              tx.redeemers,
              retirement_observer,
            )
            and {
              delegated.hub_reference_index == hub_reference_index,
              delegated.witness.predecessor_output_index == predecessor_output_index,
              delegated.witness.funds_output_index == funds_output_index,
              delegated.witness.structural_refund_output_index == structural_refund_output_index,
            }
          }
        }
      }
    }
  }

  publish(_redeemer: Data, certificate: Certificate, _tx: Transaction) {
    // Registration enables this script's observer purpose. Deregistration is
    // deliberately unavailable while the deployment's history remains live.
    expect RegisterCredential { .. } = certificate
    True
  }

  else(_) {
    fail
  }
}
