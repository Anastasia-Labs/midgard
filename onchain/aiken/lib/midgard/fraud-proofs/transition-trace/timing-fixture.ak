//// Test-only transaction context for the shared semantic timing entry point.
//// Applied admission/publication behavior is covered by emulator lifecycles.

use aiken/interval
use cardano/assets
use cardano/transaction.{InlineDatum, Input, Output, Transaction, placeholder}
use midgard/event_history/node_role
use midgard/event_history/order_facts
use midgard/event_history/ordering
use midgard/event_history/proof_fixture as history
use midgard/fraud_proofs/transition_trace/proof
use midgard/fraud_proofs/transition_trace/timed_history
use midgard/ledger_state
use midgard/user_events

pub fn evidence(
  header: ledger_state.HeaderV1,
  references: List<Input>,
) -> proof.L1EventEvidence {
  proof.L1EventEvidence {
    transaction: Transaction {
      ..placeholder,
      reference_inputs: references,
      validity_range: interval.between(header.end_time + 1, header.end_time + 2),
    },
    history_data: history.parameters(),
    reference: timed_history.Reference {
      order_index: 0,
      external_data_index: None,
    },
  }
}

/// Preserve original funds and a separately refundable structural amount.
pub fn deposit_reference(
  policy: ByteArray,
  id: ledger_state.DepositId,
  info: ledger_state.DepositInfo,
  original_assets: assets.Value,
  structural_lovelace: Int,
) -> Input {
  let payload = order_facts.DepositPayload {
    event: ledger_state.DepositEvent { id, info },
  }
  let key = user_events.out_ref_to_nonce(id)
  let reference = history.order_reference(policy, key, payload, 15)
  let facts: Data = order_facts.Facts {
    ..history.facts(payload, 15),
    structural_lovelace,
  }
  let content: Data = node_role.Order { facts }
  let node: ordering.Link<Data> = ordering.Link {
    position: ordering.Key(key),
    next: None,
    protected_until: 100_000,
    payload: content,
  }
  Input {
    ..reference,
    output: Output {
      ..reference.output,
      value: original_assets
        |> assets.merge(assets.from_lovelace(structural_lovelace))
        |> assets.add(policy, key, 1),
      datum: InlineDatum(node),
    },
  }
}
