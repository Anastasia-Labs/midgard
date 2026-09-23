//// Test-only history evidence fixtures. No validator entry point reaches these
//// constructors; applied scenarios must still exercise actual admission policies.

use aiken/interval
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address
use cardano/assets.{AssetName, PolicyId, Value}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/common/utils
use midgard/event_history/node_role
use midgard/event_history/order_facts
use midgard/event_history/ordering
use midgard/event_history/proof_evidence
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/ledger_state
use midgard/state_queue
use midgard/user_events

pub fn state_queue_policy() -> PolicyId {
  fixture.state_queue_policy_id
}

pub fn parameters() -> proof_evidence.Parameters {
  proof_evidence.Parameters {
    retention_address: address.from_script(
      #"42424242424242424242424242424242424242424242424242424242",
    ),
    inline_limit: 512,
    max_payload_bytes: 5000,
    max_payload_nodes: 512,
  }
}

pub fn original_assets() -> Value {
  assets.from_lovelace(3_000_000)
}

pub fn facts(
  payload: order_facts.Payload,
  inclusion_time: Int,
) -> order_facts.Facts {
  order_facts.Facts {
    event_id: order_facts.payload_id(payload),
    inclusion_time,
    location: order_facts.Inline { payload },
    structural_lovelace: 0,
    structural_refund_key: fixture.fraud_prover_key_hash,
  }
}

pub fn order_reference(
  policy: PolicyId,
  asset_name: AssetName,
  payload: order_facts.Payload,
  inclusion_time: Int,
) -> Input {
  let raw_facts: Data = facts(payload, inclusion_time)
  let content: Data = node_role.Order { facts: raw_facts }
  let datum: ordering.Link<Data> = ordering.Link {
    position: ordering.Key(asset_name),
    next: None,
    protected_until: 100_000,
    payload: content,
  }
  Input {
    output_reference: OutputReference {
      transaction_id: #"9898989898989898989898989898989898989898989898989898989898989898",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(policy),
      value: original_assets() |> assets.add(policy, asset_name, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

/// Equal-key filler proves absence for exactly one queried identity. Negative
/// callers substitute another key, which cannot satisfy this identity witness.
pub fn absent_reference(policy: PolicyId, id: OutputReference) -> Input {
  let key = user_events.out_ref_to_nonce(id)
  let content: Data = node_role.Filler {
    refund_key: fixture.fraud_prover_key_hash,
  }
  let datum: ordering.Link<Data> = ordering.Link {
    position: ordering.Key(key),
    next: None,
    protected_until: 100_000,
    payload: content,
  }
  Input {
    output_reference: OutputReference {
      transaction_id: #"9797979797979797979797979797979797979797979797979797979797979797",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(policy),
      value: assets.from_lovelace(3_000_000) |> assets.add(policy, key, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

pub fn after_header(tx: Transaction) -> Transaction {
  Transaction { ..tx, validity_range: interval.between(21, 22) }
}

pub fn terminal(
  tx: Transaction,
  header: ledger_state.HeaderV1,
  proof_asset: AssetName,
) -> Transaction {
  let header_hash = utils.serialise_and_hash_28(header)
  let node = state_queue.StateQueueNode {
    header,
    da_attestation: state_queue.no_da_attestation,
    proven_fraud: Some(proof_asset),
  }
  let datum: state_queue.Datum = linked_list.Element {
    data: linked_list.Node(node),
    link: None,
  }
  let reference = Input {
    output_reference: OutputReference {
      transaction_id: #"9696969696969696969696969696969696969696969696969696969696969696",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(state_queue_policy()),
      value: assets.from_lovelace(5_000_000)
        |> assets.add(
            state_queue_policy(),
            bytearray.concat(state_queue.block_asset_name_prefix, header_hash),
            1,
          ),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
  Transaction { ..after_header(tx), reference_inputs: [reference] }
}
