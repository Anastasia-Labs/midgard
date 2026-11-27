//// Source custody reaches its event pre-root through two physical counted
//// openings. Each transaction invokes the existing one-claim verifier once.

use cardano/transaction.{Input, Redeemer, ScriptPurpose}
use midgard/fraud_proofs/counted_membership_carriage_v1.{
  CarriedCountedMembershipV1,
}
use midgard/fraud_proofs/direct_script_derivation/bind_state.{EventSelectedV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, SourceBoundV1}
use midgard/fraud_proofs/field_opening_v1.{source_kind_of_anchor}
use midgard/fraud_proofs/first_fault_v1.{network_order_context_v1}
use midgard/fraud_proofs/native_tx/compact.{decode_tx_compact_view_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state

pub fn select_event_v1(
  source: SourceBoundV1,
  event_opening: CarriedCountedMembershipV1<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> EventSelectedV1 {
  EventSelectedV1 {
    subject: source.subject,
    header: source.header,
    compact_cbor: source.compact_cbor,
    step_index: substrate.subject_event_step_index_v1(
      source.header,
      source.subject,
      event_opening,
      reference_inputs,
      redeemers,
    ),
  }
}

pub fn event_pre_root_v1(
  selected: EventSelectedV1,
  transition_opening: CarriedCountedMembershipV1<
    Int,
    ledger_state.TransitionStep,
  >,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> BoundV1 {
  let pre = substrate.subject_selected_event_pre_state_v1(
    selected.header,
    selected.subject,
    selected.step_index,
    transition_opening,
    reference_inputs,
    redeemers,
  )
  BoundV1 {
    subject: selected.subject,
    compact_cbor: selected.compact_cbor,
    network_order_context: network_order_context_v1(
      decode_tx_compact_view_v1(
        source_kind_of_anchor(selected.subject.source_kind),
        selected.compact_cbor,
      ),
      selected.header,
    ),
    event_pre_root: pre.pre_root,
  }
}
