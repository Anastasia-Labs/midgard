//// Shared input-existence proof state. Every ledger decision uses the selected
//// transaction event's committed pre-state, never the block's initial root or
//// the presence/absence of a producing transaction elsewhere in the block.

use aiken/merkle_patricia_forestry as mpf
use midgard/fraud_proofs/common.{NonMembershipCarriage}
use midgard/fraud_proofs/counted_membership_carriage_v1.{
  CarriedCountedMembershipV1,
}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/first_fault_v1.{NetworkOrderContextV1}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput,
  TxCompactView} as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/ledger_state

pub type SourceStateV1 {
  subject: VerdictSubjectV1,
  header: ledger_state.HeaderV1,
  compact: TxCompactView,
}

pub type SelectArgsV1 {
  input_index: Int,
  output_index: Int,
  inputs_opening: FieldOpeningV1,
  selected_input_index: Int,
  claimed_inputs_opening: Option<FieldOpeningV1>,
  event_membership: CarriedCountedMembershipV1<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
}

pub type SelectedInputStateV1 {
  subject: VerdictSubjectV1,
  header: ledger_state.HeaderV1,
  compact: TxCompactView,
  source_kind: Int,
  input_index: Int,
  selected_input: Option<MidgardTxInput>,
  claimed_input: Option<MidgardTxInput>,
  step_index: Int,
}

pub type PreStateArgsV1 {
  input_index: Int,
  output_index: Int,
  transition_membership: CarriedCountedMembershipV1<
    Int,
    ledger_state.TransitionStep,
  >,
}

pub type LedgerStateV1 {
  subject: VerdictSubjectV1,
  compact: TxCompactView,
  network_order_context: NetworkOrderContextV1,
  source_kind: Int,
  input_index: Int,
  selected_input: Option<MidgardTxInput>,
  claimed_input: Option<MidgardTxInput>,
  pre_utxos_root: ByteArray,
}

pub type LedgerEvidenceV1 {
  OutOfDomain
  Present { value_hash: ByteArray, proof: mpf.Proof }
  Absent { non_membership: NonMembershipCarriage }
}

pub type TerminalArgsV1 {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  ledger_evidence: LedgerEvidenceV1,
}
