use aiken/cbor
use aiken/merkle_patricia_forestry as mpf
use cardano/transaction.{OutputReference}
use midgard/computation_thread as ct
use midgard/fraud_proofs/withdrawal_mistag/step_01
use midgard/ledger_state
use midgard/transition_trace

pub type State {
  challenged_header_hash: ledger_state.HeaderHash,
  withdrawal_id: ledger_state.WithdrawalId,
  withdrawal_info_hash: step_01.WithdrawalInfoHash,
  claimed_valid: Bool,
  pre_utxos_root: ledger_state.MidgardLedgerRoot,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  withdrawal_info: ledger_state.WithdrawalInfo,
  event_to_step: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  transition_step: transition_trace.IndexedTraceProof,
}

pub type Datum =
  ct.StepDatum<step_01.State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

pub fn trace_coordinate_is_exact_v1(
  input_state: step_01.State,
  event_to_step: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  transition_step: transition_trace.IndexedTraceProof,
) -> Bool {
  let event_key = ledger_state.WithdrawalEventKey {
    withdrawal_id: input_state.withdrawal_id,
  }
  and {
    transition_trace.verify_root_membership_with_bytes(
      event_to_step,
      transition_trace.EventToStepRootDomain,
      input_state.event_to_step_root,
      input_state.total_event_count,
      cbor.serialise(event_to_step.key),
      cbor.serialise(event_to_step.value),
    ),
    transition_trace.verify_indexed_trace_proof(
      transition_step,
      input_state.transition_trace_root,
      input_state.transition_step_count,
    ),
    event_to_step.key == event_key,
    event_to_step.value.phase == ledger_state.Withdrawal,
    transition_step.key == event_to_step.value.step_index,
    transition_step.value.step_index == event_to_step.value.step_index,
    transition_step.value.event_key == event_key,
    transition_step.value.phase == ledger_state.Withdrawal,
  }
}

fn fixture_outref() -> OutputReference {
  OutputReference {
    transaction_id: #"0101010101010101010101010101010101010101010101010101010101010101",
    output_index: 0,
  }
}

fn fixture_trace_pair(
  phase: ledger_state.TransitionPhase,
  step_index: Int,
) -> Pair<
  transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  transition_trace.IndexedTraceProof,
> {
  let event_key = ledger_state.WithdrawalEventKey {
    withdrawal_id: fixture_outref(),
  }
  let event_value = ledger_state.EventToStepValue { phase, step_index }
  let transition_step = ledger_state.TransitionStep {
    schema_version: ledger_state.transition_step_schema_version_v1,
    step_index,
    event_key,
    phase,
    pre_utxos_root: #"1111111111111111111111111111111111111111111111111111111111111111",
    post_utxos_root: #"2222222222222222222222222222222222222222222222222222222222222222",
  }
  let event_phas_root = mpf.root(
    mpf.insert(
      mpf.empty,
      cbor.serialise(event_key),
      cbor.serialise(event_value),
      [],
    ),
  )
  let trace_phas_root = mpf.root(
    mpf.insert(
      mpf.empty,
      cbor.serialise(step_index),
      cbor.serialise(transition_step),
      [],
    ),
  )
  Pair(
    transition_trace.RootMembershipProof {
      domain: transition_trace.EventToStepRootDomain,
      root: transition_trace.commit_counted_root(
        transition_trace.EventToStepRootDomain,
        event_phas_root,
        1,
      ),
      phas_root: event_phas_root,
      count: 1,
      key: event_key,
      value: event_value,
      proof: [],
    },
    transition_trace.RootMembershipProof {
      domain: transition_trace.TransitionTraceRootDomain,
      root: transition_trace.commit_counted_root(
        transition_trace.TransitionTraceRootDomain,
        trace_phas_root,
        1,
      ),
      phas_root: trace_phas_root,
      count: 1,
      key: step_index,
      value: transition_step,
      proof: [],
    },
  )
}

fn fixture_state(
  event_to_step: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  transition_step: transition_trace.IndexedTraceProof,
) -> step_01.State {
  step_01.State {
    challenged_header_hash: #"14141414141414141414141414141414141414141414141414141414",
    withdrawal_id: fixture_outref(),
    withdrawal_info_hash: #"3333333333333333333333333333333333333333333333333333333333333333",
    claimed_valid: True,
    event_to_step_root: event_to_step.root,
    total_event_count: 1,
    transition_trace_root: transition_step.root,
    transition_step_count: 1,
  }
}

test accepts_exact_counted_withdrawal_coordinate() {
  let Pair(event_to_step, transition_step) = fixture_trace_pair(
    ledger_state.Withdrawal,
    0,
  )
  trace_coordinate_is_exact_v1(
    fixture_state(event_to_step, transition_step),
    event_to_step,
    transition_step,
  )
}

test rejects_non_withdrawal_phase() {
  let Pair(event_to_step, transition_step) = fixture_trace_pair(
    ledger_state.Deposit,
    0,
  )
  !trace_coordinate_is_exact_v1(
    fixture_state(event_to_step, transition_step),
    event_to_step,
    transition_step,
  )
}

test rejects_mutated_transition_count() {
  let Pair(event_to_step, transition_step) = fixture_trace_pair(
    ledger_state.Withdrawal,
    0,
  )
  let state = fixture_state(event_to_step, transition_step)
  !trace_coordinate_is_exact_v1(
    step_01.State { ..state, transition_step_count: 2 },
    event_to_step,
    transition_step,
  )
}

test rejects_mutated_event_root() {
  let Pair(event_to_step, transition_step) = fixture_trace_pair(
    ledger_state.Withdrawal,
    0,
  )
  let state = fixture_state(event_to_step, transition_step)
  !trace_coordinate_is_exact_v1(
    step_01.State {
      ..state,
      event_to_step_root: #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    },
    event_to_step,
    transition_step,
  )
}
