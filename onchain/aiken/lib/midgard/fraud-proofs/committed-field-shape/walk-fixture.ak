//// Test fixtures for the committed-field-shape step-01 walk. No validator
//// reaches this module; the validator suites build every walk transaction
//// from it so that each selector differs from its positive twin in the one
//// member it names.
////
//// The block, claim, carriage and byte builders are §12.7's walk fixtures,
//// reused: the two families walk the same bytes through the same door. What
//// is this family's own is every typed value a step reads or writes — the
//// datum, the redeemer, the cursor and the step-02 state.

use midgard/computation_thread as ct
use midgard/fraud_proofs/canonical_decodability/rule.{CommittedFieldClaimV1}
use midgard/fraud_proofs/committed_field_shape/step_01.{
  Args, Datum, SpendRedeemer, WalkState,
}
use midgard/fraud_proofs/committed_field_shape/step_02
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

/// §2.5 slot 0 (spend inputs) — a fixed-stride body field, stride 40.
pub const spend_inputs_field_index: Int = 0

/// §2.5 slot 2 (outputs) — a variable-width body field.
pub const outputs_field_index: Int = 2

/// §2.5 slot 7 (address witnesses) — a fixed-stride witness-set field, stride
/// 103.
pub const address_witnesses_field_index: Int = 7

/// The thread's first step: minted with no state.
pub fn fresh_step_datum() -> Datum {
  ct.StepDatum { fraud_prover: fixture.fraud_prover_key_hash, data: None }
}

/// A later step: the state the previous batch wrote.
pub fn resumed_step_datum(state: WalkState) -> Datum {
  ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(state),
  }
}

pub fn walk_state(
  block: fixture.NativeBlockFixtureV1,
  field_index: Int,
  remaining: Int,
  offset: Int,
) -> WalkState {
  WalkState { bad_tx_id: block.tx_id, field_index, remaining, offset }
}

pub fn walk_state_data(state: WalkState) -> Data {
  let state_data: Data = state
  state_data
}

pub fn verdict_state_data(
  block: fixture.NativeBlockFixtureV1,
  field_index: Int,
  verdict: Int,
) -> Data {
  let state: step_02.State = step_02.State {
    bad_tx_id: block.tx_id,
    field_index,
    verdict,
  }
  let state_data: Data = state
  state_data
}

pub fn continue_redeemer(
  block: fixture.NativeBlockFixtureV1,
  claim: CommittedFieldClaimV1,
) -> SpendRedeemer {
  ct.Continue(
    Args { inclusion: fixture.native_inclusion_carriage_v1(block), claim },
  )
}
