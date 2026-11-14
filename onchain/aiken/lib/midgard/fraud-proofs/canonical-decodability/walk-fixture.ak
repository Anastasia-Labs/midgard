//// Test fixtures for the canonical-decodability step-01 walk. No validator
//// reaches this module; the validator suites build every walk transaction
//// from it so that each selector differs from its positive twin in the one
//// member it names.

use aiken/builtin
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/primitive/bytearray
use cardano/address
use cardano/transaction.{Input, Output, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/canonical_decodability/rule.{
  BodyFieldClaim, CommittedFieldClaimV1, WitnessFieldClaim, envelope_batch_items,
}
use midgard/fraud_proofs/canonical_decodability/step_01.{
  Args, Datum, SpendRedeemer, WalkState,
}
use midgard/fraud_proofs/canonical_decodability/step_02
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/native_tx_field_access_v1.{
  Inline, chunk_bytes_k, max_transaction_aggregate_field_bytes,
}

/// §2.5 slot 2 (outputs) — a variable-width body field.
pub const outputs_field_index: Int = 2

/// §2.5 slot 6 (script witnesses) — a witness-set field.
pub const script_witnesses_field_index: Int = 6

/// A key that is not the thread's prover.
pub const stranger_key_hash: VerificationKeyHash =
  #"00000000000000000000000000000000000000000000000000000000"

/// An otherwise honest transaction whose slot `field_index` commits `preimage`.
pub fn committed_block(
  field_index: Int,
  preimage: ByteArray,
) -> fixture.NativeBlockFixtureV1 {
  fixture.committed_field_preimage_block_fixture_v1(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    field_index,
    preimage,
  )
}

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

/// The carriage §8's ladder admits for `preimage`: inline up to `K`, certified
/// above it (reference inputs from slot 2, after the hub oracle and the
/// state-queue node).
pub fn claim_for(
  block: fixture.NativeBlockFixtureV1,
  field_index: Int,
  preimage: ByteArray,
) -> CommittedFieldClaimV1 {
  let carriage = if bytearray.length(preimage) <= chunk_bytes_k {
    Inline { preimage }
  } else {
    fixture.certified_carriage_from_v1(2, preimage)
  }
  if field_index < 6 {
    BodyFieldClaim { field_index, carriage }
  } else {
    WitnessFieldClaim {
      field_index,
      witness_set: block.witness_set_compact,
      carriage,
    }
  }
}

pub fn carriage_reference_inputs(
  block: fixture.NativeBlockFixtureV1,
  field_index: Int,
  preimage: ByteArray,
) -> List<Input> {
  if bytearray.length(preimage) <= chunk_bytes_k {
    []
  } else {
    fixture.certified_reference_inputs_v1(block.tx_id, field_index, preimage)
  }
}

pub fn continue_redeemer(
  block: fixture.NativeBlockFixtureV1,
  claim: CommittedFieldClaimV1,
) -> SpendRedeemer {
  ct.Continue(
    Args { inclusion: fixture.native_inclusion_carriage_v1(block), claim },
  )
}

/// One walk transaction: the thread input, the successor at `successor_hash`
/// carrying `output_state`, signed by `signer`.
pub fn walk_tx(
  block: fixture.NativeBlockFixtureV1,
  field_index: Int,
  preimage: ByteArray,
  successor_hash: ScriptHash,
  output_state: Data,
  signer: VerificationKeyHash,
) -> Transaction {
  let base = fixture.continue_step_l1_tx_with_carriage_v1(
    block,
    output_state,
    carriage_reference_inputs(block, field_index, preimage),
  )
  expect [output] = base.outputs
  Transaction {
    ..base,
    outputs: [Output { ..output, address: address.from_script(successor_hash) }],
    extra_signatories: [signer],
  }
}

/// `count` one-byte items (`40`, empty payload) under a minimal array head.
pub fn one_byte_items(count: Int) -> ByteArray {
  bytearray.concat(array_head(count), one_byte_run(count))
}

/// `count` `40` bytes. `replicate_byte` stops at 8,192 bytes, so longer runs
/// are doubled up from one.
pub fn one_byte_run(count: Int) -> ByteArray {
  fixture.repeated_bytes_v1(#"40", count)
}

/// `count` two-byte-head items (`58 18` and 24 payload bytes).
pub fn two_byte_items(count: Int) -> ByteArray {
  fixture.repeated_bytes_v1(two_byte_item(), count)
}

pub fn two_byte_item() -> ByteArray {
  bytearray.concat(#"5818", builtin.replicate_byte(24, 0))
}

/// The smallest three-byte-head item (`59 0100` and 256 payload bytes).
pub fn three_byte_item() -> ByteArray {
  bytearray.concat(#"590100", builtin.replicate_byte(256, 0))
}

/// §5.1's minimal `definite_array_header(count)`.
pub fn array_head(count: Int) -> ByteArray {
  if count <= 23 {
    bytearray.push(#"", 128 + count)
  } else if count <= 255 {
    bytearray.concat(#"98", bytearray.from_int_big_endian(count, 1))
  } else {
    bytearray.concat(#"99", bytearray.from_int_big_endian(count, 2))
  }
}

/// The worst batch a field can present to one step: `B` items in at most
/// 32,768 bytes, every one at the two-byte head or wider, with as many
/// three-byte heads as the byte bound pays for (each costs 233 bytes more than
/// a two-byte one).
pub fn worst_batch_preimage() -> ByteArray {
  let three_byte_count = (
    max_transaction_aggregate_field_bytes - 3 - 26 * envelope_batch_items
  ) / 233
  bytearray.concat(
    array_head(envelope_batch_items),
    bytearray.concat(
      fixture.repeated_bytes_v1(three_byte_item(), three_byte_count),
      fixture.repeated_bytes_v1(
        two_byte_item(),
        envelope_batch_items - three_byte_count,
      ),
    ),
  )
}
