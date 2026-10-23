//// Transaction-level rows for the CEK core compute arm's ComputeConstant
//// step. The step's witness is the value root of the constant term in focus;
//// the arm accepts only the post-state that returns that root, whatever the
//// size of the constant behind it.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/transaction as l1
use fraud_proofs/validation_trace/cek_core_arm_compute as core_compute
use midgard/cek_constant_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1 as machine
use midgard/cek_proof_v1 as proof
use midgard/cek_semantic_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

const ct_asset = "cek-core-arm-compute"

const data_type_cbor = #"9f08ff"

/// A Data list of `count` integers: `count + 1` nodes.
fn integers(count: Int) -> Data {
  builtin.list_data(
    list.map(list.range(1, count), fn(i) { builtin.i_data(i * 7919) }),
  )
}

/// A Data list of 240 byte strings of 62 bytes: 15,363 payload bytes.
fn near_max() -> Data {
  builtin.list_data(
    list.map(
      list.range(1, 240),
      fn(i) { builtin.b_data(builtin.replicate_byte(62, i)) },
    ),
  )
}

/// The value root of the Data constant `payload` when its memory is
/// `memory_delta` off its Cardano memory.
fn root_with_memory(payload: Data, memory_delta: Int) -> ByteArray {
  let summary = cek_data_v1.semantic_data_summary_v1(payload)
  cek_constant_v1.semantic_constant_root_v1(
    data_type_cbor,
    summary,
    summary.memory + memory_delta,
  )
}

fn root(payload: Data) -> ByteArray {
  root_with_memory(payload, 0)
}

fn state(mode: Int, focus_root: ByteArray, cpu: Int, memory: Int) {
  machine.MachineStateV1 {
    mode,
    execution_index: 0,
    focus_root,
    environment_root: proof.empty_environment_root_v1,
    continuation_root: proof.empty_continuation_root_v1,
    auxiliary: 0,
    cpu,
    memory,
  }
}

/// Spends the compute arm on the step from the term `focus_root` to the
/// value `post_root`, with the ComputeConstant witness `value_root`.
fn spend(
  focus_root: ByteArray,
  post_root: ByteArray,
  value_root: ByteArray,
) -> Bool {
  let witness_data = builtin.constr_data(1, [builtin.b_data(value_root)])
  let bound = chain.CoreBound {
    prepared: builtin.constr_data(0, []),
    pre: state(machine.mode_compute, focus_root, 100, 100),
    post: state(machine.mode_return, post_root, 16100, 200),
    witness_hash: blake2b_256(cbor.serialise(witness_data)),
    arm: 1,
    group: 0,
    progress: 0,
    facts: builtin.constr_data(0, []),
  }
  let next: Data = chain.CoreBound { ..bound, progress: 1 }
  core_compute.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(bound),
      },
    ),
    ct.Continue(
      chain.VerifyArm { input_index: 0, output_index: 0, witness: witness_data },
    ),
    fixture.own_out_ref(),
    l1.Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(ct_asset, next),
      extra_signatories: [fixture.fraud_prover_key_hash],
    },
  )
}

/// The step over the decoded constant term of `payload`, returning
/// `post_root` with the witness `value_root`.
fn compute(payload: Data, post_root: ByteArray, value_root: ByteArray) -> Bool {
  spend(proof.hash_constant_term_v1(root(payload)), post_root, value_root)
}

fn accepts(payload: Data) -> Bool {
  compute(payload, root(payload), root(payload))
}

test compute_constant_of_96_nodes_is_accepted() {
  accepts(integers(95))
}

test compute_constant_of_1000_nodes_is_accepted() {
  accepts(integers(999))
}

test compute_constant_past_the_direct_payload_bound_is_accepted() {
  let payload = near_max()
  and {
    bytearray.length(cbor.serialise(payload)) > cek_constant_v1.max_direct_payload_bytes,
    accepts(payload),
  }
}

test compute_constant_with_another_constants_root_is_refused() fail {
  let other = root(integers(95))
  compute(near_max(), other, other)
}

test compute_constant_returning_another_constants_root_is_refused() fail {
  compute(near_max(), root(integers(95)), root(near_max()))
}

test compute_constant_with_one_memory_more_is_refused() fail {
  let wrong = root_with_memory(near_max(), 1)
  compute(near_max(), wrong, wrong)
}

test compute_constant_with_one_memory_less_is_refused() fail {
  let wrong = root_with_memory(near_max(), -1)
  compute(near_max(), wrong, wrong)
}

test compute_constant_returning_one_memory_more_is_refused() fail {
  compute(near_max(), root_with_memory(near_max(), 1), root(near_max()))
}

test compute_constant_over_a_context_constant_term_is_refused() fail {
  let value_root = root(near_max())
  spend(proof.hash_context_constant_term_v1(value_root), value_root, value_root)
}
