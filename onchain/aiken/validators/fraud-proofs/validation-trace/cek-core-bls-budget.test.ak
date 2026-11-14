//// Transaction-level rows for the CEK core BLS budget step. The step takes a
//// machine applying bls12_381_finalVerify to two Miller-loop results and
//// accepts only the post-state that charges the builtin's budget: 333,849,714
//// cpu and 1 memory over a pre-state with nothing spent.

use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use cardano/transaction as l1
use fraud_proofs/validation_trace/cek_core_bls_budget as bls_budget
use midgard/cek_builtin_v1 as builtin_witness
use midgard/cek_constant_v1.{ConstantWitnessV1}
use midgard/cek_machine_v1 as machine
use midgard/cek_proof_v1 as proof
use midgard/cek_semantic_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

const left_root =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const right_root =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const ct_asset = "cek-core-bls-budget"

fn result() -> builtin_witness.ValueWitnessV1 {
  builtin_witness.ConstantValue(
    ConstantWitnessV1 { type_cbor: #"9f04ff", payload_cbor: #"d87a80" },
  )
}

fn witness() -> Data {
  let left: Data = builtin_witness.BlsMillerLoopValue(left_root)
  let right: Data = builtin_witness.BlsMillerLoopValue(right_root)
  let result_data: Data = result()
  builtin.constr_data(
    37,
    [
      builtin.b_data(left_root),
      builtin.b_data(right_root),
      left,
      right,
      result_data,
    ],
  )
}

fn pre() -> machine.MachineStateV1 {
  let Pair(root, count) = builtin_witness.arguments_root_v1(
    [
      builtin_witness.BlsMillerLoopValue(left_root),
      builtin_witness.BlsMillerLoopValue(right_root),
    ],
  )
  machine.MachineStateV1 {
    mode: machine.mode_builtin,
    execution_index: 0,
    focus_root: proof.hash_builtin_value_v1(70, 0, count, root),
    environment_root: proof.empty_environment_root_v1,
    continuation_root: proof.empty_continuation_root_v1,
    auxiliary: 0,
    cpu: 0,
    memory: 0,
  }
}

fn post(cpu: Int, memory: Int) -> machine.MachineStateV1 {
  machine.MachineStateV1 {
    mode: machine.mode_return,
    execution_index: 0,
    focus_root: builtin_witness.result_root_v1(result()),
    environment_root: proof.empty_environment_root_v1,
    continuation_root: proof.empty_continuation_root_v1,
    auxiliary: 0,
    cpu,
    memory,
  }
}

/// Spends the budget step whose bound post-state spent `cpu` and `memory`.
fn spend_with_post(cpu: Int, memory: Int) -> Bool {
  let witness_data = witness()
  let bound = chain.CoreBound {
    prepared: builtin.constr_data(0, []),
    pre: pre(),
    post: post(cpu, memory),
    witness_hash: blake2b_256(cbor.serialise(witness_data)),
    arm: 37,
    group: 8,
    progress: 0,
    facts: builtin.constr_data(0, []),
  }
  let next: Data = chain.CoreBound { ..bound, progress: 1 }
  bls_budget.main.spend(
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

test bls_budget_step_accepts_the_final_verify_budget() {
  spend_with_post(333_849_714, 1)
}

test bls_budget_step_refuses_one_cpu_over_the_budget() fail {
  spend_with_post(333_849_715, 1)
}

test bls_budget_step_refuses_one_memory_over_the_budget() fail {
  spend_with_post(333_849_714, 2)
}
