use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use cardano/transaction.{Transaction}
use fraud_proofs/validation_trace/cek_core_failure_known as failure_known
use midgard/cek_builtin_v1.{ConstantValue, ValueWitnessV1}
use midgard/cek_constant_v1.{ConstantWitnessV1}
use midgard/cek_machine_v1 as machine
use midgard/cek_proof_v1
use midgard/cek_semantic_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

// The direct failure arm (38) proving a writeBits failure over the longest
// input the aggregate direct bound reveals, beside the same step built without
// its spend.

type KnownFailureStep {
  datum: Option<chain.CoreDatum>,
  redeemer: chain.ArmRedeemer,
  transaction: Transaction,
}

/// The arm 38 step proving that builtin `tag` fails over `arguments`.
fn known_failure_step(
  tag: Int,
  arguments: List<ValueWitnessV1>,
) -> KnownFailureStep {
  let arguments_data: Data = arguments
  let witness = builtin.constr_data(38, [builtin.i_data(tag), arguments_data])
  let Pair(arguments_root, count) = cek_builtin_v1.arguments_root_v1(arguments)
  let state = machine.MachineStateV1 {
    mode: machine.mode_builtin,
    execution_index: 0,
    focus_root: cek_proof_v1.hash_builtin_value_v1(
      tag,
      0,
      count,
      arguments_root,
    ),
    environment_root: #"",
    continuation_root: #"",
    auxiliary: 0,
    cpu: 0,
    memory: 0,
  }
  let bound = chain.CoreBound {
    prepared: builtin.constr_data(0, []),
    pre: state,
    post: machine.exact_state(
      state,
      machine.mode_halt_error,
      cek_proof_v1.hash_error_term_v1(),
      cek_proof_v1.empty_environment_root_v1,
      cek_proof_v1.empty_continuation_root_v1,
      machine.error_builtin_failure,
      0,
      0,
    ),
    witness_hash: blake2b_256(cbor.serialise(witness)),
    arm: 38,
    group: 9,
    progress: 0,
    facts: builtin.constr_data(0, []),
  }
  let next: Data = chain.CoreBound { ..bound, progress: 1 }
  KnownFailureStep {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(bound),
      },
    ),
    redeemer: ct.Continue(
      chain.VerifyArm { input_index: 0, output_index: 0, witness },
    ),
    transaction: Transaction {
      ..fixture.continue_mid_step_l1_tx_v1("cek-core-failure-known", next),
      extra_signatories: [fixture.fraud_prover_key_hash],
    },
  }
}

fn spend(step: KnownFailureStep) -> Bool {
  failure_known.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

fn built(step: KnownFailureStep) -> Bool {
  when step.datum is {
    Some(_) -> True
    None -> False
  }
}

/// `length` zero bytes, at most 16,384.
fn zeros(length: Int) -> ByteArray {
  if length <= 8192 {
    builtin.replicate_byte(length, 0)
  } else {
    builtin.append_bytearray(
      builtin.replicate_byte(8192, 0),
      builtin.replicate_byte(length - 8192, 0),
    )
  }
}

/// writeBits setting bits `indices` of `source`.
fn write_bits_step(source: ByteArray, indices: List<Data>) -> KnownFailureStep {
  known_failure_step(
    80,
    [
      ConstantValue(
        ConstantWitnessV1 {
          type_cbor: #"9f01ff",
          payload_cbor: cbor.serialise(builtin.b_data(source)),
        },
      ),
      ConstantValue(
        ConstantWitnessV1 {
          type_cbor: #"9f0500ff",
          payload_cbor: cbor.serialise(builtin.list_data(indices)),
        },
      ),
      ConstantValue(
        ConstantWitnessV1 { type_cbor: #"9f04ff", payload_cbor: #"d87a80" },
      ),
    ],
  )
}

/// An input of 8,927 bytes, the longest the bound reveals beside one index.
fn longest_input_step() -> KnownFailureStep {
  write_bits_step(zeros(8927), [builtin.i_data(0)])
}

test failure_known_write_bits_at_the_longest_input() {
  spend(longest_input_step())
}

test failure_known_write_bits_fixture_only_at_the_longest_input() {
  built(longest_input_step())
}
