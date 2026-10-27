use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use cardano/transaction.{Transaction}
use fraud_proofs/validation_trace/cek_core_failure_bls_uncompress as failure_bls_uncompress
use midgard/cek_builtin_v1.{ConstantValue, ValueWitnessV1}
use midgard/cek_constant_v1.{ConstantWitnessV1}
use midgard/cek_machine_v1 as machine
use midgard/cek_proof_v1
use midgard/cek_semantic_chain as chain
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

// The BLS12-381 uncompress failure arm (arm 38, core group 12) at its
// costliest shapes, each beside the same step built without its spend: the
// failures whose point check runs in full (G1 outside the subgroup, G2 off the
// twist, G2 outside the subgroup).

type UncompressFailureStep {
  datum: Option<chain.CoreDatum>,
  redeemer: chain.ArmRedeemer,
  transaction: Transaction,
}

/// The arm 38 step, routed to core group 12, proving that builtin `tag` fails
/// over `arguments`.
fn uncompress_failure_step(
  tag: Int,
  arguments: List<ValueWitnessV1>,
) -> UncompressFailureStep {
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
    group: 12,
    progress: 0,
    facts: builtin.constr_data(0, []),
  }
  let next: Data = chain.CoreBound { ..bound, progress: 1 }
  UncompressFailureStep {
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
      ..fixture.continue_mid_step_l1_tx_v1(
        "cek-core-failure-bls-uncompress",
        next,
      ),
      extra_signatories: [fixture.fraud_prover_key_hash],
    },
  }
}

fn spend(step: UncompressFailureStep) -> Bool {
  failure_bls_uncompress.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

fn built(step: UncompressFailureStep) -> Bool {
  when step.datum is {
    Some(_) -> True
    None -> False
  }
}

/// uncompress (59 G1, 66 G2) of `encoded`.
fn uncompress_step(tag: Int, encoded: ByteArray) -> UncompressFailureStep {
  uncompress_failure_step(
    tag,
    [
      ConstantValue(
        ConstantWitnessV1 {
          type_cbor: #"9f01ff",
          payload_cbor: cbor.serialise(builtin.b_data(encoded)),
        },
      ),
    ],
  )
}

/// A G1 point on the curve, outside G1.
const g1_outside_subgroup: ByteArray =
  #"8a2b9fbe12d0fd644c1e951c5983f40ca78f18e62b3b55a1d8e62cddba3d8a3736719efc37f870af7f436c2671752974"

/// A G2 x off the twist.
const g2_off_twist: ByteArray =
  #"8b55de6ec1c1b9c65236f5282637f7e67bf524d7e4187af733ef92700eeb1086f4549c5049937408f31ba2e532c0bd501587ca03a248d9ad945e56a3e3ef50f225a92648ed8a647d159479e20ff13c8ed21188535039c8fec81090af5bef34c3"

/// A G2 point on the twist, outside G2, whose y^2 has a u part.
const g2_outside_subgroup: ByteArray =
  #"937f9ebde4497e13a660d4e184d974218b21d18c9d578ab770360ad3fe41a4217e4ac78c554e05bfd2fc02336b84d90505974a366247ac98e953e9afec7f10dd12080a46e6f7dede942d6265721ee3c807009433d1bc69bf82d69569713244a7"

test failure_bls_uncompress_g1_outside_the_subgroup() {
  spend(uncompress_step(59, g1_outside_subgroup))
}

test failure_bls_uncompress_g1_fixture_only_outside_the_subgroup() {
  built(uncompress_step(59, g1_outside_subgroup))
}

test failure_bls_uncompress_g2_off_the_twist() {
  spend(uncompress_step(66, g2_off_twist))
}

test failure_bls_uncompress_g2_fixture_only_off_the_twist() {
  built(uncompress_step(66, g2_off_twist))
}

test failure_bls_uncompress_g2_outside_the_subgroup() {
  spend(uncompress_step(66, g2_outside_subgroup))
}

test failure_bls_uncompress_g2_fixture_only_outside_the_subgroup() {
  built(uncompress_step(66, g2_outside_subgroup))
}

/// The G2 generator uncompresses, so no failure proof spends.
test failure_bls_uncompress_g2_of_the_generator_is_refused() fail {
  spend(
    uncompress_step(
      66,
      #"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8",
    ),
  )
}

/// `core_group` routes an arm 38 step to this arm (group 12) for uncompress
/// alone, and every other direct failure to the known failure arm (group 9).
test core_group_routes_uncompress_alone_to_group_12() {
  let group = fn(tag) {
    chain.core_group(
      machine.mode_builtin,
      builtin.constr_data(38, [builtin.i_data(tag), builtin.list_data([])]),
    )
  }
  and {
    group(59) == 12,
    group(66) == 12,
    group(4) == 9,
    group(56) == 9,
    group(60) == 9,
    group(80) == 9,
  }
}
