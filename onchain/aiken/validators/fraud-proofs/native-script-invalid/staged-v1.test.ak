use aiken/builtin
use aiken/crypto.{blake2b_256}
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use fraud_proofs/native_script_invalid/step_03 as step_03_validator
use fraud_proofs/native_script_invalid/step_04 as step_04_validator
use fraud_proofs/native_script_invalid/step_05 as step_05_validator
use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{
  WitnessFieldOpening, address_witnesses_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_invalid/step_03
use midgard/fraud_proofs/native_script_invalid/step_04
use midgard/fraud_proofs/native_script_invalid/step_05
use midgard/fraud_proofs/native_tx/components.{encode_midgard_versioned_script}
use midgard/fraud_proofs/native_tx/types.{
  MidgardVersionedScript, NativeCardanoScript, NativeTxWitnessSetCompact,
}
use midgard/native_tx_field_access_v1.{
  Certified, FieldPreimageCertificateV1, field_preimage_certificate_asset_name,
}
use midgard/native_tx_script_pushdown_v1 as pushdown
use midgard/q34_max_fixture_chunk_0 as chunk_0
use midgard/q34_max_fixture_chunk_1 as chunk_1
use midgard/q34_max_fixture_chunk_2 as chunk_2
use midgard/validation_machine_v1.{SignerAboveLastProof}
use midgard/validation_merkle_v1.{FrontierPeak}

const tx_id =
  #"d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23"

const compact_cbor =
  #"84018c5820f99641fc7bc7e291a96a6de01185240bbe7bd7b2ce2a5f6d816dc8e5c68a0c525820ccfbff4fea0f54213e078ce74e65bf844a143f9b74eece6fe005bb04742d239658206268504e96c250ed6ea83a2bc508404dd8dd3b900763c9d8f595dfc47d98d7cc1a000f42402020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820a6f688ee8982ebdbffbfa608b43c087d3dc1c996df57e4626618da1a8e0494e3582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820101010101010101010101010101010101010101010101010101010101010101058201010101010101010101010101010101010101010101010101010101010101010005820e352221d0b5ae02610c47f76a0f7ffb63f72b72275d931d18e03ab4696754a6800"

const addr_hash =
  #"be3c48be9923f633253c5f46da8e1410bb26eaa79b617322dc29528cc44b77ef"

const script_hash =
  #"ad4b36af564e3d2cab118663351f7d658dad69932a9e0d5e8196783f16234a8e"

const redeemer_hash =
  #"45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"

const witness_set_hash =
  #"e352221d0b5ae02610c47f76a0f7ffb63f72b72275d931d18e03ab4696754a68"

const field_hash =
  #"be3c48be9923f633253c5f46da8e1410bb26eaa79b617322dc29528cc44b77ef"

const checkpoint_32 =
  #"865820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23410743007ff54300013e4300002043000ce3"

const checkpoint_hash_32 =
  #"756e2088a16c3b1fa9ac93a05559a192a7beaf48db71533524d67a2742bd524a"

const checkpoint_64 =
  #"865820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23410743007ff54300013e43000040430019c3"

const checkpoint_hash_64 =
  #"f8201734840cc29c2bb1e092512eda2dc0dcc36be0bc0bd71caf8dc1dbcb8c09"

const checkpoint_288 =
  #"865820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23410743007ff54300013e43000120430073e3"

const checkpoint_hash_288 =
  #"42435ba5c9e1b73bc0462e7f117520a4f7507aa640f394d0cffb8f24e0e5cc5d"

const checkpoint_304 =
  #"865820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23410743007ff54300013e4300013043007a53"

const checkpoint_hash_304 =
  #"403405148e7b6bc4cb7defdfed99127b7165176b7cda0faca64ceea31dbc3b85"

const previous_32 = #"0f8101ff30ef95e19a91648426e29474d0fb04e39f9491d83cbd4411"

const checkpoint_16 =
  #"865820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23410743007ff54300013e4300001043000673"

const checkpoint_hash_16 =
  #"ad313e491443c5b3d4bb77b464c8a303f2ac71c31f4fa003711d619760386937"

const previous_16 = #"07ebf523dfab719a922bf4ff3cb790ebac51b0078132fa789fda59a9"

const previous_64 = #"27c648cbcfdbd9c23cc6e76e72c08ee8c421f579acc14d7a4e3edc48"

const previous_288 = #"ea14141719dc1075c008e3aabbb96e18b5c32d07c6ab77eabde3fea3"

const previous_304 = #"f2528f122d04b2588644a34f9d32a5aec5ed50b3335bd7030b74259a"

const previous_318 = #"ffa3561f45a11cafde2b91393f1a70fa537320fa3bc618804391f351"

fn native_script_item() -> ByteArray {
  encode_midgard_versioned_script(
    MidgardVersionedScript {
      language: NativeCardanoScript,
      script_bytes: #"8200581cffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    },
  )
}

fn input_state() -> step_03.State {
  step_03.State {
    bad_tx_id: tx_id,
    bad_tx_witness_set_hash: witness_set_hash,
    script_item_hash: blake2b_256(native_script_item()),
    validity_interval_start: 0,
    validity_interval_end: 100,
  }
}

fn peaks_32() -> List<FrontierPeak> {
  [
    FrontierPeak {
      height: 5,
      hash: #"75f1bcc6cd0bc4f065a6f0e86f50c6906dd87835c24303964d4e56eb520ebc98",
    },
  ]
}

fn peaks_16() -> List<FrontierPeak> {
  [
    FrontierPeak {
      height: 4,
      hash: #"b07c8e68e66274f6209dd8176cf2d82e6ac08eb89cac88e38f3ed414d2aa169c",
    },
  ]
}

fn peaks_64() -> List<FrontierPeak> {
  [
    FrontierPeak {
      height: 6,
      hash: #"6b63f2529d4547c9fd332b04880026d9164db5bbea16114e4a3083801c5a8b18",
    },
  ]
}

fn peaks_288() -> List<FrontierPeak> {
  [
    FrontierPeak {
      height: 5,
      hash: #"a200e88dc07a54d5ae4d1fccfe9050273fa2414859354f9e38916fdc015938e6",
    },
    FrontierPeak {
      height: 8,
      hash: #"738d1ebefdc3fc46f8ee2fc6cefa29aef784667366f75d70adfaf16612036df4",
    },
  ]
}

fn peaks_304() -> List<FrontierPeak> {
  [
    FrontierPeak {
      height: 4,
      hash: #"e752280c3a26eecf94be5da5a4955c274896b9f39bc12313f9a6499a207914c8",
    },
    FrontierPeak {
      height: 5,
      hash: #"a200e88dc07a54d5ae4d1fccfe9050273fa2414859354f9e38916fdc015938e6",
    },
    FrontierPeak {
      height: 8,
      hash: #"738d1ebefdc3fc46f8ee2fc6cefa29aef784667366f75d70adfaf16612036df4",
    },
  ]
}

fn peaks_318() -> List<FrontierPeak> {
  [
    FrontierPeak {
      height: 1,
      hash: #"b5b3961284cce7d7c275bf95f6f3431884938813f9d0a83cced879e384b0e8e5",
    },
    FrontierPeak {
      height: 2,
      hash: #"62eea386cfae9dd430a0213803f589211bff58b13982fdabf2bd3a3677187eb8",
    },
    FrontierPeak {
      height: 3,
      hash: #"3747ab530d3dd3b00b19a55cb3e9143401be1e96a41bb216e02af737f9618840",
    },
    FrontierPeak {
      height: 4,
      hash: #"e752280c3a26eecf94be5da5a4955c274896b9f39bc12313f9a6499a207914c8",
    },
    FrontierPeak {
      height: 5,
      hash: #"a200e88dc07a54d5ae4d1fccfe9050273fa2414859354f9e38916fdc015938e6",
    },
    FrontierPeak {
      height: 8,
      hash: #"738d1ebefdc3fc46f8ee2fc6cefa29aef784667366f75d70adfaf16612036df4",
    },
  ]
}

fn witness_set() -> NativeTxWitnessSetCompact {
  NativeTxWitnessSetCompact {
    addr_tx_wits_hash: addr_hash,
    script_tx_wits_hash: script_hash,
    redeemer_tx_wits_hash: redeemer_hash,
  }
}

fn opening() {
  WitnessFieldOpening {
    native_tx_compact_cbor: compact_cbor,
    witness_set: witness_set(),
    carriage: Certified {
      cert_ref_input_index: 0,
      chunk_ref_input_indices: [1, 2, 3],
    },
  }
}

fn certificate() -> FieldPreimageCertificateV1 {
  FieldPreimageCertificateV1 {
    owner: fixture.fraud_prover_key_hash,
    tx_id,
    field_index: address_witnesses_field_index,
    field_hash,
    total_length: 32757,
    chunk_digests: [
      #"26366e1cee678704dc218da2cf33cb754326bf8fe51b1de108b8ccea92e06032",
      #"96084650107d9a17a7c4d9c37ae53af1023192c2236c0e2c7267c5646ff04744",
      #"2a32995fefdf3483d00e1b3c654ede2dab8a1510b5d4e7ab6c585ea840052b3e",
    ],
  }
}

fn chunk_input(index: Int, bytes: ByteArray) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: tx_id,
      output_index: index,
    },
    output: Output {
      address: address.from_verification_key(fixture.fraud_prover_key_hash),
      value: assets.from_lovelace(2_000_000),
      datum: InlineDatum(builtin.b_data(bytes)),
      reference_script: None,
    },
  }
}

fn reference_inputs() -> List<Input> {
  [
    Input {
      output_reference: OutputReference {
        transaction_id: tx_id,
        output_index: 0,
      },
      output: Output {
        address: address.from_script(fixture.test_certificate_policy_id),
        value: assets.from_lovelace(2_000_000)
          |> assets.add(
              fixture.test_certificate_policy_id,
              field_preimage_certificate_asset_name,
              1,
            ),
        datum: InlineDatum(certificate()),
        reference_script: None,
      },
    },
    chunk_input(1, chunk_0.value),
    chunk_input(2, chunk_1.value),
    chunk_input(3, chunk_2.value),
  ]
}

fn continue_tx(output_state: Data, output_script_hash: ByteArray) -> Transaction {
  let tx = fixture.continue_mid_step_l1_tx_v1(tx_id, output_state)
  expect [output] = tx.outputs
  Transaction {
    ..tx,
    outputs: [
      Output { ..output, address: address.from_script(output_script_hash) },
    ],
    reference_inputs: reference_inputs(),
  }
}

fn state_32() -> step_04.State {
  let state = input_state()
  step_04.State {
    bad_tx_id: state.bad_tx_id,
    bad_tx_witness_set_hash: state.bad_tx_witness_set_hash,
    script_item_hash: state.script_item_hash,
    validity_interval_start: state.validity_interval_start,
    validity_interval_end: state.validity_interval_end,
    signer_checkpoint_hash: checkpoint_hash_32,
    previous_signer_hash: previous_32,
    signer_count: 32,
    signer_peaks: peaks_32(),
  }
}

fn state_16() -> step_04.State {
  let state = input_state()
  step_04.State {
    bad_tx_id: state.bad_tx_id,
    bad_tx_witness_set_hash: state.bad_tx_witness_set_hash,
    script_item_hash: state.script_item_hash,
    validity_interval_start: state.validity_interval_start,
    validity_interval_end: state.validity_interval_end,
    signer_checkpoint_hash: checkpoint_hash_16,
    previous_signer_hash: previous_16,
    signer_count: 16,
    signer_peaks: peaks_16(),
  }
}

fn state_64() -> step_04.State {
  step_04.State {
    ..state_32(),
    signer_checkpoint_hash: checkpoint_hash_64,
    previous_signer_hash: previous_64,
    signer_count: 64,
    signer_peaks: peaks_64(),
  }
}

fn state_288() -> step_04.State {
  step_04.State {
    ..state_32(),
    signer_checkpoint_hash: checkpoint_hash_288,
    previous_signer_hash: previous_288,
    signer_count: 288,
    signer_peaks: peaks_288(),
  }
}

fn state_304() -> step_04.State {
  step_04.State {
    ..state_32(),
    signer_checkpoint_hash: checkpoint_hash_304,
    previous_signer_hash: previous_304,
    signer_count: 304,
    signer_peaks: peaks_304(),
  }
}

fn state_318() -> step_05.State {
  let state = input_state()
  step_05.State {
    bad_tx_id: state.bad_tx_id,
    script_item_hash: state.script_item_hash,
    validity_interval_start: state.validity_interval_start,
    validity_interval_end: state.validity_interval_end,
    signer_count: 318,
    signer_peaks: peaks_318(),
    phase: step_05.ScriptReady,
  }
}

fn absent_signer_query_318() -> step_05.SignerQueryV1 {
  step_05.SignerQueryV1 {
    signer_hash: #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    proof: SignerAboveLastProof {
      last_signer_hash: previous_318,
      siblings: [
        #"70a34b0a50899e8f064f44a2f85d7d1e15fdbd51434435d7df2c84d604d632c6",
      ],
      peaks: peaks_318(),
    },
  }
}

fn max_unsatisfied_script_payload() -> ByteArray {
  #"8201981f82041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e882041903e8"
}

fn max_satisfied_script_payload() -> ByteArray {
  #"8201981f82051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e882051903e8"
}

fn script_item_from_payload(payload: ByteArray) -> ByteArray {
  encode_midgard_versioned_script(
    MidgardVersionedScript {
      language: NativeCardanoScript,
      script_bytes: payload,
    },
  )
}

fn max_script_state(payload: ByteArray, phase: step_05.PhaseV1) -> step_05.State {
  step_05.State {
    ..state_318(),
    script_item_hash: blake2b_256(script_item_from_payload(payload)),
    phase,
  }
}

fn max_script_first_walk(payload: ByteArray) {
  pushdown.native_script_run_with_signer_verdict(
    pushdown.open_native_script_walk(payload),
    payload,
    0,
    100,
    16,
    fn(_signer_hash) { False },
  )
}

fn max_script_next_walk(payload: ByteArray, walk) {
  pushdown.native_script_run_with_signer_verdict(
    walk,
    payload,
    0,
    100,
    16,
    fn(_signer_hash) { False },
  )
}

fn start_max_script_scan(payload: ByteArray) -> Bool {
  let ready = max_script_state(payload, step_05.ScriptReady)
  let first = max_script_first_walk(payload)
  let output_state: Data = max_script_state(
    payload,
    step_05.ScriptWalk {
      cursor_hash: pushdown.native_script_cursor_hash(first),
    },
  )
  step_05_validator.main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(ready),
      },
    ),
    ct.Continue(
      step_05.StartScriptScan {
        input_index: 0,
        output_index: 0,
        script_item_cbor: script_item_from_payload(payload),
        node_budget: 16,
        signer_queries: [],
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_same_step_l1_tx_v1(tx_id, output_state),
  )
}

fn finalize_max_script_scan(payload: ByteArray) -> Bool {
  let first = max_script_first_walk(payload)
  let second = max_script_next_walk(payload, first)
  let third = max_script_next_walk(payload, second)
  let input_state = max_script_state(
    payload,
    step_05.ScriptWalk {
      cursor_hash: pushdown.native_script_cursor_hash(third),
    },
  )
  step_05_validator.main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state),
      },
    ),
    ct.Continue(
      step_05.FinalizeScriptScan {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        script_item_cbor: script_item_from_payload(payload),
        cursor_bytes: pushdown.encode_native_script_cursor(third),
        frames: pushdown.native_script_walk_frames(third),
        node_budget: 15,
        signer_queries: [],
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(tx_id),
  )
}

test q34_staged_step_03_starts_exact_318_witness_frontier_in_16_item_batch() {
  step_03_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state()),
      },
    ),
    ct.Continue(
      step_03.StartSignerScan {
        input_index: 0,
        output_index: 0,
        script_item_cbor: native_script_item(),
        addr_tx_wits_opening: opening(),
        item_budget: 16,
      },
    ),
    fixture.own_out_ref(),
    continue_tx(state_16(), fixture.next_step_script_hash),
  )
}

test q34_staged_step_03_rejects_wrong_successor() fail {
  step_03_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state()),
      },
    ),
    ct.Continue(
      step_03.StartSignerScan {
        input_index: 0,
        output_index: 0,
        script_item_cbor: native_script_item(),
        addr_tx_wits_opening: opening(),
        item_budget: 16,
      },
    ),
    fixture.own_out_ref(),
    continue_tx(state_16(), fixture.step_script_hash),
  )
}

test q34_staged_step_03_rejects_signer_batch_above_16() fail {
  step_03_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state()),
      },
    ),
    ct.Continue(
      step_03.StartSignerScan {
        input_index: 0,
        output_index: 0,
        script_item_cbor: native_script_item(),
        addr_tx_wits_opening: opening(),
        item_budget: 17,
      },
    ),
    fixture.own_out_ref(),
    continue_tx(state_16(), fixture.next_step_script_hash),
  )
}

test q34_staged_step_04_exact_318_witness_resumes_in_16_item_batch() {
  step_04_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state_16()),
      },
    ),
    ct.Continue(
      step_04.ResumeSignerScan {
        input_index: 0,
        output_index: 0,
        addr_tx_wits_opening: opening(),
        checkpoint_bytes: checkpoint_16,
        item_budget: 16,
      },
    ),
    fixture.own_out_ref(),
    continue_tx(state_32(), fixture.step_script_hash),
  )
}

test q34_staged_step_04_rejects_mutated_checkpoint() fail {
  step_04_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state_16()),
      },
    ),
    ct.Continue(
      step_04.ResumeSignerScan {
        input_index: 0,
        output_index: 0,
        addr_tx_wits_opening: opening(),
        checkpoint_bytes: checkpoint_32,
        item_budget: 16,
      },
    ),
    fixture.own_out_ref(),
    continue_tx(state_32(), fixture.step_script_hash),
  )
}

test q34_staged_step_04_rejects_signer_batch_above_16() fail {
  step_04_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state_16()),
      },
    ),
    ct.Continue(
      step_04.ResumeSignerScan {
        input_index: 0,
        output_index: 0,
        addr_tx_wits_opening: opening(),
        checkpoint_bytes: checkpoint_16,
        item_budget: 17,
      },
    ),
    fixture.own_out_ref(),
    continue_tx(state_32(), fixture.step_script_hash),
  )
}

test q34_staged_step_04_exact_318_witness_penultimate_fit() {
  step_04_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state_288()),
      },
    ),
    ct.Continue(
      step_04.ResumeSignerScan {
        input_index: 0,
        output_index: 0,
        addr_tx_wits_opening: opening(),
        checkpoint_bytes: checkpoint_288,
        item_budget: 16,
      },
    ),
    fixture.own_out_ref(),
    continue_tx(state_304(), fixture.step_script_hash),
  )
}

test q34_staged_step_04_exact_318_witness_terminal_fit() {
  step_04_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state_304()),
      },
    ),
    ct.Continue(
      step_04.FinalizeSignerScan {
        input_index: 0,
        output_index: 0,
        addr_tx_wits_opening: opening(),
        checkpoint_bytes: checkpoint_304,
        item_budget: 14,
      },
    ),
    fixture.own_out_ref(),
    continue_tx(state_318(), fixture.next_step_script_hash),
  )
}

test q34_staged_step_05_exact_318_signer_frontier_fit() {
  step_05_validator.main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state_318()),
      },
    ),
    ct.Continue(
      step_05.StartScriptFinalize {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        script_item_cbor: native_script_item(),
        signer_queries: [absent_signer_query_318()],
        node_budget: 1,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(tx_id),
  )
}

test q34_staged_step_05_max_32_node_native_script_starts_in_16_node_batch() {
  start_max_script_scan(max_unsatisfied_script_payload())
}

test q34_staged_step_05_rejects_node_batch_above_16() fail {
  let payload = max_unsatisfied_script_payload()
  let ready = max_script_state(payload, step_05.ScriptReady)
  step_05_validator.main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(ready),
      },
    ),
    ct.Continue(
      step_05.StartScriptScan {
        input_index: 0,
        output_index: 0,
        script_item_cbor: script_item_from_payload(payload),
        node_budget: 17,
        signer_queries: [],
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_same_step_l1_tx_v1(tx_id, ready),
  )
}

test q34_staged_step_05_max_32_node_native_script_finalizes_after_16_node_batches() {
  finalize_max_script_scan(max_unsatisfied_script_payload())
}

test q34_staged_step_05_rejects_max_32_node_satisfied_script_after_16_node_batches() fail {
  finalize_max_script_scan(max_satisfied_script_payload())
}

test q34_staged_step_03_preserves_prover_cancellation() {
  step_03_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state()),
      },
    ),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(tx_id, tx_id, True),
  )
}

test q34_staged_step_04_preserves_prover_cancellation() {
  step_04_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state_32()),
      },
    ),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(tx_id, tx_id, True),
  )
}

test q34_staged_step_05_preserves_prover_cancellation() {
  step_05_validator.main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state_318()),
      },
    ),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(tx_id, tx_id, True),
  )
}
