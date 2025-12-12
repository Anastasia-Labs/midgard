use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto
use aiken/interval
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list/unordered
use aiken_design_patterns/merkelized_validator.{WithdrawRedeemer}
use cardano/address
use cardano/assets as assets
use cardano/transaction
use midgard/computation_thread.{StepDatum, Success}
use midgard/fraud_proof/common/types.{FraudProofDatum, MembershipProofInputs}
use midgard/fraud_proof/common/utils as fp_utils
use midgard/fraud_proof/double_spend/step_01 as step1_types
use midgard/fraud_proof/double_spend/step_02 as step2_types
use midgard/fraud_proof/double_spend/step_03 as step3_types
use midgard/hub_oracle as hub
use midgard/ledger_state.{
  Header, MidgardNetworkId, MidgardTxBodyCompact, MidgardTxCompact,
}
use midgard/common/utils.{verify_hash_32}

const prover_hash =
  crypto.blake2b_224(#"01")

const ct_policy_id =
  crypto.blake2b_224(#"c0ffee")

const fraud_policy_id =
  crypto.blake2b_224(#"f00d")

const hub_hash =
  crypto.blake2b_224(#"f00baa")

const merkle_validator =
  crypto.blake2b_224(#"b33f")

const state_queue_policy =
  crypto.blake2b_224(#"05")

const step2_hash =
  crypto.blake2b_224(#"0202")

const step3_hash =
  crypto.blake2b_224(#"0303")

const step4_hash =
  crypto.blake2b_224(#"0404")

const block_hash: ByteArray =
  #"11111111111111111111111111111111111111111111111111111111"

const other_block_hash: ByteArray =
  #"22222222222222222222222222222222222222222222222222222222"

const ct_asset_name =
  bytearray.concat(#"00000001", block_hash)

const tx_root =
  crypto.blake2b_256(#"0a")

const fake_path =
  #"abcd"

fn ct_value() -> assets.Value {
  assets.from_lovelace(1_000_000)
    |> assets.add(ct_policy_id, ct_asset_name, 1)
}

fn ct_value_with(ct_token_name: ByteArray) -> assets.Value {
  assets.from_lovelace(1_000_000)
    |> assets.add(ct_policy_id, ct_token_name, 1)
}

fn mk_ct_input(step_datum: StepDatum) -> (transaction.Input, transaction.OutputReference) {
  let out_ref =
    transaction.OutputReference {
      transaction_id: crypto.blake2b_256(#"0b"),
      output_index: 0,
    }

  let out =
    transaction.Output {
      address: address.from_script(ct_policy_id),
      value: ct_value(),
      datum: transaction.InlineDatum(step_datum),
      reference_script: None,
    }

  (transaction.Input { output_reference: out_ref, output: out }, out_ref)
}

fn mk_ct_input_with(
  step_datum: StepDatum,
  ct_token_name: ByteArray,
) -> (transaction.Input, transaction.OutputReference) {
  let out_ref =
    transaction.OutputReference {
      transaction_id: crypto.blake2b_256(#"0b"),
      output_index: 0,
    }

  let out =
    transaction.Output {
      address: address.from_script(ct_policy_id),
      value: ct_value_with(ct_token_name),
      datum: transaction.InlineDatum(step_datum),
      reference_script: None,
    }

  (transaction.Input { output_reference: out_ref, output: out }, out_ref)
}

fn mk_header() -> Header {
  Header {
    prev_utxos_root: crypto.blake2b_256(#"0c"),
    utxos_root: crypto.blake2b_256(#"0d"),
    transactions_root: tx_root,
    deposits_root: crypto.blake2b_256(#"0e"),
    withdrawals_root: crypto.blake2b_256(#"0f"),
    tx_orders_root: crypto.blake2b_256(#"10"),
    start_time: 0,
    end_time: 0,
    prev_header_hash: crypto.blake2b_224(#"11"),
    operator_vkey: crypto.blake2b_224(#"12"),
    protocol_version: 1,
  }
}

fn mk_state_queue_input() -> transaction.Input {
  let datum =
    unordered.NodeDatum {
      key: unordered.Key { key: block_hash },
      link: unordered.Empty,
      data: mk_header(),
    }

  let value =
    assets.from_lovelace(2_000_000)
      |> assets.add(state_queue_policy, unordered.node_prefix, 1)

  let output =
    transaction.Output {
      address: address.from_script(state_queue_policy),
      value,
      datum: transaction.InlineDatum(datum),
      reference_script: None,
    }

  transaction.Input {
    output_reference: transaction.OutputReference {
      transaction_id: crypto.blake2b_256(#"13"),
      output_index: 0,
    },
    output,
  }
}

fn dummy_addr(policy: crypto.ScriptHash) -> address.Address {
  address.from_script(policy)
}

fn mk_hub_input() -> transaction.Input {
  let hub_value =
    assets.from_lovelace(3_000_000)
      |> assets.add(hub_hash, hub.asset_name, 1)

  let dummy_policy =
    crypto.blake2b_224(#"aa")

  let datum =
    hub.Datum {
      registered_operators: dummy_policy,
      active_operators: dummy_policy,
      retired_operators: dummy_policy,
      scheduler: dummy_policy,
      state_queue: state_queue_policy,
      fraud_proof_catalogue: dummy_policy,
      fraud_proof: dummy_policy,
      deposit: dummy_policy,
      withdrawal: dummy_policy,
      tx_order: dummy_policy,
      settlement_queue: dummy_policy,
      payout: dummy_policy,
      registered_operators_addr: dummy_addr(dummy_policy),
      active_operators_addr: dummy_addr(dummy_policy),
      retired_operators_addr: dummy_addr(dummy_policy),
      scheduler_addr: dummy_addr(dummy_policy),
      state_queue_addr: dummy_addr(dummy_policy),
      fraud_proof_catalogue_addr: dummy_addr(dummy_policy),
      fraud_proof_addr: dummy_addr(dummy_policy),
      deposit_addr: dummy_addr(dummy_policy),
      withdrawal_addr: dummy_addr(dummy_policy),
      tx_order_addr: dummy_addr(dummy_policy),
      settlement_queue_addr: dummy_addr(dummy_policy),
      reserve_addr: dummy_addr(dummy_policy),
      payout_addr: dummy_addr(dummy_policy),
      reserve_observer: dummy_policy,
    }

  let output =
    transaction.Output {
      address: dummy_addr(hub_hash),
      value: hub_value,
      datum: transaction.InlineDatum(datum),
      reference_script: None,
    }

  transaction.Input {
    output_reference: transaction.OutputReference {
      transaction_id: crypto.blake2b_256(#"14"),
      output_index: 0,
    },
    output,
  }
}

fn membership_redeemers(
  proof: MembershipProofInputs,
) -> List<Pair<transaction.ScriptPurpose, transaction.Redeemer>> {
  let redeemer: transaction.Redeemer = WithdrawRedeemer(proof)
  [Pair(transaction.Withdraw(address.Script(merkle_validator)), redeemer)]
}

fn base_tx(
  inputs: List<transaction.Input>,
  reference_inputs: List<transaction.Input>,
  outputs: List<transaction.Output>,
  redeemers: List<Pair<transaction.ScriptPurpose, transaction.Redeemer>>,
  mint: assets.Value,
) -> transaction.Transaction {
  transaction.Transaction {
    inputs,
    reference_inputs,
    outputs,
    fee: 0,
    mint,
    certificates: [],
    withdrawals: [],
    validity_range: interval.everything,
    extra_signatories: [],
    redeemers,
    datums: transaction.placeholder.datums,
    id: crypto.blake2b_256(#"15"),
    votes: [],
    proposal_procedures: [],
    current_treasury_amount: None,
    treasury_donation: None,
  }
}

test step1_accepts_membership_proof() {
  let state_queue_input = mk_state_queue_input()
  let hub_input = mk_hub_input()
  let step_datum = StepDatum { fraud_prover: prover_hash, data: 0 }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let tx_hash = crypto.blake2b_256(#"16")
  let proof =
    MembershipProofInputs {
      tx_root,
      tx_hash,
      tx_info_hash: crypto.blake2b_256(#"17"),
      tx_to_root_proof_path: fake_path,
    }
  let redeemer_inputs =
    membership_redeemers(proof)
  let next_state =
    StepDatum {
      fraud_prover: prover_hash,
      data: step1_types.StepState { verified_tx1_hash: tx_hash },
    }
  let output_to_next =
    transaction.Output {
      address: address.from_script(step2_hash),
      value: ct_value(),
      datum: transaction.InlineDatum(next_state),
      reference_script: None,
    }
  let tx =
    base_tx(
      [ct_input],
      [state_queue_input, hub_input],
      [output_to_next],
      redeemer_inputs,
      assets.zero,
    )
  let ct_token_asset_name = fp_utils.get_singleton_ct_token(ct_input, ct_policy_id)
  let extracted_tx_root =
    fp_utils.get_tx_root_validate_block_hash(
      tx,
      0,
      ct_token_asset_name,
      state_queue_policy,
    )
  and {
    extracted_tx_root == tx_root,
    fp_utils.validate_membership_proof(tx, merkle_validator, proof),
    fp_utils.validate_step_output(
      tx,
      0,
      step2_hash,
      ct_policy_id,
      ct_token_asset_name,
      step_datum,
      next_state,
    ),
  }
}

test step1_membership_proof_requires_withdraw_redeemer() fail {
  let state_queue_input = mk_state_queue_input()
  let hub_input = mk_hub_input()
  let step_datum = StepDatum { fraud_prover: prover_hash, data: 0 }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let tx_hash = crypto.blake2b_256(#"23")
  let proof =
    MembershipProofInputs {
      tx_root,
      tx_hash,
      tx_info_hash: crypto.blake2b_256(#"24"),
      tx_to_root_proof_path: fake_path,
    }
  let next_state =
    StepDatum {
      fraud_prover: prover_hash,
      data: step1_types.StepState { verified_tx1_hash: tx_hash },
    }
  let output_to_next =
    transaction.Output {
      address: address.from_script(step2_hash),
      value: ct_value(),
      datum: transaction.InlineDatum(next_state),
      reference_script: None,
    }
  let tx =
    base_tx(
      [ct_input],
      [state_queue_input, hub_input],
      [output_to_next],
      [],
      assets.zero,
    )
  fp_utils.validate_membership_proof(tx, merkle_validator, proof)
}

test step1_fails_when_state_queue_reference_input_missing() fail {
  let step_datum = StepDatum { fraud_prover: prover_hash, data: 0 }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let tx =
    base_tx(
      [ct_input],
      [],
      [],
      [],
      assets.zero,
    )
  let ct_token_asset_name = fp_utils.get_singleton_ct_token(ct_input, ct_policy_id)
  expect _extracted_tx_root: ByteArray =
    fp_utils.get_tx_root_validate_block_hash(
      tx,
      0,
      ct_token_asset_name,
      state_queue_policy,
    )
}

test step1_fails_when_ct_token_block_hash_mismatches_state_queue() fail {
  let state_queue_input = mk_state_queue_input()
  let step_datum = StepDatum { fraud_prover: prover_hash, data: 0 }
  let bad_ct_asset_name = bytearray.concat(#"00000001", other_block_hash)
  let (ct_input, _out_ref) = mk_ct_input_with(step_datum, bad_ct_asset_name)
  let tx =
    base_tx(
      [ct_input],
      [state_queue_input],
      [],
      [],
      assets.zero,
    )
  let ct_token_asset_name = fp_utils.get_singleton_ct_token(ct_input, ct_policy_id)
  expect _extracted_tx_root: ByteArray =
    fp_utils.get_tx_root_validate_block_hash(
      tx,
      0,
      ct_token_asset_name,
      state_queue_policy,
    )
}

test step1_fails_when_output_goes_to_wrong_next_validator() fail {
  let state_queue_input = mk_state_queue_input()
  let hub_input = mk_hub_input()
  let step_datum = StepDatum { fraud_prover: prover_hash, data: 0 }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let tx_hash = crypto.blake2b_256(#"25")
  let proof =
    MembershipProofInputs {
      tx_root,
      tx_hash,
      tx_info_hash: crypto.blake2b_256(#"26"),
      tx_to_root_proof_path: fake_path,
    }
  let redeemer_inputs = membership_redeemers(proof)
  let next_state =
    StepDatum {
      fraud_prover: prover_hash,
      data: step1_types.StepState { verified_tx1_hash: tx_hash },
    }
  let output_to_wrong_step =
    transaction.Output {
      address: address.from_script(step3_hash),
      value: ct_value(),
      datum: transaction.InlineDatum(next_state),
      reference_script: None,
    }
  let tx =
    base_tx(
      [ct_input],
      [state_queue_input, hub_input],
      [output_to_wrong_step],
      redeemer_inputs,
      assets.zero,
    )
  let ct_token_asset_name = fp_utils.get_singleton_ct_token(ct_input, ct_policy_id)
  fp_utils.validate_step_output(
    tx,
    0,
    step2_hash,
    ct_policy_id,
    ct_token_asset_name,
    step_datum,
    next_state,
  )
}

test step1_fails_when_output_datum_state_is_wrong() fail {
  let state_queue_input = mk_state_queue_input()
  let hub_input = mk_hub_input()
  let step_datum = StepDatum { fraud_prover: prover_hash, data: 0 }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let tx_hash = crypto.blake2b_256(#"27")
  let proof =
    MembershipProofInputs {
      tx_root,
      tx_hash,
      tx_info_hash: crypto.blake2b_256(#"28"),
      tx_to_root_proof_path: fake_path,
    }
  let redeemer_inputs = membership_redeemers(proof)
  let expected_state =
    StepDatum {
      fraud_prover: prover_hash,
      data: step1_types.StepState { verified_tx1_hash: tx_hash },
    }
  let wrong_state =
    StepDatum {
      fraud_prover: prover_hash,
      data: step1_types.StepState { verified_tx1_hash: crypto.blake2b_256(#"29") },
    }
  let output_to_next =
    transaction.Output {
      address: address.from_script(step2_hash),
      value: ct_value(),
      datum: transaction.InlineDatum(wrong_state),
      reference_script: None,
    }
  let tx =
    base_tx(
      [ct_input],
      [state_queue_input, hub_input],
      [output_to_next],
      redeemer_inputs,
      assets.zero,
    )
  let ct_token_asset_name = fp_utils.get_singleton_ct_token(ct_input, ct_policy_id)
  fp_utils.validate_step_output(
    tx,
    0,
    step2_hash,
    ct_policy_id,
    ct_token_asset_name,
    step_datum,
    expected_state,
  )
}

test step1_fails_when_output_does_not_forward_ct_token() fail {
  let state_queue_input = mk_state_queue_input()
  let hub_input = mk_hub_input()
  let step_datum = StepDatum { fraud_prover: prover_hash, data: 0 }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let tx_hash = crypto.blake2b_256(#"2a")
  let proof =
    MembershipProofInputs {
      tx_root,
      tx_hash,
      tx_info_hash: crypto.blake2b_256(#"2b"),
      tx_to_root_proof_path: fake_path,
    }
  let redeemer_inputs = membership_redeemers(proof)
  let next_state =
    StepDatum {
      fraud_prover: prover_hash,
      data: step1_types.StepState { verified_tx1_hash: tx_hash },
    }
  let output_missing_ct =
    transaction.Output {
      address: address.from_script(step2_hash),
      value: assets.from_lovelace(1_000_000),
      datum: transaction.InlineDatum(next_state),
      reference_script: None,
    }
  let tx =
    base_tx(
      [ct_input],
      [state_queue_input, hub_input],
      [output_missing_ct],
      redeemer_inputs,
      assets.zero,
    )
  let ct_token_asset_name = fp_utils.get_singleton_ct_token(ct_input, ct_policy_id)
  fp_utils.validate_step_output(
    tx,
    0,
    step2_hash,
    ct_policy_id,
    ct_token_asset_name,
    step_datum,
    next_state,
  )
}

test step2_rejects_duplicate_tx_hash() fail {
  let state_queue_input = mk_state_queue_input()
  let hub_input = mk_hub_input()
  let tx_hash = crypto.blake2b_256(#"18")
  let step_datum =
    StepDatum {
      fraud_prover: prover_hash,
      data: step1_types.StepState { verified_tx1_hash: tx_hash },
    }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let proof =
    MembershipProofInputs {
      tx_root,
      tx_hash,
      tx_info_hash: crypto.blake2b_256(#"19"),
      tx_to_root_proof_path: fake_path,
    }
  let redeemer_inputs =
    membership_redeemers(proof)
  let next_state =
    StepDatum {
      fraud_prover: prover_hash,
      data: step2_types.StepState {
        verified_tx1_hash: tx_hash,
        verified_tx2_hash: tx_hash,
      },
    }
  let output_to_next =
    transaction.Output {
      address: address.from_script(step3_hash),
      value: ct_value(),
      datum: transaction.InlineDatum(next_state),
      reference_script: None,
    }
  let tx =
    base_tx(
      [ct_input],
      [state_queue_input, hub_input],
      [output_to_next],
      redeemer_inputs,
      assets.zero,
    )
  expect list.length(tx.inputs) == 1
  expect step1_types.StepState { verified_tx1_hash }: step1_types.StepState =
    step_datum.data
  expect tx_hash != verified_tx1_hash
}

test step2_accepts_membership_proof_for_tx2() {
  let state_queue_input = mk_state_queue_input()
  let hub_input = mk_hub_input()
  let tx1_hash = crypto.blake2b_256(#"2c")
  let tx2_hash = crypto.blake2b_256(#"2d")
  let step_datum =
    StepDatum {
      fraud_prover: prover_hash,
      data: step1_types.StepState { verified_tx1_hash: tx1_hash },
    }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let proof =
    MembershipProofInputs {
      tx_root,
      tx_hash: tx2_hash,
      tx_info_hash: crypto.blake2b_256(#"2e"),
      tx_to_root_proof_path: fake_path,
    }
  let redeemer_inputs = membership_redeemers(proof)
  let next_state =
    StepDatum {
      fraud_prover: prover_hash,
      data: step2_types.StepState { verified_tx1_hash: tx1_hash, verified_tx2_hash: tx2_hash },
    }
  let output_to_next =
    transaction.Output {
      address: address.from_script(step3_hash),
      value: ct_value(),
      datum: transaction.InlineDatum(next_state),
      reference_script: None,
    }
  let tx =
    base_tx(
      [ct_input],
      [state_queue_input, hub_input],
      [output_to_next],
      redeemer_inputs,
      assets.zero,
    )
  expect step1_types.StepState { verified_tx1_hash }: step1_types.StepState =
    step_datum.data
  expect tx2_hash != verified_tx1_hash
  let ct_token_asset_name = fp_utils.get_singleton_ct_token(ct_input, ct_policy_id)
  let extracted_tx_root =
    fp_utils.get_tx_root_validate_block_hash(
      tx,
      0,
      ct_token_asset_name,
      state_queue_policy,
    )
  and {
    extracted_tx_root == tx_root,
    fp_utils.validate_membership_proof(tx, merkle_validator, proof),
    fp_utils.validate_step_output(
      tx,
      0,
      step3_hash,
      ct_policy_id,
      ct_token_asset_name,
      step_datum,
      next_state,
    ),
  }
}

test cancel_burns_ct_token_and_refunds_ada() {
  let step_datum = StepDatum { fraud_prover: prover_hash, data: 0 }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let refund_lovelace = assets.lovelace_of(ct_input.output.value)
  let output_to_prover =
    transaction.Output {
      address: address.from_verification_key(prover_hash),
      value: assets.from_lovelace(refund_lovelace),
      datum: transaction.NoDatum,
      reference_script: None,
    }
  let tx =
    base_tx(
      [ct_input],
      [],
      [output_to_prover],
      [],
      assets.from_asset(ct_policy_id, ct_asset_name, -1),
    )
  fp_utils.handle_cancel(ct_input, 0, step_datum, ct_policy_id, tx)
}

test cancel_fails_if_ct_token_is_not_burned() fail {
  let step_datum = StepDatum { fraud_prover: prover_hash, data: 0 }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let refund_lovelace = assets.lovelace_of(ct_input.output.value)
  let output_to_prover =
    transaction.Output {
      address: address.from_verification_key(prover_hash),
      value: assets.from_lovelace(refund_lovelace),
      datum: transaction.NoDatum,
      reference_script: None,
    }
  let tx =
    base_tx(
      [ct_input],
      [],
      [output_to_prover],
      [],
      assets.zero,
    )
  fp_utils.handle_cancel(ct_input, 0, step_datum, ct_policy_id, tx)
}

test cancel_fails_if_refund_amount_is_wrong() fail {
  let step_datum = StepDatum { fraud_prover: prover_hash, data: 0 }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let refund_lovelace = assets.lovelace_of(ct_input.output.value)
  let output_to_prover =
    transaction.Output {
      address: address.from_verification_key(prover_hash),
      value: assets.from_lovelace(refund_lovelace - 1),
      datum: transaction.NoDatum,
      reference_script: None,
    }
  let tx =
    base_tx(
      [ct_input],
      [],
      [output_to_prover],
      [],
      assets.from_asset(ct_policy_id, ct_asset_name, -1),
    )
  fp_utils.handle_cancel(ct_input, 0, step_datum, ct_policy_id, tx)
}

fn sample_tx_body(
  inputs_list: List<transaction.OutputReference>,
) -> MidgardTxBodyCompact {
  let inputs_hash = crypto.blake2b_256(cbor.serialise(inputs_list))
  MidgardTxBodyCompact {
    spend_inputs: inputs_hash,
    reference_inputs: crypto.blake2b_256(#"1a"),
    outputs: crypto.blake2b_256(#"1b"),
    fee: 0,
    validity_interval: interval.everything,
    required_observers: crypto.blake2b_256(#"1c"),
    required_signer_hashes: crypto.blake2b_256(#"1d"),
    mint: crypto.blake2b_256(#"1e"),
    script_integrity_hash: crypto.blake2b_256(#"1f"),
    auxiliary_data_hash: crypto.blake2b_256(#"20"),
    network_id: MidgardNetworkId.Mainnet,
  }
}

fn sample_tx(body: MidgardTxBodyCompact) -> MidgardTxCompact {
  let body_hash = crypto.blake2b_256(cbor.serialise(body))
  MidgardTxCompact {
    body: body_hash,
    wits: crypto.blake2b_256(#"21"),
    is_valid: True,
  }
}

fn step3_tx(
  inputs_list: List<transaction.OutputReference>,
) -> (
  MidgardTxBodyCompact,
  MidgardTxCompact,
  Data,
  ByteArray,
) {
  let tx_body = sample_tx_body(inputs_list)
  let tx = sample_tx(tx_body)
  let input_data: Data = inputs_list
  let tx_hash = crypto.blake2b_256(cbor.serialise(tx))
  (tx_body, tx, input_data, tx_hash)
}

test step3_requires_common_input_in_both_txs() fail {
  let shared_input =
    transaction.OutputReference {
      transaction_id: crypto.blake2b_256(#"22"),
      output_index: 0,
    }
  let tx1_inputs = [shared_input]
  let tx2_inputs: List<transaction.OutputReference> = []
  let (tx1_body, tx1, tx1_input_data, tx1_hash) = step3_tx(tx1_inputs)
  let (tx2_body, tx2, tx2_input_data, tx2_hash) = step3_tx(tx2_inputs)
  let step_datum =
    StepDatum {
      fraud_prover: prover_hash,
      data: step2_types.StepState {
        verified_tx1_hash: tx1_hash,
        verified_tx2_hash: tx2_hash,
      },
    }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let expected_state =
    StepDatum {
      fraud_prover: prover_hash,
      data: step3_types.StepState {
        verified_tx1_hash: tx1_hash,
        verified_tx2_hash: tx2_hash,
        verified_common_input: shared_input,
      },
    }
  let output_to_next =
    transaction.Output {
      address: address.from_script(step4_hash),
      value: ct_value(),
      datum: transaction.InlineDatum(expected_state),
      reference_script: None,
    }
  let tx =
    base_tx(
      [ct_input],
      [],
      [output_to_next],
      [],
      assets.zero,
    )
  let ct_token_asset_name = fp_utils.get_singleton_ct_token(ct_input, ct_policy_id)
  verify_hash_32(tx1, tx1_hash)
  verify_hash_32(tx2, tx2_hash)
  verify_hash_32(tx1_body, tx1.body)
  verify_hash_32(tx2_body, tx2.body)
  verify_hash_32(tx1_input_data, tx1_body.spend_inputs)
  verify_hash_32(tx2_input_data, tx2_body.spend_inputs)
  expect list.has(builtin.un_list_data(tx1_input_data), shared_input)
  expect list.has(builtin.un_list_data(tx2_input_data), shared_input)
  fp_utils.validate_step_output(
    tx,
    0,
    step4_hash,
    ct_policy_id,
    ct_token_asset_name,
    step_datum,
    expected_state,
  )
}

test step3_accepts_when_common_input_is_in_both_txs() {
  let shared_input =
    transaction.OutputReference {
      transaction_id: crypto.blake2b_256(#"22"),
      output_index: 0,
    }
  let other_input =
    transaction.OutputReference {
      transaction_id: crypto.blake2b_256(#"2f"),
      output_index: 1,
    }
  let tx1_inputs = [shared_input]
  let tx2_inputs = [other_input, shared_input]
  let (tx1_body, tx1, tx1_input_data, tx1_hash) = step3_tx(tx1_inputs)
  let (tx2_body, tx2, tx2_input_data, tx2_hash) = step3_tx(tx2_inputs)
  let step_datum =
    StepDatum {
      fraud_prover: prover_hash,
      data: step2_types.StepState {
        verified_tx1_hash: tx1_hash,
        verified_tx2_hash: tx2_hash,
      },
    }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let expected_state =
    StepDatum {
      fraud_prover: prover_hash,
      data: step3_types.StepState {
        verified_tx1_hash: tx1_hash,
        verified_tx2_hash: tx2_hash,
        verified_common_input: shared_input,
      },
    }
  let output_to_next =
    transaction.Output {
      address: address.from_script(step4_hash),
      value: ct_value(),
      datum: transaction.InlineDatum(expected_state),
      reference_script: None,
    }
  let tx =
    base_tx(
      [ct_input],
      [],
      [output_to_next],
      [],
      assets.zero,
    )
  let ct_token_asset_name = fp_utils.get_singleton_ct_token(ct_input, ct_policy_id)
  verify_hash_32(tx1, tx1_hash)
  verify_hash_32(tx2, tx2_hash)
  verify_hash_32(tx1_body, tx1.body)
  verify_hash_32(tx2_body, tx2.body)
  verify_hash_32(tx1_input_data, tx1_body.spend_inputs)
  verify_hash_32(tx2_input_data, tx2_body.spend_inputs)
  expect list.has(builtin.un_list_data(tx1_input_data), shared_input)
  expect list.has(builtin.un_list_data(tx2_input_data), shared_input)
  fp_utils.validate_step_output(
    tx,
    0,
    step4_hash,
    ct_policy_id,
    ct_token_asset_name,
    step_datum,
    expected_state,
  )
}

test step3_fails_when_tx_hash_does_not_match_verified_hash() fail {
  let shared_input =
    transaction.OutputReference {
      transaction_id: crypto.blake2b_256(#"22"),
      output_index: 0,
    }
  let tx1_inputs = [shared_input]
  let tx2_inputs = [shared_input]
  let (_, tx1, _, _) = step3_tx(tx1_inputs)
  let (_, tx2, _, tx2_hash) = step3_tx(tx2_inputs)
  let step_datum =
    StepDatum {
      fraud_prover: prover_hash,
      data: step2_types.StepState {
        verified_tx1_hash: crypto.blake2b_256(#"30"),
        verified_tx2_hash: tx2_hash,
      },
    }
  expect step2_types.StepState { verified_tx1_hash, verified_tx2_hash }: step2_types.StepState =
    step_datum.data
  verify_hash_32(tx1, verified_tx1_hash)
  verify_hash_32(tx2, verified_tx2_hash)
}

test step4_mints_fraud_proof_and_burns_ct_token() {
  let shared_input =
    transaction.OutputReference {
      transaction_id: crypto.blake2b_256(#"22"),
      output_index: 0,
    }
  let tx1_inputs = [shared_input]
  let (_, _, _, tx1_hash) = step3_tx(tx1_inputs)
  let (_, _, _, tx2_hash) = step3_tx(tx1_inputs)
  let step_datum =
    StepDatum {
      fraud_prover: prover_hash,
      data: step3_types.StepState {
        verified_tx1_hash: tx1_hash,
        verified_tx2_hash: tx2_hash,
        verified_common_input: shared_input,
      },
    }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let fraud_output =
    transaction.Output {
      address: address.from_verification_key(prover_hash),
      value: assets.from_lovelace(5_000_000),
      datum: transaction.InlineDatum(FraudProofDatum { fraud_prover: prover_hash }),
      reference_script: None,
    }
  let ct_redeemer: transaction.Redeemer =
    Success { token_to_burn_asset_name: ct_asset_name }
  let burn_redeemers =
    [
      Pair(transaction.Mint(ct_policy_id), ct_redeemer),
    ]
  let tx =
    base_tx(
      [ct_input],
      [],
      [fraud_output],
      burn_redeemers,
      assets.from_asset(fraud_policy_id, ct_asset_name, 1)
        |> assets.add(ct_policy_id, ct_asset_name, -1),
    )
  let ct_token_asset_name = fp_utils.get_singleton_ct_token(ct_input, ct_policy_id)
  fp_utils.validate_final_step_output(
    tx,
    0,
    fraud_policy_id,
    ct_token_asset_name,
    step_datum,
    0,
  )
}

test step4_fails_if_output_datum_has_wrong_fraud_prover() fail {
  let shared_input =
    transaction.OutputReference {
      transaction_id: crypto.blake2b_256(#"22"),
      output_index: 0,
    }
  let tx1_inputs = [shared_input]
  let (_, _, _, tx1_hash) = step3_tx(tx1_inputs)
  let (_, _, _, tx2_hash) = step3_tx(tx1_inputs)
  let step_datum =
    StepDatum {
      fraud_prover: prover_hash,
      data: step3_types.StepState {
        verified_tx1_hash: tx1_hash,
        verified_tx2_hash: tx2_hash,
        verified_common_input: shared_input,
      },
    }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let fraud_output =
    transaction.Output {
      address: address.from_verification_key(prover_hash),
      value: assets.from_lovelace(5_000_000),
      datum: transaction.InlineDatum(FraudProofDatum { fraud_prover: crypto.blake2b_224(#"02") }),
      reference_script: None,
    }
  let ct_redeemer: transaction.Redeemer =
    Success { token_to_burn_asset_name: ct_asset_name }
  let tx =
    base_tx(
      [ct_input],
      [],
      [fraud_output],
      [Pair(transaction.Mint(ct_policy_id), ct_redeemer)],
      assets.from_asset(fraud_policy_id, ct_asset_name, 1)
        |> assets.add(ct_policy_id, ct_asset_name, -1),
    )
  let ct_token_asset_name = fp_utils.get_singleton_ct_token(ct_input, ct_policy_id)
  fp_utils.validate_final_step_output(
    tx,
    0,
    fraud_policy_id,
    ct_token_asset_name,
    step_datum,
    0,
  )
}

test step4_fails_if_computation_thread_success_redeemer_has_wrong_token_name() fail {
  let shared_input =
    transaction.OutputReference {
      transaction_id: crypto.blake2b_256(#"22"),
      output_index: 0,
    }
  let tx1_inputs = [shared_input]
  let (_, _, _, tx1_hash) = step3_tx(tx1_inputs)
  let (_, _, _, tx2_hash) = step3_tx(tx1_inputs)
  let step_datum =
    StepDatum {
      fraud_prover: prover_hash,
      data: step3_types.StepState {
        verified_tx1_hash: tx1_hash,
        verified_tx2_hash: tx2_hash,
        verified_common_input: shared_input,
      },
    }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let fraud_output =
    transaction.Output {
      address: address.from_verification_key(prover_hash),
      value: assets.from_lovelace(5_000_000),
      datum: transaction.InlineDatum(FraudProofDatum { fraud_prover: prover_hash }),
      reference_script: None,
    }
  let wrong_ct_asset_name = bytearray.concat(#"00000001", other_block_hash)
  let ct_redeemer: transaction.Redeemer =
    Success { token_to_burn_asset_name: wrong_ct_asset_name }
  let tx =
    base_tx(
      [ct_input],
      [],
      [fraud_output],
      [Pair(transaction.Mint(ct_policy_id), ct_redeemer)],
      assets.from_asset(fraud_policy_id, ct_asset_name, 1)
        |> assets.add(ct_policy_id, ct_asset_name, -1),
    )
  let ct_token_asset_name = fp_utils.get_singleton_ct_token(ct_input, ct_policy_id)
  fp_utils.validate_final_step_output(
    tx,
    0,
    fraud_policy_id,
    ct_token_asset_name,
    step_datum,
    0,
  )
}

test step4_fails_if_fraud_token_is_not_minted() fail {
  let shared_input =
    transaction.OutputReference {
      transaction_id: crypto.blake2b_256(#"22"),
      output_index: 0,
    }
  let tx1_inputs = [shared_input]
  let (_, _, _, tx1_hash) = step3_tx(tx1_inputs)
  let (_, _, _, tx2_hash) = step3_tx(tx1_inputs)
  let step_datum =
    StepDatum {
      fraud_prover: prover_hash,
      data: step3_types.StepState {
        verified_tx1_hash: tx1_hash,
        verified_tx2_hash: tx2_hash,
        verified_common_input: shared_input,
      },
    }
  let (ct_input, _out_ref) = mk_ct_input(step_datum)
  let fraud_output =
    transaction.Output {
      address: address.from_verification_key(prover_hash),
      value: assets.from_lovelace(5_000_000),
      datum: transaction.InlineDatum(FraudProofDatum { fraud_prover: prover_hash }),
      reference_script: None,
    }
  let ct_redeemer: transaction.Redeemer =
    Success { token_to_burn_asset_name: ct_asset_name }
  let tx =
    base_tx(
      [ct_input],
      [],
      [fraud_output],
      [Pair(transaction.Mint(ct_policy_id), ct_redeemer)],
      assets.from_asset(ct_policy_id, ct_asset_name, -1),
    )
  let ct_token_asset_name = fp_utils.get_singleton_ct_token(ct_input, ct_policy_id)
  fp_utils.validate_final_step_output(
    tx,
    0,
    fraud_policy_id,
    ct_token_asset_name,
    step_datum,
    0,
  )
}
