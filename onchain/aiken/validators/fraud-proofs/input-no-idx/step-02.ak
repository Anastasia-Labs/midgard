use aiken/collection/list
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/primitive/bytearray
use cardano/address
use cardano/assets.{
  PolicyId, add, expect_lovelace_of, from_lovelace, without_lovelace, zero,
}
use cardano/transaction.{
  DatumHash, InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/input_no_idx/step_02.{
  Complete, CompletePublished, Datum, Direct, FoldNext, FoldStart, Folding,
  PublishedSpendInputsV1, SpendRedeemer,
}
use midgard/fraud_proofs/input_no_idx/step_03
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_input_cbor, encode_midgard_tx_input,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTxInput,
}
use midgard/validation_merkle_v1

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(args) ->
        when args is {
          Complete {
            input_index,
            output_index,
            inputs_preimage,
            bad_inputs_index,
          } ->
            advance_complete(
              step_03_validator_script_hash,
              computation_thread_token_policy_id,
              datum,
              input_index,
              output_index,
              inputs_preimage,
              bad_inputs_index,
              own_out_ref,
              tx,
            )
          CompletePublished {
            input_index,
            output_index,
            publication_reference_input_index,
            bad_inputs_index,
          } ->
            advance_complete_published(
              step_03_validator_script_hash,
              computation_thread_token_policy_id,
              datum,
              input_index,
              output_index,
              publication_reference_input_index,
              bad_inputs_index,
              own_out_ref,
              tx,
            )
          FoldStart {
            input_index,
            output_index,
            bad_inputs_index,
            input_cbor,
            collection_proof,
          } ->
            start_fold(
              step_03_validator_script_hash,
              computation_thread_token_policy_id,
              datum,
              input_index,
              output_index,
              bad_inputs_index,
              input_cbor,
              collection_proof,
              own_out_ref,
              tx,
            )
          FoldNext { input_index, output_index, input_cbor, collection_proof } ->
            continue_fold(
              step_03_validator_script_hash,
              computation_thread_token_policy_id,
              datum,
              input_index,
              output_index,
              input_cbor,
              collection_proof,
              own_out_ref,
              tx,
            )
        }
    }
  }

  else(_) {
    fail
  }
}

fn advance_complete_published(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  datum: Option<Datum>,
  input_index: Int,
  output_index: Int,
  publication_reference_input_index: Int,
  bad_inputs_index: Int,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, reference_inputs, .. } = tx
  expect Some(step_datum) = datum
  let
      _own_script_hash,
      computation_thread_token_asset_name,
      fraud_prover,
      m_input_state_data,
      output_script_hash,
      output_state_data
    <- continue(
    computation_thread_token_policy_id,
    step_datum,
    input_index,
    output_index,
    own_out_ref,
    inputs,
    outputs,
  )
  expect Some(Direct { verified_tx_inputs_hash }) = m_input_state_data
  expect
      Some(Input {
        output: Output {
          address: address.Address {
            payment_credential: address.VerificationKey(publication_owner),
            stake_credential: None,
          },
          value: publication_value,
          datum: InlineDatum(publication_data),
          reference_script: None,
        },
        ..
      })
    = reference_inputs |> list.at(publication_reference_input_index)
  expect publication: PublishedSpendInputsV1 = publication_data
  expect publication.version == 1
  expect publication_owner == fraud_prover
  expect
    publication.computation_thread_policy_id == computation_thread_token_policy_id
  expect
    publication.computation_thread_asset_name == computation_thread_token_asset_name
  expect publication.fraud_prover == fraud_prover
  expect publication.verified_tx_inputs_hash == verified_tx_inputs_hash
  expect publication.item_count > 0
  expect publication.item_count == list.length(publication.inputs)
  expect bad_inputs_index >= 0
  expect bad_inputs_index < publication.item_count
  expect without_lovelace(publication_value) == zero
  expect expect_lovelace_of(publication_value) > 0
  expect
    bounded_collection_v1.from_items(
      0,
      list.map(publication.inputs, encode_midgard_tx_input),
    ) == verified_tx_inputs_hash
  let bad_input = publication.inputs |> list.expect_at(bad_inputs_index)
  expect output_script_hash == step_03_validator_script_hash
  let expected_output_state: Data = step_03.State {
    bad_input_tx_id: bad_input.tx_id,
    bad_input_output_index: bad_input.output_index,
  }
  expect output_state_data == expected_output_state
  True
}

fn advance_complete(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  datum: Option<Datum>,
  input_index: Int,
  output_index: Int,
  inputs_preimage: List<MidgardTxInput>,
  bad_inputs_index: Int,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, .. } = tx

  expect Some(step_datum) = datum

  // 1. Specified input must be authentic and properly reproduced.
  let
      _own_script_hash,
      _computation_thread_token_asset_name,
      _fraud_prover,
      m_input_state_data,
      output_script_hash,
      output_state_data
    <- continue(
    computation_thread_token_policy_id,
    step_datum,
    input_index,
    output_index,
    own_out_ref,
    inputs,
    outputs,
  )

  expect Some(Direct { verified_tx_inputs_hash }) = m_input_state_data

  // 2. Provided inputs preimage must correspond correctly to the passed
  //    along hash. Each item is the node's canonical `TransactionInput`
  //    CBOR under the consensus-ordered spend-inputs field (0), not
  //    `cbor.serialise` of an Aiken `OutputReference` list.
  expect
    bounded_collection_v1.from_items(
      0,
      list.map(inputs_preimage, encode_midgard_tx_input),
    ) == verified_tx_inputs_hash

  let bad_input = inputs_preimage |> list.expect_at(bad_inputs_index)

  // 3. The fetched input's tx id and output index must be passed to the
  //    next step so that its producing transaction can be verified
  //    against the included hash.
  expect output_script_hash == step_03_validator_script_hash
  let expected_output_state: Data = step_03.State {
    bad_input_tx_id: bad_input.tx_id,
    bad_input_output_index: bad_input.output_index,
  }
  expect output_state_data == expected_output_state

  // Done.
  True
}

fn start_fold(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  datum: Option<Datum>,
  input_index: Int,
  output_index: Int,
  bad_inputs_index: Int,
  input_cbor: ByteArray,
  collection_proof: bounded_collection_v1.ItemProofV1,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, .. } = tx
  expect Some(step_datum) = datum
  let
      own_script_hash,
      _computation_thread_token_asset_name,
      _fraud_prover,
      m_input_state_data,
      output_script_hash,
      output_state_data
    <- continue(
    computation_thread_token_policy_id,
    step_datum,
    input_index,
    output_index,
    own_out_ref,
    inputs,
    outputs,
  )
  expect Some(Direct { verified_tx_inputs_hash }) = m_input_state_data
  expect collection_proof.item_index == 0
  expect bad_inputs_index >= 0
  expect bad_inputs_index < collection_proof.item_count
  expect verify_fold_item(verified_tx_inputs_hash, input_cbor, collection_proof)
  let bad_input = decode_midgard_tx_input_cbor(input_cbor)
  let selected_input = if bad_inputs_index == 0 {
    Some(bad_input)
  } else {
    None
  }
  advance_fold_output_with_own(
    own_script_hash,
    step_03_validator_script_hash,
    output_script_hash,
    output_state_data,
    verified_tx_inputs_hash,
    collection_proof.item_count,
    1,
    bad_inputs_index,
    selected_input,
  )
}

fn continue_fold(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  datum: Option<Datum>,
  input_index: Int,
  output_index: Int,
  input_cbor: ByteArray,
  collection_proof: bounded_collection_v1.ItemProofV1,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, .. } = tx
  expect Some(step_datum) = datum
  let
      own_script_hash,
      _computation_thread_token_asset_name,
      _fraud_prover,
      m_input_state_data,
      output_script_hash,
      output_state_data
    <- continue(
    computation_thread_token_policy_id,
    step_datum,
    input_index,
    output_index,
    own_out_ref,
    inputs,
    outputs,
  )
  expect
      Some(Folding {
        verified_tx_inputs_hash,
        item_count,
        next_item_index,
        bad_inputs_index,
        selected_input,
      })
    = m_input_state_data
  expect next_item_index >= 0
  expect next_item_index < item_count
  expect bad_inputs_index >= 0
  expect bad_inputs_index < item_count
  expect collection_proof.item_count == item_count
  expect collection_proof.item_index == next_item_index
  expect verify_fold_item(verified_tx_inputs_hash, input_cbor, collection_proof)
  let input = decode_midgard_tx_input_cbor(input_cbor)
  let selected = if next_item_index == bad_inputs_index {
    Some(input)
  } else {
    selected_input
  }
  advance_fold_output_with_own(
    own_script_hash,
    step_03_validator_script_hash,
    output_script_hash,
    output_state_data,
    verified_tx_inputs_hash,
    item_count,
    next_item_index + 1,
    bad_inputs_index,
    selected,
  )
}

fn verify_fold_item(
  verified_tx_inputs_hash: ByteArray,
  input_cbor: ByteArray,
  collection_proof: bounded_collection_v1.ItemProofV1,
) -> Bool {
  if and {
    collection_proof.field_index == 0,
    collection_proof.item_length == bytearray.length(input_cbor),
    collection_proof.item_commitment == bounded_item_v1.from_bytes(
      0,
      collection_proof.item_index,
      input_cbor,
    ),
    bounded_collection_v1.verify_item(verified_tx_inputs_hash, collection_proof),
  } {
    let decoded = decode_midgard_tx_input_cbor(input_cbor)
    bytearray.length(decoded.tx_id) == 32
  } else {
    False
  }
}

fn advance_fold_output_with_own(
  own_script_hash: ScriptHash,
  step_03_validator_script_hash: ScriptHash,
  output_script_hash: ScriptHash,
  output_state_data: Data,
  verified_tx_inputs_hash: ByteArray,
  item_count: Int,
  next_item_index: Int,
  bad_inputs_index: Int,
  selected_input: Option<MidgardTxInput>,
) -> Bool {
  if next_item_index == item_count {
    expect Some(selected) = selected_input
    expect output_script_hash == step_03_validator_script_hash
    let expected: Data = step_03.State {
      bad_input_tx_id: selected.tx_id,
      bad_input_output_index: selected.output_index,
    }
    output_state_data == expected
  } else {
    expect output_script_hash == own_script_hash
    let expected: Data = Folding {
      verified_tx_inputs_hash,
      item_count,
      next_item_index,
      bad_inputs_index,
      selected_input,
    }
    output_state_data == expected
  }
}

// ## Selectors
//
// The step-02 obligation is a preimage opening: the prover must reveal the
// exact spend-inputs preimage the bad transaction committed under the block's
// counted `transactions_root`, and may forward only an input that preimage
// actually contains.

/// A transaction spending an out-of-range output index of `producing_tx_id`.
fn input_no_idx_bad_tx(producing_tx_id: ByteArray) -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: producing_tx_id, output_index: 7 }],
    },
  }
}

fn run_step_02(
  block: fixture.NativeBlockFixtureV1,
  verified_tx_inputs_hash: ByteArray,
  inputs_preimage: List<MidgardTxInput>,
  bad_inputs_index: Int,
  forwarded: MidgardTxInput,
) -> Bool {
  let expected_output_state: Data = step_03.State {
    bad_input_tx_id: forwarded.tx_id,
    bad_input_output_index: forwarded.output_index,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(Direct { verified_tx_inputs_hash }),
      },
    ),
    ct.Continue(
      Complete {
        input_index: 0,
        output_index: 0,
        inputs_preimage,
        bad_inputs_index,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(
      fixture.ct_token_asset_name(block),
      expected_output_state,
    ),
  )
}

fn one_input_opening(input: MidgardTxInput) -> bounded_collection_v1.ItemProofV1 {
  let input_cbor = encode_midgard_tx_input(input)
  let item_commitment = bounded_item_v1.from_bytes(0, 0, input_cbor)
  bounded_collection_v1.ItemProofV1 {
    version: 1,
    field_index: 0,
    item_count: 1,
    item_index: 0,
    item_length: bytearray.length(input_cbor),
    item_commitment,
    frontier: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: bounded_collection_v1.hash_item(
          0,
          0,
          bytearray.length(input_cbor),
          item_commitment,
        ),
      },
    ],
    siblings: [],
  }
}

fn two_input_opening(
  first: MidgardTxInput,
  second: MidgardTxInput,
  item_index: Int,
) -> bounded_collection_v1.ItemProofV1 {
  let first_cbor = encode_midgard_tx_input(first)
  let second_cbor = encode_midgard_tx_input(second)
  let first_commitment = bounded_item_v1.from_bytes(0, 0, first_cbor)
  let second_commitment = bounded_item_v1.from_bytes(0, 1, second_cbor)
  let first_leaf = bounded_collection_v1.hash_item(
    0,
    0,
    bytearray.length(first_cbor),
    first_commitment,
  )
  let second_leaf = bounded_collection_v1.hash_item(
    0,
    1,
    bytearray.length(second_cbor),
    second_commitment,
  )
  if item_index == 0 {
    bounded_collection_v1.ItemProofV1 {
      version: 1,
      field_index: 0,
      item_count: 2,
      item_index,
      item_length: bytearray.length(first_cbor),
      item_commitment: first_commitment,
      frontier: validation_merkle_v1.build_frontier([first_leaf,
          second_leaf]).peaks,
      siblings: [second_leaf],
    }
  } else {
    bounded_collection_v1.ItemProofV1 {
      version: 1,
      field_index: 0,
      item_count: 2,
      item_index,
      item_length: bytearray.length(second_cbor),
      item_commitment: second_commitment,
      frontier: validation_merkle_v1.build_frontier([first_leaf,
          second_leaf]).peaks,
      siblings: [first_leaf],
    }
  }
}

fn continue_fold_l1_tx(
  asset_name: ByteArray,
  output_state_data: Data,
) -> Transaction {
  let output_datum: ct.StepDatum<Data> = ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(output_state_data),
  }
  let output_datum_data: Data = output_datum
  Transaction {
    ..fixture.continue_mid_step_l1_tx_v1(asset_name, output_state_data),
    outputs: [
      Output {
        address: address.from_script(fixture.step_script_hash),
        value: fixture.ct_token_value(asset_name),
        datum: InlineDatum(output_datum_data),
        reference_script: None,
      },
    ],
  }
}

fn fold_start_two_items(
  first: MidgardTxInput,
  second: MidgardTxInput,
  bad_inputs_index: Int,
) -> Bool {
  let root = bounded_collection_v1.from_items(
    0,
    [encode_midgard_tx_input(first), encode_midgard_tx_input(second)],
  )
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let expected_state: Data = Folding {
    verified_tx_inputs_hash: root,
    item_count: 2,
    next_item_index: 1,
    bad_inputs_index,
    selected_input: if bad_inputs_index == 0 {
      Some(first)
    } else {
      None
    },
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(Direct { verified_tx_inputs_hash: root }),
      },
    ),
    ct.Continue(
      FoldStart {
        input_index: 0,
        output_index: 0,
        bad_inputs_index,
        input_cbor: encode_midgard_tx_input(first),
        collection_proof: two_input_opening(first, second, 0),
      },
    ),
    fixture.own_out_ref(),
    continue_fold_l1_tx(fixture.ct_token_asset_name(block), expected_state),
  )
}

fn fold_next_two_items(
  first: MidgardTxInput,
  second: MidgardTxInput,
  redeemer_proof: bounded_collection_v1.ItemProofV1,
  redeemer_cbor: ByteArray,
  output_state: Data,
) -> Bool {
  let root = bounded_collection_v1.from_items(
    0,
    [encode_midgard_tx_input(first), encode_midgard_tx_input(second)],
  )
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          Folding {
            verified_tx_inputs_hash: root,
            item_count: 2,
            next_item_index: 1,
            bad_inputs_index: 1,
            selected_input: None,
          },
        ),
      },
    ),
    ct.Continue(
      FoldNext {
        input_index: 0,
        output_index: 0,
        input_cbor: redeemer_cbor,
        collection_proof: redeemer_proof,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(
      fixture.ct_token_asset_name(block),
      output_state,
    ),
  )
}

fn three_input_opening(
  first: MidgardTxInput,
  second: MidgardTxInput,
  third: MidgardTxInput,
  item_index: Int,
) -> bounded_collection_v1.ItemProofV1 {
  let first_cbor = encode_midgard_tx_input(first)
  let second_cbor = encode_midgard_tx_input(second)
  let third_cbor = encode_midgard_tx_input(third)
  let first_commitment = bounded_item_v1.from_bytes(0, 0, first_cbor)
  let second_commitment = bounded_item_v1.from_bytes(0, 1, second_cbor)
  let third_commitment = bounded_item_v1.from_bytes(0, 2, third_cbor)
  let first_leaf = bounded_collection_v1.hash_item(
    0,
    0,
    bytearray.length(first_cbor),
    first_commitment,
  )
  let second_leaf = bounded_collection_v1.hash_item(
    0,
    1,
    bytearray.length(second_cbor),
    second_commitment,
  )
  let third_leaf = bounded_collection_v1.hash_item(
    0,
    2,
    bytearray.length(third_cbor),
    third_commitment,
  )
  let frontier = validation_merkle_v1.build_frontier(
    [first_leaf, second_leaf, third_leaf],
  ).peaks
  when item_index is {
    0 ->
      bounded_collection_v1.ItemProofV1 {
        version: 1,
        field_index: 0,
        item_count: 3,
        item_index,
        item_length: bytearray.length(first_cbor),
        item_commitment: first_commitment,
        frontier,
        siblings: [second_leaf],
      }
    1 ->
      bounded_collection_v1.ItemProofV1 {
        version: 1,
        field_index: 0,
        item_count: 3,
        item_index,
        item_length: bytearray.length(second_cbor),
        item_commitment: second_commitment,
        frontier,
        siblings: [first_leaf],
      }
    2 ->
      bounded_collection_v1.ItemProofV1 {
        version: 1,
        field_index: 0,
        item_count: 3,
        item_index,
        item_length: bytearray.length(third_cbor),
        item_commitment: third_commitment,
        frontier,
        siblings: [],
      }
    _ -> fail
  }
}

fn fold_next_three_items(
  first: MidgardTxInput,
  second: MidgardTxInput,
  third: MidgardTxInput,
  next_item_index: Int,
  bad_inputs_index: Int,
  selected_input: Option<MidgardTxInput>,
  redeemer_proof: bounded_collection_v1.ItemProofV1,
  redeemer_cbor: ByteArray,
  output_state: Data,
  self_loop: Bool,
) -> Bool {
  let root = bounded_collection_v1.from_items(
    0,
    [
      encode_midgard_tx_input(first),
      encode_midgard_tx_input(second),
      encode_midgard_tx_input(third),
    ],
  )
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let continuation = if self_loop {
    continue_fold_l1_tx(fixture.ct_token_asset_name(block), output_state)
  } else {
    fixture.continue_mid_step_l1_tx_v1(
      fixture.ct_token_asset_name(block),
      output_state,
    )
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          Folding {
            verified_tx_inputs_hash: root,
            item_count: 3,
            next_item_index,
            bad_inputs_index,
            selected_input,
          },
        ),
      },
    ),
    ct.Continue(
      FoldNext {
        input_index: 0,
        output_index: 0,
        input_cbor: redeemer_cbor,
        collection_proof: redeemer_proof,
      },
    ),
    fixture.own_out_ref(),
    continuation,
  )
}

/// FoldStart must self-loop to this script with the exact authenticated cursor
/// state; no external cursor or selected input is accepted from the prover.
test input_no_idx_step_02_fold_start_self_loops_with_exact_state() {
  let first = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 6,
  }
  let second = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 7,
  }
  fold_start_two_items(first, second, 1)
}

/// The final ordered FoldNext consumes index 1 and forwards the same step-03
/// state as the direct path would for the selected second item.
test input_no_idx_step_02_fold_next_terminal_forwards_identical_step_03_state() {
  let first = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 6,
  }
  let second = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 7,
  }
  let expected: Data = step_03.State {
    bad_input_tx_id: second.tx_id,
    bad_input_output_index: second.output_index,
  }
  fold_next_two_items(
    first,
    second,
    two_input_opening(first, second, 1),
    encode_midgard_tx_input(second),
    expected,
  )
}

test input_no_idx_step_02_fold_next_self_loops_and_preserves_selected_input() {
  let first = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 5,
  }
  let second = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 6,
  }
  let third = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 7,
  }
  let root = bounded_collection_v1.from_items(
    0,
    [
      encode_midgard_tx_input(first),
      encode_midgard_tx_input(second),
      encode_midgard_tx_input(third),
    ],
  )
  let expected: Data = Folding {
    verified_tx_inputs_hash: root,
    item_count: 3,
    next_item_index: 2,
    bad_inputs_index: 0,
    selected_input: Some(first),
  }
  fold_next_three_items(
    first,
    second,
    third,
    1,
    0,
    Some(first),
    three_input_opening(first, second, third, 1),
    encode_midgard_tx_input(second),
    expected,
    True,
  )
}

test input_no_idx_step_02_fold_next_self_loop_captures_selected_input() {
  let first = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 5,
  }
  let second = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 6,
  }
  let third = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 7,
  }
  let root = bounded_collection_v1.from_items(
    0,
    [
      encode_midgard_tx_input(first),
      encode_midgard_tx_input(second),
      encode_midgard_tx_input(third),
    ],
  )
  let expected: Data = Folding {
    verified_tx_inputs_hash: root,
    item_count: 3,
    next_item_index: 2,
    bad_inputs_index: 1,
    selected_input: Some(second),
  }
  fold_next_three_items(
    first,
    second,
    third,
    1,
    1,
    None,
    three_input_opening(first, second, third, 1),
    encode_midgard_tx_input(second),
    expected,
    True,
  )
}

test input_no_idx_step_02_fold_next_after_self_loop_forwards_selected_input() {
  let first = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 5,
  }
  let second = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 6,
  }
  let third = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 7,
  }
  let expected: Data = step_03.State {
    bad_input_tx_id: first.tx_id,
    bad_input_output_index: first.output_index,
  }
  fold_next_three_items(
    first,
    second,
    third,
    2,
    0,
    Some(first),
    three_input_opening(first, second, third, 2),
    encode_midgard_tx_input(third),
    expected,
    False,
  )
}

/// Fixed cursor/root/count state for one-mutation FoldNext negative controls.
fn reject_fold_next_three(
  first: MidgardTxInput,
  second: MidgardTxInput,
  third: MidgardTxInput,
  redeemer_proof: bounded_collection_v1.ItemProofV1,
  redeemer_cbor: ByteArray,
) -> Bool {
  let root = bounded_collection_v1.from_items(
    0,
    [
      encode_midgard_tx_input(first),
      encode_midgard_tx_input(second),
      encode_midgard_tx_input(third),
    ],
  )
  let expected: Data = Folding {
    verified_tx_inputs_hash: root,
    item_count: 3,
    next_item_index: 2,
    bad_inputs_index: 0,
    selected_input: Some(first),
  }
  fold_next_three_items(
    first,
    second,
    third,
    1,
    0,
    Some(first),
    redeemer_proof,
    redeemer_cbor,
    expected,
    True,
  )
}

fn three_fold_test_items() -> (MidgardTxInput, MidgardTxInput, MidgardTxInput) {
  (
    MidgardTxInput { tx_id: proof_fit_spend_input_tx_id, output_index: 5 },
    MidgardTxInput { tx_id: proof_fit_spend_input_tx_id, output_index: 6 },
    MidgardTxInput { tx_id: proof_fit_spend_input_tx_id, output_index: 7 },
  )
}

test input_no_idx_step_02_fold_next_rejects_forward_omission() fail {
  let (first, second, third) = three_fold_test_items()
  reject_fold_next_three(
    first,
    second,
    third,
    three_input_opening(first, second, third, 2),
    encode_midgard_tx_input(third),
  )
}

test input_no_idx_step_02_fold_next_rejects_duplicate_backward_replay() fail {
  let (first, second, third) = three_fold_test_items()
  reject_fold_next_three(
    first,
    second,
    third,
    three_input_opening(first, second, third, 0),
    encode_midgard_tx_input(first),
  )
}

test input_no_idx_step_02_fold_next_rejects_reordered_opening() fail {
  let (first, second, third) = three_fold_test_items()
  reject_fold_next_three(
    first,
    second,
    third,
    three_input_opening(second, first, third, 1),
    encode_midgard_tx_input(first),
  )
}

test input_no_idx_step_02_fold_next_rejects_substituted_input() fail {
  let (first, second, third) = three_fold_test_items()
  reject_fold_next_three(
    first,
    second,
    third,
    three_input_opening(first, second, third, 1),
    encode_midgard_tx_input(first),
  )
}

test input_no_idx_step_02_fold_next_rejects_trailing_input_data() fail {
  let (first, second, third) = three_fold_test_items()
  reject_fold_next_three(
    first,
    second,
    third,
    three_input_opening(first, second, third, 1),
    bytearray.concat(encode_midgard_tx_input(second), #"00"),
  )
}

test input_no_idx_step_02_fold_next_rejects_wrong_proof_version() fail {
  let (first, second, third) = three_fold_test_items()
  let proof = three_input_opening(first, second, third, 1)
  reject_fold_next_three(
    first,
    second,
    third,
    bounded_collection_v1.ItemProofV1 { ..proof, version: 2 },
    encode_midgard_tx_input(second),
  )
}

test input_no_idx_step_02_fold_next_rejects_frozen_item_count_mismatch() fail {
  let (first, second, third) = three_fold_test_items()
  let proof = three_input_opening(first, second, third, 1)
  reject_fold_next_three(
    first,
    second,
    third,
    bounded_collection_v1.ItemProofV1 { ..proof, item_count: 2 },
    encode_midgard_tx_input(second),
  )
}

/// The fold's item verifier binds field, root, index, canonical CBOR length,
/// item commitment and MMR membership before it decodes the selected input.
test input_no_idx_step_02_fold_item_authenticates_canonical_opening() {
  let input = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 7,
  }
  let input_cbor = encode_midgard_tx_input(input)
  let root = bounded_collection_v1.from_items(0, [input_cbor])
  verify_fold_item(root, input_cbor, one_input_opening(input))
}

test input_no_idx_step_02_fold_item_rejects_noncanonical_trailing_bytes() fail {
  let input = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 7,
  }
  let input_cbor = encode_midgard_tx_input(input)
  let root = bounded_collection_v1.from_items(0, [input_cbor])
  expect
    verify_fold_item(
      root,
      bytearray.concat(input_cbor, #"00"),
      one_input_opening(input),
    )
  True
}

fn fold_item_test_input() -> MidgardTxInput {
  MidgardTxInput { tx_id: proof_fit_spend_input_tx_id, output_index: 7 }
}

fn fold_item_test_root(input: MidgardTxInput) -> ByteArray {
  bounded_collection_v1.from_items(0, [encode_midgard_tx_input(input)])
}

fn must_reject_fold_opening(
  root: ByteArray,
  input_cbor: ByteArray,
  proof: bounded_collection_v1.ItemProofV1,
) -> Bool {
  expect verify_fold_item(root, input_cbor, proof)
  True
}

test input_no_idx_step_02_fold_rejects_wrong_root() fail {
  let input = fold_item_test_input()
  must_reject_fold_opening(
    #"0000000000000000000000000000000000000000000000000000000000000000",
    encode_midgard_tx_input(input),
    one_input_opening(input),
  )
}

test input_no_idx_step_02_fold_rejects_wrong_field() fail {
  let input = fold_item_test_input()
  let proof = one_input_opening(input)
  must_reject_fold_opening(
    fold_item_test_root(input),
    encode_midgard_tx_input(input),
    bounded_collection_v1.ItemProofV1 { ..proof, field_index: 1 },
  )
}

test input_no_idx_step_02_fold_rejects_wrong_count_or_index() fail {
  let input = fold_item_test_input()
  let proof = one_input_opening(input)
  must_reject_fold_opening(
    fold_item_test_root(input),
    encode_midgard_tx_input(input),
    bounded_collection_v1.ItemProofV1 { ..proof, item_index: 1 },
  )
}

test input_no_idx_step_02_fold_rejects_wrong_count() fail {
  let input = fold_item_test_input()
  let proof = one_input_opening(input)
  must_reject_fold_opening(
    fold_item_test_root(input),
    encode_midgard_tx_input(input),
    bounded_collection_v1.ItemProofV1 { ..proof, item_count: 2 },
  )
}

test input_no_idx_step_02_fold_rejects_wrong_length() fail {
  let input = fold_item_test_input()
  let proof = one_input_opening(input)
  must_reject_fold_opening(
    fold_item_test_root(input),
    encode_midgard_tx_input(input),
    bounded_collection_v1.ItemProofV1 {
      ..proof,
      item_length: proof.item_length + 1,
    },
  )
}

test input_no_idx_step_02_fold_rejects_wrong_item_commitment() fail {
  let input = fold_item_test_input()
  let proof = one_input_opening(input)
  must_reject_fold_opening(
    fold_item_test_root(input),
    encode_midgard_tx_input(input),
    bounded_collection_v1.ItemProofV1 {
      ..proof,
      item_commitment: #"0000000000000000000000000000000000000000000000000000000000000000",
    },
  )
}

test input_no_idx_step_02_fold_rejects_wrong_frontier() fail {
  let input = fold_item_test_input()
  let proof = one_input_opening(input)
  must_reject_fold_opening(
    fold_item_test_root(input),
    encode_midgard_tx_input(input),
    bounded_collection_v1.ItemProofV1 { ..proof, frontier: [] },
  )
}

test input_no_idx_step_02_fold_rejects_substituted_input() fail {
  let input = fold_item_test_input()
  let substituted = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 8,
  }
  must_reject_fold_opening(
    fold_item_test_root(input),
    encode_midgard_tx_input(substituted),
    one_input_opening(input),
  )
}

/// Positive selector: the canonical spend-inputs preimage of the committed bad
/// transaction opens the hash carried from step-01, and the out-of-range input
/// it names is forwarded verbatim to step-03.
test input_no_idx_step_02_forwards_the_committed_out_of_range_input() {
  let producing = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let block = fixture.native_block_fixture_v1(
    input_no_idx_bad_tx(producing.tx_id),
  )
  run_step_02(
    block,
    block.compact.body.spend_inputs_hash,
    block.tx.body.inputs,
    0,
    MidgardTxInput { tx_id: producing.tx_id, output_index: 7 },
  )
}

/// Valid-block negative selector: every transaction of a valid block spends an
/// input its producer really has, so a prover who substitutes a fabricated
/// out-of-range input for the committed preimage cannot open the committed
/// spend-inputs commitment.
test input_no_idx_step_02_rejects_a_forged_inputs_preimage() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged = MidgardTxInput { tx_id: valid_block.tx_id, output_index: 7 }
  run_step_02(
    valid_block,
    valid_block.compact.body.spend_inputs_hash,
    [forged],
    0,
    forged,
  )
}

const published_reference_out_ref =
  OutputReference {
    transaction_id: #"abababababababababababababababababababababababababababababababab",
    output_index: 0,
  }

const published_wrong_pkh =
  #"99999999999999999999999999999999999999999999999999999999"

const published_wrong_policy =
  #"98989898989898989898989898989898989898989898989898989898"

const published_wrong_asset = #"7777"

const published_wrong_root =
  #"9797979797979797979797979797979797979797979797979797979797979797"

fn published_test_inputs() -> List<MidgardTxInput> {
  let (first, second, third) = three_fold_test_items()
  [first, second, third]
}

fn published_test_root(inputs: List<MidgardTxInput>) -> ByteArray {
  bounded_collection_v1.from_items(0, list.map(inputs, encode_midgard_tx_input))
}

fn published_test_datum(inputs: List<MidgardTxInput>) -> PublishedSpendInputsV1 {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  PublishedSpendInputsV1 {
    version: 1,
    computation_thread_policy_id: fixture.computation_thread_policy_id,
    computation_thread_asset_name: fixture.ct_token_asset_name(block),
    fraud_prover: fixture.fraud_prover_key_hash,
    verified_tx_inputs_hash: published_test_root(inputs),
    item_count: list.length(inputs),
    inputs,
  }
}

fn published_output(
  publication: PublishedSpendInputsV1,
  owner: VerificationKeyHash,
) -> Output {
  let publication_data: Data = publication
  Output {
    address: address.from_verification_key(owner),
    value: from_lovelace(2_000_000),
    datum: InlineDatum(publication_data),
    reference_script: None,
  }
}

fn published_reference_input(output: Output) -> Input {
  Input { output_reference: published_reference_out_ref, output }
}

fn run_complete_published(
  verified_tx_inputs_hash: ByteArray,
  publication_reference_inputs: List<Input>,
  publication_reference_input_index: Int,
  bad_inputs_index: Int,
  forwarded: MidgardTxInput,
) -> Bool {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let expected_output_state: Data = step_03.State {
    bad_input_tx_id: forwarded.tx_id,
    bad_input_output_index: forwarded.output_index,
  }
  let base_tx = fixture.continue_mid_step_l1_tx_v1(
    fixture.ct_token_asset_name(block),
    expected_output_state,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(Direct { verified_tx_inputs_hash }),
      },
    ),
    ct.Continue(
      CompletePublished {
        input_index: 0,
        output_index: 0,
        publication_reference_input_index,
        bad_inputs_index,
      },
    ),
    fixture.own_out_ref(),
    Transaction { ..base_tx, reference_inputs: publication_reference_inputs },
  )
}

fn run_valid_complete_published(inputs: List<MidgardTxInput>) -> Bool {
  let publication = published_test_datum(inputs)
  let last_index = list.length(inputs) - 1
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        published_output(publication, fixture.fraud_prover_key_hash),
      ),
    ],
    0,
    last_index,
    inputs |> list.expect_at(last_index),
  )
}

test input_no_idx_step_02_complete_published_forwards_identical_step_03_state() {
  run_valid_complete_published(published_test_inputs())
}

test input_no_idx_step_02_direct_and_complete_published_reach_identical_step_03_state() {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let last_index = list.length(inputs) - 1
  let forwarded = inputs |> list.expect_at(last_index)
  and {
    run_step_02(
      fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
      publication.verified_tx_inputs_hash,
      inputs,
      last_index,
      forwarded,
    ),
    run_valid_complete_published(inputs),
  }
}

test input_no_idx_step_02_complete_published_rejects_wrong_reference_index() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        published_output(publication, fixture.fraud_prover_key_hash),
      ),
    ],
    1,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_wrong_address() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let output = published_output(publication, fixture.fraud_prover_key_hash)
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        Output {
          ..output,
          address: address.from_script(fixture.step_script_hash),
        },
      ),
    ],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_stake_credential() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let output = published_output(publication, fixture.fraud_prover_key_hash)
  let staked_address = address.Address {
    payment_credential: address.VerificationKey(fixture.fraud_prover_key_hash),
    stake_credential: Some(
      address.Inline(address.VerificationKey(published_wrong_pkh)),
    ),
  }
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [published_reference_input(Output { ..output, address: staked_address })],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_datum_hash() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let output = published_output(publication, fixture.fraud_prover_key_hash)
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        Output { ..output, datum: DatumHash(published_wrong_root) },
      ),
    ],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_non_ada_value() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let output = published_output(publication, fixture.fraud_prover_key_hash)
  let value = from_lovelace(2_000_000)
    |> add(published_wrong_policy, #"01", 1)
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [published_reference_input(Output { ..output, value })],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_zero_ada() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let output = published_output(publication, fixture.fraud_prover_key_hash)
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [published_reference_input(Output { ..output, value: from_lovelace(0) })],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_malformed_inline_datum() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let output = published_output(publication, fixture.fraud_prover_key_hash)
  let malformed: Data = step_03.State {
    bad_input_tx_id: proof_fit_spend_input_tx_id,
    bad_input_output_index: 2,
  }
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        Output { ..output, datum: InlineDatum(malformed) },
      ),
    ],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_reference_script() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let output = published_output(publication, fixture.fraud_prover_key_hash)
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        Output { ..output, reference_script: Some(fixture.step_script_hash) },
      ),
    ],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_wrong_version() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let mutated = PublishedSpendInputsV1 { ..publication, version: 2 }
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        published_output(mutated, fixture.fraud_prover_key_hash),
      ),
    ],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_wrong_thread_policy() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let mutated = PublishedSpendInputsV1 {
    ..publication,
    computation_thread_policy_id: published_wrong_policy,
  }
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        published_output(mutated, fixture.fraud_prover_key_hash),
      ),
    ],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_wrong_thread_asset() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let mutated = PublishedSpendInputsV1 {
    ..publication,
    computation_thread_asset_name: published_wrong_asset,
  }
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        published_output(mutated, fixture.fraud_prover_key_hash),
      ),
    ],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_wrong_prover() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let mutated = PublishedSpendInputsV1 {
    ..publication,
    fraud_prover: published_wrong_pkh,
  }
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        published_output(mutated, fixture.fraud_prover_key_hash),
      ),
    ],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_cross_prover_replay() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let replayed = PublishedSpendInputsV1 {
    ..publication,
    fraud_prover: published_wrong_pkh,
  }
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [published_reference_input(published_output(replayed, published_wrong_pkh))],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_wrong_root() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let mutated = PublishedSpendInputsV1 {
    ..publication,
    verified_tx_inputs_hash: published_wrong_root,
  }
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        published_output(mutated, fixture.fraud_prover_key_hash),
      ),
    ],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_wrong_count() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let mutated = PublishedSpendInputsV1 { ..publication, item_count: 2 }
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        published_output(mutated, fixture.fraud_prover_key_hash),
      ),
    ],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_bad_index() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        published_output(publication, fixture.fraud_prover_key_hash),
      ),
    ],
    0,
    3,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_omission() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let (first, second, _third) = three_fold_test_items()
  let mutated = PublishedSpendInputsV1 {
    ..publication,
    inputs: [first, second],
  }
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        published_output(mutated, fixture.fraud_prover_key_hash),
      ),
    ],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_duplicate() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let (first, second, _third) = three_fold_test_items()
  let mutated = PublishedSpendInputsV1 {
    ..publication,
    inputs: [first, second, second],
  }
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        published_output(mutated, fixture.fraud_prover_key_hash),
      ),
    ],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_reorder() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let (first, second, third) = three_fold_test_items()
  let mutated = PublishedSpendInputsV1 {
    ..publication,
    inputs: [first, third, second],
  }
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        published_output(mutated, fixture.fraud_prover_key_hash),
      ),
    ],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_substitution() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let (first, second, _third) = three_fold_test_items()
  let substitute = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 99,
  }
  let mutated = PublishedSpendInputsV1 {
    ..publication,
    inputs: [first, second, substitute],
  }
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        published_output(mutated, fixture.fraud_prover_key_hash),
      ),
    ],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_rejects_trailing_input() fail {
  let inputs = published_test_inputs()
  let publication = published_test_datum(inputs)
  let trailing = MidgardTxInput {
    tx_id: proof_fit_spend_input_tx_id,
    output_index: 100,
  }
  let mutated = PublishedSpendInputsV1 {
    ..publication,
    inputs: list.concat(inputs, [trailing]),
  }
  run_complete_published(
    publication.verified_tx_inputs_hash,
    [
      published_reference_input(
        published_output(mutated, fixture.fraud_prover_key_hash),
      ),
    ],
    0,
    2,
    inputs |> list.expect_at(2),
  )
}

test input_no_idx_step_02_complete_published_evaluates_19_inputs() {
  run_valid_complete_published(spend_inputs_from(0, 19))
}

test input_no_idx_step_02_complete_published_evaluates_20_inputs() {
  run_valid_complete_published(spend_inputs_from(0, 20))
}

/// Proof evaluation only: this fixture does not assert that the separate
/// publication transaction fits the signed transaction-byte/min-Ada/fee cap.
test input_no_idx_step_02_complete_published_evaluates_296_inputs() {
  run_valid_complete_published(spend_inputs_from(0, 296))
}

// The typed-reference tier-2 proof was measured in the same guarded module
// run and does not provide a release-valid fallback for the first direct-list
// overflow:
//
// | published inputs | mem         | cpu            | verdict                 |
// | ---------------- | ----------- | -------------- | ----------------------- |
// | 19               |  15_968_649 |  7_088_401_249 | reserve fail / raw pass |
// | 20               |  16_517_107 |  7_316_522_425 | raw memory fail         |
// | 296              | 194_507_051 | 80_903_691_577 | raw memory + CPU fail   |

// ### Maximum/adversarial proof-fit (GOAL_SPEC.md §3.2 / §3.3)
//
// Measured against `docs/consensus-profile-v1.md` §10
// (`minSupportedTransactionExecutionMemoryUnits` 16_500_000,
// `minSupportedTransactionExecutionCpuUnits` 10_000_000_000) with the §3.3
// 20% reserve, i.e. 13_200_000 mem / 8_000_000_000 cpu:
//
// | spend inputs | mem         | cpu            | verdict                     |
// | ------------ | ----------- | -------------- | --------------------------- |
// | 19           |  12_839_676 |  5_674_387_119 | fits with reserve (largest) |
// | 20           |  13_366_688 |  5_896_030_186 | under raw limit, over 20%   |
// | 296          | 186_032_040 | 77_853_784_762 | 11.3x over the raw limit    |
//
// 296 is the Cardano *script-spend shape* boundary: it is the number of
// spend inputs that each carry a spend redeemer in
// `lib/midgard/fraud-proofs/native-tx.max-redeemers.test.ak`. It is not the
// universal field-0 input limit (which may be higher when inputs carry no
// redeemers). This Q13 proof must handle that 296-input Cardano shape, while
// the consensus profile admits `maxSpendInputCount` 16384, so the complete-
// item direct redeemer of §3.2 tier 1 does **not** reach the family maximum.
// The tier-1 constructor remains consensus-valid for every list; the public
// builder applies its 19-item release reserve policy and selects this ordered
// fold above it. These measurements establish tier-1 necessity; the tier-2
// typed-reference measurements above establish that it cannot replace the
// ordered fold as the release fallback.

const largest_fitting_spend_input_count = 19

const proof_fit_spend_input_tx_id =
  #"2020202020202020202020202020202020202020202020202020202020202020"

fn spend_inputs_from(index: Int, count: Int) -> List<MidgardTxInput> {
  if index >= count {
    []
  } else {
    [
      MidgardTxInput { tx_id: proof_fit_spend_input_tx_id, output_index: index },
      ..spend_inputs_from(index + 1, count)
    ]
  }
}

/// The largest complete spend-inputs preimage that clears both §3.3 execution
/// reserves in a single step-02 redeemer, carried directly with no chunking.
test input_no_idx_step_02_carries_the_largest_fitting_spend_inputs_preimage() {
  let inputs_preimage = spend_inputs_from(0, largest_fitting_spend_input_count)
  let verified_tx_inputs_hash = bounded_collection_v1.from_items(
    0,
    list.map(inputs_preimage, encode_midgard_tx_input),
  )
  let last_index = largest_fitting_spend_input_count - 1
  run_step_02(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    verified_tx_inputs_hash,
    inputs_preimage,
    last_index,
    MidgardTxInput {
      tx_id: proof_fit_spend_input_tx_id,
      output_index: last_index,
    },
  )
}

/// This is deliberately a successful raw-validator evaluation: 20 clears the
/// L1 hard cap but fails the §3.3 20% release reserve (13,366,688 >
/// 13,200,000 memory). The exact selector makes that policy boundary
/// reproducible without falsely claiming an on-chain rejection.
test input_no_idx_step_02_measures_first_over_reserve_spend_inputs_preimage() {
  let input_count = 20
  let inputs_preimage = spend_inputs_from(0, input_count)
  let verified_tx_inputs_hash = bounded_collection_v1.from_items(
    0,
    list.map(inputs_preimage, encode_midgard_tx_input),
  )
  let last_index = input_count - 1
  run_step_02(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    verified_tx_inputs_hash,
    inputs_preimage,
    last_index,
    MidgardTxInput {
      tx_id: proof_fit_spend_input_tx_id,
      output_index: last_index,
    },
  )
}

/// Replays the Cardano script-spend shape with 296 input/redeemer positions.
/// It is expected to execute over the L1 hard cap; the reported execution
/// units are the reproducible §3.2 necessity measurement, not an acceptance.
test input_no_idx_step_02_measures_cardano_296_script_spend_shape() {
  let input_count = 296
  let inputs_preimage = spend_inputs_from(0, input_count)
  let verified_tx_inputs_hash = bounded_collection_v1.from_items(
    0,
    list.map(inputs_preimage, encode_midgard_tx_input),
  )
  let last_index = input_count - 1
  run_step_02(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    verified_tx_inputs_hash,
    inputs_preimage,
    last_index,
    MidgardTxInput {
      tx_id: proof_fit_spend_input_tx_id,
      output_index: last_index,
    },
  )
}
