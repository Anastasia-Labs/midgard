//// Builders for all-purposes walk tests: a transaction with chosen purpose
//// fields, its event pre-state and source map, and the field openings and
//// checkpoints a prover supplies at each step.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as library
use aiken/merkle_patricia_forestry/helpers.{nibble}
use aiken/primitive/bytearray
use cardano/transaction.{OutputReference}
use env
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, mint_field_index, outputs_field_index,
  required_observers_field_index, spend_inputs_field_index,
}
use midgard/fraud_proofs/first_fault_v1.{NetworkOrderContextV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact_v1, encode_forced_tx_compact_v1,
  encode_native_tx_witness_set_compact,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_tx_input, encode_midgard_tx_output,
}
use midgard/fraud_proofs/native_tx/preimages.{mint_policy_items}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1 as descriptor
use midgard/native_tx_machine_walk_v1 as walk
use midgard/rejection_reason_v1.{RejectionReasonV1}
use midgard/script_language_views_v1

// ## Identities

pub const a = #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1"

pub const b = #"b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2"

pub const c = #"c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3"

pub const key = #"d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4"

pub const spent_tx =
  #"5555555555555555555555555555555555555555555555555555555555555555"

pub const filler =
  #"1111111111111111111111111111111111111111111111111111111111111111"

pub const resume = #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

// ## Two-entry tries

pub fn common_nibbles(left: ByteArray, right: ByteArray, at: Int) -> Int {
  if nibble(left, at) == nibble(right, at) {
    common_nibbles(left, right, at + 1)
  } else {
    at
  }
}

/// The root of a trie holding the given entries (one or two), with each
/// entry's membership proof in entry order.
pub fn trie(
  entries: List<Pair<ByteArray, ByteArray>>,
) -> Pair<ByteArray, List<library.Proof>> {
  when entries is {
    [Pair(k, v)] ->
      Pair(library.insert(library.empty, k, v, []) |> library.root, [[]])
    [Pair(k1, v1), Pair(k2, v2)] -> {
      let p1 = blake2b_256(k1)
      let p2 = blake2b_256(k2)
      let skip = common_nibbles(p1, p2, 0)
      let proof1 = [library.Leaf { skip, key: p2, value: blake2b_256(v2) }]
      let proof2 = [library.Leaf { skip, key: p1, value: blake2b_256(v1) }]
      Pair(
        library.insert(
          library.insert(library.empty, k1, v1, []),
          k2,
          v2,
          proof2,
        )
          |> library.root,
        [proof1, proof2],
      )
    }
    _ -> fail @"one or two entries"
  }
}

pub fn proof_at(proofs: List<library.Proof>, index: Int) -> library.Proof {
  expect Some(proof) = list.at(proofs, index)
  proof
}

// ## Sources and ledger descriptors

pub fn source_of(hash: ByteArray, tag: Int) -> SourceV1 {
  SourceV1 {
    field_index: 6,
    item_index: 0,
    source_key: cbor.serialise(0),
    language_tag: tag,
    script_hash: hash,
    total_length: 4,
    item_commitment: filler,
  }
}

/// A completed source map over the given sources, with the witness for each.
pub fn source_map(
  sources: List<SourceV1>,
) -> Pair<source_index.StateV1, List<scan.ExecutionWitnessV1>> {
  let Pair(root, proofs) = trie(
    list.map(
      sources,
      fn(source) { Pair(source.script_hash, cbor.serialise(source)) },
    ),
  )
  Pair(
    source_index.StateV1 { root, count: list.length(sources) },
    list.indexed_map(
      sources,
      fn(i, source) {
        scan.ExecutionWitnessV1 { source, proof: proof_at(proofs, i) }
      },
    ),
  )
}

pub fn spent_input(output_index: Int) -> native_types.MidgardTxInput {
  native_types.MidgardTxInput { tx_id: spent_tx, output_index }
}

/// A ledger descriptor for a spent output locked by `credential`.
pub fn spent_descriptor(
  output_index: Int,
  credential: native_types.MidgardCredential,
) -> ByteArray {
  let summary = DataSummaryV1 { root: filler, cbor_length: 1, memory: 1 }
  descriptor.encode(
    descriptor.LedgerOutputCommitmentV1 {
      version: 1,
      output_index,
      total_length: 50,
      item_commitment: filler,
      address: when credential is {
        native_types.ScriptCredential(hash) -> bytearray.concat(#"70", hash)
        native_types.PubKeyCredential(hash) -> bytearray.concat(#"60", hash)
      },
      lovelace: 5000000,
      asset_count: 0,
      asset_frontier_commitment: filler,
      cardano_value_size: 5,
      reference_script_language: -1,
      reference_script_hash: #"",
      reference_script_total_length: 0,
      reference_script_item_commitment: #"",
      cardano_tx_out: summary,
      midgard_tx_out: summary,
      cardano_spend_datum: summary,
    },
  )
}

pub fn outref_key(output_index: Int) -> ByteArray {
  ledger_outref_key(OutputReference { transaction_id: spent_tx, output_index })
}

/// The event pre-root over the spent outputs (one or two), and each one's
/// descriptor and proof.
pub fn pre_state(
  credentials: List<native_types.MidgardCredential>,
) -> Pair<ByteArray, List<Pair<ByteArray, library.Proof>>> {
  let descriptors = list.indexed_map(
    credentials,
    fn(i, credential) { spent_descriptor(i, credential) },
  )
  let Pair(root, proofs) = trie(
    list.indexed_map(descriptors, fn(i, d) { Pair(outref_key(i), d) }),
  )
  Pair(
    root,
    list.indexed_map(descriptors, fn(i, d) { Pair(d, proof_at(proofs, i)) }),
  )
}

pub fn protected_script_output(hash: ByteArray) -> native_types.MidgardTxOutput {
  let sample = fixture.sample_native_output()
  native_types.MidgardTxOutput {
    ..sample,
    address: native_types.MidgardAddress {
      ..sample.address,
      protected: True,
      payment_credential: native_types.ScriptCredential(hash),
    },
  }
}

pub fn unprotected_script_output(
  hash: ByteArray,
) -> native_types.MidgardTxOutput {
  let sample = fixture.sample_native_output()
  native_types.MidgardTxOutput {
    ..sample,
    address: native_types.MidgardAddress {
      ..sample.address,
      payment_credential: native_types.ScriptCredential(hash),
    },
  }
}

pub fn mint_value(policies: List<ByteArray>) -> Data {
  if list.is_empty(policies) {
    builtin.list_data([])
  } else {
    builtin.map_data(
      list.map(
        policies,
        fn(hash) {
          Pair(
            builtin.b_data(hash),
            builtin.map_data([Pair(builtin.b_data(#"41"), builtin.i_data(1))]),
          )
        },
      ),
    )
  }
}

// ## The transaction under walk

pub type ShapeV1 {
  spends: List<native_types.MidgardCredential>,
  policies: List<ByteArray>,
  observers: List<ByteArray>,
  outputs: List<native_types.MidgardTxOutput>,
}

/// The transaction's committed script integrity hash: the honest one for a
/// language bitmap, or the base fixture's arbitrary value.
pub type IntegrityV1 {
  HonestFor(Int)
  Arbitrary
}

/// The verdict under challenge. Source kind 1 is a forced leaf; a rejection
/// reason makes it a wrongful-rejection subject.
pub type SubjectV1 {
  source_kind: Int,
  rejection_reason: Option<RejectionReasonV1>,
  integrity: IntegrityV1,
}

pub const accepted_normal =
  SubjectV1 { source_kind: 0, rejection_reason: None, integrity: Arbitrary }

pub type TxV1 {
  bound: BoundV1,
  witness_set_compact_cbor: ByteArray,
  spend_opening: FieldOpeningV1,
  mint_opening: FieldOpeningV1,
  observer_opening: FieldOpeningV1,
  output_opening: FieldOpeningV1,
  descriptors: List<Pair<ByteArray, library.Proof>>,
}

pub fn tx_of(shape: ShapeV1) -> TxV1 {
  subject_tx_of(shape, accepted_normal)
}

pub fn subject_tx_of(shape: ShapeV1, subject: SubjectV1) -> TxV1 {
  let inputs = list.indexed_map(shape.spends, fn(i, _) { spent_input(i) })
  let mint = mint_value(shape.policies)
  let tx = fixture.valid_native_tx_v1()
  let witness_set = fixture.witness_set_compact_v1(tx.witness_set)
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody {
        ..tx.body,
        inputs,
        mint,
        required_observers: shape.observers,
        outputs: shape.outputs,
        script_integrity_hash: when subject.integrity is {
          HonestFor(bitmap) ->
            script_language_views_v1.expected_script_integrity_hash(
              witness_set.redeemer_tx_wits_hash,
              bitmap,
            )
          Arbitrary -> tx.body.script_integrity_hash
        },
      },
    },
  )
  let compact_cbor = if subject.source_kind == 1 {
    let normal = decode_native_tx_compact_v1(block.compact_cbor)
    encode_forced_tx_compact_v1(
      native_types.ForcedTxCompact {
        body: normal.body,
        witness_set_hash: normal.witness_set_hash,
      },
    )
  } else {
    block.compact_cbor
  }
  let Pair(pre_root, descriptors) = if list.is_empty(shape.spends) {
    Pair(env.empty_merkle_tree_root, [])
  } else {
    pre_state(shape.spends)
  }
  TxV1 {
    bound: BoundV1 {
      subject: VerdictSubjectV1 {
        version: 1,
        direction: when subject.rejection_reason is {
          None -> 0
          Some(_) -> 1
        },
        source_kind: subject.source_kind,
        transaction_id: block.tx_id,
        source_key: if subject.source_kind == 1 {
          #"01"
        } else {
          #""
        },
        rejection_reason: subject.rejection_reason,
      },
      compact_cbor,
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: pre_root,
    },
    witness_set_compact_cbor: encode_native_tx_witness_set_compact(witness_set),
    spend_opening: fixture.inline_body_opening(
      compact_cbor,
      list.map(inputs, encode_midgard_tx_input),
    ),
    mint_opening: fixture.inline_body_opening(
      compact_cbor,
      mint_policy_items(mint),
    ),
    observer_opening: fixture.inline_body_opening(compact_cbor, shape.observers),
    output_opening: fixture.inline_body_opening(
      compact_cbor,
      list.map(shape.outputs, encode_midgard_tx_output),
    ),
    descriptors,
  }
}

// ## Checkpoints, as the prover re-derives them

pub const policy = fixture.test_certificate_policy_id

/// The checkpoint bytes `steps` items after the walking cursor's position.
pub fn walked(
  walking: cursor.CursorV1,
  opening: FieldOpeningV1,
  checkpoint: ByteArray,
  steps: Int,
) -> ByteArray {
  let Pair(view, position) = cursor.resume_body_at_v1(
    walking,
    walking.field_index,
    opening,
    checkpoint,
    [],
    policy,
  )
  walk_steps(view, position, steps)
}

pub fn walk_steps(view, position, steps: Int) -> ByteArray {
  if steps == 0 {
    walk.encode_field_walk_checkpoint(position)
  } else {
    let Pair(_, advanced) = walk.walk_next(view, position)
    walk_steps(view, advanced, steps - 1)
  }
}

/// The initial checkpoint bytes of a just-opened fixed-stride field.
pub fn opened_checkpoint(
  bound: BoundV1,
  field_index: Int,
  opening: FieldOpeningV1,
) -> ByteArray {
  let (_, checkpoint, _) = cursor.initial_with_completion_v1(
    bound,
    field_index,
    opening,
    [],
    policy,
  )
  checkpoint
}

/// Opens and fully certifies a variable field; returns the walking state and
/// its item-zero checkpoint bytes.
pub fn opened_variable(
  state: scan.StateV1,
  previous: Int,
  field_index: Int,
  opening: FieldOpeningV1,
) -> Pair<scan.StateV1, ByteArray> {
  let opened = scan.open_next_v1(
    state,
    previous,
    field_index,
    opening,
    [],
    policy,
  )
  let (_, grammar, _) = cursor.initial_with_completion_v1(
    state.cursor.bound,
    field_index,
    opening,
    [],
    policy,
  )
  let (_, semantic, _) = cursor.certify_with_completion_at_v1(
    opened.cursor,
    field_index,
    opening,
    grammar,
    1000,
    [],
    policy,
  )
  Pair(
    scan.certify_v1(opened, field_index, opening, grammar, 1000, [], policy),
    semantic,
  )
}

// ## Whole-walk drivers

pub type WitnessesV1 {
  spend_executions: List<Option<scan.ExecutionWitnessV1>>,
  mint: List<scan.ExecutionWitnessV1>,
  observers: List<scan.ExecutionWitnessV1>,
  receive: List<Option<scan.ExecutionWitnessV1>>,
}

pub fn spend_witnesses(
  tx: TxV1,
  executions: List<Option<scan.ExecutionWitnessV1>>,
) -> List<scan.SpendWitnessV1> {
  list.map2(
    tx.descriptors,
    executions,
    fn(descriptor_and_proof, execution) {
      let Pair(descriptor_cbor, proof) = descriptor_and_proof
      scan.SpendWitnessV1 { descriptor_cbor, proof, execution }
    },
  )
}

/// Walks every purpose field in one batch each and returns the final state.
pub fn walk_all(
  shape: ShapeV1,
  index: source_index.StateV1,
  probe: Option<ByteArray>,
  witnesses: WitnessesV1,
) -> scan.StateV1 {
  walk_tx(tx_of(shape), index, probe, witnesses)
}

/// Walks every purpose field of `tx` in one batch each.
pub fn walk_tx(
  tx: TxV1,
  index: source_index.StateV1,
  probe: Option<ByteArray>,
  witnesses: WitnessesV1,
) -> scan.StateV1 {
  let state = scan.initial_v1(
    BoundIndexV1 { bound: tx.bound, source_index: index },
    probe,
    tx.spend_opening,
    [],
    policy,
  )
  let state = if state.complete {
    state
  } else {
    scan.resolve_spends_v1(
      scan.take_spends_batch_v1(
        state,
        resume,
        tx.spend_opening,
        opened_checkpoint(tx.bound, spend_inputs_field_index, tx.spend_opening),
        list.length(tx.descriptors),
        [],
        policy,
      ),
      spend_witnesses(tx, witnesses.spend_executions),
    )
  }
  let Pair(state, checkpoint) = opened_variable(
    state,
    spend_inputs_field_index,
    mint_field_index,
    tx.mint_opening,
  )
  let state = if state.complete {
    state
  } else {
    scan.advance_mint_v1(
      state,
      tx.mint_opening,
      checkpoint,
      witnesses.mint,
      [],
      policy,
    )
  }
  let state = scan.open_next_v1(
    state,
    mint_field_index,
    required_observers_field_index,
    tx.observer_opening,
    [],
    policy,
  )
  let state = if state.complete {
    state
  } else {
    scan.advance_observers_v1(
      state,
      tx.observer_opening,
      opened_checkpoint(
        tx.bound,
        required_observers_field_index,
        tx.observer_opening,
      ),
      witnesses.observers,
      [],
      policy,
    )
  }
  let Pair(state, checkpoint) = opened_variable(
    state,
    required_observers_field_index,
    outputs_field_index,
    tx.output_opening,
  )
  if state.complete {
    state
  } else {
    scan.advance_receive_v1(
      state,
      tx.output_opening,
      checkpoint,
      witnesses.receive,
      [],
      policy,
    )
  }
}

pub fn facts_of(
  shape: ShapeV1,
  sources: List<SourceV1>,
  probe: Option<ByteArray>,
  pick: fn(List<scan.ExecutionWitnessV1>) -> WitnessesV1,
) -> scan.FactsV1 {
  let Pair(index, executions) = source_map(sources)
  scan.result_v1(walk_all(shape, index, probe, pick(executions))).facts
}

pub fn at(
  executions: List<scan.ExecutionWitnessV1>,
  i: Int,
) -> scan.ExecutionWitnessV1 {
  expect Some(execution) = list.at(executions, i)
  execution
}

pub const key_spend = native_types.PubKeyCredential(key)

pub fn no_purposes() -> ShapeV1 {
  ShapeV1 { spends: [key_spend], policies: [], observers: [], outputs: [] }
}

pub fn none(_executions) -> WitnessesV1 {
  WitnessesV1 { spend_executions: [None], mint: [], observers: [], receive: [] }
}
