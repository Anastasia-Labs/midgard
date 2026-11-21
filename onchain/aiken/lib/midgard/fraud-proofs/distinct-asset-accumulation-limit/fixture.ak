//// Builders for `distinctAssetAccumulationLimit` tests: a transaction with
//// chosen spent-input asset counts, reference inputs, output values and mint,
//// its event pre-state, and a driver that runs the whole derivation the way
//// the prover pages it.

use aiken/builtin
use aiken/collection/list
use aiken/primitive/bytearray
use env
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/fraud_proofs/all_purposes_scan_fixture_v1.{
  filler, opened_checkpoint, outref_key, policy, proof_at, resume, spent_input,
  trie,
}
use midgard/fraud_proofs/direct_script_derivation/cursor.{CursorV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/fold.{
  FieldDone, FieldStepV1, FieldWalked, InputWitnessV1, InputsCross, InputsStepV1,
  InputsWalked, TerminalV1, WalkV1,
}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, mint_field_index, outputs_field_index,
  spend_inputs_field_index,
}
use midgard/fraud_proofs/first_fault_v1.{NetworkOrderContextV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact_v1, encode_forced_tx_compact_v1,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_tx_input, encode_midgard_tx_output,
}
use midgard/fraud_proofs/native_tx/preimages.{mint_policy_items}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/ledger_output_commitment_v1 as descriptor
use midgard/native_tx_machine_walk_v1 as walk
use midgard/rejection_reason_v1.{RejectionReasonV1}

// ## Values

/// The `i`-th test policy; policies sort by `i`.
pub fn policy_of(i: Int) -> ByteArray {
  bytearray.push(#"e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0", i)
}

fn asset_name(j: Int) -> ByteArray {
  bytearray.from_int_big_endian(j, 2)
}

/// One output holding `counts[k]` assets of policy `k`.
pub fn output_of(counts: List<Int>) -> native_types.MidgardTxOutput {
  let sample = fixture.sample_native_output()
  native_types.MidgardTxOutput {
    ..sample,
    value: native_types.MidgardValue {
      lovelace: 5_000_000,
      assets: list.indexed_foldr(
        counts,
        [],
        fn(k, count, rest) {
          list.foldr(
            list.range(0, count - 1),
            rest,
            fn(j, acc) {
              [Pair(bytearray.concat(policy_of(k), asset_name(j)), 1), ..acc]
            },
          )
        },
      ),
    },
  }
}

/// A mint of `counts[k]` assets under policy `k`.
pub fn mint_of(counts: List<Int>) -> Data {
  if list.is_empty(counts) {
    builtin.list_data([])
  } else {
    builtin.map_data(
      list.indexed_map(
        counts,
        fn(k, count) {
          Pair(
            builtin.b_data(policy_of(k)),
            builtin.map_data(
              list.map(
                list.range(0, count - 1),
                fn(j) { Pair(builtin.b_data(asset_name(j)), builtin.i_data(1)) },
              ),
            ),
          )
        },
      ),
    )
  }
}

/// A ledger descriptor for spent output `output_index` holding `asset_count`
/// asset entries.
pub fn spent_descriptor(output_index: Int, asset_count: Int) -> ByteArray {
  let summary = DataSummaryV1 { root: filler, cbor_length: 1, memory: 1 }
  descriptor.encode(
    descriptor.LedgerOutputCommitmentV1 {
      version: 1,
      output_index,
      total_length: 50,
      item_commitment: filler,
      address: #"60d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4",
      lovelace: 5000000,
      asset_count,
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

// ## The transaction under derivation

/// `spends` holds each spent input's asset count (one or two inputs, spent
/// from `spent_tx` at indexes 0 and 1); `outputs` each output's per-policy
/// asset counts; `mint` the per-policy minted asset counts.
pub type ShapeV1 {
  spends: List<Int>,
  references: List<native_types.MidgardTxInput>,
  outputs: List<List<Int>>,
  mint: List<Int>,
}

/// Source kind 1 is a forced leaf; a rejection reason makes it a
/// wrongful-rejection subject.
pub type SubjectV1 {
  source_kind: Int,
  rejection_reason: Option<RejectionReasonV1>,
}

pub const accepted_normal = SubjectV1 { source_kind: 0, rejection_reason: None }

pub type TxV1 {
  bound: BoundV1,
  spend_opening: FieldOpeningV1,
  reference_opening: FieldOpeningV1,
  output_opening: FieldOpeningV1,
  mint_opening: FieldOpeningV1,
  spend_items: List<ByteArray>,
  reference_items: List<ByteArray>,
  witnesses: List<InputWitnessV1>,
}

pub fn subject_tx_of(shape: ShapeV1, subject: SubjectV1) -> TxV1 {
  let inputs = list.indexed_map(shape.spends, fn(i, _) { spent_input(i) })
  let outputs = list.map(shape.outputs, output_of)
  let mint = mint_of(shape.mint)
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody {
        ..tx.body,
        inputs,
        reference_inputs: shape.references,
        outputs,
        mint,
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
  let descriptors = list.indexed_map(
    shape.spends,
    fn(i, count) { spent_descriptor(i, count) },
  )
  let Pair(pre_root, proofs) = if list.is_empty(shape.spends) {
    Pair(env.empty_merkle_tree_root, [])
  } else {
    trie(list.indexed_map(descriptors, fn(i, d) { Pair(outref_key(i), d) }))
  }
  let spend_items = list.map(inputs, encode_midgard_tx_input)
  let reference_items = list.map(shape.references, encode_midgard_tx_input)
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
    spend_opening: fixture.inline_body_opening(compact_cbor, spend_items),
    reference_opening: fixture.inline_body_opening(
      compact_cbor,
      reference_items,
    ),
    output_opening: fixture.inline_body_opening(
      compact_cbor,
      list.map(outputs, encode_midgard_tx_output),
    ),
    mint_opening: fixture.inline_body_opening(
      compact_cbor,
      mint_policy_items(mint),
    ),
    spend_items,
    reference_items,
    witnesses: list.indexed_map(
      descriptors,
      fn(i, d) {
        InputWitnessV1 { descriptor_cbor: d, proof: proof_at(proofs, i) }
      },
    ),
  }
}

pub fn tx_of(shape: ShapeV1) -> TxV1 {
  subject_tx_of(shape, accepted_normal)
}

// ## Checkpoints, as the prover re-derives them

fn seek(
  view,
  position: walk.FieldWalkCheckpointV1,
  hash: ByteArray,
) -> ByteArray {
  if walk.field_walk_checkpoint_hash(position) == hash {
    walk.encode_field_walk_checkpoint(position)
  } else {
    let Pair(_, advanced) = walk.walk_next(view, position)
    seek(view, advanced, hash)
  }
}

/// The checkpoint bytes `after` commits, found by walking on from `before`'s
/// checkpoint `bytes`.
pub fn advanced_checkpoint(
  before: CursorV1,
  after: CursorV1,
  opening: FieldOpeningV1,
  bytes: ByteArray,
) -> ByteArray {
  let Pair(view, position) = cursor.resume_body_at_v1(
    before,
    before.field_index,
    opening,
    bytes,
    [],
    policy,
  )
  seek(view, position, after.checkpoint_hash)
}

/// Opens and fully certifies a variable field; returns the walking state and
/// its item-zero checkpoint bytes.
pub fn opened_variable(
  state: WalkV1,
  previous: Int,
  field_index: Int,
  opening: FieldOpeningV1,
) -> Pair<WalkV1, ByteArray> {
  let opened = fold.open_next_v1(
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
    fold.certify_v1(opened, field_index, opening, grammar, 1000, [], policy),
    semantic,
  )
}

/// The number of reference items strictly below `item`: its rank.
pub fn rank_of(tx: TxV1, item: ByteArray) -> Int {
  list.count(
    tx.reference_items,
    fn(reference) { bytearray.compare(reference, item) == Less },
  )
}

// ## Whole-derivation driver

/// Pages: spent inputs per take, output work budget, mint policies per step.
pub type PagesV1 {
  inputs: Int,
  outputs: Int,
  mint: Int,
}

pub const one_page = PagesV1 { inputs: 2, outputs: 100_000, mint: 100 }

fn inputs_pages(
  tx: TxV1,
  state: WalkV1,
  bytes: ByteArray,
  witnesses: List<InputWitnessV1>,
  page: Int,
) -> InputsStepV1 {
  if state.complete {
    InputsWalked(state)
  } else {
    let pending = fold.take_inputs_v1(
      state,
      resume,
      tx.spend_opening,
      bytes,
      if page < list.length(witnesses) {
        page
      } else {
        list.length(witnesses)
      },
      [],
      policy,
    )
    when
      fold.resolve_inputs_v1(
        pending,
        list.take(witnesses, list.length(pending.pending)),
      )
    is {
      InputsCross(rank) -> InputsCross(rank)
      InputsWalked(walked) ->
        inputs_pages(
          tx,
          walked,
          advanced_checkpoint(
            state.cursor,
            walked.cursor,
            tx.spend_opening,
            bytes,
          ),
          list.drop(witnesses, list.length(pending.pending)),
          page,
        )
    }
  }
}

fn field_pages(
  tx: TxV1,
  opening: FieldOpeningV1,
  state: WalkV1,
  bytes: ByteArray,
  page: Int,
  is_mint: Bool,
) -> FieldStepV1 {
  if state.complete {
    FieldWalked(state)
  } else {
    let step = if is_mint {
      fold.advance_mint_v1(state, opening, bytes, page, [], policy)
    } else {
      fold.advance_outputs_v1(state, opening, bytes, page, [], policy)
    }
    when step is {
      FieldDone(terminal) -> FieldDone(terminal)
      FieldWalked(walked) ->
        field_pages(
          tx,
          opening,
          walked,
          advanced_checkpoint(state.cursor, walked.cursor, opening, bytes),
          page,
          is_mint,
        )
    }
  }
}

/// Runs the whole derivation in the given pages, ranking an input crossing
/// at `rank_of`.
pub fn derive_paged(tx: TxV1, pages: PagesV1) -> TerminalV1 {
  let state = fold.initial_v1(tx.bound, tx.spend_opening, [], policy)
  when
    inputs_pages(
      tx,
      state,
      opened_checkpoint(tx.bound, spend_inputs_field_index, tx.spend_opening),
      tx.witnesses,
      pages.inputs,
    )
  is {
    InputsCross(rank) ->
      fold.rank_v1(
        rank,
        tx.reference_opening,
        rank_of(tx, rank.input_item),
        [],
        policy,
      )
    InputsWalked(state) -> {
      let Pair(state, bytes) = opened_variable(
        state,
        spend_inputs_field_index,
        outputs_field_index,
        tx.output_opening,
      )
      when
        field_pages(tx, tx.output_opening, state, bytes, pages.outputs, False)
      is {
        FieldDone(terminal) -> terminal
        FieldWalked(state) -> {
          let Pair(state, bytes) = opened_variable(
            state,
            outputs_field_index,
            mint_field_index,
            tx.mint_opening,
          )
          if state.complete {
            TerminalV1 { bound: state.cursor.bound, crossing: None }
          } else {
            expect FieldDone(terminal) = field_pages(
              tx,
              tx.mint_opening,
              state,
              bytes,
              pages.mint,
              True,
            )
            terminal
          }
        }
      }
    }
  }
}

pub fn derive(tx: TxV1) -> TerminalV1 {
  derive_paged(tx, one_page)
}
