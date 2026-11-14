//// Builders for `unusedScriptWitness` tests: a transaction with chosen purpose
//// fields and chosen field-6 script witnesses, the completed all-purposes walk
//// over it, and the witness walk to one field-6 index.

use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use env
use midgard/fraud_proofs/all_purposes_scan_fixture_v1.{
  ShapeV1, SubjectV1, TxV1, WitnessesV1, filler, mint_value, policy, pre_state,
  spent_input, walk_tx,
}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose_workers.{
  ProgressArgsV1,
}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/first_fault_v1.{NetworkOrderContextV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact_v1, encode_forced_tx_compact_v1,
  encode_native_tx_witness_set_compact,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_tx_input, encode_midgard_tx_output,
  encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/preimages.{mint_policy_items}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/fraud_proofs/unused_script_witness/walk.{EntryArgsV1, WalkStateV1}
use midgard/script_proof_v1

// ## Scripts

fn repeated(byte: Int, times: Int) -> ByteArray {
  if times == 0 {
    #""
  } else {
    bytearray.push(repeated(byte, times - 1), byte)
  }
}

/// A native signature script over a 28-byte key hash of `byte`s.
pub fn native_script(byte: Int) -> native_types.MidgardVersionedScript {
  native_types.MidgardVersionedScript {
    language: native_types.NativeCardanoScript,
    script_bytes: bytearray.concat(#"8200581c", repeated(byte, 28)),
  }
}

/// A Plutus V3 script body of `length` bytes of `byte`.
pub fn plutus_script(
  byte: Int,
  length: Int,
) -> native_types.MidgardVersionedScript {
  native_types.MidgardVersionedScript {
    language: native_types.PlutusV3Script,
    script_bytes: repeated(byte, length),
  }
}

pub fn hash_of(script: native_types.MidgardVersionedScript) -> ByteArray {
  script_proof_v1.versioned_script_hash(script)
}

/// The source record the source walk writes for the witness at `item_index`.
pub fn witness_source(
  item_index: Int,
  script: native_types.MidgardVersionedScript,
) -> SourceV1 {
  source.witness_source_v1(item_index, encode_midgard_versioned_script(script))
}

/// A resolved reference input's script source for `script`, as the source walk
/// writes it for a reference input (field 1).
pub fn reference_source(script: native_types.MidgardVersionedScript) -> SourceV1 {
  SourceV1 {
    field_index: 1,
    item_index: 0,
    source_key: cbor.serialise(0),
    language_tag: script_proof_v1.language_tag(script.language),
    script_hash: hash_of(script),
    total_length: bytearray.length(script.script_bytes),
    item_commitment: filler,
  }
}

// ## The transaction under walk

/// A transaction with the purposes of `shape` and the field-6 witnesses
/// `scripts`, under the verdict `subject`, with its field-6 opening.
pub fn case_tx(
  shape: ShapeV1,
  subject: SubjectV1,
  scripts: List<native_types.MidgardVersionedScript>,
) -> Pair<TxV1, FieldOpeningV1> {
  let inputs = list.indexed_map(shape.spends, fn(i, _) { spent_input(i) })
  let mint = mint_value(shape.policies)
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody {
        ..tx.body,
        inputs,
        mint,
        required_observers: shape.observers,
        outputs: shape.outputs,
      },
      witness_set: native_types.MidgardTransactionWitnessSet {
        ..tx.witness_set,
        script_tx_wits: scripts,
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
  Pair(
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
      witness_set_compact_cbor: encode_native_tx_witness_set_compact(
        block.witness_set_compact,
      ),
      spend_opening: fixture.inline_body_opening(
        compact_cbor,
        list.map(inputs, encode_midgard_tx_input),
      ),
      mint_opening: fixture.inline_body_opening(
        compact_cbor,
        mint_policy_items(mint),
      ),
      observer_opening: fixture.inline_body_opening(
        compact_cbor,
        shape.observers,
      ),
      output_opening: fixture.inline_body_opening(
        compact_cbor,
        list.map(shape.outputs, encode_midgard_tx_output),
      ),
      descriptors,
    },
    fixture.inline_witness_opening(
      compact_cbor,
      block.witness_set_compact,
      list.map(scripts, encode_midgard_versioned_script),
    ),
  )
}

// ## The walks

pub fn progress(
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  budget: Int,
) -> ProgressArgsV1 {
  ProgressArgsV1 {
    input_index: 0,
    output_index: 0,
    opening,
    checkpoint_bytes,
    budget,
  }
}

pub fn entry_args(opening: FieldOpeningV1, script_index: Int) -> EntryArgsV1 {
  EntryArgsV1 { input_index: 0, output_index: 0, script_index, opening }
}

/// Every stage of the witness walk to `script_index`: the opened state, the
/// grammar arguments, the certified state, the walk arguments (one batch of
/// `budget` items) and the walked state.
pub type StagesV1 {
  entry: EntryArgsV1,
  opened: WalkStateV1,
  grammar: ProgressArgsV1,
  certified: WalkStateV1,
  scan: ProgressArgsV1,
  walked: WalkStateV1,
}

pub fn stages(
  state: scan.StateV1,
  opening: FieldOpeningV1,
  script_index: Int,
  budget: Int,
) -> StagesV1 {
  let entry = entry_args(opening, script_index)
  let opened = walk.open_v1(state, entry, [], policy)
  let (_, grammar_bytes, _) = cursor.initial_with_completion_v1(
    opened.result.bound,
    6,
    opening,
    [],
    policy,
  )
  let grammar = progress(opening, grammar_bytes, 16)
  let certified = walk.certify_v1(opened, grammar, [], policy)
  let (_, scan_bytes, _) = cursor.certify_with_completion_at_v1(
    opened.cursor,
    6,
    opening,
    grammar_bytes,
    16,
    [],
    policy,
  )
  let scan_args = progress(opening, scan_bytes, budget)
  let walked = walk.advance_v1(certified, scan_args, [], policy)
  StagesV1 { entry, opened, grammar, certified, scan: scan_args, walked }
}

/// The completed all-purposes walk over `tx` with `probe`, then the witness
/// walk to `script_index` in one batch.
pub fn walked(
  tx: Pair<TxV1, FieldOpeningV1>,
  index: source_index.StateV1,
  probe: ByteArray,
  witnesses: WitnessesV1,
  script_index: Int,
) -> WalkStateV1 {
  let Pair(body, opening) = tx
  stages(
    walk_tx(body, index, Some(probe), witnesses),
    opening,
    script_index,
    16,
  ).walked
}
