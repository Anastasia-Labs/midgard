use aiken/cbor
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/primitive/bytearray
use cardano/address.{Credential}
use cardano/assets.{PolicyId}
use cardano/transaction.{Transaction}
use midgard/cbor_deserialise
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, BodyTxOpening, FieldOpeningV1, NativeTxAnchorV1,
  anchored_field_walk, anchored_native_tx, anchored_native_tx_version,
  outputs_field_index, spend_inputs_field_index, unanchored_validity_code_of,
}
use midgard/fraud_proofs/field_opening_v1 as opening
use midgard/fraud_proofs/native_tx/components
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/fraud_proofs/transition_trace/final_yield.{
  WalkStep, dispatched, one_step_witness,
}
use midgard/fraud_proofs/transition_trace/final_yield as delegation
use midgard/fraud_proofs/transition_trace/output_pipeline as pipeline
use midgard/fraud_proofs/transition_trace/proof
use midgard/fraud_proofs/transition_trace/proof_carriage
use midgard/ledger_state
use midgard/native_tx_field_access_v1.{FieldCarriageV1}
use midgard/native_tx_machine_walk_v1 as walk
use midgard/transition_trace.{IndexedTraceProof, RootMembershipProof}

/// Spend inputs deleted per phase-1 step. A compiled constant, so the
/// successor of a phase-1 state is unique: each step deletes exactly
/// `min(delete_batch, remaining)` inputs.
pub const delete_batch = 1

/// Output envelopes certified per phase-10 step.
pub const outputs_grammar_batch = 32

/// The longest output a phase-11 step replays whole: an address of 57 bytes
/// and a Value of lovelace alone, its coin in a 9-byte head.
pub const output_step_bytes = 73

/// Whether the phase-11 step replays `item` whole: at most
/// `output_step_bytes` long and of the shape `{0: address, 1: [coin, {}]}`,
/// with no inline datum, no reference script and no native assets, so its
/// replay cost is bounded by its length. Every other output leaves phase 11
/// for the staged phases 2, 7, 8, 3 and 4. The answer depends on the bytes
/// alone, so every output has exactly one path.
///
/// In canonical bytes the address is a byte string with a one-byte length
/// head, `58 n`, so the coin's head stands at `6 + n` and the asset map's head
/// right after the coin, whose head is 1, 2, 3, 5 or 9 bytes long; `a0` is the
/// empty map.
pub fn output_step_is_whole(item: ByteArray) -> Bool {
  if bytearray.length(item) > output_step_bytes || bytearray.at(item, 0) != 0xa2 {
    False
  } else {
    let coin = 6 + bytearray.at(item, 3)
    let head = bytearray.at(item, coin)
    let assets = coin + if head < 24 {
      1
    } else {
      bytearray.at(#"02030509", head - 24)
    }
    bytearray.at(item, assets) == 0xa0
  }
}

/// One phase-1 step: the resumed field-0 walk and exactly one delete witness
/// per input it visits, in field order.
pub type DeleteStep {
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
  witnesses: List<proof.LedgerDeleteWitness>,
}

/// One phase-11 step: the resumed outputs walk and, for an output replayed
/// whole, its ledger insert witness. A staged output reads only `walk`.
pub type OutputStep {
  walk: WalkStep,
  insert: proof.LedgerInsertWitness,
}

/// Insert the output at `state.output_index` into the ledger. The witness is
/// keyed by the produced output reference and carries `descriptor`.
fn insert_output(
  state: delegation.State,
  witness: proof.LedgerInsertWitness,
  descriptor: ByteArray,
) -> ByteArray {
  expect
    witness.key == components.encode_midgard_tx_input(
      MidgardTxInput { tx_id: state.tx_id, output_index: state.output_index },
    )
  expect witness.value == descriptor
  proof.apply_insert_witness(state.current_root, witness)
}

fn body_anchor(state: delegation.State) -> NativeTxAnchorV1 {
  BodyAnchor { tx_id: state.tx_id, source_kind: 0 }
}

/// Phase 0: authenticate the committed source leaf, anchor the transaction and
/// open its spend-inputs walk at the derived start.
validator l2_open(
  dispatcher: ScriptHash,
  field_preimage_certificate_policy_id: PolicyId,
) {
  withdraw(carriage: FieldCarriageV1, _credential: Credential, tx: Transaction) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.kind == 0 && state.phase == 0
    let (_, fault) = proof_carriage.fields(
      proof_carriage.read(
        state.proof_commitment,
        args.proof_ref_indices,
        tx.reference_inputs,
      ),
    )
    expect [trace_data, _, source_data] = one_step_witness(fault, 4)
    expect trace_proof: IndexedTraceProof = trace_data
    expect source_membership: RootMembershipProof<ByteArray, ByteArray> = source_data
    let source_data <- cbor_deserialise.expect_deserialise(
      source_membership.value,
    )
    expect source: ledger_state.L2TransactionSourceV1 = source_data
    let anchored = anchored_native_tx(
      BodyTxOpening { native_tx_compact_cbor: source.source.compact_cbor },
      BodyAnchor { tx_id: source_membership.key, source_kind: 0 },
    )
    expect unanchored_validity_code_of(anchored) == 0
    let Pair(_, start) = anchored_field_walk(
      anchored,
      spend_inputs_field_index,
      carriage,
      tx.reference_inputs,
      field_preimage_certificate_policy_id,
    )
    and {
      cbor.serialise(source) == source_membership.value,
      source.tx_id == source_membership.key,
      anchored_native_tx_version(anchored) == 1,
      delegation.advance(
        datum,
        args,
        tx.outputs,
        delegation.State {
          ..state,
          phase: if walk.walk_is_complete(start) {
            10
          } else {
            1
          },
          tx_id: source_membership.key,
          inputs_walk: walk.field_walk_checkpoint_hash(start),
          current_root: trace_proof.value.pre_utxos_root,
        },
      ),
    }
  }

  else(_) {
    fail
  }
}

/// Phase 1: delete the next `min(delete_batch, remaining)` spend inputs, each
/// keyed by the walked field-0 item, then leave for phase 10 once the walk is
/// complete.
validator l2_delete(
  dispatcher: ScriptHash,
  field_preimage_certificate_policy_id: PolicyId,
) {
  withdraw(step: DeleteStep, _credential: Credential, tx: Transaction) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.kind == 0 && state.phase == 1
    let Pair(view, prior) = opening.resume_opened_field_walk(
      step.opening,
      body_anchor(state),
      spend_inputs_field_index,
      state.inputs_walk,
      step.checkpoint_bytes,
      tx.reference_inputs,
      field_preimage_certificate_policy_id,
    )
    let Pair(Pair(unused, root), next) = walk.walk_fold(
      view,
      prior,
      delete_batch,
      Pair(step.witnesses, state.current_root),
      fn(acc, _index, item) {
        let Pair(witnesses, root) = acc
        expect [witness, ..rest] = witnesses
        expect
          witness.key == components.encode_midgard_tx_input(
            components.decode_midgard_tx_input_cbor(item),
          )
        Pair(rest, proof.apply_delete_witness(root, witness))
      },
    )
    expect unused == []
    delegation.advance(
      datum,
      args,
      tx.outputs,
      delegation.State {
        ..state,
        phase: if walk.walk_is_complete(next) {
          10
        } else {
          1
        },
        inputs_walk: walk.field_walk_checkpoint_hash(next),
        current_root: root,
      },
    )
  }

  else(_) {
    fail
  }
}

/// Phase 10: certify the outputs field's envelopes, `outputs_grammar_batch`
/// per step. The step that completes certification opens the certified walk
/// and records the output count it authenticates.
validator l2_outputs(
  dispatcher: ScriptHash,
  field_preimage_certificate_policy_id: PolicyId,
) {
  withdraw(step: WalkStep, _credential: Credential, tx: Transaction) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.kind == 0 && state.phase == 10
    let anchor = body_anchor(state)
    let Pair(view, prior) = if state.outputs_walk == #"" {
      expect step.checkpoint_bytes == #""
      opening.opened_field_grammar_certification(
        step.opening,
        anchor,
        outputs_field_index,
        tx.reference_inputs,
        field_preimage_certificate_policy_id,
      )
    } else {
      opening.resume_opened_field_grammar_certification(
        step.opening,
        anchor,
        outputs_field_index,
        state.outputs_walk,
        step.checkpoint_bytes,
        tx.reference_inputs,
        field_preimage_certificate_policy_id,
      )
    }
    let next = walk.certify_field_grammar(view, prior, outputs_grammar_batch)
    let successor = if walk.field_grammar_is_complete(next) {
      let Pair(_, start) = opening.opened_certified_field_walk_from_grammar(
        step.opening,
        anchor,
        outputs_field_index,
        walk.field_grammar_checkpoint_hash(next),
        walk.encode_field_grammar_checkpoint(next),
        tx.reference_inputs,
        field_preimage_certificate_policy_id,
      )
      let count = walk.walk_remaining(start)
      delegation.State {
        ..state,
        phase: if count == 0 {
          5
        } else {
          11
        },
        outputs_walk: walk.field_walk_checkpoint_hash(start),
        output_count: count,
      }
    } else {
      delegation.State {
        ..state,
        outputs_walk: walk.field_grammar_checkpoint_hash(next),
      }
    }
    delegation.advance(datum, args, tx.outputs, successor)
  }

  else(_) {
    fail
  }
}

/// Phase 11: take the next output off the certified walk, which must stand
/// at `output_index`, so the insert key built from the same index names the
/// output whose bytes were walked.
///
/// An output that `output_step_is_whole` admits is replayed whole here and
/// the step moves on to the next output (or phase 5 after the last). Its scan,
/// value fold, summaries and descriptor are checked by the two withdraw-zero
/// verifiers of `output_step`, whose exact credentials are parameters: the
/// descriptor verifier's claim must name `output_index` and the walked bytes,
/// the summary verifier's claim must repeat the same bytes, scan facts and
/// summaries, and the descriptor it checked is the value inserted. Any other
/// output records its hash for the staged phases, which read its bytes from
/// published chunks, and moves to phase 2.
validator l2_output_step(
  dispatcher: ScriptHash,
  field_preimage_certificate_policy_id: PolicyId,
  descriptor_verifier: ScriptHash,
  summary_verifier: ScriptHash,
) {
  withdraw(step: OutputStep, _credential: Credential, tx: Transaction) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.kind == 0 && state.phase == 11
    let Pair(view, prior) = opening.resume_opened_field_walk(
      step.walk.opening,
      body_anchor(state),
      outputs_field_index,
      state.outputs_walk,
      step.walk.checkpoint_bytes,
      tx.reference_inputs,
      field_preimage_certificate_policy_id,
    )
    expect walk.walk_next_item_index(prior) == state.output_index
    let Pair(item, next) = walk.walk_next(view, prior)
    let outputs_walk = walk.field_walk_checkpoint_hash(next)
    let successor = if !output_step_is_whole(item) {
      delegation.State {
        ..state,
        phase: 2,
        outputs_walk,
        output_hash: blake2b_256(item),
      }
    } else {
      let descriptor = pipeline.verified_descriptor(
        tx,
        descriptor_verifier,
        summary_verifier,
        state.output_index,
        item,
      )
      let next_index = state.output_index + 1
      delegation.State {
        ..state,
        phase: if next_index == state.output_count {
          5
        } else {
          11
        },
        outputs_walk,
        output_index: next_index,
        current_root: insert_output(state, step.insert, descriptor),
      }
    }
    delegation.advance(datum, args, tx.outputs, successor)
  }

  else(_) {
    fail
  }
}

/// Phases 9 (deposit only), 4 and 5. A kind-0 insert carries its one ledger
/// witness in this redeemer; its key is the produced output reference and its
/// value the descriptor phase 3 assembled.
validator l2_replay(dispatcher: ScriptHash) {
  withdraw(redeemer: Data, _credential: Credential, tx: Transaction) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    let committed = fn() {
      let data = proof_carriage.read(
        state.proof_commitment,
        args.proof_ref_indices,
        tx.reference_inputs,
      )
      let (_, fault) = proof_carriage.fields(data)
      fault
    }
    if state.phase == 5 {
      let fault = committed()
      let tag = if state.kind == 0 {
        4
      } else {
        expect state.kind == 1
        3
      }
      expect [trace_data, ..] = one_step_witness(fault, tag)
      expect trace_proof: IndexedTraceProof = trace_data
      state.current_root != trace_proof.value.post_utxos_root
    } else if state.phase == 9 {
      expect state.kind == 1
      expect [_,
            _, source_data, projected_data] = one_step_witness(committed(), 3)
      expect source: RootMembershipProof<ledger_state.DepositId, ByteArray> = source_data
      expect witness: proof.LedgerInsertWitness = projected_data
      expect
        witness.key == proof.ledger_outref_key(source.key) && witness.value == state.descriptor_cbor
      delegation.advance(
        datum,
        args,
        tx.outputs,
        delegation.State { ..state, phase: 4 },
      )
    } else {
      expect state.phase == 4
      let root = if state.kind == 0 {
        expect witness: proof.LedgerInsertWitness = redeemer
        insert_output(state, witness, state.descriptor_cbor)
      } else {
        expect state.kind == 1
        expect [_, _, _, projected_data] = one_step_witness(committed(), 3)
        expect witness: proof.LedgerInsertWitness = projected_data
        proof.apply_insert_witness(state.current_root, witness)
      }
      let next_index = state.output_index + 1
      delegation.advance(
        datum,
        args,
        tx.outputs,
        delegation.State {
          ..state,
          phase: if next_index == state.output_count {
            5
          } else {
            11
          },
          output_index: next_index,
          output_hash: #"",
          current_root: root,
          summaries: delegation.OutputSummaries { summaries: [] },
          scan_cbor: #"",
          value_cbor: #"",
          value_start: 0,
          value_end: 0,
          value_summary: None,
          descriptor_cbor: #"",
        },
      )
    }
  }

  else(_) {
    fail
  }
}
