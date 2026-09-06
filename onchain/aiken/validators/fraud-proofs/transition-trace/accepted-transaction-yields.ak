use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use midgard/common/utils
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, BodyTxOpening, anchored_native_tx, anchored_native_tx_version,
  unanchored_validity_code_of,
}
use midgard/fraud_proofs/native_tx/components
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/fraud_proofs/transition_trace/final_yield.{
  OpenedOutputs, dispatched, one_step_witness,
}
use midgard/fraud_proofs/transition_trace/final_yield as delegation
use midgard/fraud_proofs/transition_trace/proof
use midgard/fraud_proofs/transition_trace/proof_carriage
use midgard/ledger_state
use midgard/transition_trace.{IndexedTraceProof, RootMembershipProof}
use midgard/validation_claim_v1

validator l2_open(dispatcher: ScriptHash) {
  withdraw(opened: OpenedOutputs, _credential: Credential, tx: Transaction) {
    let (_, fault, _) = dispatched(
      dispatcher,
      tx.inputs,
      tx.redeemers,
      tx.reference_inputs,
    )
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.kind == 0 && state.phase == 0
    expect [trace_data,
          _,
          source_data,
          inputs_data, outputs_data, _, _] = one_step_witness(fault, 4)
    expect trace_proof: IndexedTraceProof = trace_data
    expect source_membership: RootMembershipProof<ByteArray, ByteArray> = source_data
    expect inputs_preimage: ByteArray = inputs_data
    expect outputs_preimage: ByteArray = outputs_data
    expect Some(source_data) = cbor.deserialise(source_membership.value)
    expect source: ledger_state.L2TransactionSourceV1 = source_data
    let anchored = anchored_native_tx(
      BodyTxOpening { native_tx_compact_cbor: source.source.compact_cbor },
      BodyAnchor { tx_id: source_membership.key },
    )
    expect unanchored_validity_code_of(anchored) == 0
    let inputs = proof.door_body_field_items(
      anchored,
      0,
      inputs_preimage,
      components.decode_midgard_tx_input_cbor,
    )
    let outputs = proof.door_body_field_items(
      anchored,
      2,
      outputs_preimage,
      fn(bytes) { bytes },
    )
    and {
      cbor.serialise(source) == source_membership.value,
      source.tx_id == source_membership.key,
      anchored_native_tx_version(anchored) == 1,
      opened.spend_input_keys == list.map(
        inputs,
        components.encode_midgard_tx_input,
      ),
      opened.output_hashes == list.map(outputs, blake2b_256),
      delegation.advance(
        datum,
        args,
        tx.outputs,
        delegation.State {
          ..state,
          opened,
          phase: 1,
          current_root: trace_proof.value.pre_utxos_root,
        },
      ),
    }
  }

  else(_) {
    fail
  }
}

validator l2_replay(dispatcher: ScriptHash) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    let data = proof_carriage.read(
      state.proof_commitment,
      args.proof_ref_indices,
      tx.reference_inputs,
    )
    let (_, fault) = proof_carriage.fields(data)
    let (trace_data, spent_data, produced_data) = if state.kind == 0 {
      expect [trace_data,
            _, _, _, _, spent_data, produced_data] = one_step_witness(fault, 4)
      (trace_data, spent_data, produced_data)
    } else {
      expect state.kind == 1
      expect [trace_data,
            _, _, _, _, projected_data] = one_step_witness(fault, 3)
      let empty: Data = []
      let produced: Data = [projected_data]
      (trace_data, empty, produced)
    }
    expect trace_proof: IndexedTraceProof = trace_data
    if state.phase == 9 {
      let key = if state.kind == 0 {
        expect [_, _, source_data, _, _, _, _] = one_step_witness(fault, 4)
        expect source: RootMembershipProof<ByteArray, ByteArray> = source_data
        components.encode_midgard_tx_input(
          MidgardTxInput { tx_id: source.key, output_index: state.output_index },
        )
      } else {
        expect [_, _, source_data, _, _, _] = one_step_witness(fault, 3)
        expect
            source: RootMembershipProof<
              ledger_state.DepositId,
              ledger_state.DepositInfo,
            >
          = source_data
        proof.ledger_outref_key(source.key)
      }
      expect produced: List<proof.LedgerInsertWitness> = produced_data
      let witness = list.expect_at(produced, state.output_index)
      expect witness.key == key && witness.value == state.descriptor_cbor
      delegation.advance(
        datum,
        args,
        tx.outputs,
        delegation.State { ..state, phase: 4 },
      )
    } else if state.phase == 5 {
      state.current_root != trace_proof.value.post_utxos_root
    } else if state.phase == 1 {
      expect state.kind == 0
      expect spent: List<proof.LedgerDeleteWitness> = spent_data
      if state.input_index == list.length(state.opened.spend_input_keys) {
        expect state.input_index == list.length(spent)
        delegation.advance(
          datum,
          args,
          tx.outputs,
          delegation.State {
            ..state,
            phase: if state.opened.output_hashes == [] {
              5
            } else {
              2
            },
          },
        )
      } else {
        let key = list.expect_at(
          state.opened.spend_input_keys,
          state.input_index,
        )
        let witness = list.expect_at(spent, state.input_index)
        expect witness.key == key
        let root = proof.apply_delete_witness(state.current_root, witness)
        delegation.advance(
          datum,
          args,
          tx.outputs,
          delegation.State {
            ..state,
            input_index: state.input_index + 1,
            current_root: root,
          },
        )
      }
    } else {
      expect state.phase == 4
      expect produced: List<proof.LedgerInsertWitness> = produced_data
      let witness = list.expect_at(produced, state.output_index)
      let root = proof.apply_insert_witness(state.current_root, witness)
      let next_index = state.output_index + 1
      let done = next_index == list.length(state.opened.output_hashes)
      expect !done || next_index == list.length(produced)
      delegation.advance(
        datum,
        args,
        tx.outputs,
        delegation.State {
          ..state,
          phase: if done {
            5
          } else {
            2
          },
          output_index: next_index,
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

fn claim(
  dispatcher: ScriptHash,
  tx: Transaction,
) -> (ledger_state.HeaderV1, validation_claim_v1.ValidationClaimWitnessV1) {
  let (header_data, fault, _) = dispatched(
    dispatcher,
    tx.inputs,
    tx.redeemers,
    tx.reference_inputs,
  )
  expect builtin.unconstr_index(fault) == 9
  expect [witness] = builtin.unconstr_fields(fault)
  expect [claim_data, _] = builtin.unconstr_fields(witness)
  expect header: ledger_state.HeaderV1 = header_data
  expect claim: validation_claim_v1.ValidationClaimWitnessV1 = claim_data
  (header, claim)
}

validator claim_structure(dispatcher: ScriptHash) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (header, witness) = claim(dispatcher, tx)
    expect
      validation_claim_v1.committed_claim_structure_is_valid(header, witness)
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.kind == 2 && state.phase == 0
    delegation.advance(
      datum,
      args,
      tx.outputs,
      delegation.State { ..state, phase: 1 },
    )
  }

  else(_) {
    fail
  }
}

validator claim_source(dispatcher: ScriptHash) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (header, witness) = claim(dispatcher, tx)
    expect
      validation_claim_v1.committed_claim_source_is_authenticated(
        header,
        witness,
      )
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.kind == 2 && state.phase == 1
    delegation.advance(
      datum,
      args,
      tx.outputs,
      delegation.State { ..state, phase: 2 },
    )
  }

  else(_) {
    fail
  }
}

validator claim_endpoints(dispatcher: ScriptHash) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (header, witness) = claim(dispatcher, tx)
    expect
      validation_claim_v1.committed_claim_endpoints_and_source_are_valid(
        header,
        witness,
      )
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.kind == 2 && state.phase == 2
    delegation.advance(
      datum,
      args,
      tx.outputs,
      delegation.State { ..state, phase: 3 },
    )
  }

  else(_) {
    fail
  }
}
