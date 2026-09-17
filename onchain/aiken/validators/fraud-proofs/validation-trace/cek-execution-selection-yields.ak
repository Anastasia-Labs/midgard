use aiken/crypto.{ScriptHash}
use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use midgard/cek_proof_v1 as proof
use midgard/cek_selection_witness.{decode_selection_auxiliary}
use midgard/cek_selection_yield as selection
use midgard/validation_machine/cek.{
  authenticate_cek_execution_selection, verify_cek_execution_selection_successor,
}
use midgard/validation_machine/machine_types.{ValidationOneStepEvidenceV1}
use midgard/validation_machine/witness.{cek_witness_control_v1}
use midgard/validation_resolver_v1 as resolver

validator authenticate(dispatcher_script_hash: ScriptHash) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (_, action, _) = selection.unique_dispatch(
      dispatcher_script_hash,
      tx.inputs,
      tx.redeemers,
    )
    let auxiliary = decode_selection_auxiliary(action.auxiliary)
    let cek = cek_witness_control_v1(action.transition)
    and {
      authenticate_cek_execution_selection(
        auxiliary,
        cek.control,
        cek.execution_cursor,
      ),
      if selection.selection_language(action.auxiliary) == 0 {
        action.material_route == resolver.NoCekMaterial
      } else {
        resolver.cek_selection_envelope_cbor_v1(
          ValidationOneStepEvidenceV1 {
            transition: action.transition,
            auxiliary,
          },
        ) == Some(selection.route_envelope(action.material_route))
      },
    }
  }

  else(_) {
    fail
  }
}

validator successor(dispatcher_script_hash: ScriptHash) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (datum, action, _) = selection.unique_dispatch(
      dispatcher_script_hash,
      tx.inputs,
      tx.redeemers,
    )
    expect Some(state) = datum.data
    let auxiliary = decode_selection_auxiliary(action.auxiliary)
    let cek = cek_witness_control_v1(action.transition)
    and {
      if selection.selection_language(action.auxiliary) == 0 {
        True
      } else {
        expect Some(envelope) = proof.inspect_program_envelope_v1(
          selection.route_envelope(action.material_route),
        )
        and {
          action.envelope_facts.term_root == envelope.term_root,
          action.envelope_facts.node_count == envelope.node_count,
          action.envelope_facts.material_byte_length == envelope.material_byte_length,
        }
      },
      verify_cek_execution_selection_successor(
        state.resolution.pre_state,
        action.transition,
        auxiliary,
        cek.control,
        cek.execution_cursor,
        cek.completed_cpu,
        cek.completed_memory,
      ),
    }
  }

  else(_) {
    fail
  }
}

validator material_program(
  dispatcher_script_hash: ScriptHash,
  cek_program_material_script_hash: ScriptHash,
) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (_, action, _) = selection.unique_dispatch(
      dispatcher_script_hash,
      tx.inputs,
      tx.redeemers,
    )
    expect Some(entries) = resolver.material_entries_for_route(
      action.material_route,
      tx.reference_inputs,
      cek_program_material_script_hash,
    )
    let result = proof.verify_program_material_partition(
      action.envelope_facts.term_root,
      entries,
      action.material_facts,
    )
    result
  }

  else(_) {
    fail
  }
}

validator material_data(
  dispatcher_script_hash: ScriptHash,
  cek_program_material_script_hash: ScriptHash,
) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (_, action, _) = selection.unique_dispatch(
      dispatcher_script_hash,
      tx.inputs,
      tx.redeemers,
    )
    expect Some(entries) = resolver.material_entries_for_route(
      action.material_route,
      tx.reference_inputs,
      cek_program_material_script_hash,
    )
    let result = proof.verify_data_material_partition(
      entries,
      action.material_facts,
      action.envelope_facts.node_count,
      action.envelope_facts.material_byte_length,
    )
    result
  }

  else(_) {
    fail
  }
}
