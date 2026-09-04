use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/address.{Address, Credential}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/cek_proof_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/ledger_state.{CekProgramMaterialDatumV1}
use midgard/native_script_scan_v1
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_resolution_v1
use midgard/validation_trace_v1

pub const phase_a_script_preconditions_semantic_resolver_count: Int = 2

pub const script_sources_semantic_resolver_count: Int = 29

pub type Datum =
  ct.StepDatum<validation_resolution_v1.ValidationResolutionStateV1>

/// Complete CEK material is resolver evidence, never a machine auxiliary.
pub type CekMaterialRouteV1 {
  NoCekMaterial
  DirectCekMaterial { envelope_cbor: ByteArray, sidecar_cbor: ByteArray }
  SinglePublicationCekMaterial {
    envelope_cbor: ByteArray,
    reference_input_index: Int,
  }
  MinimumMultiOutputCekMaterial {
    envelope_cbor: ByteArray,
    reference_input_indices: List<Int>,
  }
  IncrementalCekMaterial { program_envelope_hash: ByteArray }
}

pub type CekSinglePublicationDatumV1 {
  version: Int,
  program_envelope_hash: ByteArray,
  sidecar_cbor: ByteArray,
}

fn cek_selection_envelope_cbor_v1(
  evidence: vm_machine_types.ValidationOneStepEvidenceV1,
) -> Option<ByteArray> {
  when evidence.auxiliary is {
    vm_machine_types.NativeExecutionScanWitness {
      language_tag,
      script_total_length,
      first_chunk_proof,
      ..
    } ->
      if ( language_tag == 3 || language_tag == 128 ) && first_chunk_proof.chunk_index == 0 && first_chunk_proof.total_length == script_total_length {
        when
          native_script_scan_v1.versioned_script_header_v1(
            first_chunk_proof.chunk,
            script_total_length,
          )
        is {
          Some(header) ->
            if header.language_tag == language_tag && header.payload_offset >= 0 && header.payload_length > 0 && header.payload_offset + header.payload_length <= bytearray.length(
              first_chunk_proof.chunk,
            ) {
              Some(
                bytearray.slice(
                  first_chunk_proof.chunk,
                  header.payload_offset,
                  header.payload_offset + header.payload_length - 1,
                ),
              )
            } else {
              None
            }
          None -> None
        }
      } else {
        None
      }
    _ -> None
  }
}

fn cek_envelope_hash_v1(envelope_cbor: ByteArray) -> Option<ByteArray> {
  when cek_proof_v1.inspect_program_envelope_v1(envelope_cbor) is {
    Some(envelope) ->
      Some(
        cek_proof_v1.hash_program_envelope_v1(
          1,
          1,
          0,
          envelope.term_root,
          envelope.node_count,
          envelope.material_byte_length,
        ),
      )
    None -> None
  }
}

fn material_reference_input_at(
  reference_inputs: List<Input>,
  index: Int,
  cek_program_material_script_hash: ScriptHash,
) -> Option<Data> {
  when list.at(reference_inputs, index) is {
    Some(Input {
      output: Output {
        address: Address {
          payment_credential: Credential.Script(script_hash),
          ..
        },
        datum: InlineDatum(datum),
        ..
      },
      ..
    }) ->
      if script_hash == cek_program_material_script_hash {
        Some(datum)
      } else {
        None
      }
    _ -> None
  }
}

fn verify_single_publication_route_v1(
  envelope_cbor: ByteArray,
  reference_inputs: List<Input>,
  reference_input_index: Int,
  cek_program_material_script_hash: ScriptHash,
) -> Bool {
  when
    material_reference_input_at(
      reference_inputs,
      reference_input_index,
      cek_program_material_script_hash,
    )
  is {
    Some(datum_data) -> {
      expect
          CekSinglePublicationDatumV1 {
            version,
            program_envelope_hash,
            sidecar_cbor,
          }
        = datum_data
      when cek_envelope_hash_v1(envelope_cbor) is {
        Some(expected_hash) -> and {
            version == 1,
            program_envelope_hash == expected_hash,
            cek_proof_v1.verify_complete_program_material_v1(
              envelope_cbor,
              sidecar_cbor,
            ),
          }
        None -> False
      }
    }
    None -> False
  }
}

pub fn cek_reference_indices_unique_nonnegative_v1(indices: List<Int>) -> Bool {
  when indices is {
    [] -> True
    [first, ..rest] ->
      first >= 0 && !list.has(rest, first) && cek_reference_indices_unique_nonnegative_v1(
        rest,
      )
  }
}

fn material_entries_from_references_v1(
  reference_inputs: List<Input>,
  indices: List<Int>,
  cek_program_material_script_hash: ScriptHash,
) -> Option<List<CekProgramMaterialDatumV1>> {
  when indices is {
    [] -> Some([])
    [index, ..rest] ->
      when
        material_reference_input_at(
          reference_inputs,
          index,
          cek_program_material_script_hash,
        )
      is {
        Some(datum_data) -> {
          expect entry: CekProgramMaterialDatumV1 = datum_data
          when
            material_entries_from_references_v1(
              reference_inputs,
              rest,
              cek_program_material_script_hash,
            )
          is {
            Some(entries) -> Some([entry, ..entries])
            None -> None
          }
        }
        None -> None
      }
  }
}

pub fn verify_cek_material_route_for_selected_envelope_v1(
  selected_envelope: ByteArray,
  material_route: CekMaterialRouteV1,
  reference_inputs: List<Input>,
  cek_program_material_script_hash: ScriptHash,
) -> Bool {
  when material_route is {
    DirectCekMaterial { envelope_cbor, sidecar_cbor } ->
      envelope_cbor == selected_envelope && cek_proof_v1.verify_complete_program_material_v1(
        envelope_cbor,
        sidecar_cbor,
      )
    SinglePublicationCekMaterial { envelope_cbor, reference_input_index } ->
      envelope_cbor == selected_envelope && verify_single_publication_route_v1(
        envelope_cbor,
        reference_inputs,
        reference_input_index,
        cek_program_material_script_hash,
      )
    MinimumMultiOutputCekMaterial { envelope_cbor, reference_input_indices } ->
      if and {
        envelope_cbor == selected_envelope,
        list.length(reference_input_indices) > 0,
        cek_reference_indices_unique_nonnegative_v1(reference_input_indices),
      } {
        when
          material_entries_from_references_v1(
            reference_inputs,
            reference_input_indices,
            cek_program_material_script_hash,
          )
        is {
          Some(entries) ->
            cek_proof_v1.verify_complete_program_material_entries_v1(
              envelope_cbor,
              entries,
            )
          None -> False
        }
      } else {
        False
      }
    // Fails closed on L1. The predicate this branch used to carry was
    // `program_envelope_hash == cek_envelope_hash_v1(selected_envelope)`, and
    // `cek_envelope_hash_v1` is a pure function of `selected_envelope` alone,
    // so both sides came from the disputer's own evidence: the check was the
    // tautology `f(x) == chosen` with `chosen := f(x)`. It read neither
    // `reference_inputs` nor `cek_program_material_script_hash`, so a CEK
    // finalization could mint its fraud proof with ZERO program material
    // published on L1, while the three complete routes above each reconstruct
    // the whole content-addressed graph against the selected envelope.
    //
    // The off-chain incremental grammar cannot be lifted here as specified:
    // it orders one or more `proofContinuation` transactions AFTER the
    // `proofConsumption` transaction, and the consumption is this
    // finalization. Material verified after the dispute resolves secures
    // nothing. A sound incremental route needs an authenticated
    // cross-transaction traversal accumulator — a step chain whose datum
    // carries the partial material frontier, reachable-node count and
    // material byte length, advanced over published entries and required by
    // the finalization to have already completed against the selected
    // envelope. No such accumulator exists on-chain: program material
    // publications are permissionless, self-authenticating and unspendable
    // (`validators/user-events/cek-program-material-v1.ak`) with no minting
    // policy and no aggregate completion commitment.
    //
    // Until that accumulator lands under its own lease this route rejects, so
    // every accepted CEK material route verifies the complete material graph
    // on L1. Data availability is unaffected: publication is permissionless,
    // so the graph is always publishable; only the bounded multi-transaction
    // VERIFICATION is unavailable, which is a liveness limit on oversized
    // programs rather than a soundness hole.
    IncrementalCekMaterial { .. } -> False
    NoCekMaterial -> False
  }
}

/// Complete CEK program material is admitted only at the authenticated CEK
/// execution-selection boundary: a selection step's envelope must be backed by
/// an accepted material route, and every other cek step must carry no
/// material at all. The material route is resolver evidence, never part of
/// the hashed one-step evidence, so the selection semantic resolver checks it
/// against the transaction's reference inputs after `continue_winning` has
/// bound transition and auxiliary to the prepared evidence hash.
pub fn verify_cek_route_v1(
  evidence: vm_machine_types.ValidationOneStepEvidenceV1,
  material_route: CekMaterialRouteV1,
  reference_inputs: List<Input>,
  cek_program_material_script_hash: ScriptHash,
) -> Bool {
  when cek_selection_envelope_cbor_v1(evidence) is {
    Some(selected_envelope) ->
      verify_cek_material_route_for_selected_envelope_v1(
        selected_envelope,
        material_route,
        reference_inputs,
        cek_program_material_script_hash,
      )
    None -> material_route == NoCekMaterial
  }
}

pub type PrepareActionV1 {
  Prepare {
    input_index: Int,
    output_index: Int,
    transition: vm_machine_types.ValidationOneStepWitnessV1,
    auxiliary: Data,
  }
}

pub type PrepareSpendRedeemer =
  ct.StepRedeemer<PrepareActionV1>

pub type PrepareSelectedActionV1 {
  PrepareSelected {
    input_index: Int,
    output_index: Int,
    semantic_resolver_index: Int,
    transition: vm_machine_types.ValidationOneStepWitnessV1,
    auxiliary: Data,
  }
}

pub type PrepareSelectedSpendRedeemer =
  ct.StepRedeemer<PrepareSelectedActionV1>

/// Authenticates one semantic resolver selection against the complete deployed
/// phase group. Keeping the exact group cardinality in this selector makes a
/// stale or truncated parameter list fail closed before resolver handoff.
pub fn select_semantic_resolver(
  semantic_resolver_script_hashes: List<ScriptHash>,
  expected_semantic_resolver_count: Int,
  semantic_resolver_index: Int,
) -> Option<ScriptHash> {
  if and {
    expected_semantic_resolver_count > 0,
    list.length(semantic_resolver_script_hashes) == expected_semantic_resolver_count,
    semantic_resolver_index >= 0,
    semantic_resolver_index < expected_semantic_resolver_count,
  } {
    list.at(semantic_resolver_script_hashes, semantic_resolver_index)
  } else {
    None
  }
}

pub fn prepare(
  expected_phase: validation_trace_v1.ValidationPhase,
  semantic_resolver_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  datum: Option<Datum>,
  redeemer: PrepareSpendRedeemer,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
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
    ct.Continue(Prepare { input_index, output_index, transition, auxiliary }) -> {
      let Transaction { inputs, outputs, .. } = tx
      expect Some(step_datum) = datum
      continue(
        computation_thread_token_policy_id,
        step_datum,
        input_index,
        output_index,
        own_out_ref,
        inputs,
        outputs,
        fn(
          _input_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          input_state,
          output_script_hash,
          output_state,
        ) {
          expect Some(state) = input_state
          let expected_output_state: Data = validation_resolution_v1.prepare_semantic_resolution(
            state,
            transition,
            auxiliary,
          )
          and {
            state.pre_state.phase == expected_phase,
            output_script_hash == semantic_resolver_script_hash,
            output_state == expected_output_state,
          }
        },
      )
    }
  }
}

/// Routes one authenticated phase-level boundary to exactly one narrow
/// semantic proof family. The selector is intentionally not part of the
/// evidence hash: the selected validator reconstructs and authenticates the
/// complete transition/auxiliary evidence, so choosing the wrong family
/// cannot produce a winning resolution.
pub fn prepare_selected(
  expected_phase: validation_trace_v1.ValidationPhase,
  semantic_resolver_script_hashes: List<ScriptHash>,
  expected_semantic_resolver_count: Int,
  computation_thread_token_policy_id: PolicyId,
  datum: Option<Datum>,
  redeemer: PrepareSelectedSpendRedeemer,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
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
    ct.Continue(PrepareSelected {
      input_index,
      output_index,
      semantic_resolver_index,
      transition,
      auxiliary,
    }) -> {
      let Transaction { inputs, outputs, .. } = tx
      expect Some(step_datum) = datum
      expect Some(semantic_resolver_script_hash) = select_semantic_resolver(
        semantic_resolver_script_hashes,
        expected_semantic_resolver_count,
        semantic_resolver_index,
      )
      continue(
        computation_thread_token_policy_id,
        step_datum,
        input_index,
        output_index,
        own_out_ref,
        inputs,
        outputs,
        fn(
          _input_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          input_state,
          output_script_hash,
          output_state,
        ) {
          expect Some(state) = input_state
          let expected_output_state: Data = validation_resolution_v1.prepare_semantic_resolution(
            state,
            transition,
            auxiliary,
          )
          and {
            state.pre_state.phase == expected_phase,
            output_script_hash == semantic_resolver_script_hash,
            output_state == expected_output_state,
          }
        },
      )
    }
  }
}
