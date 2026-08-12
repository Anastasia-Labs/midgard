use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets.{PolicyId, from_lovelace}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/cek_proof_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTransactionWitnessSet,
  MidgardTxInput, TxIsValid, native_tx_version_v1,
}
use midgard/ledger_state.{CekProgramMaterialDatumV1}
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_machine_v1
use midgard/validation_merkle_v1
use midgard/validation_resolution_v1
use midgard/validation_resolver_v1
use midgard/validation_semantic_v1
use midgard/validation_trace_v1

/// §8's door context for a tier-1 step. See the twin in
/// `validation-machine-v1.test.ak`: every step here carries an `Inline`
/// carriage, so neither reference inputs (tier 2) nor the §8.6 certificate
/// minting policy id (tier 3) is reachable or needed.
fn inline_field_door() -> field_door.MachineFieldDoorV1 {
  field_door.MachineFieldDoorV1 {
    reference_inputs: [],
    certificate_policy_id: #"",
  }
}

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const material_script_hash: ScriptHash =
  #"77777777777777777777777777777777777777777777777777777777"

const computation_thread_policy: PolicyId =
  #"22222222222222222222222222222222222222222222222222222222"

const prepare_script_hash: ScriptHash =
  #"33333333333333333333333333333333333333333333333333333333"

const award_script_hash: ScriptHash =
  #"66666666666666666666666666666666666666666666666666666666"

const fraud_prover = #"44444444444444444444444444444444444444444444444444444444"

const computation_thread_asset_name =
  #"00000006aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const own_out_ref =
  OutputReference {
    transaction_id: #"5555555555555555555555555555555555555555555555555555555555555555",
    output_index: 0,
  }

const validation_context_cbor =
  #"8701546d6964676172642d636f6e73656e7375732d763118640000001864"

test cek_material_reference_indices_are_unique_reference_selectors() {
  and {
    validation_resolver_v1.cek_reference_indices_unique_nonnegative_v1([]),
    validation_resolver_v1.cek_reference_indices_unique_nonnegative_v1([0]),
    validation_resolver_v1.cek_reference_indices_unique_nonnegative_v1(
      [2, 0, 1],
    ),
    !validation_resolver_v1.cek_reference_indices_unique_nonnegative_v1([-1]),
    !validation_resolver_v1.cek_reference_indices_unique_nonnegative_v1([1, 1]),
  }
}

test cek_direct_route_binds_the_selected_envelope() {
  let preimage = #"8106"
  let root = cek_proof_v1.hash_error_term_v1()
  let envelope = cek_proof_v1.encode_program_envelope_v1(1, 1, 0, root, 1, 2)
  let sidecar = #"82"
    |> bytearray.concat(#"01")
    |> bytearray.concat(#"81")
    |> bytearray.concat(#"82")
    |> bytearray.concat(#"5820")
    |> bytearray.concat(root)
    |> bytearray.concat(#"83")
    |> bytearray.concat(#"01")
    |> bytearray.concat(#"00")
    |> bytearray.concat(#"42")
    |> bytearray.concat(preimage)
  and {
    validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.DirectCekMaterial {
        envelope_cbor: envelope,
        sidecar_cbor: sidecar,
      },
      [],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.DirectCekMaterial {
        envelope_cbor: #"8106",
        sidecar_cbor: sidecar,
      },
      [],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.DirectCekMaterial {
        envelope_cbor: envelope,
        sidecar_cbor: sidecar |> bytearray.concat(#"00"),
      },
      [],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.NoCekMaterial,
      [],
      material_script_hash,
    ),
  }
}

fn material_reference_input(datum: Data, hash: ScriptHash) -> Input {
  Input {
    output_reference: own_out_ref,
    output: Output {
      address: script_address(hash),
      value: from_lovelace(2_000_000),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn one_node_envelope() -> ByteArray {
  cek_proof_v1.encode_program_envelope_v1(
    1,
    1,
    0,
    cek_proof_v1.hash_error_term_v1(),
    1,
    2,
  )
}

fn one_node_envelope_hash() -> ByteArray {
  cek_proof_v1.hash_program_envelope_v1(
    1,
    1,
    0,
    cek_proof_v1.hash_error_term_v1(),
    1,
    2,
  )
}

fn one_node_material_entry() -> Data {
  let entry: Data = CekProgramMaterialDatumV1 {
    kind: 0,
    root: cek_proof_v1.hash_error_term_v1(),
    preimage: #"8106",
  }
  entry
}

/// Atomic: the exact exploit. A self-consistent `program_envelope_hash` derived
/// from the disputer's own selected envelope, published with NO material.
/// Single assertion, so a failure can only be this assertion.
test cek_incremental_route_rejects_zero_published_material() {
  !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
    one_node_envelope(),
    validation_resolver_v1.IncrementalCekMaterial {
      program_envelope_hash: one_node_envelope_hash(),
    },
    [],
    material_script_hash,
  )
}

/// Atomic: self-consistent hash pair carrying the COMPLETE material as a
/// reference input. Single assertion.
test cek_incremental_route_rejects_complete_published_material() {
  !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
    one_node_envelope(),
    validation_resolver_v1.IncrementalCekMaterial {
      program_envelope_hash: one_node_envelope_hash(),
    },
    [material_reference_input(one_node_material_entry(), material_script_hash)],
    material_script_hash,
  )
}

/// Atomic: self-consistent hash pair carrying SUBSTITUTED material. Single
/// assertion.
test cek_incremental_route_rejects_substituted_published_material() {
  let substituted: Data = CekProgramMaterialDatumV1 {
    kind: 0,
    root: cek_proof_v1.hash_variable_term_v1(0),
    preimage: #"820000",
  }
  !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
    one_node_envelope(),
    validation_resolver_v1.IncrementalCekMaterial {
      program_envelope_hash: one_node_envelope_hash(),
    },
    [material_reference_input(substituted, material_script_hash)],
    material_script_hash,
  )
}

/// Atomic positive control for the three atomic negatives above: the same
/// selected envelope and the same reference input are ACCEPTED through the
/// complete multi-output route, so those rejections are attributable to the
/// route selector alone.
test cek_complete_multi_output_route_accepts_the_same_material() {
  validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
    one_node_envelope(),
    validation_resolver_v1.MinimumMultiOutputCekMaterial {
      envelope_cbor: one_node_envelope(),
      reference_input_indices: [0],
    },
    [material_reference_input(one_node_material_entry(), material_script_hash)],
    material_script_hash,
  )
}

/// Pins the closed rejection of the incremental CEK material route against the
/// exact exploit it used to admit: a self-consistent `program_envelope_hash`
/// derived from the disputer's own selected envelope, published with no
/// material at all. Every negative here is attributable, because the same
/// selected envelope and the same reference inputs are ACCEPTED through a
/// complete route in the paired positive conjuncts — so the rejection can only
/// come from the route selector, never from a malformed envelope, a malformed
/// entry, or an empty reference-input list.
test cek_incremental_route_fails_closed_with_self_consistent_hash() {
  let preimage = #"8106"
  let root = cek_proof_v1.hash_error_term_v1()
  let envelope = cek_proof_v1.encode_program_envelope_v1(1, 1, 0, root, 1, 2)
  let sidecar = #"820181825820"
    |> bytearray.concat(root)
    |> bytearray.concat(#"830100428106")
  // The honest envelope commitment: exactly what `cek_envelope_hash_v1` derives
  // from `envelope`, i.e. the value the old tautology compared against itself.
  let envelope_hash = cek_proof_v1.hash_program_envelope_v1(1, 1, 0, root, 1, 2)
  let entry: Data = CekProgramMaterialDatumV1 { kind: 0, root, preimage }
  let substituted: Data = CekProgramMaterialDatumV1 {
    kind: 0,
    root: cek_proof_v1.hash_variable_term_v1(0),
    preimage: #"820000",
  }
  and {
    // Positive control 1: the complete direct route accepts this envelope with
    // zero reference inputs, so an empty reference-input list is not what makes
    // the incremental conjuncts below fail.
    validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.DirectCekMaterial {
        envelope_cbor: envelope,
        sidecar_cbor: sidecar,
      },
      [],
      material_script_hash,
    ),
    // Positive control 2: the complete multi-output route accepts this exact
    // envelope over this exact reference input, so `entry` is well formed,
    // reachable, and complete for `envelope`.
    validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.MinimumMultiOutputCekMaterial {
        envelope_cbor: envelope,
        reference_input_indices: [0],
      },
      [material_reference_input(entry, material_script_hash)],
      material_script_hash,
    ),
    // THE EXPLOIT: self-consistent hash pair, zero published material.
    // Previously accepted; now rejected.
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.IncrementalCekMaterial {
        program_envelope_hash: envelope_hash,
      },
      [],
      material_script_hash,
    ),
    // Self-consistent hash pair with the COMPLETE material actually referenced.
    // Still rejected: positive control 2 proves the material and the reference
    // input are acceptable, so the route selector is the sole cause.
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.IncrementalCekMaterial {
        program_envelope_hash: envelope_hash,
      },
      [material_reference_input(entry, material_script_hash)],
      material_script_hash,
    ),
    // Substituted material carried alongside the self-consistent hash pair.
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.IncrementalCekMaterial {
        program_envelope_hash: envelope_hash,
      },
      [material_reference_input(substituted, material_script_hash)],
      material_script_hash,
    ),
    // The substituted entry really is a substitution: the complete route
    // rejects it for this envelope.
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.MinimumMultiOutputCekMaterial {
        envelope_cbor: envelope,
        reference_input_indices: [0],
      },
      [material_reference_input(substituted, material_script_hash)],
      material_script_hash,
    ),
    // A substituted envelope hash is rejected too, so the closure is not a
    // hash comparison that a different hash could satisfy.
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.IncrementalCekMaterial {
        program_envelope_hash: h_a,
      },
      [],
      material_script_hash,
    ),
    // An empty envelope-hash field is rejected: the closure does not depend on
    // the redeemer field at all.
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.IncrementalCekMaterial {
        program_envelope_hash: #"",
      },
      [],
      material_script_hash,
    ),
  }
}

/// Partial-material negative on a two-node graph, plus the proof that the
/// two-node graph and its complete reference set are otherwise acceptable.
test cek_incremental_route_fails_closed_with_partial_material() {
  let child_root = cek_proof_v1.hash_error_term_v1()
  let child_preimage = #"8106"
  let root_preimage = #"82015820"
    |> bytearray.concat(child_root)
  let root = cek_proof_v1.hash_delay_term_v1(child_root)
  // Two reachable nodes; 36 + 2 material bytes.
  let envelope = cek_proof_v1.encode_program_envelope_v1(1, 1, 0, root, 2, 38)
  let envelope_hash = cek_proof_v1.hash_program_envelope_v1(
    1,
    1,
    0,
    root,
    2,
    38,
  )
  let root_entry: Data = CekProgramMaterialDatumV1 {
    kind: 0,
    root,
    preimage: root_preimage,
  }
  let child_entry: Data = CekProgramMaterialDatumV1 {
    kind: 0,
    root: child_root,
    preimage: child_preimage,
  }
  // `verify_complete_program_material_entries_v1` requires strictly ascending
  // roots in index order, so present the two entries sorted by root.
  let complete_inputs = if bytearray.compare(root, child_root) == Less {
    [
      material_reference_input(root_entry, material_script_hash),
      material_reference_input(child_entry, material_script_hash),
    ]
  } else {
    [
      material_reference_input(child_entry, material_script_hash),
      material_reference_input(root_entry, material_script_hash),
    ]
  }
  let partial_inputs = [
    material_reference_input(root_entry, material_script_hash),
  ]
  and {
    // The two-node graph and its complete reference set are acceptable through
    // the complete multi-output route.
    validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.MinimumMultiOutputCekMaterial {
        envelope_cbor: envelope,
        reference_input_indices: [0, 1],
      },
      complete_inputs,
      material_script_hash,
    ),
    // The partial set is genuinely partial: the complete route rejects it.
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.MinimumMultiOutputCekMaterial {
        envelope_cbor: envelope,
        reference_input_indices: [0],
      },
      partial_inputs,
      material_script_hash,
    ),
    // Incremental with partial material: rejected.
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.IncrementalCekMaterial {
        program_envelope_hash: envelope_hash,
      },
      partial_inputs,
      material_script_hash,
    ),
    // Incremental with COMPLETE material: still rejected, and the first
    // conjunct proves the completeness of exactly this reference set.
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.IncrementalCekMaterial {
        program_envelope_hash: envelope_hash,
      },
      complete_inputs,
      material_script_hash,
    ),
  }
}

/// Ordinary complete-item carriage is preserved: for one selected envelope,
/// every complete route accepts and only the incremental route is closed.
test cek_complete_item_carriage_survives_the_incremental_closure() {
  let preimage = #"8106"
  let root = cek_proof_v1.hash_error_term_v1()
  let envelope = cek_proof_v1.encode_program_envelope_v1(1, 1, 0, root, 1, 2)
  let sidecar = #"820181825820"
    |> bytearray.concat(root)
    |> bytearray.concat(#"830100428106")
  let envelope_hash = cek_proof_v1.hash_program_envelope_v1(1, 1, 0, root, 1, 2)
  let entry: Data = CekProgramMaterialDatumV1 { kind: 0, root, preimage }
  let single: Data = validation_resolver_v1.CekSinglePublicationDatumV1 {
    version: 1,
    program_envelope_hash: envelope_hash,
    sidecar_cbor: sidecar,
  }
  and {
    validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.DirectCekMaterial {
        envelope_cbor: envelope,
        sidecar_cbor: sidecar,
      },
      [],
      material_script_hash,
    ),
    validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.SinglePublicationCekMaterial {
        envelope_cbor: envelope,
        reference_input_index: 0,
      },
      [material_reference_input(single, material_script_hash)],
      material_script_hash,
    ),
    validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.MinimumMultiOutputCekMaterial {
        envelope_cbor: envelope,
        reference_input_indices: [0],
      },
      [material_reference_input(entry, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.IncrementalCekMaterial {
        program_envelope_hash: envelope_hash,
      },
      [material_reference_input(entry, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.NoCekMaterial,
      [],
      material_script_hash,
    ),
  }
}

test cek_single_and_multi_reference_routes_verify_typed_material() {
  let preimage = #"8106"
  let root = cek_proof_v1.hash_error_term_v1()
  let envelope = cek_proof_v1.encode_program_envelope_v1(1, 1, 0, root, 1, 2)
  let sidecar = #"820181825820"
    |> bytearray.concat(root)
    |> bytearray.concat(#"830100428106")
  let envelope_hash = cek_proof_v1.hash_program_envelope_v1(1, 1, 0, root, 1, 2)
  let single: Data = validation_resolver_v1.CekSinglePublicationDatumV1 {
    version: 1,
    program_envelope_hash: envelope_hash,
    sidecar_cbor: sidecar,
  }
  let bad_version: Data = validation_resolver_v1.CekSinglePublicationDatumV1 {
    version: 2,
    program_envelope_hash: envelope_hash,
    sidecar_cbor: sidecar,
  }
  let bad_hash: Data = validation_resolver_v1.CekSinglePublicationDatumV1 {
    version: 1,
    program_envelope_hash: h_a,
    sidecar_cbor: sidecar,
  }
  let entry: Data = CekProgramMaterialDatumV1 { kind: 0, root, preimage }
  and {
    validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.SinglePublicationCekMaterial {
        envelope_cbor: envelope,
        reference_input_index: 0,
      },
      [material_reference_input(single, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.SinglePublicationCekMaterial {
        envelope_cbor: envelope,
        reference_input_index: 1,
      },
      [material_reference_input(single, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.SinglePublicationCekMaterial {
        envelope_cbor: envelope,
        reference_input_index: 0,
      },
      [material_reference_input(bad_version, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.SinglePublicationCekMaterial {
        envelope_cbor: envelope,
        reference_input_index: 0,
      },
      [material_reference_input(bad_hash, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.SinglePublicationCekMaterial {
        envelope_cbor: envelope,
        reference_input_index: 0,
      },
      [material_reference_input(entry, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.SinglePublicationCekMaterial {
        envelope_cbor: envelope,
        reference_input_index: 0,
      },
      [material_reference_input(single, prepare_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.MinimumMultiOutputCekMaterial {
        envelope_cbor: envelope,
        reference_input_indices: [0, 0],
      },
      [material_reference_input(entry, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.MinimumMultiOutputCekMaterial {
        envelope_cbor: envelope,
        reference_input_indices: [1],
      },
      [material_reference_input(entry, material_script_hash)],
      material_script_hash,
    ),
    validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.MinimumMultiOutputCekMaterial {
        envelope_cbor: envelope,
        reference_input_indices: [0],
      },
      [material_reference_input(entry, material_script_hash)],
      material_script_hash,
    ),
    !validation_resolver_v1.verify_cek_material_route_for_selected_envelope_v1(
      envelope,
      validation_resolver_v1.MinimumMultiOutputCekMaterial {
        envelope_cbor: envelope,
        reference_input_indices: [],
      },
      [material_reference_input(entry, material_script_hash)],
      material_script_hash,
    ),
  }
}

// Bound by the SDK cross-language test to buildFaultProofContracts over a
// freshly generated, compiler-pinned testnet plutus.json and deterministic
// fixture policy IDs. The same test independently applies the Phase-A and
// ScriptSources prepare validators to these exact semantic groups. These are
// parameterized production-builder fixture identities, not a claim about a
// live deployment.
fn phase_a_script_precondition_resolvers() -> List<ScriptHash> {
  [
    #"e2bc97da9ec252d996282fb9715a5c76df04107357605f9f58875ef8",
    #"6f7267cf1da8c83c5a3f5135ee1bb15a6bf5a62184ebab67d9f5c254",
  ]
}

fn input_sets_resolvers() -> List<ScriptHash> {
  [prepare_script_hash, award_script_hash]
}

fn script_source_resolvers() -> List<ScriptHash> {
  [
    #"4f04d75309a25fa8b2d286a08f235ad2bbc04fc4255bc9de56e6eb1e",
    #"dbc5185f10bfd71ba6d0b0ae3198164fbbfbf9c8a7689710967dfbf5",
    #"cfea8848ff4fdd5e87019edb3646d055bc4a62909198984e189ae2f1",
    #"041552a027e42180cb14f848b989b1b9f207139d267a776b18205545",
    #"ebdcae5094b7733b6f535782fcb8a61912312b0cb25ac02151b5d921",
    #"d97b8f23efb31e32538c89a0d97009dcbbaffacbc73e097a86355ba2",
    #"37618d1d07d266924ac5f0631a1791c27d9bd76d3bacad893466f6ca",
    #"ba604acdd10a0101a6a293f6237a411f03a1013d984f279156657d5d",
    #"8cc9c85b11695228483e63d0a59684a989d8236ca2ec654f8e8ec0c1",
    #"be5e382cd992ec856b427e1dac37aad24d07765a09aefb7f8509fae7",
    #"119a1d8af560ea6f3d1ae222b67d85d5319fb93f09766ededccba946",
    #"74e43c22efb8fc97476bc810fc522a35ce10fc41f11268f01a627651",
    #"95c59bf506b660fc4c59296d7c957ecd6def9f303192d9cb291d2494",
    #"1e48d46d39a29e6158d40da127654959b1b64fdb88700969db0b1f94",
    #"7319aa9013feca18760134d3b9929d5b135829216fe34a6f05a16508",
    #"f8894d5a675072e9c6b294e1108d67da83e4042963d7d109355cd0f0",
    #"3d0e2e530d0781721e5d1b8efab92eadf6100c36ce69fc61b3ce794c",
    #"9d043f251606bd2efadc45a142cbf9885a82e9822fe654e43f6b0d9a",
    #"cd74112e0e53e710bd27b5f0e3c0ca6b60bbf65b609ccec69c36a153",
    #"fee060fa54bb273c2ed31b50ea6e1b59047263cd29021c93eb0f0001",
    #"10b4c378158f236c02fd8a4abe3d9ecca63b48e65f5830e2726081f0",
    #"c8623402d6dcb7e378af3d8ef01603ce614c31be1c0ce2912cf7f3d7",
    #"f6397e77074fa9018d72613465ae86768b85d82447accd00cff58633",
    #"00d2cf824ca845f19880af8f9a32a0a1e6b93c74907735af96c93e50",
    #"b350f95cfaaf97c5bad1b1d82e5df8592782a12f7cdbbbf5b0285cf2",
    #"dc6561fe389cf36a1b520934cf508eafd2428e53e1208b77b53d545c",
    #"8b2be118846589dc7584d74cbf8a35b17ca14b9ef49a0a3187ba1fe3",
    #"594f9a0e996ff5857847edee440f9fe63cb297e05bbbf196cff650c7",
    #"51858cd4f6422203d0968108084d1a6072bb30e9c8a0682aac172e90",
  ]
}

fn script_address(script_hash: ScriptHash) -> Address {
  Address {
    payment_credential: address.Script(script_hash),
    stake_credential: None,
  }
}

fn empty_v1_transaction() -> MidgardTransaction {
  let empty_mint: Data = []
  MidgardTransaction {
    version: native_tx_version_v1,
    validity: TxIsValid,
    body: MidgardTransactionBody {
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
      reference_inputs: [],
      outputs: [],
      fee: 0,
      validity_interval_start: -1,
      validity_interval_end: -1,
      required_observers: [],
      required_signers: [],
      mint: empty_mint,
      script_integrity_hash: zero,
      auxiliary_data_hash: zero,
      network_id: 255,
    },
    witness_set: MidgardTransactionWitnessSet {
      addr_tx_wits: [],
      script_tx_wits: [],
      redeemer_tx_wits: [],
    },
  }
}

fn phase_a_semantic_fixture() -> Pair<
  validation_trace_v1.ValidationMachineStateV1,
  validation_machine_v1.ValidationOneStepWitnessV1,
> {
  let tx = empty_v1_transaction()
  let compact = native_transaction.midgard_transaction_to_compact(tx)
  let proof_source = native_transaction.midgard_transaction_proof_source_v1(tx)
  let schedule_hash = validation_machine_v1.transaction_resolution_schedule_hash(
    tx.body.inputs,
    tx.body.reference_inputs,
  )
  let signer_frontier_commitment = validation_merkle_v1.frontier_commitment(
    0,
    [],
  )
  let work_witness_cbor = validation_machine_v1.encode_phase_a_script_preconditions_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    schedule_hash,
    0,
    signer_frontier_commitment,
    False,
    0,
    0,
    #"",
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: native_tx_id_for_version(
      native_tx_version_v1,
      encode_native_tx_body_compact(compact.body),
    ),
    transaction_commitment: native_transaction.midgard_transaction_proof_commitment_v1(
      tx,
    ),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_b,
    phase: validation_trace_v1.PhaseAScriptPreconditions,
    program_counter: 6,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseAScriptPreconditions,
      6,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: h_c,
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.ResolveInputs,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      7,
      validation_machine_v1.encode_resolve_inputs_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        validation_context_cbor,
        0,
        validation_machine_v1.initial_resolution_accumulator(),
        schedule_hash,
        0,
        signer_frontier_commitment,
        None,
        schedule_hash,
      ),
    ),
  }
  Pair(
    pre,
    validation_machine_v1.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
  )
}

fn pre_state(
  phase: validation_trace_v1.ValidationPhase,
  work_witness_cbor: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: h_b,
    transaction_commitment: h_c,
    validation_context_hash: h_a,
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_b,
    phase,
    program_counter: 6,
    work_root: validation_trace_v1.hash_work_witness(
      phase,
      6,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: h_c,
  }
}

fn successor_state(
  pre: validation_trace_v1.ValidationMachineStateV1,
  work_root: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: pre.program_counter + 1,
    work_root,
  }
}

fn resolution_state(
  pre: validation_trace_v1.ValidationMachineStateV1,
  transition: validation_machine_v1.ValidationOneStepWitnessV1,
) -> validation_resolution_v1.ValidationResolutionStateV1 {
  validation_resolution_v1.ValidationResolutionStateV1 {
    version: validation_resolution_v1.resolution_version,
    pre_state: pre,
    operator_successor_hash: h_a,
    challenger_successor_hash: validation_trace_v1.hash_machine_state(
      transition.claimed_successor,
    ),
  }
}

fn step_datum(
  state: validation_resolution_v1.ValidationResolutionStateV1,
) -> validation_resolver_v1.Datum {
  ct.StepDatum { fraud_prover, data: Some(state) }
}

fn no_auxiliary_data() -> Data {
  let auxiliary_data: Data = validation_machine_v1.NoAuxiliaryWitness
  auxiliary_data
}

fn script_sources_auxiliary() -> validation_machine_v1.ValidationAuxiliaryWitnessV1 {
  validation_machine_v1.ScriptPurposeScanWitness {
    purpose_kind: 0,
    purpose_index: 0,
    script_hash: prepare_script_hash,
    subject: #"80",
    siblings: [],
  }
}

fn script_sources_auxiliary_data() -> Data {
  let auxiliary_data: Data = script_sources_auxiliary()
  auxiliary_data
}

fn prepare_redeemer(
  semantic_resolver_index: Int,
  transition: validation_machine_v1.ValidationOneStepWitnessV1,
  auxiliary: Data,
) -> validation_resolver_v1.PrepareSelectedSpendRedeemer {
  ct.Continue(
    validation_resolver_v1.PrepareSelected {
      input_index: 0,
      output_index: 0,
      semantic_resolver_index,
      transition,
      auxiliary,
    },
  )
}

fn prepare_transaction(
  state: validation_resolution_v1.ValidationResolutionStateV1,
  transition: validation_machine_v1.ValidationOneStepWitnessV1,
  auxiliary: Data,
  output_script_hash: ScriptHash,
) -> Transaction {
  let input_datum_data: Data = step_datum(state)
  let prepared_state = validation_resolution_v1.prepare_semantic_resolution(
    state,
    transition,
    auxiliary,
  )
  let output_datum_data: Data = ct.StepDatum {
    fraud_prover,
    data: Some(prepared_state),
  }
  Transaction {
    ..transaction.placeholder,
    inputs: [
      transaction.Input {
        output_reference: own_out_ref,
        output: transaction.Output {
          address: script_address(prepare_script_hash),
          value: assets.from_lovelace(2_000_000)
            |> assets.add(
                computation_thread_policy,
                computation_thread_asset_name,
                1,
              ),
          datum: transaction.InlineDatum(input_datum_data),
          reference_script: None,
        },
      },
    ],
    outputs: [
      transaction.Output {
        address: script_address(output_script_hash),
        value: assets.from_lovelace(2_000_000)
          |> assets.add(
              computation_thread_policy,
              computation_thread_asset_name,
              1,
            ),
        datum: transaction.InlineDatum(output_datum_data),
        reference_script: None,
      },
    ],
  }
}

fn prepared_step_datum(
  prepared_state: validation_resolution_v1.PreparedValidationResolutionStateV1,
) -> validation_semantic_v1.Datum {
  ct.StepDatum { fraud_prover, data: Some(prepared_state) }
}

fn semantic_transaction(
  prepared_state: validation_resolution_v1.PreparedValidationResolutionStateV1,
  input_script_hash: ScriptHash,
) -> Transaction {
  let input_datum_data: Data = prepared_step_datum(prepared_state)
  let output_datum_data: Data = ct.StepDatum {
    fraud_prover,
    data: Some(validation_resolution_v1.winning_resolution()),
  }
  Transaction {
    ..transaction.placeholder,
    inputs: [
      transaction.Input {
        output_reference: own_out_ref,
        output: transaction.Output {
          address: script_address(input_script_hash),
          value: assets.from_lovelace(2_000_000)
            |> assets.add(
                computation_thread_policy,
                computation_thread_asset_name,
                1,
              ),
          datum: transaction.InlineDatum(input_datum_data),
          reference_script: None,
        },
      },
    ],
    outputs: [
      transaction.Output {
        address: script_address(award_script_hash),
        value: assets.from_lovelace(2_000_000)
          |> assets.add(
              computation_thread_policy,
              computation_thread_asset_name,
              1,
            ),
        datum: transaction.InlineDatum(output_datum_data),
        reference_script: None,
      },
    ],
  }
}

fn resolver_at(group: List<ScriptHash>, count: Int, index: Int) -> ScriptHash {
  expect Some(script_hash) = validation_resolver_v1.select_semantic_resolver(
    group,
    count,
    index,
  )
  script_hash
}

fn every_route_uses_the_deployed_hash(
  expected_phase: validation_trace_v1.ValidationPhase,
  group: List<ScriptHash>,
  count: Int,
  state: validation_resolution_v1.ValidationResolutionStateV1,
  transition: validation_machine_v1.ValidationOneStepWitnessV1,
  auxiliary: Data,
  index: Int,
) -> Bool {
  if index == count {
    True
  } else {
    let output_script_hash = resolver_at(group, count, index)
    and {
      validation_resolver_v1.prepare_selected(
        expected_phase,
        group,
        count,
        computation_thread_policy,
        Some(step_datum(state)),
        prepare_redeemer(index, transition, auxiliary),
        own_out_ref,
        prepare_transaction(state, transition, auxiliary, output_script_hash),
      ),
      every_route_uses_the_deployed_hash(
        expected_phase,
        group,
        count,
        state,
        transition,
        auxiliary,
        index + 1,
      ),
    }
  }
}

test phase_a_applied_group_routes_through_the_production_prepare_path() {
  let work_witness_cbor = #"80"
  let pre = pre_state(
    validation_trace_v1.PhaseAScriptPreconditions,
    work_witness_cbor,
  )
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: successor_state(pre, h_b),
  }
  let state = resolution_state(pre, transition)
  let auxiliary = no_auxiliary_data()
  let group = phase_a_script_precondition_resolvers()
  every_route_uses_the_deployed_hash(
    validation_trace_v1.PhaseAScriptPreconditions,
    group,
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    state,
    transition,
    auxiliary,
    0,
  )
}

test script_sources_applied_group_routes_through_the_production_prepare_path() {
  let work_witness_cbor = #"81"
  let pre = pre_state(validation_trace_v1.ScriptSources, work_witness_cbor)
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: successor_state(pre, h_b),
  }
  let state = resolution_state(pre, transition)
  let auxiliary = script_sources_auxiliary_data()
  let group = script_source_resolvers()
  every_route_uses_the_deployed_hash(
    validation_trace_v1.ScriptSources,
    group,
    validation_resolver_v1.script_sources_semantic_resolver_count,
    state,
    transition,
    auxiliary,
    0,
  )
}

test production_prepare_rejects_a_wrong_semantic_resolver_index() fail {
  let work_witness_cbor = #"80"
  let pre = pre_state(
    validation_trace_v1.PhaseAScriptPreconditions,
    work_witness_cbor,
  )
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: successor_state(pre, h_b),
  }
  let state = resolution_state(pre, transition)
  let auxiliary = no_auxiliary_data()
  let group = phase_a_script_precondition_resolvers()
  let index_one_output = resolver_at(
    group,
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    1,
  )
  validation_resolver_v1.prepare_selected(
    validation_trace_v1.PhaseAScriptPreconditions,
    group,
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    computation_thread_policy,
    Some(step_datum(state)),
    prepare_redeemer(0, transition, auxiliary),
    own_out_ref,
    prepare_transaction(state, transition, auxiliary, index_one_output),
  )
}

test production_prepare_rejects_a_wrong_output_script_hash() {
  let work_witness_cbor = #"80"
  let pre = pre_state(
    validation_trace_v1.PhaseAScriptPreconditions,
    work_witness_cbor,
  )
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: successor_state(pre, h_b),
  }
  let state = resolution_state(pre, transition)
  let auxiliary = no_auxiliary_data()
  let wrong_output_script_hash = resolver_at(
    input_sets_resolvers(),
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    0,
  )
  !validation_resolver_v1.prepare_selected(
    validation_trace_v1.PhaseAScriptPreconditions,
    phase_a_script_precondition_resolvers(),
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    computation_thread_policy,
    Some(step_datum(state)),
    prepare_redeemer(1, transition, auxiliary),
    own_out_ref,
    prepare_transaction(state, transition, auxiliary, wrong_output_script_hash),
  )
}

test production_prepare_rejects_same_cardinality_group_substitution() {
  let work_witness_cbor = #"80"
  let pre = pre_state(
    validation_trace_v1.PhaseAScriptPreconditions,
    work_witness_cbor,
  )
  let transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: successor_state(pre, h_b),
  }
  let state = resolution_state(pre, transition)
  let auxiliary = no_auxiliary_data()
  let deployed_group = phase_a_script_precondition_resolvers()
  let deployed_output = resolver_at(
    deployed_group,
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    1,
  )
  !validation_resolver_v1.prepare_selected(
    validation_trace_v1.PhaseAScriptPreconditions,
    input_sets_resolvers(),
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    computation_thread_policy,
    Some(step_datum(state)),
    prepare_redeemer(1, transition, auxiliary),
    own_out_ref,
    prepare_transaction(state, transition, auxiliary, deployed_output),
  )
}

test semantic_handoff_accepts_exact_prepared_phase_a_evidence() {
  let Pair(pre, transition) = phase_a_semantic_fixture()
  let state = resolution_state(pre, transition)
  let auxiliary = validation_machine_v1.NoAuxiliaryWitness
  let auxiliary_data: Data = auxiliary
  let prepared_state = validation_resolution_v1.prepare_semantic_resolution(
    state,
    transition,
    auxiliary_data,
  )
  let group = phase_a_script_precondition_resolvers()
  let input_script_hash = resolver_at(
    group,
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    0,
  )
  let evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition,
    auxiliary,
  }
  and {
    validation_machine_v1.verify_phase_a_script_preconditions_semantics_v1(
      pre,
      evidence,
      inline_field_door(),
    ),
    validation_semantic_v1.continue_winning(
      validation_trace_v1.PhaseAScriptPreconditions,
      award_script_hash,
      computation_thread_policy,
      Some(prepared_step_datum(prepared_state)),
      0,
      0,
      transition,
      auxiliary_data,
      validation_machine_v1.verify_phase_a_script_preconditions_semantics_v1(
        pre,
        evidence,
        inline_field_door(),
      ),
      own_out_ref,
      semantic_transaction(prepared_state, input_script_hash),
    ),
  }
}

test semantic_handoff_rejects_script_sources_auxiliary_substitution() {
  let Pair(pre, transition) = phase_a_semantic_fixture()
  let state = resolution_state(pre, transition)
  let committed_auxiliary = validation_machine_v1.NoAuxiliaryWitness
  let committed_auxiliary_data: Data = committed_auxiliary
  let prepared_state = validation_resolution_v1.prepare_semantic_resolution(
    state,
    transition,
    committed_auxiliary_data,
  )
  let input_script_hash = resolver_at(
    phase_a_script_precondition_resolvers(),
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    0,
  )
  let committed_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition,
    auxiliary: committed_auxiliary,
  }
  let cross_family_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition,
    auxiliary: script_sources_auxiliary(),
  }
  and {
    validation_machine_v1.verify_phase_a_script_preconditions_semantics_v1(
      pre,
      committed_evidence,
      inline_field_door(),
    ),
    !validation_semantic_v1.continue_winning(
      validation_trace_v1.PhaseAScriptPreconditions,
      award_script_hash,
      computation_thread_policy,
      Some(prepared_step_datum(prepared_state)),
      0,
      0,
      transition,
      committed_auxiliary_data,
      validation_machine_v1.verify_phase_a_script_preconditions_semantics_v1(
        pre,
        cross_family_evidence,
        inline_field_door(),
      ),
      own_out_ref,
      semantic_transaction(prepared_state, input_script_hash),
    ),
  }
}

test semantic_handoff_rejects_cross_family_action_substitution() {
  let Pair(pre, committed_transition) = phase_a_semantic_fixture()
  let committed_auxiliary = validation_machine_v1.NoAuxiliaryWitness
  let committed_auxiliary_data: Data = committed_auxiliary
  let cross_family_post = validation_trace_v1.ValidationMachineStateV1 {
    ..committed_transition.claimed_successor,
    phase: validation_trace_v1.ScriptSources,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      committed_transition.claimed_successor.program_counter,
      committed_transition.work_witness_cbor,
    ),
  }
  let cross_family_transition = validation_machine_v1.ValidationOneStepWitnessV1 {
    ..committed_transition,
    claimed_successor: cross_family_post,
  }
  let state = resolution_state(pre, committed_transition)
  let prepared_state = validation_resolution_v1.prepare_semantic_resolution(
    state,
    committed_transition,
    committed_auxiliary_data,
  )
  let input_script_hash = resolver_at(
    phase_a_script_precondition_resolvers(),
    validation_resolver_v1.phase_a_script_preconditions_semantic_resolver_count,
    0,
  )
  let committed_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: committed_transition,
    auxiliary: committed_auxiliary,
  }
  let cross_family_evidence = validation_machine_v1.ValidationOneStepEvidenceV1 {
    transition: cross_family_transition,
    auxiliary: committed_auxiliary,
  }
  and {
    validation_machine_v1.verify_phase_a_script_preconditions_semantics_v1(
      pre,
      committed_evidence,
      inline_field_door(),
    ),
    !validation_semantic_v1.continue_winning(
      validation_trace_v1.PhaseAScriptPreconditions,
      award_script_hash,
      computation_thread_policy,
      Some(prepared_step_datum(prepared_state)),
      0,
      0,
      committed_transition,
      committed_auxiliary_data,
      validation_machine_v1.verify_phase_a_script_preconditions_semantics_v1(
        pre,
        cross_family_evidence,
        inline_field_door(),
      ),
      own_out_ref,
      semantic_transaction(prepared_state, input_script_hash),
    ),
  }
}
