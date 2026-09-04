use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_collection_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardCredential, NativeTxBodyCompact, NativeTxCompact,
  NativeTxFieldPreimageLengthsV1, NativeTxWitnessSetCompact, PubKeyCredential,
  ScriptCredential,
}
use midgard/mpf_proof_fold_v1
use midgard/script_proof_v1
use midgard/validation_machine/machine_types.{
  EmptySignerSetProof, InputSignerAuthorizationV1, InputSignerAuthorized,
  InputSignerMissing, InputSignerProofMalformed, NoSignerSetProof,
  ScriptDiscoveryControlV1, SignerAboveLastProof, SignerBelowFirstProof,
  SignerBetweenProof, SignerMembershipProof, SignerSetProofV1,
  ValidationContextV1, ValidationOneStepWitnessV1,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

fn immutable_context_matches(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
) -> Bool {
  and {
    pre.machine_version == post.machine_version,
    pre.event_key_hash == post.event_key_hash,
    pre.transaction_id == post.transaction_id,
    pre.transaction_commitment == post.transaction_commitment,
    pre.validation_context_hash == post.validation_context_hash,
    pre.source_kind == post.source_kind,
    pre.prior_ledger_root == post.prior_ledger_root,
    pre.ledger_delta_root == post.ledger_delta_root,
  }
}

fn phase_successor_is_valid(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
) -> Bool {
  if post.phase == validation_trace_v1.Terminal {
    and {
      post.verdict != validation_trace_v1.Pending,
      pre.phase != validation_trace_v1.Terminal,
    }
  } else {
    and {
      post.verdict == validation_trace_v1.Pending,
      when pre.phase is {
        validation_trace_v1.CanonicalDecode -> or {
            post.phase == validation_trace_v1.CanonicalDecode,
            post.phase == validation_trace_v1.CompactBinding,
          }
        validation_trace_v1.CompactBinding ->
          post.phase == validation_trace_v1.StaticLedgerRules
        validation_trace_v1.StaticLedgerRules ->
          post.phase == validation_trace_v1.InputSets
        validation_trace_v1.InputSets ->
          post.phase == validation_trace_v1.Signatures
        validation_trace_v1.Signatures ->
          post.phase == validation_trace_v1.PhaseANativeScripts
        validation_trace_v1.PhaseANativeScripts ->
          post.phase == validation_trace_v1.PhaseAScriptPreconditions
        validation_trace_v1.PhaseAScriptPreconditions -> or {
            post.phase == validation_trace_v1.PhaseAScriptPreconditions,
            post.phase == validation_trace_v1.ResolveInputs,
          }
        validation_trace_v1.ResolveInputs -> or {
            post.phase == validation_trace_v1.ResolveInputs,
            post.phase == validation_trace_v1.ScriptSources,
          }
        validation_trace_v1.ScriptSources -> or {
            post.phase == validation_trace_v1.ScriptSources,
            post.phase == validation_trace_v1.NativeScripts,
          }
        validation_trace_v1.NativeScripts -> or {
            post.phase == validation_trace_v1.NativeScripts,
            post.phase == validation_trace_v1.ScriptIntegrity,
          }
        validation_trace_v1.ScriptIntegrity -> or {
            post.phase == validation_trace_v1.ScriptIntegrity,
            post.phase == validation_trace_v1.Cek,
          }
        validation_trace_v1.Cek -> or {
            post.phase == validation_trace_v1.Cek,
            post.phase == validation_trace_v1.ValueAndMint,
          }
        validation_trace_v1.ValueAndMint -> or {
            post.phase == validation_trace_v1.ValueAndMint,
            post.phase == validation_trace_v1.LedgerDelta,
          }
        validation_trace_v1.LedgerDelta ->
          post.phase == validation_trace_v1.LedgerDelta
        validation_trace_v1.Terminal -> False
      },
    }
  }
}

fn budget_progress_is_valid(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
) -> Bool {
  if pre.phase == validation_trace_v1.Cek {
    and {
      post.execution_cpu >= pre.execution_cpu,
      post.execution_memory >= pre.execution_memory,
    }
  } else {
    and {
      post.execution_cpu == pre.execution_cpu,
      post.execution_memory == pre.execution_memory,
    }
  }
}

pub fn structural_transition_is_valid(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let post = witness.claimed_successor
  and {
    validation_trace_v1.machine_state_is_well_formed(pre),
    validation_trace_v1.machine_state_is_well_formed(post),
    pre.verdict == validation_trace_v1.Pending,
    validation_trace_v1.hash_work_witness(
      pre.phase,
      pre.program_counter,
      witness.work_witness_cbor,
    ) == pre.work_root,
    post.program_counter == pre.program_counter + 1,
    immutable_context_matches(pre, post),
    phase_successor_is_valid(pre, post),
    budget_progress_is_valid(pre, post),
  }
}

pub fn encode_terminal_rejection_witness(
  rejection_code: ByteArray,
  prior_ledger_root: ByteArray,
) -> ByteArray {
  #"84"
    |> bytearray.concat(cbor.serialise(2))
    |> bytearray.concat(encode_definite_bytes(rejection_code))
    |> bytearray.concat(encode_definite_bytes(prior_ledger_root))
    |> bytearray.concat(encode_definite_bytes(#"80"))
}

pub fn canonical_argument_header_size(value: Int) -> Int {
  expect value >= 0
  if value < 24 {
    1
  } else if value < 256 {
    2
  } else if value < 65536 {
    3
  } else if value < 4294967296 {
    5
  } else {
    9
  }
}

pub fn transaction_field_preimage_length(
  lengths: NativeTxFieldPreimageLengthsV1,
  field_index: Int,
) -> Int {
  when field_index is {
    0 -> lengths.spend_inputs
    1 -> lengths.reference_inputs
    2 -> lengths.outputs
    3 -> lengths.required_observers
    4 -> lengths.required_signers
    5 -> lengths.mint
    6 -> lengths.script_witnesses
    7 -> lengths.address_witnesses
    8 -> lengths.redeemers
    _ -> fail @"unknown transaction field"
  }
}

pub fn transaction_field_commitment(
  compact: NativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
) -> ByteArray {
  let NativeTxCompact { body, .. } = compact
  when field_index is {
    0 -> body.spend_inputs_hash
    1 -> body.reference_inputs_hash
    2 -> body.outputs_hash
    3 -> body.required_observers_hash
    4 -> body.required_signers_hash
    5 -> body.mint_hash
    6 -> witness_set.script_tx_wits_hash
    7 -> witness_set.addr_tx_wits_hash
    8 -> witness_set.redeemer_tx_wits_hash
    _ -> fail @"unknown transaction field"
  }
}

/// How many §5.1 preimage bytes item `i` of field `field_index` occupies —
/// its definite byte-string wrapper plus its payload — or `None` when that
/// width is not a legal encoding for the field, which is what
/// `reject_invalid_field_type` renders.
///
/// **This function was stale, and #592 is where it showed.** §5.1 gave all nine
/// fields one envelope at #567: every item is a definite byte string, so its
/// encoded width is uniformly `header(len) + len`. This function still carried
/// the *retired counted grammar's* three-way split — fields 6 and 8 concatenated
/// raw item CBOR with no wrapper (`Some(item_length)`), and a field-5 policy item
/// was the map entry with its one-byte array wrapper removed
/// (`Some(item_length - 1)`). Under the counted scheme nothing measured it
/// against real bytes, because the item's extent came from a prover's
/// `ItemProofV1` rather than from the preimage. §8's door derives the extent from
/// the authenticated preimage instead, so the walk position this function
/// advances is now compared with §5.1's own arithmetic on every step, and the
/// disagreement is immediate: a field-6, field-8 or field-5 scan computed a
/// `next_encoded_length` short by the wrapper and therefore rendered
/// `reject_field_preimage_size` for an honest transaction. Three of #592's
/// twenty-two rows failed on exactly that
/// (`canonical_decode_authenticates_one_bounded_script_item`,
/// `maximum_cardano_spend_redeemer_terminal_fold_matches_typescript`,
/// `maximum_mint_field_terminal_matches_typescript`), and a fourth
/// (`canonical_decode_rejects_a_wrong_claimed_successor`) *passed the wrong
/// successor* because the rejection it claimed was the one the stale arithmetic
/// produced.
///
/// The one width that is not a legal encoding is kept, and one is added:
///
///   * field 5 with a zero-length item — a mint-policy item is
///     `[policy_id, asset_map]` and cannot be empty;
///   * field 2 above `max_serialized_output_preimage_bytes` — the §5.3 output
///     cap, and the case the field-access door's header names as the surviving
///     `reject_invalid_field_type` path. It is reachable only for the
///     variable-width fields: the six fixed-stride ones refuse a wrong width at
///     the read itself (`field_item_extent`), before any rule sees it.
pub fn transaction_field_item_encoded_length(
  field_index: Int,
  item_length: Int,
) -> Option<Int> {
  expect item_length >= 0
  expect field_index >= 0
  expect field_index <= 8
  if and {
    field_index == 5,
    item_length == 0,
  } {
    None
  } else if and {
    field_index == 2,
    item_length > max_serialized_output_preimage_bytes,
  } {
    None
  } else {
    Some(canonical_argument_header_size(item_length) + item_length)
  }
}

const consensus_profile_v1_id = #"6d6964676172642d636f6e73656e7375732d7631"

pub fn decode_validation_context(context_cbor: ByteArray) -> ValidationContextV1 {
  expect Some(data) = cbor.deserialise(context_cbor)
  expect
      [
        version_data, profile_id_data, block_end_time_data,
        expected_network_id_data, min_fee_a_data, min_fee_b_data,
        block_slot_data,
      ]
    = builtin.un_list_data(data)
  expect builtin.un_i_data(version_data) == 1
  expect builtin.un_b_data(profile_id_data) == consensus_profile_v1_id
  let block_end_time_ms = builtin.un_i_data(block_end_time_data)
  let expected_network_id = builtin.un_i_data(expected_network_id_data)
  let min_fee_a = builtin.un_i_data(min_fee_a_data)
  let min_fee_b = builtin.un_i_data(min_fee_b_data)
  let block_slot = builtin.un_i_data(block_slot_data)
  expect block_end_time_ms >= 0
  expect or {
      expected_network_id == 0,
      expected_network_id == 1,
    }
  expect min_fee_a >= 0
  expect min_fee_b >= 0
  expect block_slot >= 0
  ValidationContextV1 {
    block_end_time_ms,
    expected_network_id,
    min_fee_a,
    min_fee_b,
    block_slot,
  }
}

// C49 (#540). The canonical parameterized minimum-Ada floor for one ledger
// output, in lovelace, at the target-network protocol parameters. It is the
// Cardano rule verbatim -- `coins_per_utxo_byte * (overhead + serialized output
// bytes)` -- where `min_ada_output_overhead_bytes` is the fixed 160-byte UTxO
// entry overhead the target ledger adds to every output before pricing it. The
// `coins_per_utxo_byte` this is instantiated at is `env.coins_per_utxo_byte`,
// the compiled deployment pin of the C70 target snapshot
// (`PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.coinsPerUtxoByte == 4_310`), held
// identical in `env/default.ak` and `env/testnet.ak`.
//
// THE TYPESCRIPT TWIN IS A HAND MIRROR, NOT A LEDGER-LIBRARY CALL:
// `MIN_ADA_OUTPUT_OVERHEAD_BYTES_V1`/`minAdaLovelaceV1`/`outputMeetsMinAdaV1` in
// demo/midgard-validation/src/value-accounting.ts. An earlier version of this
// comment claimed the twin computed the floor through `CML.min_ada_required` in
// `minimumLovelaceForValidationProofItemPublicationV1`
// (demo/midgard-sdk/src/fraud-proof/validation-proof-item.ts); that call
// funds a _proof publication output_ on L1 and is not this consensus quantity
// (decision 0003, Q27). Because both copies are written by hand, they are bound
// to each other on pinned absolute lovelace vectors -- including the
// exactly-at-the-floor accept and the one-lovelace-below reject legs -- by
// demo/midgard-validation/tests/min-ada-twin-cross-check.test.ts and by
// `min_ada_absolute_vectors_match_the_typescript_twin` in the adjacent test
// module. The TypeScript suite parses this declaration and the two functions
// below out of this file as source text, so a slope, an intercept, a comparison or a
// reshaped declaration on either side alone fails loudly (D-S4, #618).
//
// NO ARBITRARY SUB-CAP. The floor is exactly the target-network limit: it is
// linear in the serialized size with slope `coins_per_utxo_byte` and carries no
// additional Midgard margin, so an output the target network funds adequately
// is never rejected here for carrying too little Ada.
//
// WIRED (#618 ruling 1; R8 of decision 0005). The counterpart duty -- binding
// this floor to a machine rejection code -- is discharged in
// `value_and_mint_stage_three` below: the ValueAndMint output-descriptor step
// evaluates `output_meets_min_ada_v1` over the descriptor it has just
// authenticated and, on failure, takes the exact rejecting terminal carrying
// `reject_min_ada` (`E_MIN_ADA`). The rate is a compiled deployment constant,
// not an operator-declared field, per the owner ruling on #627 (option B): a
// rate change is a redeploy event.
pub const min_ada_output_overhead_bytes = 160

pub fn min_ada_lovelace_v1(
  coins_per_utxo_byte: Int,
  serialized_output_bytes: Int,
) -> Int {
  coins_per_utxo_byte * (
    min_ada_output_overhead_bytes + serialized_output_bytes
  )
}

pub fn output_meets_min_ada_v1(
  coins_per_utxo_byte: Int,
  serialized_output_bytes: Int,
  lovelace: Int,
) -> Bool {
  lovelace >= min_ada_lovelace_v1(coins_per_utxo_byte, serialized_output_bytes)
}

pub fn rejected_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  rejection_code: ByteArray,
) -> Bool {
  and {
    post.phase == validation_trace_v1.Terminal,
    post.verdict == validation_trace_v1.Rejected,
    post.rejection_code_hash == validation_trace_v1.hash_rejection_code(
      rejection_code,
    ),
    // NOTE: there is deliberately NO clause clearing `post.ledger_delta_root`
    // here, and one must not be re-introduced. `ledger_delta_root` is the
    // operator's immutable claimed delta commitment for the transaction under
    // adjudication: it is written by no transition, is read only at
    // `ledger_delta_operation` membership and at the accepting terminal, and is
    // pinned pre == post by `immutable_context_matches`. Requiring the
    // rejecting terminal to *write* the empty commitment contradicted that
    // immutability, which made every rejection one-step unprovable from any
    // pre-state whose claimed delta is non-empty -- i.e. from every adversarial
    // pre-state that matters, since a real transaction always claims a
    // non-empty delta. The "a rejecting terminal derives no operations" duty is
    // discharged elsewhere, per `docs/consensus-profile-v1.md` section 8: by
    // `encode_terminal_rejection_witness` (post root = prior root, empty
    // operation list), by the `pre_utxos_root == post_utxos_root` clause in
    // `validation-claim-v1.ak`, and by the unilateral no-op faults in
    // `fraud-proofs/transition-trace/proof.ak`.
    post.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      pre.program_counter + 1,
      encode_terminal_rejection_witness(rejection_code, pre.prior_ledger_root),
    ),
  }
}

pub const max_tx_size_derived_collection_item_count: Int =
  bounded_collection_v1.max_tx_size_derived_item_count

pub const max_serialized_output_preimage_bytes: Int = 16384

pub fn validity_interval_is_malformed(
  validity_interval_start: Int,
  validity_interval_end: Int,
) -> Bool {
  or {
    validity_interval_start < -1,
    validity_interval_end < -1,
    and {
      validity_interval_start >= 0,
      validity_interval_end >= 0,
      validity_interval_start > validity_interval_end,
    },
  }
}

pub const max_aggregate_field_preimage_bytes: Int = 32768

pub fn validity_interval_contains_slot(
  body: NativeTxBodyCompact,
  block_slot: Int,
) -> Bool {
  and {
    or {
      body.validity_interval_start < 0,
      block_slot >= body.validity_interval_start,
    },
    or {
      body.validity_interval_end < 0,
      block_slot <= body.validity_interval_end,
    },
  }
}

pub fn signer_frontier_matches(
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  peaks: List<validation_merkle_v1.FrontierPeak>,
) -> Bool {
  and {
    validation_merkle_v1.frontier_is_well_formed(signer_count, peaks),
    validation_merkle_v1.frontier_commitment(signer_count, peaks) == signer_frontier_commitment,
  }
}

pub fn signer_membership_is_valid(
  signer_hash: ByteArray,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  peaks: List<validation_merkle_v1.FrontierPeak>,
  signer_index: Int,
  siblings: List<ByteArray>,
) -> Bool {
  and {
    bytearray.length(signer_hash) == 28,
    signer_frontier_matches(signer_count, signer_frontier_commitment, peaks),
    validation_merkle_v1.verify_membership(
      signer_count,
      peaks,
      signer_index,
      script_proof_v1.signer_leaf_hash(signer_hash),
      siblings,
    ),
  }
}

pub fn signer_non_membership_is_valid(
  signer_hash: ByteArray,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  proof: SignerSetProofV1,
) -> Bool {
  when proof is {
    EmptySignerSetProof { peaks } -> and {
        signer_count == 0,
        signer_frontier_matches(signer_count, signer_frontier_commitment, peaks),
      }
    SignerBelowFirstProof { peaks, first_signer_hash, siblings } -> and {
        bytearray.compare(signer_hash, first_signer_hash) == Less,
        signer_membership_is_valid(
          first_signer_hash,
          signer_count,
          signer_frontier_commitment,
          peaks,
          0,
          siblings,
        ),
      }
    SignerAboveLastProof { peaks, last_signer_hash, siblings } -> and {
        signer_count > 0,
        bytearray.compare(last_signer_hash, signer_hash) == Less,
        signer_membership_is_valid(
          last_signer_hash,
          signer_count,
          signer_frontier_commitment,
          peaks,
          signer_count - 1,
          siblings,
        ),
      }
    SignerBetweenProof {
      peaks,
      lower_index,
      lower_signer_hash,
      lower_siblings,
      upper_signer_hash,
      upper_siblings,
    } -> and {
        lower_index >= 0,
        lower_index + 1 < signer_count,
        bytearray.compare(lower_signer_hash, signer_hash) == Less,
        bytearray.compare(signer_hash, upper_signer_hash) == Less,
        signer_membership_is_valid(
          lower_signer_hash,
          signer_count,
          signer_frontier_commitment,
          peaks,
          lower_index,
          lower_siblings,
        ),
        signer_membership_is_valid(
          upper_signer_hash,
          signer_count,
          signer_frontier_commitment,
          peaks,
          lower_index + 1,
          upper_siblings,
        ),
      }
    NoSignerSetProof -> False
    SignerMembershipProof { .. } -> False
  }
}

pub fn input_signer_authorization(
  source_kind: Int,
  address: MidgardAddress,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  signer_proof: SignerSetProofV1,
) -> InputSignerAuthorizationV1 {
  if source_kind == 1 {
    if signer_proof == NoSignerSetProof {
      InputSignerAuthorized
    } else {
      InputSignerProofMalformed
    }
  } else {
    expect source_kind == 0
    payment_credential_signer_authorization(
      address.payment_credential,
      signer_count,
      signer_frontier_commitment,
      signer_proof,
    )
  }
}

pub fn payment_credential_signer_authorization(
  payment_credential: MidgardCredential,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  signer_proof: SignerSetProofV1,
) -> InputSignerAuthorizationV1 {
  when payment_credential is {
    ScriptCredential(_) ->
      if signer_proof == NoSignerSetProof {
        InputSignerAuthorized
      } else {
        InputSignerProofMalformed
      }
    PubKeyCredential(signer_hash) ->
      when signer_proof is {
        SignerMembershipProof { peaks, signer_index, siblings } ->
          if signer_membership_is_valid(
            signer_hash,
            signer_count,
            signer_frontier_commitment,
            peaks,
            signer_index,
            siblings,
          ) {
            InputSignerAuthorized
          } else {
            InputSignerProofMalformed
          }
        _ ->
          if signer_non_membership_is_valid(
            signer_hash,
            signer_count,
            signer_frontier_commitment,
            signer_proof,
          ) {
            InputSignerMissing
          } else {
            InputSignerProofMalformed
          }
      }
  }
}

pub fn script_discovery_bit(index: Int) -> Int {
  expect index >= 0
  if index == 0 {
    1
  } else {
    2 * script_discovery_bit(index - 1)
  }
}

pub fn script_discovery_bitmap_has(bitmap: Int, index: Int) -> Bool {
  let bit = script_discovery_bit(index)
  bitmap / bit % 2 == 1
}

pub fn script_discovery_bitmap_insert(bitmap: Int, index: Int) -> Int {
  if script_discovery_bitmap_has(bitmap, index) {
    bitmap
  } else {
    bitmap + script_discovery_bit(index)
  }
}

pub fn append_script_execution(
  discovery: ScriptDiscoveryControlV1,
  purpose_leaf: ByteArray,
  source_leaf: ByteArray,
  language_tag: Int,
  redeemer_leaf: ByteArray,
) -> validation_merkle_v1.BuiltFrontier {
  validation_merkle_v1.BuiltFrontier {
    count: discovery.execution_count + 1,
    peaks: validation_merkle_v1.append_leaf(
      discovery.execution_count,
      discovery.execution_peaks,
      script_proof_v1.execution_leaf_hash(
        language_tag,
        purpose_leaf,
        source_leaf,
        redeemer_leaf,
      ),
    ),
  }
}

pub fn current_script_purpose_leaf(
  discovery: ScriptDiscoveryControlV1,
) -> ByteArray {
  script_proof_v1.purpose_leaf_hash(
    discovery.current_purpose_kind,
    discovery.current_purpose_index,
    discovery.current_script_hash,
    discovery.current_subject,
  )
}

pub fn redeemer_tag_for_purpose_kind_v1(purpose_kind: Int) -> Option<Int> {
  if purpose_kind == 0 {
    Some(0)
  } else if purpose_kind == 1 {
    Some(1)
  } else if purpose_kind == 2 {
    Some(3)
  } else if purpose_kind == 3 {
    Some(6)
  } else {
    None
  }
}

pub fn redeemer_pointer_matches_purpose_v1(
  purpose_kind: Int,
  purpose_index: Int,
  redeemer_tag: Int,
  redeemer_index: Int,
) -> Bool {
  when redeemer_tag_for_purpose_kind_v1(purpose_kind) is {
    None -> False
    Some(expected_tag) -> and {
        redeemer_tag == expected_tag,
        redeemer_index == purpose_index,
      }
  }
}

pub fn purpose_kind_to_redeemer_tag(purpose_kind: Int) -> Int {
  expect Some(redeemer_tag) = redeemer_tag_for_purpose_kind_v1(purpose_kind)
  redeemer_tag
}

const mint_asset_leaf_domain = #"4d6964676172644d696e7441737365744c6561665631"

pub const no_rejection_code_hash =
  #"0000000000000000000000000000000000000000000000000000000000000000"

pub const value_map_maximum_proof_steps = 16

pub fn mint_asset_leaf_hash(
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
) -> ByteArray {
  expect bytearray.length(policy_id) == 28
  expect bytearray.length(asset_name) <= 32
  expect quantity != 0
  mint_asset_leaf_domain
    |> bytearray.concat(encode_definite_bytes(policy_id))
    |> bytearray.concat(encode_definite_bytes(asset_name))
    |> bytearray.concat(cbor.serialise(quantity))
    |> blake2b_256
}

const ledger_delta_operation_domain =
  #"4d69646761726456616c69646174696f6e4c656467657244656c74614f7065726174696f6e5631"

pub fn ledger_delta_operation_leaf_hash(
  operation_kind: Int,
  key: ByteArray,
  value: ByteArray,
  descriptor: mpf_proof_fold_v1.ProofDescriptorV1,
) -> ByteArray {
  expect operation_kind == 0 || operation_kind == 1
  expect mpf_proof_fold_v1.descriptor_is_well_formed_v1(descriptor)
  expect
    if operation_kind == 0 {
      value == #""
    } else {
      True
    }
  ledger_delta_operation_domain
    |> bytearray.concat(cbor.serialise(operation_kind))
    |> bytearray.concat(encode_definite_bytes(key))
    |> bytearray.concat(encode_definite_bytes(value))
    |> bytearray.concat(
        mpf_proof_fold_v1.encode_proof_descriptor_v1(descriptor),
      )
    |> blake2b_256
}

pub fn cek_machine_state_hash(state: cek_machine_v1.MachineStateV1) -> ByteArray {
  cek_proof_v1.hash_machine_state_v1(
    state.mode,
    state.execution_index,
    state.focus_root,
    state.environment_root,
    state.continuation_root,
    state.auxiliary,
    state.cpu,
    state.memory,
  )
}

pub fn prepend_cek_observer_item_v1(
  observer_hash: ByteArray,
  midgard_encoding: Bool,
  tail: cek_data_v1.DataSequenceSummaryV1,
) -> cek_data_v1.DataSequenceSummaryV1 {
  expect bytearray.length(observer_hash) == 28
  if midgard_encoding {
    cek_data_v1.prepend_data_list_summary_v1(
      builtin.b_data(observer_hash)
        |> cek_data_v1.semantic_data_summary_v1,
      tail,
    )
  } else {
    cek_data_v1.prepend_data_pair_summary_v1(
      builtin.constr_data(1, [builtin.b_data(observer_hash)])
        |> cek_data_v1.semantic_data_summary_v1,
      builtin.i_data(0)
        |> cek_data_v1.semantic_data_summary_v1,
      tail,
    )
  }
}

pub fn finalize_cek_observer_items_v1(
  items: cek_data_v1.DataSequenceSummaryV1,
  midgard_encoding: Bool,
) -> cek_data_v1.DataSummaryV1 {
  if midgard_encoding {
    cek_data_v1.list_data_summary_v1(items)
  } else {
    cek_data_v1.map_data_summary_v1(items)
  }
}

pub fn prepend_mint_asset_summary(
  asset_name: ByteArray,
  quantity: Int,
  tail: cek_data_v1.DataSequenceSummaryV1,
) -> cek_data_v1.DataSequenceSummaryV1 {
  cek_data_v1.prepend_data_pair_summary_v1(
    builtin.b_data(asset_name) |> cek_data_v1.semantic_data_summary_v1,
    builtin.i_data(quantity) |> cek_data_v1.semantic_data_summary_v1,
    tail,
  )
}
