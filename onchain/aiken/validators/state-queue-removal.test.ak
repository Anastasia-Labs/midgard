use aiken/builtin
use aiken/crypto
use aiken/interval.{between}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, ScriptPurpose,
}
use midgard/availability_challenge as availability
use midgard/ledger_state.{ConfirmedState, HeaderV1, protocol_version_v1}
use midgard/state_queue
use state_queue as state_queue_validator

const state_queue_policy =
  #"11111111111111111111111111111111111111111111111111111111"

const faulty_header_hash =
  #"22222222222222222222222222222222222222222222222222222222"

const intermediate_header_hash =
  #"33333333333333333333333333333333333333333333333333333333"

const descendant_header_hash =
  #"44444444444444444444444444444444444444444444444444444444"

const parent_header_hash =
  #"55555555555555555555555555555555555555555555555555555555"

const faulty_operator =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const rotated_operator =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const da_attestation_policy =
  #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

const da_bond_asset_name =
  #"4441424edddddddddddddddddddddddddddddddddddddddddddddddddddddddd"

const challenge_asset_name =
  #"44414348eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

const other_challenge_asset_name =
  #"44414348ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

const availability_policy =
  #"abababababababababababababababababababababababababababab"

fn availability_status_mint_bond_redeemer() -> availability.MintRedeemerV1 {
  availability.MintBondFromAttestation {
    yield_to_ref_input_index: 0,
    hub_oracle_ref_input_index: 0,
    da_attestation_input_index: 1,
    da_attestation_mint_redeemer_index: 0,
    bond_output_index: 1,
    state_queue_input_index: 2,
    state_queue_output_index: 3,
  }
}

fn availability_status_redeemers(
  redeemer: availability.MintRedeemerV1,
) -> List<Pair<ScriptPurpose, Data>> {
  let data: Data = redeemer
  [Pair(Mint(availability_policy), data)]
}

test availability_status_spend_accepts_exact_mint_bond_binding() {
  state_queue_validator.availability_status_update_is_authorized_v1(
    availability_status_redeemers(availability_status_mint_bond_redeemer()),
    availability_policy,
    0,
    2,
    3,
  )
}

test availability_status_spend_rejects_wrong_input_or_output_index() {
  and {
    !state_queue_validator.availability_status_update_is_authorized_v1(
      availability_status_redeemers(availability_status_mint_bond_redeemer()),
      availability_policy,
      0,
      1,
      3,
    ),
    !state_queue_validator.availability_status_update_is_authorized_v1(
      availability_status_redeemers(availability_status_mint_bond_redeemer()),
      availability_policy,
      0,
      2,
      4,
    ),
  }
}

test availability_status_spend_rejects_timeout_removal_arm() {
  let timeout =
    availability.TimeoutChallenge {
      yield_to_ref_input_index: 0,
      hub_oracle_ref_input_index: 0,
      bond_input_index: 1,
      terminal_accumulator_input_index: 2,
      state_queue_mint_redeemer_index: 0,
      da_slash_output_index: 0,
      challenger_refund_output_index: 1,
    }
  !state_queue_validator.availability_status_update_is_authorized_v1(
    availability_status_redeemers(timeout),
    availability_policy,
    0,
    2,
    3,
  )
}

test availability_status_spend_rejects_wrong_policy() fail {
  state_queue_validator.availability_status_update_is_authorized_v1(
    availability_status_redeemers(availability_status_mint_bond_redeemer()),
    #"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd",
    0,
    2,
    3,
  )
}

const empty_root =
  #"0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

fn header(operator: ByteArray, previous_header_hash: ByteArray) -> HeaderV1 {
  HeaderV1 {
    prev_utxos_root: empty_root,
    utxos_root: empty_root,
    withdrawals_root: empty_root,
    forced_transactions_root: empty_root,
    transactions_root: empty_root,
    deposits_root: empty_root,
    transition_trace_root: empty_root,
    event_to_step_root: empty_root,
    validation_traces_root: empty_root,
    withdrawal_count: 0,
    forced_transaction_count: 0,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 0,
    transition_step_count: 0,
    validation_trace_count: 0,
    start_time: 1_000,
    end_time: 2_000,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: previous_header_hash,
    operator_vkey: operator,
    protocol_version: protocol_version_v1,
  }
}

fn out_ref(output_index: Int) -> OutputReference {
  OutputReference {
    transaction_id: #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
    output_index,
  }
}

fn node_asset_name(header_hash: ByteArray) -> ByteArray {
  state_queue.block_asset_name_prefix
    |> bytearray.concat(header_hash)
}

fn node_input(
  output_index: Int,
  header_hash: ByteArray,
  node_header: HeaderV1,
  link: Option<ByteArray>,
) -> Input {
  node_input_with_attestation(
    output_index,
    header_hash,
    node_header,
    state_queue.no_da_attestation,
    link,
  )
}

fn node_input_with_attestation(
  output_index: Int,
  header_hash: ByteArray,
  node_header: HeaderV1,
  da_attestation: availability.StateQueueStatusV1,
  link: Option<ByteArray>,
) -> Input {
  let datum: state_queue.Datum =
    linked_list.Element {
      data: linked_list.Node(
        state_queue.StateQueueNode {
          header: node_header,
          da_attestation,
          proven_fraud: None,
        },
      ),
      link,
    }
  Input {
    output_reference: out_ref(output_index),
    output: Output {
      address: address.from_script(state_queue_policy),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(state_queue_policy, node_asset_name(header_hash), 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn root_input(output_index: Int, link: Option<ByteArray>) -> Input {
  let datum: state_queue.Datum =
    linked_list.Element {
      data: linked_list.Root(
        ConfirmedState {
          header_hash: parent_header_hash,
          prev_header_hash: intermediate_header_hash,
          utxo_root: empty_root,
          start_time: 0,
          end_time: 1_000,
          protocol_version: protocol_version_v1,
        },
      ),
      link,
    }
  Input {
    output_reference: out_ref(output_index),
    output: Output {
      address: address.from_script(state_queue_policy),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(
            state_queue_policy,
            state_queue.confirmed_state_asset_name,
            1,
          ),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn hash_header(node_header: HeaderV1) -> ByteArray {
  crypto.blake2b_224(builtin.serialise_data(node_header))
}

fn continue_node(input: Input, link: Option<ByteArray>) -> Output {
  expect InlineDatum(input_datum_data) = input.output.datum
  expect linked_list.Element { data, .. }: state_queue.Datum = input_datum_data
  let continued_datum: state_queue.Datum = linked_list.Element { data, link }
  Output { ..input.output, datum: InlineDatum(continued_datum) }
}

fn attach_da_bond(input: Input, bond_asset_name: ByteArray) -> Output {
  expect InlineDatum(input_datum_data) = input.output.datum
  expect linked_list.Element {
    data: linked_list.Node { data: input_node },
    link,
  }: state_queue.Datum = input_datum_data
  let continued_datum: state_queue.Datum =
    linked_list.Element {
      data: linked_list.Node(
        state_queue.StateQueueNode {
          ..input_node,
          da_attestation: availability.Attested {
            da_bond_asset_name: bond_asset_name,
          },
        },
      ),
      link,
    }
  Output { ..input.output, datum: InlineDatum(continued_datum) }
}

fn burn_node(header_hash: ByteArray) -> assets.Value {
  assets.from_asset(state_queue_policy, node_asset_name(header_hash), -1)
}

/// W-C9 regression: after an earlier child was pruned, the faulty anchor can
/// point directly at a later descendant.  The descendant retains its original
/// predecessor hash and may have been committed by a rotated operator; neither
/// fact weakens the authenticated linked-list descendant relation.
test state_queue_removal_prunes_transitive_descendant_from_rotated_operator() {
  let fraudulent_anchor =
    node_input(
      0,
      faulty_header_hash,
      header(faulty_operator, parent_header_hash),
      Some(descendant_header_hash),
    )
  let transitive_descendant =
    node_input(
      1,
      descendant_header_hash,
      header(rotated_operator, intermediate_header_hash),
      None,
    )
  state_queue_validator.remove_fraudulent_blocks_link_v1(
    state_queue_policy,
    inputs: [fraudulent_anchor, transitive_descendant],
    outputs: [continue_node(fraudulent_anchor, None)],
    mint: burn_node(descendant_header_hash),
    fraudulent_blocks_header_hash: faulty_header_hash,
    fraudulent_node_input_outref: fraudulent_anchor.output_reference,
    fraudulent_node_output_index: 0,
  )
}

/// The descendant-pruning route stays tied to the fraud-proved anchor.  A
/// caller cannot use a proof for one header to prune from another queue node.
test state_queue_removal_rejects_wrong_fraud_proved_anchor() fail {
  let fraudulent_anchor =
    node_input(
      0,
      faulty_header_hash,
      header(faulty_operator, parent_header_hash),
      Some(descendant_header_hash),
    )
  let descendant =
    node_input(
      1,
      descendant_header_hash,
      header(rotated_operator, faulty_header_hash),
      None,
    )
  state_queue_validator.remove_fraudulent_blocks_link_v1(
    state_queue_policy,
    inputs: [fraudulent_anchor, descendant],
    outputs: [continue_node(fraudulent_anchor, None)],
    mint: burn_node(descendant_header_hash),
    fraudulent_blocks_header_hash: intermediate_header_hash,
    fraudulent_node_input_outref: fraudulent_anchor.output_reference,
    fraudulent_node_output_index: 0,
  )
}

/// W-T5 terminal-branch control: once every descendant is gone, the target
/// itself can be removed only when its header is terminal and names the
/// operator being slashed.
test state_queue_removal_removes_terminal_fraud_proved_block() {
  let parent =
    node_input(
      0,
      parent_header_hash,
      header(rotated_operator, intermediate_header_hash),
      Some(faulty_header_hash),
    )
  let fraudulent_tail =
    node_input(
      1,
      faulty_header_hash,
      header(faulty_operator, parent_header_hash),
      None,
    )
  state_queue_validator.remove_last_fraudulent_block_v1(
    state_queue_policy,
    inputs: [parent, fraudulent_tail],
    outputs: [continue_node(parent, None)],
    mint: burn_node(faulty_header_hash),
    fraudulent_operator: faulty_operator,
    fraudulent_blocks_header_hash: faulty_header_hash,
    anchor_element_input_outref: parent.output_reference,
    anchor_element_output_index: 0,
  )
}

/// The target-removal route cannot slash a different operator from the one
/// carried by the fraud-proved block header.
test state_queue_removal_rejects_wrong_target_operator() fail {
  let parent =
    node_input(
      0,
      parent_header_hash,
      header(rotated_operator, intermediate_header_hash),
      Some(faulty_header_hash),
    )
  let fraudulent_tail =
    node_input(
      1,
      faulty_header_hash,
      header(faulty_operator, parent_header_hash),
      None,
    )
  state_queue_validator.remove_last_fraudulent_block_v1(
    state_queue_policy,
    inputs: [parent, fraudulent_tail],
    outputs: [continue_node(parent, None)],
    mint: burn_node(faulty_header_hash),
    fraudulent_operator: rotated_operator,
    fraudulent_blocks_header_hash: faulty_header_hash,
    anchor_element_input_outref: parent.output_reference,
    anchor_element_output_index: 0,
  )
}

/// The target itself cannot be removed while it still has a descendant; the
/// link-pruning route must be used first so correction remains ordered.
test state_queue_removal_rejects_non_terminal_target() fail {
  let parent =
    node_input(
      0,
      parent_header_hash,
      header(rotated_operator, intermediate_header_hash),
      Some(faulty_header_hash),
    )
  let fraudulent_non_tail =
    node_input(
      1,
      faulty_header_hash,
      header(faulty_operator, parent_header_hash),
      Some(descendant_header_hash),
    )
  state_queue_validator.remove_last_fraudulent_block_v1(
    state_queue_policy,
    inputs: [parent, fraudulent_non_tail],
    outputs: [continue_node(parent, Some(descendant_header_hash))],
    mint: burn_node(faulty_header_hash),
    fraudulent_operator: faulty_operator,
    fraudulent_blocks_header_hash: faulty_header_hash,
    anchor_element_input_outref: parent.output_reference,
    anchor_element_output_index: 0,
  )
}

// ---------------------------------------------------------------------------
// Q61 — permissionless attestation-timeout correction.
// ---------------------------------------------------------------------------

test attestation_timeout_accepts_exact_boundary_and_rejects_adjacent_premature() {
  let timed_out_header = header(faulty_operator, parent_header_hash)
  let timed_out_header_hash = hash_header(timed_out_header)
  let node =
    state_queue.StateQueueNode {
      proven_fraud: None,
      header: timed_out_header,
      da_attestation: state_queue.no_da_attestation,
    }
  let boundary =
    timed_out_header.end_time + state_queue.da_attestation_timeout_v1
  and {
    state_queue.da_attestation_timeout_v1 == 3_600_000,
    state_queue_validator.unattested_block_timeout_elapsed_v1(
      timed_out_header_hash,
      timed_out_header_hash,
      node,
      between(boundary, boundary + 1_000),
    ),
    !state_queue_validator.unattested_block_timeout_elapsed_v1(
      timed_out_header_hash,
      timed_out_header_hash,
      node,
      between(boundary - 1, boundary + 999),
    ),
  }
}

/// Descendants are removed first while the timed-out block remains the root's
/// current head. The root reference makes this topology claim conflict with a
/// concurrent merge or head replacement.
test attestation_timeout_prunes_descendant_of_current_head() {
  let timed_out_header = header(faulty_operator, parent_header_hash)
  let timed_out_header_hash = hash_header(timed_out_header)
  let root = root_input(0, Some(timed_out_header_hash))
  let timed_out_head =
    node_input(
      1,
      timed_out_header_hash,
      timed_out_header,
      Some(descendant_header_hash),
    )
  let descendant =
    node_input(
      2,
      descendant_header_hash,
      header(rotated_operator, timed_out_header_hash),
      None,
    )
  let boundary =
    timed_out_header.end_time + state_queue.da_attestation_timeout_v1
  state_queue_validator.prune_unattested_block_descendant_v1(
    state_queue_policy,
    inputs: [timed_out_head, descendant],
    outputs: [continue_node(timed_out_head, None)],
    reference_inputs: [root],
    mint: burn_node(descendant_header_hash),
    validity_range: between(boundary, boundary + 1_000),
    timed_out_header_hash: timed_out_header_hash,
    predecessor_ref_input_index: 0,
    timed_out_node_input_outref: timed_out_head.output_reference,
    timed_out_node_output_index: 0,
  )
}

/// A reference that does not link to the target cannot authorize pruning.
test attestation_timeout_rejects_unlinked_predecessor_reference() fail {
  let timed_out_header = header(faulty_operator, parent_header_hash)
  let timed_out_header_hash = hash_header(timed_out_header)
  let root = root_input(0, Some(intermediate_header_hash))
  let timed_out_node =
    node_input(
      1,
      timed_out_header_hash,
      timed_out_header,
      Some(descendant_header_hash),
    )
  let descendant =
    node_input(
      2,
      descendant_header_hash,
      header(rotated_operator, timed_out_header_hash),
      None,
    )
  let boundary =
    timed_out_header.end_time + state_queue.da_attestation_timeout_v1
  state_queue_validator.prune_unattested_block_descendant_v1(
    state_queue_policy,
    inputs: [timed_out_node, descendant],
    outputs: [continue_node(timed_out_node, None)],
    reference_inputs: [root],
    mint: burn_node(descendant_header_hash),
    validity_range: between(boundary, boundary + 1_000),
    timed_out_header_hash: timed_out_header_hash,
    predecessor_ref_input_index: 0,
    timed_out_node_input_outref: timed_out_node.output_reference,
    timed_out_node_output_index: 0,
  )
}

test attestation_timeout_removes_terminal_unattested_head_at_boundary() {
  let timed_out_header = header(faulty_operator, parent_header_hash)
  let timed_out_header_hash = hash_header(timed_out_header)
  let root = root_input(0, Some(timed_out_header_hash))
  let timed_out_head =
    node_input(1, timed_out_header_hash, timed_out_header, None)
  let boundary =
    timed_out_header.end_time + state_queue.da_attestation_timeout_v1
  state_queue_validator.remove_last_unattested_block_v1(
    state_queue_policy,
    inputs: [root, timed_out_head],
    outputs: [continue_node(root, None)],
    mint: burn_node(timed_out_header_hash),
    validity_range: between(boundary, boundary + 1_000),
    timed_out_header_hash: timed_out_header_hash,
    predecessor_input_outref: root.output_reference,
    predecessor_output_index: 0,
  )
}

/// If `AttachDaAttestation` wins the eUTxO race, the continued node carries the
/// DA policy marker. A timeout transaction rebuilt against that new node must
/// reject even though the wall-clock boundary has passed.
test attestation_timeout_rejects_attested_head_after_race() fail {
  let timed_out_header = header(faulty_operator, parent_header_hash)
  let timed_out_header_hash = hash_header(timed_out_header)
  let root = root_input(0, Some(timed_out_header_hash))
  let attested_head =
    node_input_with_attestation(
      1,
      timed_out_header_hash,
      timed_out_header,
      availability.Attested { da_bond_asset_name },
      None,
    )
  let boundary =
    timed_out_header.end_time + state_queue.da_attestation_timeout_v1
  state_queue_validator.remove_last_unattested_block_v1(
    state_queue_policy,
    inputs: [root, attested_head],
    outputs: [continue_node(root, None)],
    mint: burn_node(timed_out_header_hash),
    validity_range: between(boundary, boundary + 1_000),
    timed_out_header_hash: timed_out_header_hash,
    predecessor_input_outref: root.output_reference,
    predecessor_output_index: 0,
  )
}

test attestation_timeout_rejects_nonterminal_head_removal() fail {
  let timed_out_header = header(faulty_operator, parent_header_hash)
  let timed_out_header_hash = hash_header(timed_out_header)
  let root = root_input(0, Some(timed_out_header_hash))
  let timed_out_head =
    node_input(
      1,
      timed_out_header_hash,
      timed_out_header,
      Some(descendant_header_hash),
    )
  let boundary =
    timed_out_header.end_time + state_queue.da_attestation_timeout_v1
  state_queue_validator.remove_last_unattested_block_v1(
    state_queue_policy,
    inputs: [root, timed_out_head],
    outputs: [continue_node(root, Some(descendant_header_hash))],
    mint: burn_node(timed_out_header_hash),
    validity_range: between(boundary, boundary + 1_000),
    timed_out_header_hash: timed_out_header_hash,
    predecessor_input_outref: root.output_reference,
    predecessor_output_index: 0,
  )
}

test attestation_timeout_rejects_wrong_header_identity() fail {
  let timed_out_header = header(faulty_operator, parent_header_hash)
  let timed_out_header_hash = hash_header(timed_out_header)
  let root = root_input(0, Some(timed_out_header_hash))
  let timed_out_head =
    node_input(1, timed_out_header_hash, timed_out_header, None)
  let boundary =
    timed_out_header.end_time + state_queue.da_attestation_timeout_v1
  state_queue_validator.remove_last_unattested_block_v1(
    state_queue_policy,
    inputs: [root, timed_out_head],
    outputs: [continue_node(root, None)],
    mint: burn_node(timed_out_header_hash),
    validity_range: between(boundary, boundary + 1_000),
    timed_out_header_hash: descendant_header_hash,
    predecessor_input_outref: root.output_reference,
    predecessor_output_index: 0,
  )
}

/// The append fence closes the starvation race at the same boundary used by
/// timeout correction: an unattested head may be extended only when the append
/// window ends strictly before it. A head that attached in time remains
/// appendable later; an unknown marker does not masquerade as that attachment.
test attestation_timeout_append_fence_pins_boundary_and_marker_identity() {
  let head_header = header(faulty_operator, parent_header_hash)
  let boundary = head_header.end_time + state_queue.da_attestation_timeout_v1
  and {
    state_queue_validator.state_queue_head_allows_append_v1(
      state_queue.StateQueueNode {
        proven_fraud: None,
        header: head_header,
        da_attestation: state_queue.no_da_attestation,
      },
      da_attestation_policy,
      between(boundary - 1_000, boundary - 1),
    ),
    !state_queue_validator.state_queue_head_allows_append_v1(
      state_queue.StateQueueNode {
        proven_fraud: None,
        header: head_header,
        da_attestation: state_queue.no_da_attestation,
      },
      da_attestation_policy,
      between(boundary - 1_000, boundary),
    ),
    state_queue_validator.state_queue_head_allows_append_v1(
      state_queue.StateQueueNode {
        proven_fraud: None,
        header: head_header,
        da_attestation: availability.Attested { da_bond_asset_name },
      },
      da_attestation_policy,
      between(boundary, boundary + 1_000),
    ),
    !state_queue_validator.state_queue_head_allows_append_v1(
      state_queue.StateQueueNode {
        proven_fraud: None,
        header: head_header,
        da_attestation: availability.Challenged {
          da_bond_asset_name,
          challenge_asset_name: da_bond_asset_name,
        },
      },
      da_attestation_policy,
      between(boundary - 1_000, boundary - 1),
    ),
  }
}

/// The append fence resolves the singleton root's head both when the consumed
/// tail is that head and when a deeper queue needs a separate authenticated
/// head reference. This is the topology seam that makes tail extension observe
/// timeout pruning.
test attestation_timeout_append_fence_authenticates_single_and_deep_queue_heads() {
  let head_header = header(faulty_operator, parent_header_hash)
  let head_header_hash = hash_header(head_header)
  let head_node =
    state_queue.StateQueueNode {
      proven_fraud: None,
      header: head_header,
      da_attestation: state_queue.no_da_attestation,
    }
  let head_data: Data = head_node
  let root = root_input(0, Some(head_header_hash))
  let head_ref = node_input(1, head_header_hash, head_header, None)
  let tail_data: Data =
    state_queue.StateQueueNode {
      proven_fraud: None,
      header: header(rotated_operator, head_header_hash),
      da_attestation: state_queue.no_da_attestation,
    }
  and {
    state_queue_validator.authenticated_queue_head_for_append_v1(
      [root],
      state_queue_policy,
      head_header_hash,
      head_data,
      0,
      None,
    ) == head_node,
    state_queue_validator.authenticated_queue_head_for_append_v1(
      [root, head_ref],
      state_queue_policy,
      descendant_header_hash,
      tail_data,
      0,
      Some(1),
    ) == head_node,
  }
}

test attestation_timeout_append_fence_rejects_missing_deep_head_reference() fail {
  let head_header = header(faulty_operator, parent_header_hash)
  let head_header_hash = hash_header(head_header)
  let root = root_input(0, Some(head_header_hash))
  let tail_data: Data =
    state_queue.StateQueueNode {
      proven_fraud: None,
      header: header(rotated_operator, head_header_hash),
      da_attestation: state_queue.no_da_attestation,
    }
  state_queue_validator.authenticated_queue_head_for_append_v1(
    [root],
    state_queue_policy,
    descendant_header_hash,
    tail_data,
    0,
    None,
  ) == state_queue.StateQueueNode {
    proven_fraud: None,
    header: head_header,
    da_attestation: state_queue.no_da_attestation,
  }
}

/// A quorum application whose window closes exactly at the deadline is the
/// last admissible attachment. It spends the same head UTxO as timeout
/// correction, so the ledger serializes the exact-boundary race.
test da_attestation_apply_accepts_window_ending_at_timeout_boundary() {
  let head_header = header(faulty_operator, parent_header_hash)
  let head_header_hash = hash_header(head_header)
  let unattested_head = node_input(0, head_header_hash, head_header, None)
  let boundary = head_header.end_time + state_queue.da_attestation_timeout_v1
  state_queue.validate_da_attestation_attachment(
    [unattested_head],
    [attach_da_bond(unattested_head, da_bond_asset_name)],
    state_queue_policy,
    0,
    0,
    head_header_hash,
    da_bond_asset_name,
    between(boundary - 1_000, boundary),
  )
}

/// No transaction whose validity window extends beyond the deadline can make
/// a late quorum win by landing before timeout correction.
test da_attestation_apply_rejects_window_ending_after_timeout() fail {
  let head_header = header(faulty_operator, parent_header_hash)
  let head_header_hash = hash_header(head_header)
  let unattested_head = node_input(0, head_header_hash, head_header, None)
  let boundary = head_header.end_time + state_queue.da_attestation_timeout_v1
  state_queue.validate_da_attestation_attachment(
    [unattested_head],
    [attach_da_bond(unattested_head, da_bond_asset_name)],
    state_queue_policy,
    0,
    0,
    head_header_hash,
    da_bond_asset_name,
    between(boundary - 999, boundary + 1),
  )
}

// ---------------------------------------------------------------------------
// Q58 — challenge-timeout correction.
// ---------------------------------------------------------------------------

test availability_timeout_prunes_descendant_from_exact_challenged_head() {
  let unavailable_header = header(faulty_operator, parent_header_hash)
  let unavailable_header_hash = hash_header(unavailable_header)
  let root = root_input(0, Some(unavailable_header_hash))
  let unavailable_head =
    node_input_with_attestation(
      1,
      unavailable_header_hash,
      unavailable_header,
      availability.Challenged { da_bond_asset_name, challenge_asset_name },
      Some(descendant_header_hash),
    )
  let descendant =
    node_input(
      2,
      descendant_header_hash,
      header(rotated_operator, unavailable_header_hash),
      None,
    )
  state_queue_validator.prune_unavailable_block_descendant_v1(
    state_queue_policy,
    inputs: [unavailable_head, descendant],
    outputs: [continue_node(unavailable_head, None)],
    reference_inputs: [root],
    mint: burn_node(descendant_header_hash),
    unavailable_header_hash: unavailable_header_hash,
    challenge_asset_name: challenge_asset_name,
    confirmed_state_ref_input_index: 0,
    unavailable_node_input_outref: unavailable_head.output_reference,
    unavailable_node_output_index: 0,
  )
}

test availability_timeout_removes_terminal_exact_challenged_head() {
  let unavailable_header = header(faulty_operator, parent_header_hash)
  let unavailable_header_hash = hash_header(unavailable_header)
  let root = root_input(0, Some(unavailable_header_hash))
  let unavailable_head =
    node_input_with_attestation(
      1,
      unavailable_header_hash,
      unavailable_header,
      availability.Challenged { da_bond_asset_name, challenge_asset_name },
      None,
    )
  state_queue_validator.remove_unavailable_head_v1(
    state_queue_policy,
    inputs: [root, unavailable_head],
    outputs: [continue_node(root, None)],
    mint: burn_node(unavailable_header_hash),
    unavailable_header_hash: unavailable_header_hash,
    challenge_asset_name: challenge_asset_name,
    confirmed_state_input_outref: root.output_reference,
    confirmed_state_output_index: 0,
  )
}

test availability_timeout_rejects_substituted_challenge_identity() fail {
  let unavailable_header = header(faulty_operator, parent_header_hash)
  let unavailable_header_hash = hash_header(unavailable_header)
  let root = root_input(0, Some(unavailable_header_hash))
  let unavailable_head =
    node_input_with_attestation(
      1,
      unavailable_header_hash,
      unavailable_header,
      availability.Challenged { da_bond_asset_name, challenge_asset_name },
      None,
    )
  state_queue_validator.remove_unavailable_head_v1(
    state_queue_policy,
    inputs: [root, unavailable_head],
    outputs: [continue_node(root, None)],
    mint: burn_node(unavailable_header_hash),
    unavailable_header_hash: unavailable_header_hash,
    challenge_asset_name: other_challenge_asset_name,
    confirmed_state_input_outref: root.output_reference,
    confirmed_state_output_index: 0,
  )
}

test availability_timeout_rejects_attested_but_unchallenged_head() fail {
  let unavailable_header = header(faulty_operator, parent_header_hash)
  let unavailable_header_hash = hash_header(unavailable_header)
  let root = root_input(0, Some(unavailable_header_hash))
  let attested_head =
    node_input_with_attestation(
      1,
      unavailable_header_hash,
      unavailable_header,
      availability.Attested { da_bond_asset_name },
      None,
    )
  state_queue_validator.remove_unavailable_head_v1(
    state_queue_policy,
    inputs: [root, attested_head],
    outputs: [continue_node(root, None)],
    mint: burn_node(unavailable_header_hash),
    unavailable_header_hash: unavailable_header_hash,
    challenge_asset_name: challenge_asset_name,
    confirmed_state_input_outref: root.output_reference,
    confirmed_state_output_index: 0,
  )
}

// Generalized timeout correction must preserve an honest prefix whose ordinary
// seven-day maturity has not elapsed. The timeout target is its immediate child.
type TimeoutSuffixFixture {
  TimeoutSuffixFixture {
    predecessor: Input,
    target: Input,
    descendant: Input,
    target_hash: ByteArray,
    boundary: Int,
  }
}

fn timeout_suffix_fixture(with_descendant: Bool) -> TimeoutSuffixFixture {
  let predecessor_header = header(rotated_operator, parent_header_hash)
  let predecessor_hash = hash_header(predecessor_header)
  let target_header =
    HeaderV1 {
      ..header(faulty_operator, predecessor_hash),
      start_time: 2_000,
      end_time: 3_000,
    }
  let target_hash = hash_header(target_header)
  let descendant_header =
    HeaderV1 {
      ..header(rotated_operator, target_hash),
      start_time: 3_000,
      end_time: 4_000,
    }
  let descendant_hash = hash_header(descendant_header)
  TimeoutSuffixFixture {
    predecessor: node_input_with_attestation(
      0,
      predecessor_hash,
      predecessor_header,
      availability.Attested { da_bond_asset_name },
      Some(target_hash),
    ),
    target: node_input(
      1,
      target_hash,
      target_header,
      if with_descendant {
        Some(descendant_hash)
      } else {
        None
      },
    ),
    descendant: node_input(2, descendant_hash, descendant_header, None),
    target_hash,
    boundary: target_header.end_time + state_queue.da_attestation_timeout_v1,
  }
}

fn remove_timeout_suffix_tail(
  fixture: TimeoutSuffixFixture,
  continued_predecessor: Output,
  lower_bound: Int,
) -> Bool {
  state_queue_validator.remove_last_unattested_block_v1(
    state_queue_policy,
    inputs: [fixture.predecessor, fixture.target],
    outputs: [continued_predecessor],
    mint: burn_node(fixture.target_hash),
    validity_range: between(lower_bound, lower_bound + 1_000),
    timed_out_header_hash: fixture.target_hash,
    predecessor_input_outref: fixture.predecessor.output_reference,
    predecessor_output_index: 0,
  )
}

fn prune_timeout_suffix_child(
  fixture: TimeoutSuffixFixture,
  predecessor_references: List<Input>,
) -> Bool {
  expect InlineDatum(descendant_datum) = fixture.descendant.output.datum
  expect linked_list.Element { data: linked_list.Node { data: node }, .. }: state_queue.Datum =
    descendant_datum
  let removed_hash = crypto.blake2b_224(builtin.serialise_data(node.header))
  state_queue_validator.prune_unattested_block_descendant_v1(
    state_queue_policy,
    inputs: [fixture.target, fixture.descendant],
    outputs: [continue_node(fixture.target, None)],
    reference_inputs: predecessor_references,
    mint: burn_node(removed_hash),
    validity_range: between(fixture.boundary, fixture.boundary + 1_000),
    timed_out_header_hash: fixture.target_hash,
    predecessor_ref_input_index: 0,
    timed_out_node_input_outref: fixture.target.output_reference,
    timed_out_node_output_index: 0,
  )
}

test unattested_timeout_removes_tail_behind_immature_attested_prefix() {
  let fixture = timeout_suffix_fixture(False)
  and {
    fixture.boundary < 2_000 + 604_800_000,
    remove_timeout_suffix_tail(
      fixture,
      continue_node(fixture.predecessor, None),
      fixture.boundary,
    ),
  }
}

test unattested_timeout_prunes_interior_then_removes_target_preserving_prefix() {
  let fixture = timeout_suffix_fixture(True)
  let pruned =
    TimeoutSuffixFixture {
      ..fixture,
      target: Input {
        ..fixture.target,
        output_reference: out_ref(3),
        output: continue_node(fixture.target, None),
      },
    }
  and {
    fixture.boundary < 2_000 + 604_800_000,
    prune_timeout_suffix_child(fixture, [fixture.predecessor]),
    remove_timeout_suffix_tail(
      pruned,
      continue_node(fixture.predecessor, None),
      fixture.boundary,
    ),
  }
}

test unattested_timeout_rejects_missing_predecessor_reference() fail {
  let fixture = timeout_suffix_fixture(True)
  prune_timeout_suffix_child(fixture, [])
}

test unattested_timeout_rejects_forged_predecessor_reference_token() fail {
  let fixture = timeout_suffix_fixture(True)
  let forged =
    Input {
      ..fixture.predecessor,
      output: Output {
        ..fixture.predecessor.output,
        value: assets.from_lovelace(2_000_000)
          |> assets.add(
              availability_policy,
              node_asset_name(parent_header_hash),
              1,
            ),
      },
    }
  prune_timeout_suffix_child(fixture, [forged])
}

test unattested_timeout_rejects_predecessor_header_hash_substitution() fail {
  let fixture = timeout_suffix_fixture(True)
  let forged =
    Input {
      ..fixture.predecessor,
      output: Output {
        ..fixture.predecessor.output,
        value: assets.from_lovelace(2_000_000)
          |> assets.add(
              state_queue_policy,
              node_asset_name(parent_header_hash),
              1,
            ),
      },
    }
  prune_timeout_suffix_child(fixture, [forged])
}

test unattested_timeout_rejects_replaced_predecessor_link() fail {
  let fixture = timeout_suffix_fixture(True)
  let replaced =
    Input {
      ..fixture.predecessor,
      output_reference: out_ref(4),
      output: continue_node(fixture.predecessor, Some(parent_header_hash)),
    }
  prune_timeout_suffix_child(fixture, [replaced])
}

test unattested_timeout_rejects_attested_interior_target_after_race() fail {
  let fixture = timeout_suffix_fixture(True)
  let attested =
    TimeoutSuffixFixture {
      ..fixture,
      target: Input {
        ..fixture.target,
        output_reference: out_ref(4),
        output: attach_da_bond(fixture.target, da_bond_asset_name),
      },
    }
  prune_timeout_suffix_child(attested, [fixture.predecessor])
}

test unattested_timeout_rejects_premature_tail_removal() fail {
  let fixture = timeout_suffix_fixture(False)
  remove_timeout_suffix_tail(
    fixture,
    continue_node(fixture.predecessor, None),
    fixture.boundary - 1,
  )
}

test unattested_timeout_rejects_nonterminal_interior_target() fail {
  let fixture = timeout_suffix_fixture(True)
  expect InlineDatum(target_datum) = fixture.target.output.datum
  expect linked_list.Element { link, .. }: state_queue.Datum = target_datum
  remove_timeout_suffix_tail(
    fixture,
    continue_node(fixture.predecessor, link),
    fixture.boundary,
  )
}

test unattested_timeout_rejects_changed_predecessor_value() fail {
  let fixture = timeout_suffix_fixture(False)
  let continued = continue_node(fixture.predecessor, None)
  let changed =
    Output { ..continued, value: continued.value |> assets.add(#"", #"", -1) }
  remove_timeout_suffix_tail(fixture, changed, fixture.boundary)
}

test unattested_timeout_rejects_changed_predecessor_datum() fail {
  let fixture = timeout_suffix_fixture(False)
  let continued = continue_node(fixture.predecessor, None)
  expect InlineDatum(datum) = continued.datum
  expect linked_list.Element { data: linked_list.Node { data: node }, link }: state_queue.Datum =
    datum
  let changed_datum: state_queue.Datum =
    linked_list.Element {
      data: linked_list.Node(
        state_queue.StateQueueNode {
          ..node,
          da_attestation: availability.Published {
            terminal_commitment: empty_root,
          },
        },
      ),
      link,
    }
  remove_timeout_suffix_tail(
    fixture,
    Output { ..continued, datum: InlineDatum(changed_datum) },
    fixture.boundary,
  )
}

// A completed proof consumes its exact queue node before maturity. Open proof
// threads do not have this authority; topology and DA status are preserved.
const completed_fraud_policy =
  #"12121212121212121212121212121212121212121212121212121212"

fn completed_fraud_asset() -> ByteArray {
  #"00000001"
    |> bytearray.concat(
        hash_header(header(faulty_operator, parent_header_hash)),
      )
}

fn marked_output(output: Output, marker: Option<ByteArray>) -> Output {
  expect InlineDatum(data) = output.datum
  expect element: state_queue.Datum = data
  expect linked_list.Node(node) = element.data
  let continued: state_queue.Datum = linked_list.Element {
    ..element,
    data: linked_list.Node(
      state_queue.StateQueueNode { ..node, proven_fraud: marker },
    ),
  }
  Output { ..output, datum: InlineDatum(continued) }
}

fn completed_fraud_tx() -> transaction.Transaction {
  let block = header(faulty_operator, parent_header_hash)
  let input = node_input(
    0,
    hash_header(block),
    block,
    Some(descendant_header_hash),
  )
  transaction.Transaction {
    ..transaction.placeholder,
    inputs: [input],
    outputs: [marked_output(input.output, Some(completed_fraud_asset()))],
    mint: assets.from_asset(completed_fraud_policy, completed_fraud_asset(), 1),
    validity_range: between(3_000, 4_000),
  }
}

fn record_completed_fraud(tx: transaction.Transaction) -> Bool {
  state_queue_validator.spend.spend(
    state_queue_policy,
    da_attestation_policy,
    availability_policy,
    completed_fraud_policy,
    None,
    state_queue.RecordCompletedFraud {
      state_queue_input_index: 0,
      state_queue_output_index: 0,
      fraud_proof_asset_name: completed_fraud_asset(),
    },
    out_ref(0),
    tx,
  )
}

test completed_fraud_marks_exact_header_with_fresh_terminal_mint() {
  record_completed_fraud(completed_fraud_tx())
}

test completed_fraud_marker_allows_permissionless_min_ada_topup() {
  let tx = completed_fraud_tx()
  expect [output] = tx.outputs
  record_completed_fraud(
    transaction.Transaction {
      ..tx,
      outputs: [
        Output {
          ..output,
          value: assets.add(output.value, #"", #"", 1_000_000),
        },
      ],
    },
  )
}

test completed_fraud_marker_rejects_missing_proof_mint() {
  let tx = completed_fraud_tx()
  !record_completed_fraud(transaction.Transaction { ..tx, mint: assets.zero })
}

test completed_fraud_marker_rejects_foreign_proof_policy() {
  let tx = completed_fraud_tx()
  !record_completed_fraud(
    transaction.Transaction {
      ..tx,
      mint: assets.from_asset(availability_policy, completed_fraud_asset(), 1),
    },
  )
}

test completed_fraud_marker_rejects_nonunit_proof_mint() {
  let tx = completed_fraud_tx()
  !record_completed_fraud(
    transaction.Transaction {
      ..tx,
      mint: assets.from_asset(
        completed_fraud_policy,
        completed_fraud_asset(),
        2,
      ),
    },
  )
}

test completed_fraud_marker_rejects_simultaneous_queue_mutation() {
  let tx = completed_fraud_tx()
  !record_completed_fraud(
    transaction.Transaction {
      ..tx,
      mint: assets.add(
        tx.mint,
        state_queue_policy,
        node_asset_name(descendant_header_hash),
        -1,
      ),
    },
  )
}

test completed_fraud_marker_rejects_lower_bound_at_header_end() {
  let tx = completed_fraud_tx()
  !record_completed_fraud(
    transaction.Transaction { ..tx, validity_range: between(2_000, 3_000) },
  )
}

test completed_fraud_marker_rejects_terminal_at_maturity() {
  let tx = completed_fraud_tx()
  let boundary = 2_000 + ledger_state.block_maturity_duration_v1
  !record_completed_fraud(
    transaction.Transaction {
      ..tx,
      validity_range: between(boundary - 1_000, boundary),
    },
  )
}

test completed_fraud_marker_accepts_last_inclusive_tick_before_maturity() {
  let tx = completed_fraud_tx()
  let boundary = 2_000 + ledger_state.block_maturity_duration_v1
  record_completed_fraud(
    transaction.Transaction {
      ..tx,
      validity_range: between(boundary - 1_000, boundary - 1),
    },
  )
}

test completed_fraud_marker_rejects_removing_locked_ada() {
  let tx = completed_fraud_tx()
  expect [output] = tx.outputs
  !record_completed_fraud(
    transaction.Transaction {
      ..tx,
      outputs: [
        Output { ..output, value: assets.add(output.value, #"", #"", -1) },
      ],
    },
  )
}

test completed_fraud_marker_rejects_changing_address() {
  let tx = completed_fraud_tx()
  expect [output] = tx.outputs
  !record_completed_fraud(
    transaction.Transaction {
      ..tx,
      outputs: [
        Output { ..output, address: address.from_script(availability_policy) },
      ],
    },
  )
}

test completed_fraud_marker_rejects_attaching_reference_script() {
  let tx = completed_fraud_tx()
  expect [output] = tx.outputs
  !record_completed_fraud(
    transaction.Transaction {
      ..tx,
      outputs: [
        Output { ..output, reference_script: Some(availability_policy) },
      ],
    },
  )
}

test completed_fraud_marker_rejects_different_output_proof() {
  let tx = completed_fraud_tx()
  expect [output] = tx.outputs
  !record_completed_fraud(
    transaction.Transaction {
      ..tx,
      outputs: [
        marked_output(
          output,
          Some(
            #"00000002"
              |> bytearray.concat(
                  hash_header(header(faulty_operator, parent_header_hash)),
                ),
          ),
        ),
      ],
    },
  )
}

test completed_fraud_marker_rejects_repeated_write() {
  let tx = completed_fraud_tx()
  expect [input] = tx.inputs
  expect [output] = tx.outputs
  !record_completed_fraud(
    transaction.Transaction { ..tx, inputs: [Input { ..input, output }] },
  )
}

test completed_fraud_marker_rejects_redirected_spending_index() fail {
  let tx = completed_fraud_tx()
  expect [input] = tx.inputs
  record_completed_fraud(
    transaction.Transaction {
      ..tx,
      inputs: [Input { ..input, output_reference: out_ref(1) }],
    },
  )
}

test completed_fraud_marked_head_cannot_append() {
  !state_queue_validator.state_queue_head_allows_append_v1(
    state_queue.StateQueueNode {
      header: header(faulty_operator, parent_header_hash),
      da_attestation: availability.Attested { da_bond_asset_name },
      proven_fraud: Some(completed_fraud_asset()),
    },
    da_attestation_policy,
    between(3_000, 4_000),
  )
}

test completed_fraud_terminal_requires_exact_new_output_marker() {
  state_queue.require_completed_fraud_witness(
    completed_fraud_tx(), state_queue_policy, completed_fraud_asset(),
    state_queue.RecordedOutput { output_index: 0 },
  )
}

test completed_fraud_terminal_rejects_an_unmarked_header() fail {
  let tx = completed_fraud_tx()
  expect [input] = tx.inputs
  state_queue.require_completed_fraud_witness(
    transaction.Transaction { ..tx, outputs: [input.output] },
    state_queue_policy, completed_fraud_asset(),
    state_queue.RecordedOutput { output_index: 0 },
  )
}

test completed_fraud_terminal_rejects_a_different_new_marker() {
  let tx = completed_fraud_tx()
  let other_proof = #"00000002" |> bytearray.concat(hash_header(header(faulty_operator, parent_header_hash)))
  !state_queue.require_completed_fraud_witness(
    tx, state_queue_policy, other_proof,
    state_queue.RecordedOutput { output_index: 0 },
  )
}

test completed_fraud_terminal_references_an_already_marked_header_without_rewriting() {
  let tx = completed_fraud_tx()
  expect [output] = tx.outputs
  let other_proof = #"00000002" |> bytearray.concat(hash_header(header(faulty_operator, parent_header_hash)))
  state_queue.require_completed_fraud_witness(
    transaction.Transaction {
      ..tx, inputs: [], outputs: [],
      reference_inputs: [Input { output_reference: out_ref(1), output }],
    },
    state_queue_policy, other_proof,
    state_queue.PreviouslyRecorded { reference_input_index: 0 },
  )
}

test completed_fraud_terminal_rejects_a_reference_to_another_header() {
  let tx = completed_fraud_tx()
  expect [output] = tx.outputs
  let other_proof = #"00000002" |> bytearray.concat(descendant_header_hash)
  !state_queue.require_completed_fraud_witness(
    transaction.Transaction { ..tx, reference_inputs: [Input { output_reference: out_ref(1), output }] },
    state_queue_policy, other_proof,
    state_queue.PreviouslyRecorded { reference_input_index: 0 },
  )
}

test completed_fraud_terminal_rejects_unmarked_reference() fail {
  let tx = completed_fraud_tx()
  state_queue.require_completed_fraud_witness(
    transaction.Transaction { ..tx, reference_inputs: tx.inputs },
    state_queue_policy, completed_fraud_asset(),
    state_queue.PreviouslyRecorded { reference_input_index: 0 },
  )
}

test completed_fraud_terminal_rejects_foreign_queue_policy() fail {
  state_queue.require_completed_fraud_witness(
    completed_fraud_tx(), availability_policy, completed_fraud_asset(),
    state_queue.RecordedOutput { output_index: 0 },
  )
}

test completed_fraud_terminal_rejects_expired_terminal_even_when_already_recorded() {
  let tx = completed_fraud_tx()
  expect [output] = tx.outputs
  let boundary = 2_000 + ledger_state.block_maturity_duration_v1
  !state_queue.require_completed_fraud_witness(
    transaction.Transaction {
      ..tx, validity_range: between(boundary, boundary + 1_000),
      reference_inputs: [Input { output_reference: out_ref(1), output }],
    },
    state_queue_policy, completed_fraud_asset(),
    state_queue.PreviouslyRecorded { reference_input_index: 0 },
  )
}

test completed_fraud_da_update_preserves_marker() {
  let tx = completed_fraud_tx()
  expect [input] = tx.inputs
  expect [marked] = tx.outputs
  expect InlineDatum(data) = marked.datum
  expect element: state_queue.Datum = data
  expect linked_list.Node(node) = element.data
  let attested = availability.Attested { da_bond_asset_name }
  let continued: state_queue.Datum = linked_list.Element {
    ..element,
    data: linked_list.Node(state_queue.StateQueueNode { ..node, da_attestation: attested }),
  }
  state_queue.validate_da_availability_status_transition(
    [Input { ..input, output: marked }],
    [Output { ..marked, datum: InlineDatum(continued) }],
    state_queue_policy, 0, 0,
    hash_header(node.header), state_queue.no_da_attestation, attested,
  )
}

test completed_fraud_da_update_cannot_clear_marker() fail {
  let tx = completed_fraud_tx()
  expect [input] = tx.inputs
  expect [marked] = tx.outputs
  let attested = availability.Attested { da_bond_asset_name }
  let block = header(faulty_operator, parent_header_hash)
  let unmarked = node_input_with_attestation(0, hash_header(block), block, attested, Some(descendant_header_hash))
  state_queue.validate_da_availability_status_transition(
    [Input { ..input, output: marked }], [unmarked.output],
    state_queue_policy, 0, 0, hash_header(block), state_queue.no_da_attestation, attested,
  )
}
