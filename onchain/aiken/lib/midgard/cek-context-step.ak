use aiken/builtin
use aiken/primitive/bytearray
use midgard/cek_constant_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_mint_order
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/compact.{decode_tx_compact_view_v1}
use midgard/script_context_v1
use midgard/validation_machine/machine_types.{
  CekContextControlV1, CekContextPartsControlV1, CekFinalContextControlV1,
  CekMintHead, CekTxInfoAssemblyControlV1, NativeScriptsControlV1,
}
use midgard/validation_machine/shared.{
  mint_asset_leaf_hash, prepend_mint_asset_summary,
}
use midgard/validation_machine/witness as witness
use midgard/validation_merkle_v1.{FrontierPeak}
use midgard/validation_trace_v1.{ValidationSourceKind}

/// Only authenticated native fields consumed while constructing a context.
pub type NativeFacts {
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  resolved_input_count: Int,
  spend_input_count: Int,
  resolved_item_peaks: List<FrontierPeak>,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  redeemer_count: Int,
  redeemer_peaks: List<FrontierPeak>,
  purpose_count: Int,
  purpose_peaks: List<FrontierPeak>,
  output_count: Int,
  output_descriptor_peaks: List<FrontierPeak>,
  mint_count: Int,
  mint_peaks: List<FrontierPeak>,
}

pub fn native_facts(control: NativeScriptsControlV1) -> NativeFacts {
  NativeFacts {
    compact_cbor: control.compact_cbor,
    witness_set_compact_cbor: control.witness_set_compact_cbor,
    field_preimage_lengths_cbor: control.field_preimage_lengths_cbor,
    resolved_input_count: control.resolved_input_count,
    spend_input_count: control.spend_input_count,
    resolved_item_peaks: control.resolved_item_peaks,
    signer_count: control.signer_count,
    signer_frontier_commitment: control.signer_frontier_commitment,
    redeemer_count: control.redeemer_count,
    redeemer_peaks: control.redeemer_peaks,
    purpose_count: control.purpose_count,
    purpose_peaks: control.purpose_peaks,
    output_count: control.output_count,
    output_descriptor_peaks: control.output_descriptor_peaks,
    mint_count: control.mint_count,
    mint_peaks: control.mint_peaks,
  }
}

fn no_auxiliary(auxiliary: Data) -> Bool {
  builtin.un_constr_data(auxiliary) == Pair(0, [])
}

pub fn next_resolved(
  context: CekContextControlV1,
  native: NativeFacts,
  auxiliary: Data,
  source_kind: Int,
) -> CekContextControlV1 {
  let count = if source_kind == 1 {
    native.resolved_input_count - native.spend_input_count
  } else {
    native.spend_input_count
  }
  let current = if source_kind == 1 {
    context.reference_items
  } else {
    context.spend_items
  }
  if current.length == count {
    expect no_auxiliary(auxiliary)
    CekContextControlV1 {
      ..context,
      stage: if source_kind == 1 {
        2
      } else {
        3
      },
    }
  } else {
    expect
        Pair(13,
        [kind_data, index_data, key_data, descriptor_data, siblings_data])
      = builtin.un_constr_data(auxiliary)
    expect kind: Int = kind_data
    expect index: Int = index_data
    expect key: ByteArray = key_data
    expect descriptor: ByteArray = descriptor_data
    expect siblings: List<ByteArray> = siblings_data
    expect kind == source_kind
    expect Some(items) = script_context_v1.prepend_resolved_descriptor_tx_in_info_v1(
      native.resolved_input_count,
      native.resolved_item_peaks,
      native.spend_input_count,
      kind,
      index,
      key,
      descriptor,
      siblings,
      context.language_tag == 128,
      current,
    )
    if source_kind == 1 {
      CekContextControlV1 { ..context, reference_items: items }
    } else {
      CekContextControlV1 { ..context, spend_items: items }
    }
  }
}

pub fn next_output(
  context: CekContextControlV1,
  native: NativeFacts,
  auxiliary: Data,
) -> CekContextControlV1 {
  if context.output_items.length == native.output_count {
    expect no_auxiliary(auxiliary)
    CekContextControlV1 { ..context, stage: 4 }
  } else {
    expect Pair(14, [index_data,
          descriptor_data, siblings_data]) = builtin.un_constr_data(auxiliary)
    expect index: Int = index_data
    expect descriptor: ByteArray = descriptor_data
    expect siblings: List<ByteArray> = siblings_data
    expect Some(items) = script_context_v1.prepend_output_descriptor_v1(
      native.output_count,
      native.output_descriptor_peaks,
      index,
      descriptor,
      siblings,
      context.language_tag == 128,
      context.output_items,
    )
    CekContextControlV1 { ..context, output_items: items }
  }
}

pub fn next_signer(
  context: CekContextControlV1,
  native: NativeFacts,
  auxiliary: Data,
) -> CekContextControlV1 {
  if context.signer_items.length == native.signer_count {
    expect no_auxiliary(auxiliary)
    CekContextControlV1 { ..context, stage: 5 }
  } else {
    expect Pair(15, [peaks_data,
          index_data,
          hash_data, siblings_data]) = builtin.un_constr_data(auxiliary)
    expect peaks: List<FrontierPeak> = peaks_data
    expect index: Int = index_data
    expect hash: ByteArray = hash_data
    expect siblings: List<ByteArray> = siblings_data
    expect Some(items) = script_context_v1.prepend_signer_v1(
      native.signer_count,
      native.signer_frontier_commitment,
      peaks,
      index,
      hash,
      siblings,
      context.signer_items,
    )
    CekContextControlV1 { ..context, signer_items: items }
  }
}

pub fn next_mint_init(
  context: CekContextControlV1,
  native: NativeFacts,
  auxiliary: Data,
) -> CekContextControlV1 {
  expect no_auxiliary(auxiliary)
  if native.mint_count == 0 {
    CekContextControlV1 {
      ..context,
      stage: 9,
      mint_summary: cek_data_v1.map_data_summary_v1(
        cek_data_v1.empty_data_pair_summary_v1(),
      ),
    }
  } else {
    CekContextControlV1 { ..context, stage: 8 }
  }
}

pub fn next_assemble(
  context: CekContextControlV1,
  transaction_id: ByteArray,
  auxiliary: Data,
) -> CekContextControlV1 {
  expect Pair(21, [control_data]) = builtin.un_constr_data(auxiliary)
  expect control: CekContextPartsControlV1 = control_data
  expect
    witness.hash_cek_context_parts_control_v1(control) == context.redeemer_context_control_hash
  let assembly = CekTxInfoAssemblyControlV1 {
    tail_fields: script_context_v1.tx_info_tail_fields_summary_v1(
      context.language_tag == 128,
      context.observer_summary,
      context.signer_items,
      context.mint_summary,
      control.redeemer_items,
      transaction_id,
    ),
    redeemer: control.redeemer,
    script_info: control.script_info,
  }
  CekContextControlV1 {
    ..context,
    stage: 12,
    redeemer_context_control_hash: witness.hash_cek_tx_info_assembly_control_v1(
      assembly,
    ),
  }
}

pub fn next_tx_info(
  source_kind: ValidationSourceKind,
  context: CekContextControlV1,
  native: NativeFacts,
  auxiliary: Data,
) -> CekContextControlV1 {
  expect Pair(22, [control_data]) = builtin.un_constr_data(auxiliary)
  expect control: CekTxInfoAssemblyControlV1 = control_data
  expect
    witness.hash_cek_tx_info_assembly_control_v1(control) == context.redeemer_context_control_hash
  let compact = decode_tx_compact_view_v1(source_kind, native.compact_cbor)
  let tx_info = script_context_v1.tx_info_from_tail_summary_v1(
    context.language_tag == 128,
    context.spend_items,
    context.reference_items,
    context.output_items,
    compact.body.fee,
    compact.body.validity_interval_start,
    compact.body.validity_interval_end,
    context.observer_summary,
    context.mint_summary,
    control.tail_fields,
  )
  let final_context = CekFinalContextControlV1 {
    tx_info,
    redeemer: control.redeemer,
    script_info: control.script_info,
  }
  CekContextControlV1 {
    ..context,
    stage: 13,
    redeemer_context_control_hash: witness.hash_cek_final_context_control_v1(
      final_context,
    ),
  }
}

pub fn seed_hash(
  context: CekContextControlV1,
  execution_cursor: Int,
  auxiliary: Data,
) -> ByteArray {
  expect Pair(23, [control_data]) = builtin.un_constr_data(auxiliary)
  expect control: CekFinalContextControlV1 = control_data
  expect
    witness.hash_cek_final_context_control_v1(control) == context.redeemer_context_control_hash
  let summary = script_context_v1.script_context_summary_v1(
    control.tx_info,
    control.redeemer,
    control.script_info,
  )
  let context_value = cek_constant_v1.semantic_data_constant_root_v1(summary)
  let context_term = cek_proof_v1.hash_context_constant_term_v1(context_value)
  cek_machine_v1.hash_state_v1(
    cek_machine_v1.MachineStateV1 {
      mode: cek_machine_v1.mode_compute,
      execution_index: execution_cursor,
      focus_root: cek_proof_v1.hash_application_term_v1(
        context.program_term_root,
        context_term,
      ),
      environment_root: cek_proof_v1.empty_environment_root_v1,
      continuation_root: cek_proof_v1.empty_continuation_root_v1,
      auxiliary: 0,
      cpu: 0,
      memory: 0,
    },
  )
}

fn finalize_mint_policy(
  context: CekContextControlV1,
) -> cek_data_v1.DataSequenceSummaryV1 {
  expect bytearray.length(context.current_mint_policy) == 28
  cek_data_v1.prepend_data_pair_summary_v1(
    cek_data_v1.semantic_data_summary_v1(
      builtin.b_data(context.current_mint_policy),
    ),
    cek_data_v1.map_data_summary_v1(context.current_mint_assets),
    context.mint_policies,
  )
}

pub fn next_mint_item(
  context: CekContextControlV1,
  native: NativeFacts,
  auxiliary: Data,
) -> CekContextControlV1 {
  if context.mint_cursor == native.mint_count {
    expect no_auxiliary(auxiliary)
    let policies = finalize_mint_policy(context)
    CekContextControlV1 {
      ..context,
      stage: 9,
      current_mint_policy: #"",
      current_mint_assets: cek_data_v1.empty_data_pair_summary_v1(),
      mint_policies: policies,
      mint_summary: cek_data_v1.map_data_summary_v1(policies),
    }
  } else {
    expect
        Pair(16,
        [
          index_data, policy_data, name_data, quantity_data, siblings_data,
          previous_data,
        ])
      = builtin.un_constr_data(auxiliary)
    expect index: Int = index_data
    expect policy: ByteArray = policy_data
    expect name: ByteArray = name_data
    expect quantity: Int = quantity_data
    expect siblings: List<ByteArray> = siblings_data
    expect previous: Option<CekMintHead> = previous_data
    expect cek_mint_order.order_is_valid(context, policy, name, previous)
    expect
      validation_merkle_v1.verify_membership(
        native.mint_count,
        native.mint_peaks,
        index,
        mint_asset_leaf_hash(policy, name, quantity),
        siblings,
      )
    if context.current_mint_policy == #"" {
      CekContextControlV1 {
        ..context,
        mint_cursor: context.mint_cursor + 1,
        current_mint_policy: policy,
        current_mint_assets: prepend_mint_asset_summary(
          name,
          quantity,
          context.current_mint_assets,
        ),
      }
    } else if policy == context.current_mint_policy {
      CekContextControlV1 {
        ..context,
        mint_cursor: context.mint_cursor + 1,
        current_mint_assets: prepend_mint_asset_summary(
          name,
          quantity,
          context.current_mint_assets,
        ),
      }
    } else {
      expect bytearray.compare(policy, context.current_mint_policy) == Less
      let policies = finalize_mint_policy(context)
      CekContextControlV1 {
        ..context,
        mint_cursor: context.mint_cursor + 1,
        current_mint_policy: policy,
        current_mint_assets: prepend_mint_asset_summary(
          name,
          quantity,
          cek_data_v1.empty_data_pair_summary_v1(),
        ),
        mint_policies: policies,
      }
    }
  }
}
