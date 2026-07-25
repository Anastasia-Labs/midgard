use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_input_cbor, encode_midgard_tx_input,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardCredential, MidgardTxInput, MidgardTxOutput, PubKeyCredential,
  ScriptCredential,
}
use midgard/ledger_output_v1
use midgard/script_proof_v1
use midgard/validation_merkle_v1

fn credential_data(credential: MidgardCredential) -> Data {
  when credential is {
    PubKeyCredential(hash) -> builtin.constr_data(0, [builtin.b_data(hash)])
    ScriptCredential(hash) -> builtin.constr_data(1, [builtin.b_data(hash)])
  }
}

fn stake_credential_data(credential: Option<MidgardCredential>) -> Data {
  when credential is {
    None -> builtin.constr_data(1, [])
    Some(value) ->
      builtin.constr_data(0, [builtin.constr_data(0, [credential_data(value)])])
  }
}

fn address_data(output: MidgardTxOutput, midgard_encoding: Bool) -> Data {
  builtin.constr_data(
    if midgard_encoding && output.address.protected {
      1
    } else {
      0
    },
    [
      credential_data(output.address.payment_credential),
      stake_credential_data(output.address.stake_credential),
    ],
  )
}

fn output_value_data(output_data: Data) -> Option<Data> {
  let entries = builtin.un_map_data(output_data)
  when entries is {
    [Pair(_, _), Pair(_, value_data)] -> Some(value_data)
    [Pair(_, _), Pair(_, value_data), Pair(_, _)] -> Some(value_data)
    [Pair(_, _), Pair(_, value_data), Pair(_, _), Pair(_, _)] ->
      Some(value_data)
    _ -> None
  }
}

fn script_value_data(value_data: Data) -> Data {
  expect [lovelace_data, assets_data] = builtin.un_list_data(value_data)
  let lovelace = builtin.un_i_data(lovelace_data)
  let assets = builtin.un_map_data(assets_data)
  if lovelace == 0 {
    builtin.map_data(assets)
  } else {
    builtin.map_data(
      [
        Pair(
          builtin.b_data(#""),
          builtin.map_data(
            [Pair(builtin.b_data(#""), builtin.i_data(lovelace))],
          ),
        ),
        ..assets
      ],
    )
  }
}

fn datum_data(output: MidgardTxOutput) -> Data {
  when output.datum_cbor is {
    None -> builtin.constr_data(0, [])
    Some(datum_cbor) -> {
      expect Some(datum) = cbor.deserialise(datum_cbor)
      builtin.constr_data(2, [datum])
    }
  }
}

fn reference_script_data(output: MidgardTxOutput) -> Data {
  when output.script_ref is {
    None -> builtin.constr_data(1, [])
    Some(script) ->
      builtin.constr_data(
        0,
        [builtin.b_data(script_proof_v1.versioned_script_hash(script))],
      )
  }
}

/// Reconstructs the exact `TxOut` Data subtree exposed to a PlutusV3 or
/// MidgardV1 program from one independently bounded canonical ledger output.
/// The only context-language difference is the protected-address constructor.
pub fn tx_out_data_v1(
  output_cbor: ByteArray,
  midgard_encoding: Bool,
) -> Option<Data> {
  when ledger_output_v1.decode_canonical_output(output_cbor) is {
    None -> None
    Some(output) ->
      when cbor.deserialise(output_cbor) is {
        None -> None
        Some(output_data) ->
          when output_value_data(output_data) is {
            None -> None
            Some(value_data) ->
              Some(
                builtin.constr_data(
                  0,
                  [
                    address_data(output, midgard_encoding),
                    script_value_data(value_data),
                    datum_data(output),
                    reference_script_data(output),
                  ],
                ),
              )
          }
      }
  }
}

pub fn tx_out_summary_v1(
  output_cbor: ByteArray,
  midgard_encoding: Bool,
) -> Option<cek_data_v1.DataSummaryV1> {
  when tx_out_data_v1(output_cbor, midgard_encoding) is {
    None -> None
    Some(data) -> Some(cek_data_v1.semantic_data_summary_v1(data))
  }
}

fn tx_out_ref_data(input: MidgardTxInput) -> Data {
  builtin.constr_data(
    0,
    [builtin.b_data(input.tx_id), builtin.i_data(input.output_index)],
  )
}

/// Reconstructs one `TxInInfo` context item from the canonical transaction
/// input and the independently authenticated ledger-output preimage.
pub fn tx_in_info_data_v1(
  input_cbor: ByteArray,
  output_cbor: ByteArray,
  midgard_encoding: Bool,
) -> Option<Data> {
  let input = decode_midgard_tx_input_cbor(input_cbor)
  if encode_midgard_tx_input(input) != input_cbor {
    None
  } else {
    when tx_out_data_v1(output_cbor, midgard_encoding) is {
      None -> None
      Some(output) ->
        Some(builtin.constr_data(0, [tx_out_ref_data(input), output]))
    }
  }
}

pub fn tx_in_info_summary_v1(
  input_cbor: ByteArray,
  output_cbor: ByteArray,
  midgard_encoding: Bool,
) -> Option<cek_data_v1.DataSummaryV1> {
  when tx_in_info_data_v1(input_cbor, output_cbor, midgard_encoding) is {
    None -> None
    Some(data) -> Some(cek_data_v1.semantic_data_summary_v1(data))
  }
}

/// Prepends one authenticated item while walking a committed collection from
/// its last index to zero. This produces the exact linked-list semantic root
/// used by `serialiseData`, without revealing the full collection in one L1
/// transaction.
pub fn prepend_authenticated_list_item_v1(
  collection_kind: Int,
  item_count: Int,
  item_peaks: List<validation_merkle_v1.FrontierPeak>,
  item_index: Int,
  item: cek_data_v1.DataSummaryV1,
  siblings: List<ByteArray>,
  tail: cek_data_v1.DataSequenceSummaryV1,
) -> Option<cek_data_v1.DataSequenceSummaryV1> {
  if and {
    item_count >= 0,
    validation_merkle_v1.frontier_is_well_formed(item_count, item_peaks),
    item_index == item_count - tail.length - 1,
    validation_merkle_v1.verify_membership(
      item_count,
      item_peaks,
      item_index,
      script_proof_v1.context_item_leaf_hash(
        collection_kind,
        item_index,
        item.root,
        item.cbor_length,
        item.memory,
      ),
      siblings,
    ),
  } {
    Some(cek_data_v1.prepend_data_list_summary_v1(item, tail))
  } else {
    None
  }
}

/// Converts one raw resolved-input commitment directly into the exact
/// language-specific `TxInInfo` semantic item and prepends it while walking
/// either the spend or reference-input collection in reverse. The raw
/// commitment is shared by both context languages; only this bounded step
/// selects the Cardano or Midgard protected-address encoding.
pub fn prepend_resolved_tx_in_info_v1(
  resolved_count: Int,
  resolved_peaks: List<validation_merkle_v1.FrontierPeak>,
  spend_count: Int,
  source_kind: Int,
  item_index: Int,
  input_cbor: ByteArray,
  output_cbor: ByteArray,
  siblings: List<ByteArray>,
  midgard_encoding: Bool,
  tail: cek_data_v1.DataSequenceSummaryV1,
) -> Option<cek_data_v1.DataSequenceSummaryV1> {
  let collection_count = if source_kind == 0 {
    spend_count
  } else {
    resolved_count - spend_count
  }
  let collection_offset = if source_kind == 0 {
    0
  } else {
    spend_count
  }
  if and {
    resolved_count >= 0,
    spend_count >= 0,
    spend_count <= resolved_count,
    or {
      source_kind == 0,
      source_kind == 1,
    },
    validation_merkle_v1.frontier_is_well_formed(resolved_count, resolved_peaks),
    item_index == collection_offset + collection_count - tail.length - 1,
    validation_merkle_v1.verify_membership(
      resolved_count,
      resolved_peaks,
      item_index,
      script_proof_v1.resolved_context_item_leaf_hash(
        source_kind,
        item_index,
        input_cbor,
        output_cbor,
      ),
      siblings,
    ),
  } {
    when tx_in_info_summary_v1(input_cbor, output_cbor, midgard_encoding) is {
      None -> None
      Some(item) -> Some(cek_data_v1.prepend_data_list_summary_v1(item, tail))
    }
  } else {
    None
  }
}

/// Converts one authenticated transaction output into its exact script-context
/// `TxOut` item and prepends it in reverse output order.
pub fn prepend_output_v1(
  output_count: Int,
  output_peaks: List<validation_merkle_v1.FrontierPeak>,
  output_index: Int,
  output_cbor: ByteArray,
  siblings: List<ByteArray>,
  midgard_encoding: Bool,
  tail: cek_data_v1.DataSequenceSummaryV1,
) -> Option<cek_data_v1.DataSequenceSummaryV1> {
  if and {
    output_count >= 0,
    validation_merkle_v1.frontier_is_well_formed(output_count, output_peaks),
    output_index == output_count - tail.length - 1,
    validation_merkle_v1.verify_membership(
      output_count,
      output_peaks,
      output_index,
      script_proof_v1.output_leaf_hash(output_index, output_cbor),
      siblings,
    ),
  } {
    when tx_out_summary_v1(output_cbor, midgard_encoding) is {
      None -> None
      Some(item) -> Some(cek_data_v1.prepend_data_list_summary_v1(item, tail))
    }
  } else {
    None
  }
}

fn byte_data_items(items: List<ByteArray>) -> List<Data> {
  items |> list.map(builtin.b_data)
}

/// Prepends one authenticated signer in reverse canonical order. The frontier
/// peaks are supplied with the one-step witness and are bound to the compact
/// Phase-A commitment, so the CEK control need not repeat all signer hashes.
pub fn prepend_signer_v1(
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  signer_peaks: List<validation_merkle_v1.FrontierPeak>,
  signer_index: Int,
  signer_hash: ByteArray,
  siblings: List<ByteArray>,
  tail: cek_data_v1.DataSequenceSummaryV1,
) -> Option<cek_data_v1.DataSequenceSummaryV1> {
  if and {
    bytearray.length(signer_frontier_commitment) == 32,
    bytearray.length(signer_hash) == 28,
    validation_merkle_v1.frontier_is_well_formed(signer_count, signer_peaks),
    validation_merkle_v1.frontier_commitment(signer_count, signer_peaks) == signer_frontier_commitment,
    signer_index == signer_count - tail.length - 1,
    validation_merkle_v1.verify_membership(
      signer_count,
      signer_peaks,
      signer_index,
      script_proof_v1.signer_leaf_hash(signer_hash),
      siblings,
    ),
  } {
    Some(
      cek_data_v1.prepend_data_list_summary_v1(
        builtin.b_data(signer_hash)
          |> cek_data_v1.semantic_data_summary_v1,
        tail,
      ),
    )
  } else {
    None
  }
}

fn observer_withdrawal_pairs(
  observers: List<ByteArray>,
) -> List<Pair<Data, Data>> {
  observers
    |> list.map(
        fn(observer) {
          Pair(
            builtin.constr_data(1, [builtin.b_data(observer)]),
            builtin.i_data(0),
          )
        },
      )
}

/// Exact observer representation for the selected context language:
/// Cardano uses a zero-valued withdrawals map keyed by script credentials;
/// Midgard uses the canonical observer byte-string list.
pub fn observer_collection_summary_v1(
  observers: List<ByteArray>,
  midgard_encoding: Bool,
) -> Option<cek_data_v1.DataSummaryV1> {
  let canonical = observers
    |> list.sort(bytearray.compare)
    |> list.unique
  if and {
    list.length(canonical) <= 16,
    list.all(canonical, fn(hash) { bytearray.length(hash) == 28 }),
  } {
    Some(
      if midgard_encoding {
        canonical
          |> byte_data_items
          |> builtin.list_data
          |> cek_data_v1.semantic_data_summary_v1
      } else {
        canonical
          |> observer_withdrawal_pairs
          |> builtin.map_data
          |> cek_data_v1.semantic_data_summary_v1
      },
    )
  } else {
    None
  }
}

fn bool_data(value: Bool) -> Data {
  builtin.constr_data(
    if value {
      1
    } else {
      0
    },
    [],
  )
}

fn interval_bound_data(value: Int) -> Data {
  if value < 0 {
    builtin.constr_data(0, [])
  } else {
    builtin.constr_data(1, [builtin.i_data(value)])
  }
}

pub fn validity_interval_summary_v1(
  start: Int,
  end: Int,
) -> cek_data_v1.DataSummaryV1 {
  builtin.constr_data(
    0,
    [
      builtin.constr_data(0, [interval_bound_data(start), bool_data(True)]),
      builtin.constr_data(0, [interval_bound_data(end), bool_data(False)]),
    ],
  )
    |> cek_data_v1.semantic_data_summary_v1
}

pub fn fee_summary_v1(fee: Int) -> cek_data_v1.DataSummaryV1 {
  builtin.i_data(fee) |> cek_data_v1.semantic_data_summary_v1
}

pub fn transaction_id_summary_v1(
  transaction_id: ByteArray,
) -> cek_data_v1.DataSummaryV1 {
  expect bytearray.length(transaction_id) == 32
  builtin.b_data(transaction_id) |> cek_data_v1.semantic_data_summary_v1
}

/// Exact script-purpose key used by the language-specific redeemer map.
/// Cardano's context omits Midgard receive purposes; MidgardV1 preserves all
/// four purpose kinds and additionally binds the script hash for spends.
pub fn script_purpose_summary_v1(
  purpose_kind: Int,
  script_hash: ByteArray,
  subject: ByteArray,
  midgard_encoding: Bool,
) -> Option<cek_data_v1.DataSummaryV1> {
  if bytearray.length(script_hash) != 28 {
    None
  } else {
    let purpose = if purpose_kind == 0 {
      let input = decode_midgard_tx_input_cbor(subject)
      if encode_midgard_tx_input(input) != subject {
        None
      } else {
        Some(
          builtin.constr_data(
            1,
            if midgard_encoding {
              [builtin.b_data(script_hash), tx_out_ref_data(input)]
            } else {
              [tx_out_ref_data(input)]
            },
          ),
        )
      }
    } else if purpose_kind == 1 {
      Some(builtin.constr_data(0, [builtin.b_data(script_hash)]))
    } else if purpose_kind == 2 {
      Some(
        builtin.constr_data(
          2,
          [
            if midgard_encoding {
              builtin.b_data(script_hash)
            } else {
              credential_data(ScriptCredential(script_hash))
            },
          ],
        ),
      )
    } else if and {
      purpose_kind == 3,
      midgard_encoding,
    } {
      Some(builtin.constr_data(3, [builtin.b_data(script_hash)]))
    } else {
      None
    }
    when purpose is {
      None -> None
      Some(data) -> Some(cek_data_v1.semantic_data_summary_v1(data))
    }
  }
}

fn data_fields_summary(
  fields: List<cek_data_v1.DataSummaryV1>,
) -> cek_data_v1.DataSequenceSummaryV1 {
  list.foldr(
    fields,
    cek_data_v1.empty_data_list_summary_v1(),
    fn(field, tail) { cek_data_v1.prepend_data_list_summary_v1(field, tail) },
  )
}

fn small_constr_summary(
  constructor: Int,
  fields: List<cek_data_v1.DataSummaryV1>,
) -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.small_constr_data_summary_v1(
    constructor,
    data_fields_summary(fields),
  )
}

fn empty_list_summary() -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.list_data_summary_v1(cek_data_v1.empty_data_list_summary_v1())
}

fn empty_map_summary() -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.map_data_summary_v1(cek_data_v1.empty_data_pair_summary_v1())
}

fn none_summary() -> cek_data_v1.DataSummaryV1 {
  small_constr_summary(1, [])
}

/// The independently foldable tail of PlutusV3/MidgardV1 TxInfo. Keeping the
/// tail as a content-addressed sequence lets the validation machine construct
/// the complete value without one oversized dispute transition.
pub fn tx_info_tail_fields_summary_v1(
  midgard_encoding: Bool,
  observers: cek_data_v1.DataSummaryV1,
  signers: cek_data_v1.DataSequenceSummaryV1,
  mint: cek_data_v1.DataSummaryV1,
  redeemer_items: cek_data_v1.DataSequenceSummaryV1,
  transaction_id: ByteArray,
) -> cek_data_v1.DataSequenceSummaryV1 {
  let signer_list = cek_data_v1.list_data_summary_v1(signers)
  let redeemer_map = cek_data_v1.map_data_summary_v1(redeemer_items)
  let tx_id = transaction_id_summary_v1(transaction_id)
  data_fields_summary(
    if midgard_encoding {
      [observers, signer_list, mint, redeemer_map, tx_id]
    } else {
      [
        signer_list,
        redeemer_map,
        empty_map_summary(),
        tx_id,
        empty_map_summary(),
        empty_list_summary(),
        none_summary(),
        none_summary(),
      ]
    },
  )
}

/// Exact PlutusV3 or MidgardV1 transaction-info root from independently
/// authenticated field summaries and a previously committed tail fold.
pub fn tx_info_from_tail_summary_v1(
  midgard_encoding: Bool,
  inputs: cek_data_v1.DataSequenceSummaryV1,
  reference_inputs: cek_data_v1.DataSequenceSummaryV1,
  outputs: cek_data_v1.DataSequenceSummaryV1,
  fee: Int,
  validity_start: Int,
  validity_end: Int,
  observers: cek_data_v1.DataSummaryV1,
  mint: cek_data_v1.DataSummaryV1,
  tail_fields: cek_data_v1.DataSequenceSummaryV1,
) -> cek_data_v1.DataSummaryV1 {
  let input_list = cek_data_v1.list_data_summary_v1(inputs)
  let reference_list = cek_data_v1.list_data_summary_v1(reference_inputs)
  let output_list = cek_data_v1.list_data_summary_v1(outputs)
  let fee_data = fee_summary_v1(fee)
  let validity = validity_interval_summary_v1(validity_start, validity_end)
  let head_fields = if midgard_encoding {
    [input_list, reference_list, output_list, fee_data, validity]
  } else {
    [
      input_list,
      reference_list,
      output_list,
      fee_data,
      mint,
      empty_list_summary(),
      observers,
      validity,
    ]
  }
  cek_data_v1.small_constr_data_summary_v1(
    0,
    list.foldr(
      head_fields,
      tail_fields,
      fn(field, tail) { cek_data_v1.prepend_data_list_summary_v1(field, tail) },
    ),
  )
}

/// Convenience composition used by off-chain mirrors and exactness tests.
pub fn tx_info_summary_v1(
  midgard_encoding: Bool,
  inputs: cek_data_v1.DataSequenceSummaryV1,
  reference_inputs: cek_data_v1.DataSequenceSummaryV1,
  outputs: cek_data_v1.DataSequenceSummaryV1,
  fee: Int,
  validity_start: Int,
  validity_end: Int,
  observers: cek_data_v1.DataSummaryV1,
  signers: cek_data_v1.DataSequenceSummaryV1,
  mint: cek_data_v1.DataSummaryV1,
  redeemer_items: cek_data_v1.DataSequenceSummaryV1,
  transaction_id: ByteArray,
) -> cek_data_v1.DataSummaryV1 {
  tx_info_from_tail_summary_v1(
    midgard_encoding,
    inputs,
    reference_inputs,
    outputs,
    fee,
    validity_start,
    validity_end,
    observers,
    mint,
    tx_info_tail_fields_summary_v1(
      midgard_encoding,
      observers,
      signers,
      mint,
      redeemer_items,
      transaction_id,
    ),
  )
}

pub fn spend_datum_summary_v1(
  output_cbor: ByteArray,
) -> Option<cek_data_v1.DataSummaryV1> {
  when ledger_output_v1.decode_canonical_output(output_cbor) is {
    None -> None
    Some(output) ->
      when output.datum_cbor is {
        None -> Some(none_summary())
        Some(datum_cbor) ->
          when cbor.deserialise(datum_cbor) is {
            None -> None
            Some(datum) ->
              if cbor.serialise(datum) == datum_cbor {
                Some(
                  small_constr_summary(
                    0,
                    [cek_data_v1.semantic_data_summary_v1(datum)],
                  ),
                )
              } else {
                None
              }
          }
      }
  }
}

/// The PlutusV3 ScriptInfo differs from ScriptPurpose only for spends, where
/// the datum of the authenticated consumed output is included.
pub fn cardano_script_info_summary_v1(
  purpose_kind: Int,
  script_hash: ByteArray,
  subject: ByteArray,
  spend_output_cbor: Option<ByteArray>,
) -> Option<cek_data_v1.DataSummaryV1> {
  if purpose_kind == 0 {
    expect Some(output_cbor) = spend_output_cbor
    let input = decode_midgard_tx_input_cbor(subject)
    if encode_midgard_tx_input(input) != subject {
      None
    } else {
      when spend_datum_summary_v1(output_cbor) is {
        None -> None
        Some(datum) ->
          Some(
            small_constr_summary(
              1,
              [
                cek_data_v1.semantic_data_summary_v1(tx_out_ref_data(input)),
                datum,
              ],
            ),
          )
      }
    }
  } else if spend_output_cbor != None {
    None
  } else {
    script_purpose_summary_v1(purpose_kind, script_hash, subject, False)
  }
}

pub fn script_context_summary_v1(
  tx_info: cek_data_v1.DataSummaryV1,
  redeemer: cek_data_v1.DataSummaryV1,
  script_info: cek_data_v1.DataSummaryV1,
) -> cek_data_v1.DataSummaryV1 {
  small_constr_summary(0, [tx_info, redeemer, script_info])
}
