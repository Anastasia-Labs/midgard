use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/bounded_collection_v1
use midgard/canonical_plutus_data_v1
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_input_cbor, encode_midgard_tx_input,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardCredential, MidgardTxInput, MidgardTxOutput, PubKeyCredential,
  ScriptCredential,
}
use midgard/ledger_output_commitment_v1
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

/// The one place the canonicity/materialisability split is visible. Since the
/// spec section 6.2 re-pin a canonical datum can carry a bignum (tags 2/3) or a
/// tag-102 constructor. Both are canonical and both reach the output decoder,
/// but neither survives the stdlib decoder: tag 102 it declines outright, so
/// `deserialise` answers `None`; tags 2/3 it does not decline at all. Its
/// major-6 arm computes `constr_data(tag - 121, ..)` with no tag-range guard,
/// so tag 2 or 3 yields a negative alternative, and under PlutusV3 builtin
/// semantics variant E -- mainnet protocol major version 11 -- that alternative
/// is a `Word64`, so a negative one fails the machine on real L1, not merely in
/// the test runner. Asking is therefore an abort, and the screen has to run
/// first.
///
/// The screen is the Canonical-Data Acceptor's `is_materialisable_plutus_data_v1`
/// (spec section 11.2, issue 571). It is recursive, which is what the head-byte
/// screen it replaces (issue 567) could not be: that one read byte zero, so a
/// bignum nested at any depth inside an otherwise ordinary canonical datum
/// still reached the decoder and still aborted. This one reaches every tag site
/// the datum contains, so depth is not a way around it and this function is
/// total on canonical input.
///
/// Declining to materialise is not declaring non-canonical. The section 6.2
/// predicate still accepts these bytes, and issue 567's AC4 requires that it
/// does.
///
/// A behaviour change worth stating, because it is wider than the acceptance
/// criteria that produced it: before issue 571 this function screened the head
/// byte only -- issue 567's `datum_head_is_bignum`, which this change removes,
/// declined a `c2`/`c3` at byte zero and let a bignum nested at any depth reach
/// `deserialise` and abort. The recursive screen strictly subsumes it, since
/// `is_materialisable_plutus_data_v1` implies the absence of a bignum head, so
/// swapping them can only narrow what materialises. The narrowing is empty on
/// the path that reaches this function: `tx_out_data_v1` and
/// `spend_datum_summary_v1` both obtain their `MidgardTxOutput` from
/// `ledger_output_v1.decode_canonical_output`, which has already required
/// `is_canonical_plutus_data_v1` of the datum. So the only datums this now
/// declines are the section 6.2 re-pinned forms, and those did not previously
/// succeed -- tag 102 was already `None`, and a bignum was an abort rather than
/// a value. No datum that materialised before this change fails to materialise
/// after it.
fn datum_data(output: MidgardTxOutput) -> Option<Data> {
  when output.datum_cbor is {
    None -> Some(builtin.constr_data(0, []))
    Some(datum_cbor) ->
      if canonical_plutus_data_v1.is_materialisable_plutus_data_v1(datum_cbor) {
        when cbor.deserialise(datum_cbor) is {
          None -> None
          Some(datum) -> Some(builtin.constr_data(2, [datum]))
        }
      } else {
        None
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
              when datum_data(output) is {
                None -> None
                Some(datum) ->
                  Some(
                    builtin.constr_data(
                      0,
                      [
                        address_data(output, midgard_encoding),
                        script_value_data(value_data),
                        datum,
                        reference_script_data(output),
                      ],
                    ),
                  )
              }
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

fn tx_in_info_from_output_summary_v1(
  input_cbor: ByteArray,
  output: cek_data_v1.DataSummaryV1,
) -> Option<cek_data_v1.DataSummaryV1> {
  let input = decode_midgard_tx_input_cbor(input_cbor)
  if encode_midgard_tx_input(input) != input_cbor {
    None
  } else {
    Some(
      small_constr_summary(
        0,
        [cek_data_v1.semantic_data_summary_v1(tx_out_ref_data(input)), output],
      ),
    )
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

/// Converts one authenticated compact ledger descriptor directly into the
/// exact language-specific `TxInInfo` semantic item. The descriptor is the
/// value committed by ScriptSources replay, so no complete ledger output is
/// revealed or decoded during CEK context assembly.
pub fn prepend_resolved_descriptor_tx_in_info_v1(
  resolved_count: Int,
  resolved_peaks: List<validation_merkle_v1.FrontierPeak>,
  spend_count: Int,
  source_kind: Int,
  item_index: Int,
  input_cbor: ByteArray,
  descriptor_cbor: ByteArray,
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
  let input = decode_midgard_tx_input_cbor(input_cbor)
  let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
  let output = if midgard_encoding {
    descriptor.midgard_tx_out
  } else {
    descriptor.cardano_tx_out
  }
  if and {
    resolved_count >= 0,
    spend_count >= 0,
    spend_count <= resolved_count,
    or {
      source_kind == 0,
      source_kind == 1,
    },
    encode_midgard_tx_input(input) == input_cbor,
    descriptor.output_index == input.output_index,
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
        descriptor_cbor,
      ),
      siblings,
    ),
  } {
    when tx_in_info_from_output_summary_v1(input_cbor, output) is {
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

/// Prepends the exact language-specific `TxOut` semantic summary from an
/// authenticated compact output descriptor. Complete output CBOR remains in
/// DA and is not part of the CEK one-step witness.
pub fn prepend_output_descriptor_v1(
  output_count: Int,
  output_descriptor_peaks: List<validation_merkle_v1.FrontierPeak>,
  output_index: Int,
  descriptor_cbor: ByteArray,
  siblings: List<ByteArray>,
  midgard_encoding: Bool,
  tail: cek_data_v1.DataSequenceSummaryV1,
) -> Option<cek_data_v1.DataSequenceSummaryV1> {
  let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
  let output = if midgard_encoding {
    descriptor.midgard_tx_out
  } else {
    descriptor.cardano_tx_out
  }
  if and {
    output_count >= 0,
    descriptor.output_index == output_index,
    validation_merkle_v1.frontier_is_well_formed(
      output_count,
      output_descriptor_peaks,
    ),
    output_index == output_count - tail.length - 1,
    validation_merkle_v1.verify_membership(
      output_count,
      output_descriptor_peaks,
      output_index,
      script_proof_v1.output_descriptor_leaf_hash(output_index, descriptor_cbor),
      siblings,
    ),
  } {
    Some(cek_data_v1.prepend_data_list_summary_v1(output, tail))
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
    list.length(canonical) <= bounded_collection_v1.max_tx_size_derived_item_count,
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
          // Two questions, one walk. The section 6.2 predicate decides
          // canonicity and is the gate; materialisability decides whether the
          // stdlib decoder can be asked at all. A canonical bignum or tag-102
          // datum passes the first and fails the second, and declines here
          // rather than aborting -- see datum_data above. The acceptor answers
          // both from a single scan, so screening recursively costs less than
          // the head-byte screen it replaces, which ran beside a separate
          // canonicity walk.
          if canonical_plutus_data_v1.is_materialisable_plutus_data_v1(
            datum_cbor,
          ) {
            when cbor.deserialise(datum_cbor) is {
              None -> None
              Some(datum) ->
                Some(
                  small_constr_summary(
                    0,
                    [cek_data_v1.semantic_data_summary_v1(datum)],
                  ),
                )
            }
          } else {
            None
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

/// Builds the exact PlutusV3 spend `ScriptInfo` from the datum summary already
/// bound into an authenticated compact ledger descriptor.
pub fn cardano_spend_script_info_from_descriptor_v1(
  subject: ByteArray,
  descriptor_cbor: ByteArray,
) -> Option<cek_data_v1.DataSummaryV1> {
  let input = decode_midgard_tx_input_cbor(subject)
  let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
  if and {
    encode_midgard_tx_input(input) == subject,
    descriptor.output_index == input.output_index,
  } {
    Some(
      small_constr_summary(
        1,
        [
          cek_data_v1.semantic_data_summary_v1(tx_out_ref_data(input)),
          descriptor.cardano_spend_datum,
        ],
      ),
    )
  } else {
    None
  }
}

pub fn script_context_summary_v1(
  tx_info: cek_data_v1.DataSummaryV1,
  redeemer: cek_data_v1.DataSummaryV1,
  script_info: cek_data_v1.DataSummaryV1,
) -> cek_data_v1.DataSummaryV1 {
  small_constr_summary(0, [tx_info, redeemer, script_info])
}
