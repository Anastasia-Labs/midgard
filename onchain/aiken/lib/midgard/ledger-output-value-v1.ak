use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/ledger_output_commitment_v1
use midgard/validation_merkle_v1

pub const version: Int = 1

pub const stage_assets: Int = 0

pub const stage_finalize: Int = 1

pub const stage_terminal: Int = 2

const uint32_max = 4294967295

const uint64_max = 18446744073709551615

pub type LedgerOutputValueControlV1 {
  version: Int,
  stage: Int,
  asset_remaining: Int,
  current_policy: ByteArray,
  current_assets: cek_data_v1.DataSequenceSummaryV1,
  value_entries: cek_data_v1.DataSequenceSummaryV1,
  result: Option<cek_data_v1.DataSummaryV1>,
}

pub type LedgerOutputValueWitnessV1 {
  LedgerOutputValueNoWitness
  LedgerOutputValueAsset {
    policy_id: ByteArray,
    asset_name: ByteArray,
    quantity: Int,
    siblings: List<ByteArray>,
  }
}

fn uint64_is_valid(value: Int) -> Bool {
  value >= 0 && value <= uint64_max
}

fn sequence_is_well_formed(
  summary: cek_data_v1.DataSequenceSummaryV1,
) -> Bool {
  and {
    bytearray.length(summary.root) == 32,
    summary.length >= 0,
    summary.length <= uint32_max,
    uint64_is_valid(summary.payload_cbor_length),
    uint64_is_valid(summary.memory),
  }
}

fn summary_is_well_formed(summary: cek_data_v1.DataSummaryV1) -> Bool {
  and {
    bytearray.length(summary.root) == 32,
    uint64_is_valid(summary.cbor_length),
    uint64_is_valid(summary.memory),
  }
}

fn empty_pair_sequence(
  summary: cek_data_v1.DataSequenceSummaryV1,
) -> Bool {
  summary == cek_data_v1.empty_data_pair_summary_v1()
}

pub fn control_is_well_formed(
  control: LedgerOutputValueControlV1,
) -> Bool {
  and {
    control.version == version,
    control.stage >= stage_assets,
    control.stage <= stage_terminal,
    control.asset_remaining >= 0,
    control.asset_remaining <= uint32_max,
    or {
      control.current_policy == #"",
      bytearray.length(control.current_policy) == 28,
    },
    sequence_is_well_formed(control.current_assets),
    sequence_is_well_formed(control.value_entries),
    when control.result is {
      None -> control.stage != stage_terminal
      Some(result) ->
        and {
          control.stage == stage_terminal,
          summary_is_well_formed(result),
        }
    },
    if control.stage == stage_terminal {
      and {
        control.asset_remaining == 0,
        control.current_policy == #"",
        empty_pair_sequence(control.current_assets),
        empty_pair_sequence(control.value_entries),
      }
    } else {
      and {
        if control.current_policy == #"" {
          empty_pair_sequence(control.current_assets)
        } else {
          True
        },
        if control.stage == stage_finalize {
          control.asset_remaining == 0
        } else {
          True
        },
      }
    },
  }
}

pub fn initial_control_v1(
  asset_count: Int,
) -> LedgerOutputValueControlV1 {
  let control = LedgerOutputValueControlV1 {
    version,
    stage: stage_assets,
    asset_remaining: asset_count,
    current_policy: #"",
    current_assets: cek_data_v1.empty_data_pair_summary_v1(),
    value_entries: cek_data_v1.empty_data_pair_summary_v1(),
    result: None,
  }
  expect control_is_well_formed(control)
  control
}

fn encode_sequence(
  summary: cek_data_v1.DataSequenceSummaryV1,
) -> ByteArray {
  expect sequence_is_well_formed(summary)
  encode_definite_array_header(4)
    |> bytearray.concat(encode_definite_bytes(summary.root))
    |> bytearray.concat(cbor.serialise(summary.length))
    |> bytearray.concat(cbor.serialise(summary.payload_cbor_length))
    |> bytearray.concat(cbor.serialise(summary.memory))
}

fn encode_summary(summary: cek_data_v1.DataSummaryV1) -> ByteArray {
  expect summary_is_well_formed(summary)
  encode_definite_array_header(3)
    |> bytearray.concat(encode_definite_bytes(summary.root))
    |> bytearray.concat(cbor.serialise(summary.cbor_length))
    |> bytearray.concat(cbor.serialise(summary.memory))
}

fn encode_optional_summary(
  summary: Option<cek_data_v1.DataSummaryV1>,
) -> ByteArray {
  when summary is {
    None -> #"d87a80"
    Some(value) ->
      encode_summary(value)
        |> bytearray.concat(#"ff")
        |> bytearray.concat(#"d8799f", _)
  }
}

pub fn encode_control_v1(
  control: LedgerOutputValueControlV1,
) -> ByteArray {
  expect control_is_well_formed(control)
  encode_definite_array_header(7)
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(cbor.serialise(control.asset_remaining))
    |> bytearray.concat(encode_definite_bytes(control.current_policy))
    |> bytearray.concat(encode_sequence(control.current_assets))
    |> bytearray.concat(encode_sequence(control.value_entries))
    |> bytearray.concat(encode_optional_summary(control.result))
}

fn sequence_from_data(
  data: Data,
) -> cek_data_v1.DataSequenceSummaryV1 {
  expect [root_data, length_data, payload_data, memory_data] =
    builtin.un_list_data(data)
  let summary = cek_data_v1.DataSequenceSummaryV1 {
    root: builtin.un_b_data(root_data),
    length: builtin.un_i_data(length_data),
    payload_cbor_length: builtin.un_i_data(payload_data),
    memory: builtin.un_i_data(memory_data),
  }
  expect sequence_is_well_formed(summary)
  summary
}

fn summary_from_data(data: Data) -> cek_data_v1.DataSummaryV1 {
  expect [root_data, cbor_length_data, memory_data] =
    builtin.un_list_data(data)
  let summary = cek_data_v1.DataSummaryV1 {
    root: builtin.un_b_data(root_data),
    cbor_length: builtin.un_i_data(cbor_length_data),
    memory: builtin.un_i_data(memory_data),
  }
  expect summary_is_well_formed(summary)
  summary
}

fn optional_summary_from_data(
  data: Data,
) -> Option<cek_data_v1.DataSummaryV1> {
  let Pair(index, fields) = builtin.un_constr_data(data)
  if index == 1 {
    expect fields == []
    None
  } else {
    expect index == 0
    expect [summary_data] = fields
    Some(summary_from_data(summary_data))
  }
}

pub fn control_from_data_v1(
  data: Data,
) -> LedgerOutputValueControlV1 {
  expect
    [
      version_data, stage_data, asset_remaining_data, current_policy_data,
      current_assets_data, value_entries_data, result_data,
    ] = builtin.un_list_data(data)
  let control = LedgerOutputValueControlV1 {
    version: builtin.un_i_data(version_data),
    stage: builtin.un_i_data(stage_data),
    asset_remaining: builtin.un_i_data(asset_remaining_data),
    current_policy: builtin.un_b_data(current_policy_data),
    current_assets: sequence_from_data(current_assets_data),
    value_entries: sequence_from_data(value_entries_data),
    result: optional_summary_from_data(result_data),
  }
  expect control_is_well_formed(control)
  control
}

pub fn decode_control_v1(
  control_cbor: ByteArray,
) -> LedgerOutputValueControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  let control = control_from_data_v1(data)
  expect encode_control_v1(control) == control_cbor
  control
}

fn bounded_integer_summary(
  integer: Int,
) -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.semantic_data_summary_v1(builtin.i_data(integer))
}

fn bounded_bytes_summary(
  bytes: ByteArray,
) -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.semantic_data_summary_v1(builtin.b_data(bytes))
}

fn finalize_current_policy(
  control: LedgerOutputValueControlV1,
) -> cek_data_v1.DataSequenceSummaryV1 {
  if control.current_policy == #"" {
    control.value_entries
  } else {
    cek_data_v1.prepend_data_pair_summary_v1(
      bounded_bytes_summary(control.current_policy),
      cek_data_v1.map_data_summary_v1(control.current_assets),
      control.value_entries,
    )
  }
}

fn advanced(
  control: LedgerOutputValueControlV1,
) -> Option<LedgerOutputValueControlV1> {
  if control_is_well_formed(control) {
    Some(control)
  } else {
    None
  }
}

fn asset_step(
  control: LedgerOutputValueControlV1,
  asset_count: Int,
  asset_peaks: List<validation_merkle_v1.FrontierPeak>,
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
  siblings: List<ByteArray>,
) -> Option<LedgerOutputValueControlV1> {
  let leaf_index = control.asset_remaining - 1
  if or {
    bytearray.length(policy_id) != 28,
    bytearray.length(asset_name) > 32,
    quantity <= 0,
    quantity > uint64_max,
    !validation_merkle_v1.verify_membership(
      asset_count,
      asset_peaks,
      leaf_index,
      ledger_output_commitment_v1.asset_leaf_hash(
        policy_id,
        asset_name,
        quantity,
      ),
      siblings,
    ),
  } {
    None
  } else {
    let policy_order = if control.current_policy == #"" {
      Less
    } else {
      bytearray.compare(policy_id, control.current_policy)
    }
    if policy_order == Greater {
      None
    } else {
      let policy_changed = policy_order == Less
      let value_entries = if and {
        policy_changed,
        control.current_policy != #"",
      } {
        finalize_current_policy(control)
      } else {
        control.value_entries
      }
      let current_assets = if policy_changed {
        cek_data_v1.empty_data_pair_summary_v1()
      } else {
        control.current_assets
      }
      advanced(
        LedgerOutputValueControlV1 {
          ..control,
          asset_remaining: leaf_index,
          current_policy: policy_id,
          current_assets: cek_data_v1.prepend_data_pair_summary_v1(
            bounded_bytes_summary(asset_name),
            bounded_integer_summary(quantity),
            current_assets,
          ),
          value_entries,
        },
      )
    }
  }
}

fn finalize_step(
  control: LedgerOutputValueControlV1,
  lovelace: Int,
) -> Option<LedgerOutputValueControlV1> {
  let policy_entries = finalize_current_policy(control)
  let value_entries = if lovelace == 0 {
    policy_entries
  } else {
    let empty_bytes = bounded_bytes_summary(#"")
    let coin_assets = cek_data_v1.prepend_data_pair_summary_v1(
      empty_bytes,
      bounded_integer_summary(lovelace),
      cek_data_v1.empty_data_pair_summary_v1(),
    )
    cek_data_v1.prepend_data_pair_summary_v1(
      empty_bytes,
      cek_data_v1.map_data_summary_v1(coin_assets),
      policy_entries,
    )
  }
  advanced(
    LedgerOutputValueControlV1 {
      ..control,
      stage: stage_terminal,
      current_policy: #"",
      current_assets: cek_data_v1.empty_data_pair_summary_v1(),
      value_entries: cek_data_v1.empty_data_pair_summary_v1(),
      result: Some(cek_data_v1.map_data_summary_v1(value_entries)),
    },
  )
}

pub fn step_v1(
  control: LedgerOutputValueControlV1,
  asset_count: Int,
  asset_peaks: List<validation_merkle_v1.FrontierPeak>,
  lovelace: Int,
  witness: LedgerOutputValueWitnessV1,
) -> Option<LedgerOutputValueControlV1> {
  if or {
    !control_is_well_formed(control),
    asset_count < 0,
    asset_count > uint32_max,
    control.asset_remaining > asset_count,
    !validation_merkle_v1.frontier_is_well_formed(
      asset_count,
      asset_peaks,
    ),
    !uint64_is_valid(lovelace),
  } {
    None
  } else if control.stage == stage_assets {
    if control.asset_remaining == 0 {
      if witness == LedgerOutputValueNoWitness {
        advanced(LedgerOutputValueControlV1 {
          ..control,
          stage: stage_finalize,
        })
      } else {
        None
      }
    } else {
      when witness is {
        LedgerOutputValueAsset {
          policy_id,
          asset_name,
          quantity,
          siblings,
        } ->
          asset_step(
            control,
            asset_count,
            asset_peaks,
            policy_id,
            asset_name,
            quantity,
            siblings,
          )
        _ -> None
      }
    }
  } else if control.stage == stage_finalize {
    if witness == LedgerOutputValueNoWitness {
      finalize_step(control, lovelace)
    } else {
      None
    }
  } else {
    None
  }
}

pub fn finalize_v1(
  control: LedgerOutputValueControlV1,
) -> Option<cek_data_v1.DataSummaryV1> {
  if and {
    control_is_well_formed(control),
    control.stage == stage_terminal,
  } {
    control.result
  } else {
    None
  }
}
