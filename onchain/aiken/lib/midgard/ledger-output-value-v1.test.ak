use aiken/builtin
use midgard/cek_data_v1
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_value_v1
use midgard/validation_merkle_v1

const terminal_control =
  #"8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f835820f6d68f04ebaaf198c28e605965e60a233a8a46428e2284e19f344f6290e7464d18591888ff"

fn policy(byte: Int) -> ByteArray {
  builtin.replicate_byte(28, byte)
}

fn asset_leaves() -> List<ByteArray> {
  [
    ledger_output_commitment_v1.asset_leaf_hash(policy(17), #"", 1),
    ledger_output_commitment_v1.asset_leaf_hash(policy(17), #"aa", 42),
    ledger_output_commitment_v1.asset_leaf_hash(
      policy(34),
      #"bbcc",
      18446744073709551615,
    ),
  ]
}

fn asset_peaks() -> List<validation_merkle_v1.FrontierPeak> {
  expect [first, second, third] = asset_leaves()
  let first_peaks = validation_merkle_v1.append_leaf(0, [], first)
  let second_peaks = validation_merkle_v1.append_leaf(1, first_peaks, second)
  validation_merkle_v1.append_leaf(2, second_peaks, third)
}

test folds_the_exact_plutus_value_from_reverse_memberships() {
  expect [first, second, _third] = asset_leaves()
  let peaks = asset_peaks()
  let control0 = ledger_output_value_v1.initial_control_v1(3)
  expect Some(control1) = ledger_output_value_v1.step_v1(
    control0,
    3,
    peaks,
    8000000,
    ledger_output_value_v1.LedgerOutputValueAsset {
      policy_id: policy(34),
      asset_name: #"bbcc",
      quantity: 18446744073709551615,
      siblings: [],
    },
  )
  expect Some(control2) = ledger_output_value_v1.step_v1(
    control1,
    3,
    peaks,
    8000000,
    ledger_output_value_v1.LedgerOutputValueAsset {
      policy_id: policy(17),
      asset_name: #"aa",
      quantity: 42,
      siblings: [first],
    },
  )
  expect Some(control3) = ledger_output_value_v1.step_v1(
    control2,
    3,
    peaks,
    8000000,
    ledger_output_value_v1.LedgerOutputValueAsset {
      policy_id: policy(17),
      asset_name: #"",
      quantity: 1,
      siblings: [second],
    },
  )
  expect Some(control4) = ledger_output_value_v1.step_v1(
    control3,
    3,
    peaks,
    8000000,
    ledger_output_value_v1.LedgerOutputValueNoWitness,
  )
  expect Some(terminal) = ledger_output_value_v1.step_v1(
    control4,
    3,
    peaks,
    8000000,
    ledger_output_value_v1.LedgerOutputValueNoWitness,
  )
  let expected = builtin.map_data(
    [
      Pair(
        builtin.b_data(#""),
        builtin.map_data([Pair(builtin.b_data(#""), builtin.i_data(8000000))]),
      ),
      Pair(
        builtin.b_data(policy(17)),
        builtin.map_data(
          [
            Pair(builtin.b_data(#""), builtin.i_data(1)),
            Pair(builtin.b_data(#"aa"), builtin.i_data(42)),
          ],
        ),
      ),
      Pair(
        builtin.b_data(policy(34)),
        builtin.map_data(
          [Pair(builtin.b_data(#"bbcc"), builtin.i_data(18446744073709551615))],
        ),
      ),
    ],
  )
  and {
    ledger_output_value_v1.finalize_v1(terminal) == Some(
      cek_data_v1.semantic_data_summary_v1(expected),
    ),
    ledger_output_value_v1.encode_control_v1(terminal) == terminal_control,
  }
}

test terminal_cross_language_control_decodes_canonically() {
  let control = ledger_output_value_v1.decode_control_v1(terminal_control)
  and {
    control.stage == ledger_output_value_v1.stage_terminal,
    ledger_output_value_v1.encode_control_v1(control) == terminal_control,
  }
}

test substituted_asset_facts_fail_closed() {
  let peaks = asset_peaks()
  let control = ledger_output_value_v1.initial_control_v1(3)
  ledger_output_value_v1.step_v1(
    control,
    3,
    peaks,
    8000000,
    ledger_output_value_v1.LedgerOutputValueAsset {
      policy_id: policy(34),
      asset_name: #"bbcc",
      quantity: 18446744073709551614,
      siblings: [],
    },
  ) == None
}

test empty_value_map_is_supported() {
  let control0 = ledger_output_value_v1.initial_control_v1(0)
  expect Some(control1) = ledger_output_value_v1.step_v1(
    control0,
    0,
    [],
    0,
    ledger_output_value_v1.LedgerOutputValueNoWitness,
  )
  expect Some(terminal) = ledger_output_value_v1.step_v1(
    control1,
    0,
    [],
    0,
    ledger_output_value_v1.LedgerOutputValueNoWitness,
  )
  ledger_output_value_v1.finalize_v1(terminal) == Some(
    cek_data_v1.semantic_data_summary_v1(builtin.map_data([])),
  )
}
