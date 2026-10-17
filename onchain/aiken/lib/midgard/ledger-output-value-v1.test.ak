use aiken/builtin
use midgard/cek_data_v1
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_value_v1
use midgard/validation_merkle_v1

const terminal_control =
  #"8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f835820df41255aaa09dd3fb8afcd43d7a4c207d45cca0d806297cd985c1c2e2331c3e31859184dff"

/// Terminal control emitted by the TypeScript producer for the exact
/// 5,000-byte maximum nested Cardano Value (1,592 assets across seven
/// policies).
///
/// Every `typescript_maximum_value_*` constant below is **generated**: it is
/// rebound from the vector `nested-value-boundary.test.ts` publishes by
/// `demo/midgard-validation/scripts/generate-nested-boundary-aiken-goldens.mjs`,
/// whose `--check` run is a required CI job. Do not hand-edit them; run
/// `pnpm --dir demo/midgard-validation run fixtures:nested-boundary-aiken:sync`.
const typescript_maximum_value_terminal_control =
  #"8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f83582098f2dd618837f4c61150ee34ed36bfe3e9503f75601c0efdb8c4212914ec5fb619138a193e9bff"

/// Pre-terminal (finalize-stage) control for the same maximum Value, so the
/// agreement covers the last real transition and not only the fixed point.
const typescript_maximum_value_pre_terminal_control =
  #"87010100581c1111111111111111111111111111111111111111111111111111111184582018a9e6706a9c0f115695a7d384af88baa135d31fa409e78ee3ab09ca6fe4cf8f18e41902ab1908e8845820b2ccb22904f13353b7fd9d21b42f971fe61eda32228b552b5a39d9901dd061b5061910b6193590d87a80"

const typescript_maximum_value_root =
  #"98f2dd618837f4c61150ee34ed36bfe3e9503f75601c0efdb8c4212914ec5fb6"

const typescript_maximum_value_cbor_length = 5002

const typescript_maximum_value_memory = 16027

const typescript_maximum_value_asset_count = 1592

const typescript_maximum_value_lovelace = 30000000

const typescript_maximum_value_asset_peak_hash_0 =
  #"d7186f5f4ba03f35771f5fe9a2bb1d98c1b6647fdb307a3a990674bdef9f44eb"

const typescript_maximum_value_asset_peak_hash_1 =
  #"3b64e0dee5d4fb8dea89b2e43cec29e9d042e70855235cdf5ee5dfba032370c8"

const typescript_maximum_value_asset_peak_hash_2 =
  #"99005b0619518750da865e276edd6ded5162e42e975b84ca5a0b39e2106d0c04"

const typescript_maximum_value_asset_peak_hash_3 =
  #"e140a7bcc4ae85c668a78b45e94afac7712d80519d9d4e1ad42c1e09afd56c71"

const typescript_maximum_value_asset_peak_hash_4 =
  #"d0f89d5bf0b4028db4ba39470ca2ff296bc5733936ddfef6beb77278f1003833"

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

/// C22 cross-language terminal agreement at the genuine maximum nested Value.
///
/// The 5,000-byte Value boundary is measured off-chain against the real
/// `validateOutputTooBigUTxO` predicate; this pins the on-chain fold's terminal
/// against that producer's exact bytes so a representation change in either
/// language breaks the gate. The finalize transition is replayed from the
/// producer's own pre-terminal control over the producer's exact 1,592-leaf
/// asset frontier, so agreement covers the last real step, not only the fixed
/// point.
test maximum_nested_value_terminal_agrees_with_typescript() {
  let asset_count = typescript_maximum_value_asset_count
  let lovelace = typescript_maximum_value_lovelace
  // The peak *heights* are the set bits of `asset_count`, so they are
  // structural rather than measured: a producer that moved the boundary to a
  // different asset count fails `frontier_is_well_formed` below rather than
  // silently agreeing. The hashes are generated alongside the controls.
  let asset_peaks = [
    validation_merkle_v1.FrontierPeak {
      height: 3,
      hash: typescript_maximum_value_asset_peak_hash_0,
    },
    validation_merkle_v1.FrontierPeak {
      height: 4,
      hash: typescript_maximum_value_asset_peak_hash_1,
    },
    validation_merkle_v1.FrontierPeak {
      height: 5,
      hash: typescript_maximum_value_asset_peak_hash_2,
    },
    validation_merkle_v1.FrontierPeak {
      height: 9,
      hash: typescript_maximum_value_asset_peak_hash_3,
    },
    validation_merkle_v1.FrontierPeak {
      height: 10,
      hash: typescript_maximum_value_asset_peak_hash_4,
    },
  ]
  let pre_terminal = ledger_output_value_v1.decode_control_v1(
    typescript_maximum_value_pre_terminal_control,
  )
  expect Some(terminal) = ledger_output_value_v1.step_v1(
    pre_terminal,
    asset_count,
    asset_peaks,
    lovelace,
    ledger_output_value_v1.LedgerOutputValueNoWitness,
  )
  let decoded = ledger_output_value_v1.decode_control_v1(
    typescript_maximum_value_terminal_control,
  )
  expect Some(result) = ledger_output_value_v1.finalize_v1(terminal)
  and {
    validation_merkle_v1.frontier_is_well_formed(asset_count, asset_peaks),
    pre_terminal.stage == ledger_output_value_v1.stage_finalize,
    pre_terminal.asset_remaining == 0,
    pre_terminal.current_policy == policy(17),
    terminal == decoded,
    ledger_output_value_v1.encode_control_v1(terminal) == typescript_maximum_value_terminal_control,
    result.root == typescript_maximum_value_root,
    result.cbor_length == typescript_maximum_value_cbor_length,
    result.memory == typescript_maximum_value_memory,
    ledger_output_value_v1.finalize_v1(pre_terminal) == None,
    ledger_output_value_v1.step_v1(
      pre_terminal,
      asset_count,
      asset_peaks,
      lovelace + 1,
      ledger_output_value_v1.LedgerOutputValueNoWitness,
    ) != Some(decoded),
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

// The first step opens the last leaf. Before the index was derived, opening
// any leaf first was accepted.
test lower_first_index_is_refused() {
  expect [first, _second, _third] = asset_leaves()
  ledger_output_value_v1.step_v1(
    ledger_output_value_v1.initial_control_v1(3),
    3,
    asset_peaks(),
    8000000,
    ledger_output_value_v1.LedgerOutputValueAsset {
      policy_id: policy(17),
      asset_name: #"aa",
      quantity: 42,
      siblings: [first],
    },
  ) == None
}

// After the last leaf the step opens the one before it; skipping it to the
// first leaf is refused.
test skipped_asset_is_refused() {
  expect [_first, second, _third] = asset_leaves()
  let peaks = asset_peaks()
  expect Some(control1) = ledger_output_value_v1.step_v1(
    ledger_output_value_v1.initial_control_v1(3),
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
  ledger_output_value_v1.step_v1(
    control1,
    3,
    peaks,
    8000000,
    ledger_output_value_v1.LedgerOutputValueAsset {
      policy_id: policy(17),
      asset_name: #"",
      quantity: 1,
      siblings: [second],
    },
  ) == None
}

// Mixed-width battery: the frontier commits assets in plain byte order,
// `#"0102"` before the shorter `#"aa"`, the same order as the evaluated context
// map. The fold walks the leaves from the last to the first.

fn mixed_width_leaves() -> List<ByteArray> {
  [
    ledger_output_commitment_v1.asset_leaf_hash(policy(17), #"0102", 7),
    ledger_output_commitment_v1.asset_leaf_hash(policy(17), #"aa", 5),
  ]
}

fn mixed_width_peaks() -> List<validation_merkle_v1.FrontierPeak> {
  expect [first, second] = mixed_width_leaves()
  let first_peaks = validation_merkle_v1.append_leaf(0, [], first)
  validation_merkle_v1.append_leaf(1, first_peaks, second)
}

fn mixed_width_first_witness() -> ledger_output_value_v1.LedgerOutputValueWitnessV1 {
  expect [first, _second] = mixed_width_leaves()
  ledger_output_value_v1.LedgerOutputValueAsset {
    policy_id: policy(17),
    asset_name: #"aa",
    quantity: 5,
    siblings: [first],
  }
}

fn mixed_width_first_step() -> ledger_output_value_v1.LedgerOutputValueControlV1 {
  expect Some(control1) = ledger_output_value_v1.step_v1(
    ledger_output_value_v1.initial_control_v1(2),
    2,
    mixed_width_peaks(),
    8000000,
    mixed_width_first_witness(),
  )
  control1
}

fn mixed_width_second_witness() -> ledger_output_value_v1.LedgerOutputValueWitnessV1 {
  expect [_first, second] = mixed_width_leaves()
  ledger_output_value_v1.LedgerOutputValueAsset {
    policy_id: policy(17),
    asset_name: #"0102",
    quantity: 7,
    siblings: [second],
  }
}

test mixed_width_descending_walk_folds_to_the_lexicographic_map() {
  let peaks = mixed_width_peaks()
  expect Some(control2) = ledger_output_value_v1.step_v1(
    mixed_width_first_step(),
    2,
    peaks,
    8000000,
    mixed_width_second_witness(),
  )
  expect Some(control3) = ledger_output_value_v1.step_v1(
    control2,
    2,
    peaks,
    8000000,
    ledger_output_value_v1.LedgerOutputValueNoWitness,
  )
  expect Some(terminal) = ledger_output_value_v1.step_v1(
    control3,
    2,
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
            Pair(builtin.b_data(#"0102"), builtin.i_data(7)),
            Pair(builtin.b_data(#"aa"), builtin.i_data(5)),
          ],
        ),
      ),
    ],
  )
  ledger_output_value_v1.finalize_v1(terminal) == Some(
    cek_data_v1.semantic_data_summary_v1(expected),
  )
}

test finalize_is_refused_while_assets_remain() {
  // Skipping to the finalize stage early would commit a Value without the
  // assets not yet folded.
  let peaks = mixed_width_peaks()
  let untouched = ledger_output_value_v1.initial_control_v1(2)
  let partial = mixed_width_first_step()
  and {
    partial.asset_remaining == 1,
    ledger_output_value_v1.step_v1(
      untouched,
      2,
      peaks,
      8000000,
      ledger_output_value_v1.LedgerOutputValueNoWitness,
    ) == None,
    ledger_output_value_v1.step_v1(
      partial,
      2,
      peaks,
      8000000,
      ledger_output_value_v1.LedgerOutputValueNoWitness,
    ) == None,
    ledger_output_value_v1.finalize_v1(untouched) == None,
    ledger_output_value_v1.finalize_v1(partial) == None,
  }
}

test reopened_asset_is_refused() {
  // The second step re-opens the asset the first step consumed; the derived
  // index is 0 and that leaf is the other asset, so membership refuses.
  ledger_output_value_v1.step_v1(
    mixed_width_first_step(),
    2,
    mixed_width_peaks(),
    8000000,
    mixed_width_first_witness(),
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

/// A control is only ever written well formed, which is what lets a thread
/// read its own control back without re-checking it.
test encoding_refuses_a_malformed_control() fail {
  let malformed = ledger_output_value_v1.LedgerOutputValueControlV1 {
    ..ledger_output_value_v1.initial_control_v1(0),
    current_policy: #"00",
  }
  ledger_output_value_v1.encode_control_v1(malformed) != #""
}

/// Controls written by `encode_control_v1`, with and without a result, read
/// back unchanged without the checks `decode_control_v1` repeats.
test written_control_reads_back_unchanged() {
  let reads_back = fn(cbor) {
    ledger_output_value_v1.decode_written_control_v1(cbor) == ledger_output_value_v1.decode_control_v1(
      cbor,
    )
  }
  and {
    reads_back(terminal_control),
    reads_back(typescript_maximum_value_pre_terminal_control),
    reads_back(
      ledger_output_value_v1.encode_control_v1(
        ledger_output_value_v1.initial_control_v1(3),
      ),
    ),
  }
}
