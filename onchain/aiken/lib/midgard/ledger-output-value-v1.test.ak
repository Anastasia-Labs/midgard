use aiken/builtin
use midgard/cek_data_v1
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_value_v1
use midgard/validation_merkle_v1

const terminal_control =
  #"8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f835820f6d68f04ebaaf198c28e605965e60a233a8a46428e2284e19f344f6290e7464d18591888ff"

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
  #"8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f83582035df7dc7ebdd5dba96f45ab79dbf23ba6a6325fc3b51f99b7c0cf62c8a31efb419138a193f46ff"

/// Pre-terminal (finalize-stage) control for the same maximum Value, so the
/// agreement covers the last real transition and not only the fixed point.
const typescript_maximum_value_pre_terminal_control =
  #"87010100581c1111111111111111111111111111111111111111111111111111111184582018a9e6706a9c0f115695a7d384af88baa135d31fa409e78ee3ab09ca6fe4cf8f18e41902ab1908e8845820ae48ba80db2f915cc4653c8bfd3a3394d4fe56f340dbe7cf66225478a2a93a65061910b6193620d87a80"

const typescript_maximum_value_root =
  #"35df7dc7ebdd5dba96f45ab79dbf23ba6a6325fc3b51f99b7c0cf62c8a31efb4"

const typescript_maximum_value_cbor_length = 5002

const typescript_maximum_value_memory = 16198

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
      asset_index: 2,
      policy_id: policy(34),
      asset_name: #"bbcc",
      quantity: 18446744073709551615,
      siblings: [],
      previous: None,
    },
  )
  expect Some(control2) = ledger_output_value_v1.step_v1(
    control1,
    3,
    peaks,
    8000000,
    ledger_output_value_v1.LedgerOutputValueAsset {
      asset_index: 1,
      policy_id: policy(17),
      asset_name: #"aa",
      quantity: 42,
      siblings: [first],
      previous: None,
    },
  )
  expect Some(control3) = ledger_output_value_v1.step_v1(
    control2,
    3,
    peaks,
    8000000,
    ledger_output_value_v1.LedgerOutputValueAsset {
      asset_index: 0,
      policy_id: policy(17),
      asset_name: #"",
      quantity: 1,
      siblings: [second],
      previous: Some(
        ledger_output_value_v1.LedgerOutputValueHeadV1 {
          asset_name: #"aa",
          quantity: 42,
          tail: cek_data_v1.empty_data_pair_summary_v1(),
        },
      ),
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
      asset_index: 2,
      policy_id: policy(34),
      asset_name: #"bbcc",
      quantity: 18446744073709551614,
      siblings: [],
      previous: None,
    },
  ) == None
}

// Mixed-width permutation battery: the frontier commits assets in canonical
// (length-then-bytes) order, `#"aa"` before `#"0102"`, while the evaluated
// context map is lexicographic, `#"0102"` before `#"aa"`. The fold must accept
// exactly the descending-lexical traversal with per-step membership indices
// and refuse every mutation at its clause.

fn mixed_width_leaves() -> List<ByteArray> {
  [
    ledger_output_commitment_v1.asset_leaf_hash(policy(17), #"aa", 5),
    ledger_output_commitment_v1.asset_leaf_hash(policy(17), #"0102", 7),
  ]
}

fn mixed_width_peaks() -> List<validation_merkle_v1.FrontierPeak> {
  expect [first, second] = mixed_width_leaves()
  let first_peaks = validation_merkle_v1.append_leaf(0, [], first)
  validation_merkle_v1.append_leaf(1, first_peaks, second)
}

fn mixed_width_first_step() -> ledger_output_value_v1.LedgerOutputValueControlV1 {
  expect [_first, second] = mixed_width_leaves()
  expect Some(control1) = ledger_output_value_v1.step_v1(
    ledger_output_value_v1.initial_control_v1(2),
    2,
    mixed_width_peaks(),
    8000000,
    ledger_output_value_v1.LedgerOutputValueAsset {
      asset_index: 0,
      policy_id: policy(17),
      asset_name: #"aa",
      quantity: 5,
      siblings: [second],
      previous: None,
    },
  )
  control1
}

fn mixed_width_second_witness(
  asset_index: Int,
  previous: Option<ledger_output_value_v1.LedgerOutputValueHeadV1>,
) -> ledger_output_value_v1.LedgerOutputValueWitnessV1 {
  expect [first, _second] = mixed_width_leaves()
  ledger_output_value_v1.LedgerOutputValueAsset {
    asset_index,
    policy_id: policy(17),
    asset_name: #"0102",
    quantity: 7,
    siblings: [first],
    previous,
  }
}

fn mixed_width_head() -> ledger_output_value_v1.LedgerOutputValueHeadV1 {
  ledger_output_value_v1.LedgerOutputValueHeadV1 {
    asset_name: #"aa",
    quantity: 5,
    tail: cek_data_v1.empty_data_pair_summary_v1(),
  }
}

test mixed_width_permutation_folds_to_the_lexicographic_map() {
  let peaks = mixed_width_peaks()
  expect Some(control2) = ledger_output_value_v1.step_v1(
    mixed_width_first_step(),
    2,
    peaks,
    8000000,
    mixed_width_second_witness(1, Some(mixed_width_head())),
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

test duplicate_membership_index_is_refused() {
  // The second step claims the already-consumed frontier slot 0; the leaf at
  // that index is the first asset, so the membership clause refuses.
  ledger_output_value_v1.step_v1(
    mixed_width_first_step(),
    2,
    mixed_width_peaks(),
    8000000,
    mixed_width_second_witness(0, Some(mixed_width_head())),
  ) == None
}

test out_of_range_membership_index_is_refused() {
  and {
    ledger_output_value_v1.step_v1(
      mixed_width_first_step(),
      2,
      mixed_width_peaks(),
      8000000,
      mixed_width_second_witness(2, Some(mixed_width_head())),
    ) == None,
    ledger_output_value_v1.step_v1(
      mixed_width_first_step(),
      2,
      mixed_width_peaks(),
      8000000,
      mixed_width_second_witness(-1, Some(mixed_width_head())),
    ) == None,
  }
}

test same_policy_step_without_head_opening_is_refused() {
  // Claiming the identity traversal (no previous head) on a same-policy step
  // is exactly the permuted-but-identity-claimed shape; the order clause
  // refuses it.
  ledger_output_value_v1.step_v1(
    mixed_width_first_step(),
    2,
    mixed_width_peaks(),
    8000000,
    mixed_width_second_witness(1, None),
  ) == None
}

test non_strictly_smaller_head_is_refused() {
  // Repeating the head name (`compare == Equal`) must fail the strict order
  // clause even though the head opening itself is genuine.
  expect [_first, second] = mixed_width_leaves()
  ledger_output_value_v1.step_v1(
    mixed_width_first_step(),
    2,
    mixed_width_peaks(),
    8000000,
    ledger_output_value_v1.LedgerOutputValueAsset {
      asset_index: 0,
      policy_id: policy(17),
      asset_name: #"aa",
      quantity: 5,
      siblings: [second],
      previous: Some(mixed_width_head()),
    },
  ) == None
}

test forged_head_opening_is_refused() {
  // A head whose reconstruction does not equal the accumulated within-policy
  // commitment must be refused even when its name orders correctly.
  ledger_output_value_v1.step_v1(
    mixed_width_first_step(),
    2,
    mixed_width_peaks(),
    8000000,
    mixed_width_second_witness(
      1,
      Some(
        ledger_output_value_v1.LedgerOutputValueHeadV1 {
          asset_name: #"aa",
          quantity: 6,
          tail: cek_data_v1.empty_data_pair_summary_v1(),
        },
      ),
    ),
  ) == None
}

test first_step_with_head_opening_is_refused() {
  expect [_first, second] = mixed_width_leaves()
  ledger_output_value_v1.step_v1(
    ledger_output_value_v1.initial_control_v1(2),
    2,
    mixed_width_peaks(),
    8000000,
    ledger_output_value_v1.LedgerOutputValueAsset {
      asset_index: 0,
      policy_id: policy(17),
      asset_name: #"aa",
      quantity: 5,
      siblings: [second],
      previous: Some(mixed_width_head()),
    },
  ) == None
}

test policy_change_with_head_opening_is_refused() {
  expect [first, second, _third] = asset_leaves()
  let peaks = asset_peaks()
  let control0 = ledger_output_value_v1.initial_control_v1(3)
  expect Some(control1) = ledger_output_value_v1.step_v1(
    control0,
    3,
    peaks,
    8000000,
    ledger_output_value_v1.LedgerOutputValueAsset {
      asset_index: 2,
      policy_id: policy(34),
      asset_name: #"bbcc",
      quantity: 18446744073709551615,
      siblings: [],
      previous: None,
    },
  )
  ledger_output_value_v1.step_v1(
    control1,
    3,
    peaks,
    8000000,
    ledger_output_value_v1.LedgerOutputValueAsset {
      asset_index: 1,
      policy_id: policy(17),
      asset_name: #"aa",
      quantity: 42,
      siblings: [first],
      previous: Some(
        ledger_output_value_v1.LedgerOutputValueHeadV1 {
          asset_name: #"bbcc",
          quantity: 18446744073709551615,
          tail: cek_data_v1.empty_data_pair_summary_v1(),
        },
      ),
    },
  ) == None
}

test stale_first_step_resumption_is_refused() {
  // A crafted control that still claims an empty current policy after a step
  // has been consumed must not accept a fresh no-head first step.
  let crafted = ledger_output_value_v1.LedgerOutputValueControlV1 {
    ..ledger_output_value_v1.initial_control_v1(2),
    asset_remaining: 1,
  }
  expect [_first, second] = mixed_width_leaves()
  ledger_output_value_v1.step_v1(
    crafted,
    2,
    mixed_width_peaks(),
    8000000,
    ledger_output_value_v1.LedgerOutputValueAsset {
      asset_index: 0,
      policy_id: policy(17),
      asset_name: #"aa",
      quantity: 5,
      siblings: [second],
      previous: None,
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
