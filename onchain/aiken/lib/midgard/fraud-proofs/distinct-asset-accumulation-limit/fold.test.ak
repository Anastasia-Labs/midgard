use midgard/fraud_proofs/all_purposes_scan_fixture_v1.{policy, spent_input}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/fixture.{
  PagesV1, ShapeV1, derive, derive_paged, one_page, output_of, rank_of, tx_of,
}
use midgard/fraud_proofs/distinct_asset_accumulation_limit/fold.{
  Admitted, Crosses, OutputCounted, OutputPaused, RankV1, TerminalV1,
  admit_entries_v1, count_output_v1, output_value_start_v1, rank_v1,
}
use midgard/fraud_proofs/first_fault_v1.{
  InputAssetFault, MintAssetFault, OutputAssetFault, ValueFaultPositionV1,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_tx_input, encode_midgard_tx_output,
}
use midgard/fraud_proofs/native_tx/types as native_types

fn spends(counts: List<Int>) -> ShapeV1 {
  ShapeV1 { spends: counts, references: [], outputs: [], mint: [] }
}

fn crossing(shape: ShapeV1) -> Option<ValueFaultPositionV1> {
  derive(tx_of(shape)).crossing
}

fn crossing_paged(
  shape: ShapeV1,
  pages: PagesV1,
) -> Option<ValueFaultPositionV1> {
  derive_paged(tx_of(shape), pages).crossing
}

// ## The admission rule: an entry is refused once 16,384 precede it

test admit_up_to_the_bound() {
  and {
    admit_entries_v1(0, 16_383) == Admitted(16_383),
    admit_entries_v1(0, 16_384) == Admitted(16_384),
    admit_entries_v1(16_383, 1) == Admitted(16_384),
    admit_entries_v1(16_384, 0) == Admitted(16_384),
  }
}

test admit_past_the_bound_names_the_refused_entry() {
  and {
    admit_entries_v1(0, 16_385) == Crosses(16_384),
    admit_entries_v1(16_384, 1) == Crosses(0),
    admit_entries_v1(16_383, 2) == Crosses(1),
    admit_entries_v1(16_000, 1_000) == Crosses(384),
  }
}

// ## Inputs: each spent input counts its descriptor's asset entries

test inputs_below_the_bound_do_not_cross() {
  crossing(spends([16_383])) == None
}

test inputs_at_the_bound_do_not_cross() {
  crossing(spends([16_384])) == None
}

test inputs_past_the_bound_cross_at_the_refused_entry() {
  crossing(spends([16_385])) == Some(
    InputAssetFault { input_index: 0, asset_index: 16_384 },
  )
}

test inputs_count_every_entry_across_inputs() {
  and {
    crossing(spends([16_000, 384])) == None,
    crossing(spends([16_000, 385])) == Some(
      InputAssetFault { input_index: 1, asset_index: 384 },
    ),
  }
}

test inputs_crossing_is_the_same_in_single_input_pages() {
  crossing_paged(spends([16_000, 385]), PagesV1 { ..one_page, inputs: 1 }) == Some(
    InputAssetFault { input_index: 1, asset_index: 384 },
  )
}

// ## The input arm's coordinate is the schedule position

fn with_references(references: List<native_types.MidgardTxInput>) -> ShapeV1 {
  ShapeV1 { spends: [16_000, 385], references, outputs: [], mint: [] }
}

const low =
  native_types.MidgardTxInput {
    tx_id: #"0000000000000000000000000000000000000000000000000000000000000000",
    output_index: 0,
  }

const high =
  native_types.MidgardTxInput {
    tx_id: #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    output_index: 0,
  }

test reference_inputs_after_the_spent_input_do_not_move_it() {
  crossing(with_references([high])) == Some(
    InputAssetFault { input_index: 1, asset_index: 384 },
  )
}

test reference_inputs_before_the_spent_input_move_it() {
  crossing(with_references([low])) == Some(
    InputAssetFault { input_index: 2, asset_index: 384 },
  )
}

test a_reference_input_equal_to_an_earlier_spent_input_moves_it() {
  // The reference equal to spent input 0 sorts after spent input 0 and
  // before spent input 1.
  crossing(with_references([low, spent_input(0), high])) == Some(
    InputAssetFault { input_index: 3, asset_index: 384 },
  )
}

test a_reference_input_equal_to_the_spent_input_sorts_after_it() {
  crossing(
    ShapeV1 {
      spends: [16_385],
      references: [spent_input(0)],
      outputs: [],
      mint: [],
    },
  ) == Some(InputAssetFault { input_index: 0, asset_index: 16_384 })
}

fn ranked(references, rank: Int) -> TerminalV1 {
  let tx = tx_of(with_references(references))
  let item = encode_midgard_tx_input(spent_input(1))
  rank_v1(
    RankV1 {
      bound: tx.bound,
      input_ordinal: 1,
      input_item: item,
      asset_index: 384,
    },
    tx.reference_opening,
    rank,
    [],
    policy,
  )
}

test rank_accepts_the_exact_count_of_smaller_references() {
  let tx = tx_of(with_references([low, spent_input(0), high]))
  let item = encode_midgard_tx_input(spent_input(1))
  and {
    rank_of(tx, item) == 2,
    ranked([low, spent_input(0), high], 2).crossing == Some(
      InputAssetFault { input_index: 3, asset_index: 384 },
    ),
  }
}

test rank_refuses_one_too_few() fail {
  ranked([low, spent_input(0), high], 1).crossing != None
}

test rank_refuses_one_too_many() fail {
  ranked([low, spent_input(0), high], 3).crossing != None
}

test rank_refuses_a_rank_past_the_field() fail {
  ranked([low], 2).crossing != None
}

test rank_refuses_a_negative_rank() fail {
  ranked([low], -1).crossing != None
}

test rank_refuses_counting_an_equal_reference() fail {
  // A reference equal to the spent input sorts after it, so it is not
  // counted.
  ranked([spent_input(1)], 1).crossing != None
}

test rank_counts_no_equal_reference() {
  ranked([spent_input(1)], 0).crossing == Some(
    InputAssetFault { input_index: 1, asset_index: 384 },
  )
}

// ## Outputs: each output counts its policies' asset-map sizes

fn outputs(input: Int, values: List<List<Int>>) -> ShapeV1 {
  ShapeV1 { spends: [input], references: [], outputs: values, mint: [] }
}

test outputs_below_the_bound_do_not_cross() {
  crossing(outputs(16_382, [[1]])) == None
}

test outputs_at_the_bound_do_not_cross() {
  crossing(outputs(16_382, [[2]])) == None
}

test outputs_past_the_bound_cross_at_the_refused_entry() {
  crossing(outputs(16_382, [[3]])) == Some(
    OutputAssetFault { output_index: 0, asset_index: 2 },
  )
}

test outputs_index_assets_across_policies() {
  and {
    crossing(outputs(16_382, [[1, 1]])) == None,
    crossing(outputs(16_382, [[1, 2]])) == Some(
      OutputAssetFault { output_index: 0, asset_index: 2 },
    ),
  }
}

test outputs_carry_the_count_across_outputs() {
  and {
    crossing(outputs(16_382, [[1], [1]])) == None,
    crossing(outputs(16_382, [[1], [2]])) == Some(
      OutputAssetFault { output_index: 1, asset_index: 1 },
    ),
    crossing(outputs(16_384, [[], [1]])) == Some(
      OutputAssetFault { output_index: 1, asset_index: 0 },
    ),
  }
}

test outputs_crossing_is_the_same_at_every_budget() {
  let shape = outputs(16_380, [[], [2, 3], [1]])
  let expected = Some(OutputAssetFault { output_index: 1, asset_index: 4 })
  and {
    crossing(shape) == expected,
    crossing_paged(shape, PagesV1 { ..one_page, outputs: 1 }) == expected,
    crossing_paged(shape, PagesV1 { ..one_page, outputs: 2 }) == expected,
    crossing_paged(shape, PagesV1 { ..one_page, outputs: 3 }) == expected,
  }
}

test outputs_without_a_crossing_finish_at_every_budget() {
  let shape = outputs(16_370, [[2, 3], [], [4, 1]])
  and {
    crossing_paged(shape, PagesV1 { ..one_page, outputs: 1 }) == None,
    crossing_paged(shape, PagesV1 { ..one_page, outputs: 2 }) == None,
  }
}

fn output_item(counts: List<Int>) -> ByteArray {
  encode_midgard_tx_output(output_of(counts))
}

test the_last_policy_needs_no_skip() {
  // One unit per policy plus one per skipped asset of an earlier policy.
  let item = output_item([1, 5])
  count_output_v1(item, output_value_start_v1(item), 0, 3) == OutputCounted {
    entry_count: 6,
    budget: 0,
  }
}

test a_spent_budget_pauses_inside_an_output() {
  let item = output_item([2, 5])
  when count_output_v1(item, output_value_start_v1(item), 0, 2) is {
    OutputPaused { entry_count, resume } -> and {
        entry_count == 2,
        resume.policies_remaining == 1,
        resume.assets_remaining == 1,
        resume.asset_index == 2,
        count_output_v1(item, resume, entry_count, 2) == OutputCounted {
          entry_count: 7,
          budget: 0,
        },
      }
    _ -> False
  }
}

// ## Mint: each mint item counts its policy's declared asset count

fn minted(input: Int, counts: List<Int>) -> ShapeV1 {
  ShapeV1 { spends: [input], references: [], outputs: [], mint: counts }
}

test mint_below_the_bound_does_not_cross() {
  crossing(minted(16_382, [1])) == None
}

test mint_at_the_bound_does_not_cross() {
  crossing(minted(16_382, [2])) == None
}

test mint_past_the_bound_crosses_at_the_refused_entry() {
  crossing(minted(16_382, [3])) == Some(MintAssetFault { mint_index: 2 })
}

test mint_index_is_flat_across_policies() {
  and {
    crossing(minted(16_381, [2, 1])) == None,
    crossing(minted(16_381, [2, 2])) == Some(MintAssetFault { mint_index: 3 }),
  }
}

test mint_crossing_is_the_same_one_policy_per_step() {
  crossing_paged(minted(16_381, [2, 2]), PagesV1 { ..one_page, mint: 1 }) == Some(
    MintAssetFault { mint_index: 3 },
  )
}

test the_fold_carries_inputs_and_outputs_into_mint() {
  crossing(
    ShapeV1 { spends: [16_380], references: [], outputs: [[1], [2]], mint: [2] },
  ) == Some(MintAssetFault { mint_index: 1 })
}

test an_empty_mint_ends_the_fold_without_a_crossing() {
  crossing(
    ShapeV1 { spends: [16_383], references: [], outputs: [[1]], mint: [] },
  ) == None
}
