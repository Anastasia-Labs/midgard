//// The ValueAndMint asset bound counts entries, not distinct units: every
//// input, output and mint asset step adds one entry, whether or not its unit
//// was folded before. The step that would take the count past
//// `max_distinct_asset_count` is refused before its mutation witness is read.

use aiken/cbor
use aiken/primitive/bytearray
use midgard/ledger_output_v1.{max_distinct_asset_count}
use midgard/validation_machine/machine_types.{
  ValueAccumulatorAssetLimitExceeded, ValueAccumulatorMutationInvalid,
  ValueAccumulatorUpdated, ValueAccumulatorV1, ValueAssetMutationWitnessV1,
}
use midgard/validation_machine/value_and_mint.{apply_value_asset_mutation}
use midgard/validation_machine_tests/constants.{h_28_a}
use midgard/validation_machine_tests/fixtures_phase_a.{singleton_root}

fn unit() -> ByteArray {
  h_28_a |> bytearray.concat(#"beef")
}

/// An accumulator holding one unit at delta 5, with `entries` entries counted.
fn holding(entries: Int) -> ValueAccumulatorV1 {
  ValueAccumulatorV1 {
    lovelace_delta: 0,
    asset_root: singleton_root(unit(), cbor.serialise(5)),
    asset_entry_count: entries,
    nonzero_asset_count: 1,
  }
}

/// The honest witness for a step on the unit `holding` already holds.
const present =
  ValueAssetMutationWitnessV1 {
    delta_was_present: True,
    old_delta: 5,
    delta_proof: [],
  }

/// A witness naming a delta the root does not hold.
const forged =
  ValueAssetMutationWitnessV1 {
    delta_was_present: True,
    old_delta: 6,
    delta_proof: [],
  }

test a_repeated_unit_takes_the_count_to_the_bound() {
  let pre = holding(max_distinct_asset_count - 1)
  apply_value_asset_mutation(pre, unit(), 3, present) == ValueAccumulatorUpdated(
    ValueAccumulatorV1 {
      ..pre,
      asset_root: singleton_root(unit(), cbor.serialise(8)),
      asset_entry_count: max_distinct_asset_count,
    },
  )
}

test a_repeated_unit_returning_to_zero_still_counts() {
  let pre = holding(max_distinct_asset_count - 1)
  apply_value_asset_mutation(pre, unit(), -5, present) == ValueAccumulatorUpdated(
    ValueAccumulatorV1 {
      ..pre,
      asset_root: singleton_root(unit(), cbor.serialise(0)),
      asset_entry_count: max_distinct_asset_count,
      nonzero_asset_count: 0,
    },
  )
}

test a_repeated_unit_past_the_bound_crosses() {
  apply_value_asset_mutation(
    holding(max_distinct_asset_count),
    unit(),
    3,
    present,
  ) == ValueAccumulatorAssetLimitExceeded
}

test the_crossing_reads_no_mutation_witness() {
  apply_value_asset_mutation(
    holding(max_distinct_asset_count),
    unit(),
    3,
    forged,
  ) == ValueAccumulatorAssetLimitExceeded
}

test below_the_bound_a_forged_witness_is_refused() {
  apply_value_asset_mutation(
    holding(max_distinct_asset_count - 1),
    unit(),
    3,
    forged,
  ) == ValueAccumulatorMutationInvalid
}
