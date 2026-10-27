use aiken/cbor
use midgard/fraud_proofs/common.{RedeemerCarriedMembership}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/execution_native_script_invalid/fixture as ens_fixture
use midgard/fraud_proofs/execution_native_script_invalid/spend_rank.{
  OpenActionV1,
}
use midgard/fraud_proofs/execution_native_script_invalid/types.{
  FinalV1, NoExecution, SpendRankV1,
}
use midgard/fraud_proofs/field_opening_v1
use midgard/fraud_proofs/first_fault_v1.{PurposeCoordinateV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/native_tx_machine_walk_v1 as walk

fn open(
  key: Bool,
  recorded_index: Int,
  raw_item_index: Int,
  budget: Int,
  membership_root: Option<ByteArray>,
) -> SpendRankV1 {
  let (state, opening, value, outref_key) = ens_fixture.key_spend_fixture(
    key,
    recorded_index,
  )
  let redeemer_root = when membership_root is {
    None -> state.bound.event_pre_root
    Some(other) -> other
  }
  spend_rank.open_v1(
    state,
    OpenActionV1 {
      input_index: 0,
      output_index: 0,
      raw_item_index,
      opening,
      descriptor_cbor: value,
      membership: RedeemerCarriedMembership {
        membership_proof: [],
        membership_proof_script_redeemer_index: 0,
      },
      budget,
    },
    [],
    [fixture.phas_withdraw_redeemer_entry(redeemer_root, outref_key, value)],
    fixture.test_certificate_policy_id,
  )
}

/// Opens with a one-item page, then ranks the remaining two.
fn completed(recorded_index: Int) -> SpendRankV1 {
  let (state, opening, _, _) = ens_fixture.key_spend_fixture(
    True,
    recorded_index,
  )
  let first = open(True, recorded_index, 2, 1, None)
  let Pair(view, position) = field_opening_v1.opened_field_walk(
    opening,
    cursor.anchor_v1(state.bound, 0),
    0,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(_, after_first) = workers.fold_spend_rank_v1(
    view,
    position,
    1,
    0,
    #"",
  )
  spend_rank.advance_v1(
    first,
    workers.ProgressArgsV1 {
      input_index: 0,
      output_index: 0,
      opening,
      checkpoint_bytes: walk.encode_field_walk_checkpoint(after_first),
      budget: 2,
    },
    [],
    fixture.test_certificate_policy_id,
  )
}

test key_spend_ranked_in_one_page_has_no_execution_at_its_rank() {
  let state = open(True, 1, 2, 3, None)
  and {
    state.complete,
    state.rank == 1,
    spend_rank.finish_v1(state).fact == NoExecution {
      coordinate: PurposeCoordinateV1 { kind: 0, index: 1 },
    },
  }
}

test key_spend_ranked_across_pages_has_no_execution_at_its_rank() {
  let state = completed(1)
  and {
    state.complete,
    state.rank == 1,
    spend_rank.finish_v1(state) == FinalV1 {
      bound: state.cursor.bound,
      fact: NoExecution {
        coordinate: PurposeCoordinateV1 { kind: 0, index: 1 },
      },
    },
  }
}

test script_locked_spend_is_refused() fail {
  open(False, 1, 2, 3, None).complete
}

test key_spend_at_another_rank_is_refused() fail {
  cbor.serialise(spend_rank.finish_v1(open(True, 2, 2, 3, None))) != #""
}

test raw_position_is_not_the_rank() fail {
  // Raw item two is not the spend recorded at ordinal two.
  cbor.serialise(spend_rank.finish_v1(completed(2))) != #""
}

test incomplete_rank_is_refused() fail {
  let state = open(True, 1, 2, 1, None)
  cbor.serialise(spend_rank.finish_v1(state)) != #""
}

test a_completed_rank_cannot_advance() fail {
  let state = completed(1)
  let (_, opening, _, _) = ens_fixture.key_spend_fixture(True, 1)
  spend_rank.advance_v1(
    state,
    workers.ProgressArgsV1 {
      input_index: 0,
      output_index: 0,
      opening,
      checkpoint_bytes: #"",
      budget: 1,
    },
    [],
    fixture.test_certificate_policy_id,
  ).complete
}

test descriptor_of_another_raw_item_is_refused() fail {
  open(True, 1, 1, 3, None).complete
}

test membership_under_another_root_is_refused() fail {
  open(True, 1, 2, 3, Some(ens_fixture.root)).complete
}

test open_needs_a_nonempty_page() fail {
  open(True, 1, 2, 0, None).complete
}

test open_page_is_bounded_by_the_scan_batch() fail {
  open(True, 1, 2, workers.scan_batch + 1, None).complete
}
