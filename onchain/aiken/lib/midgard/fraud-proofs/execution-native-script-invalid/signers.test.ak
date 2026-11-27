use aiken/collection/list
use aiken/crypto.{blake2b_224}
use midgard/fraud_proofs/execution_native_script_invalid/fixture as ens_fixture
use midgard/fraud_proofs/execution_native_script_invalid/signers.{
  PageV1, signer_leaf_v1,
}
use midgard/fraud_proofs/execution_native_script_invalid/types.{EvalV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_address_witness}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/validation_merkle_v1

fn page(opening: FieldOpeningV1, budget: Int) -> PageV1 {
  PageV1 { input_index: 0, output_index: 0, opening, budget }
}

fn run(
  state: EvalV1,
  opening: FieldOpeningV1,
  budget: Int,
) -> Pair<Bool, EvalV1> {
  signers.page_v1(
    state,
    page(opening, budget),
    [],
    fixture.test_certificate_policy_id,
  )
}

fn expected(items: List<native_types.MidgardAddressWitness>) {
  validation_merkle_v1.build_frontier(
    list.map(
      items,
      fn(item) { signer_leaf_v1(blake2b_224(item.verification_key)) },
    ),
  )
}

test signer_of_an_item_is_the_hash_of_its_key() {
  let item = ens_fixture.witness(#"07")
  signers.signer_of_item_v1(encode_midgard_address_witness(item)) == blake2b_224(
    item.verification_key,
  )
}

test one_page_builds_the_whole_frontier() {
  let (state, opening) = ens_fixture.signers_fixture(
    ens_fixture.witnesses(),
    ens_fixture.witnesses(),
  )
  let Pair(complete, next) = run(state, opening, 8)
  let built = expected(ens_fixture.witnesses())
  and {
    complete,
    next.signer_count == 3,
    next.signer_peaks == built.peaks,
    next.bound == state.bound,
    next.cursor == 0,
  }
}

test pages_build_the_frontier_in_field_order() {
  let (state, opening) = ens_fixture.signers_fixture(
    ens_fixture.witnesses(),
    ens_fixture.witnesses(),
  )
  let Pair(first_complete, first) = run(state, opening, 2)
  let Pair(second_complete, second) = run(first, opening, 2)
  let built = expected(ens_fixture.witnesses())
  and {
    !first_complete,
    first.signer_count == 2,
    second_complete,
    second.signer_count == 3,
    second.signer_peaks == built.peaks,
  }
}

test page_ending_exactly_at_the_field_end_completes() {
  let (state, opening) = ens_fixture.signers_fixture(
    ens_fixture.witnesses(),
    ens_fixture.witnesses(),
  )
  let Pair(complete, next) = run(state, opening, 3)
  complete && next.signer_count == 3
}

test an_empty_field_completes_with_no_signers() {
  let (state, opening) = ens_fixture.signers_fixture([], [])
  let Pair(complete, next) = run(state, opening, 1)
  complete && next.signer_count == 0 && next.signer_peaks == []
}

test a_forged_field_is_refused() fail {
  let (state, opening) = ens_fixture.signers_fixture(
    ens_fixture.witnesses(),
    [ens_fixture.witness(#"01"), ens_fixture.witness(#"03")],
  )
  let Pair(complete, _) = run(state, opening, 8)
  complete
}

test a_zero_budget_page_is_refused() fail {
  let (state, opening) = ens_fixture.signers_fixture(
    ens_fixture.witnesses(),
    ens_fixture.witnesses(),
  )
  let Pair(complete, _) = run(state, opening, 0)
  complete
}
