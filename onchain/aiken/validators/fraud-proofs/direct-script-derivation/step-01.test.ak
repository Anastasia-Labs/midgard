//// The shared direct-derivation step 01 forced arm, run through the real
//// spending handler: it hands on a forced leaf of the forced root of the
//// header its thread names, and refuses any other. This handler is step 01 of
//// distinct-asset-accumulation-limit, execution-native-script-invalid,
//// execution-source-script-decoding, receive-purpose-language,
//// unused-redeemer, unused-script-witness and missing-script-source.

use fraud_proofs/direct_script_derivation/step_01 as step_01_validator
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/step_01.{Args, ForcedSource}
use midgard/fraud_proofs/forced_step_01_fixture_v1.{ForcedStep01CaseV1} as cases
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

fn run_forced(case: ForcedStep01CaseV1) -> Bool {
  step_01_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        source: ForcedSource {
          input_index: 0,
          output_index: 0,
          header: case.header,
          membership: case.membership,
        },
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(case.thread_name, case.expected_state),
  )
}

test direct_derivation_step_01_hands_on_a_forced_leaf_of_the_thread_header() {
  run_forced(cases.member_of_thread_header_v1())
}

test direct_derivation_step_01_refuses_a_forced_root_other_than_the_header_commits() fail {
  run_forced(cases.root_other_than_the_header_commits_v1())
}

test direct_derivation_step_01_refuses_a_forced_count_other_than_the_header_commits() fail {
  run_forced(cases.count_other_than_the_header_commits_v1())
}

test direct_derivation_step_01_refuses_a_forced_leaf_with_key_and_value_swapped() fail {
  run_forced(cases.key_and_value_swapped_v1())
}

test direct_derivation_step_01_refuses_a_forced_leaf_under_another_key() fail {
  run_forced(cases.key_other_than_the_committed_one_v1())
}

test direct_derivation_step_01_refuses_a_forced_leaf_of_a_header_the_thread_does_not_name() fail {
  run_forced(cases.leaf_of_a_header_the_thread_does_not_name_v1())
}
