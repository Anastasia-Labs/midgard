use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/address
use cardano/assets.{PolicyId, quantity_of}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Redeemer, ScriptPurpose,
  Transaction, placeholder,
}
use midgard/claim_registry_v1 as registry
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/hub_oracle as hub
use midgard/state_queue

fn mint_redeemer(
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  policy_id: PolicyId,
) -> Data {
  expect Some(Pair(_, redeemer)) = redeemers
    |> list.find(fn(Pair(purpose, _)) { purpose == Mint(policy_id) })
  redeemer
}

fn state_queue_mint_redeemer(
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  state_queue_policy_id: PolicyId,
) -> state_queue.MintRedeemer {
  expect redeemer: state_queue.MintRedeemer = mint_redeemer(
    redeemers,
    state_queue_policy_id,
  )
  redeemer
}

fn hub_datum_from_spent_input(
  inputs: List<Input>,
  hub_oracle_policy_id: PolicyId,
  hub_oracle_input_index: Int,
) -> hub.Datum {
  let hub_input = utils.get_authentic_input_of(
    inputs,
    hub_oracle_policy_id,
    hub.asset_name,
    hub_oracle_input_index,
  )
  expect InlineDatum(datum_data) = hub_input.output.datum
  expect datum: hub.Datum = datum_data
  datum
}

fn computation_thread_transition_matches(
  mutation: registry.Mutation,
  computation_thread_redeemer: ct.MintRedeemer,
  mint: assets.Value,
  computation_thread_policy_id: PolicyId,
) -> Bool {
  let claim_id = registry.mutation_claim_id(mutation)
  when mutation is {
    registry.OpenClaim { .. } ->
      when computation_thread_redeemer is {
        ct.Init { .. } ->
          quantity_of(mint, computation_thread_policy_id, claim_id) == 1
        _ -> False
      }
    registry.CancelClaim { .. } ->
      when computation_thread_redeemer is {
        ct.BurnForCancellation { burning_token_asset_name } -> and {
            burning_token_asset_name == claim_id,
            quantity_of(mint, computation_thread_policy_id, claim_id) == -1,
          }
        _ -> False
      }
    registry.CloseClaim { .. } ->
      when computation_thread_redeemer is {
        ct.Success { burning_token_asset_name } -> and {
            burning_token_asset_name == claim_id,
            quantity_of(mint, computation_thread_policy_id, claim_id) == -1,
          }
        _ -> False
      }
  }
}

validator spend(hub_oracle_policy_id: PolicyId) {
  spend(
    datum: Option<registry.Datum>,
    redeemer: registry.Redeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(current_datum) = datum
    expect Some(own_input) = tx.inputs
      |> list.find(
          fn(Input { output_reference, .. }) { output_reference == own_ref },
        )
    let authenticated_input = registry.unique_input(
      tx.inputs,
      hub_oracle_policy_id,
      own_input.output.address,
    )
    expect authenticated_input.output_reference == own_ref
    expect registry.decode_datum(own_input.output) == current_datum

    when redeemer is {
      registry.Mutate { mutation } -> {
        expect Some(next_root) = registry.successor_root(
          current_datum.claims_root,
          mutation,
          tx.reference_inputs,
        )
        let registry_output = registry.unique_output(
          tx.outputs,
          hub_oracle_policy_id,
          own_input.output.address,
        )
        expect
          registry.decode_datum(registry_output) == registry.Datum {
            claims_root: next_root,
            computation_thread_policy_id: current_datum.computation_thread_policy_id,
          }
        expect computation_thread_redeemer: ct.MintRedeemer = mint_redeemer(
          tx.redeemers,
          current_datum.computation_thread_policy_id,
        )
        and {
          quantity_of(tx.mint, hub_oracle_policy_id, registry.asset_name) == 0,
          registry_output.address == own_input.output.address,
          registry_output.value == own_input.output.value,
          computation_thread_transition_matches(
            mutation,
            computation_thread_redeemer,
            tx.mint,
            current_datum.computation_thread_policy_id,
          ),
        }
      }
      registry.Deinit { hub_oracle_input_index } -> {
        let hub_datum = hub_datum_from_spent_input(
          tx.inputs,
          hub_oracle_policy_id,
          hub_oracle_input_index,
        )
        expect
          state_queue_mint_redeemer(tx.redeemers, hub_datum.state_queue) == state_queue.Deinit
        and {
          quantity_of(tx.mint, hub_oracle_policy_id, registry.asset_name) == -1,
          registry.has_no_output(tx.outputs, hub_oracle_policy_id),
        }
      }
    }
  }

  else(_) {
    fail
  }
}

const test_hub_policy =
  #"11111111111111111111111111111111111111111111111111111111"

const test_computation_thread_policy =
  #"22222222222222222222222222222222222222222222222222222222"

const test_claim_registry_script =
  #"33333333333333333333333333333333333333333333333333333333"

const test_category_id = #"0000001f"

const test_header_hash =
  #"abababababababababababababababababababababababababababab"

fn test_claim_id() -> ByteArray {
  test_category_id |> bytearray.concat(test_header_hash)
}

fn test_other_claim_id() -> ByteArray {
  #"0000001e" |> bytearray.concat(test_header_hash)
}

fn test_registry_outref(index: Int) -> OutputReference {
  OutputReference {
    transaction_id: #"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd",
    output_index: index,
  }
}

fn test_registry_datum(root: ByteArray) -> registry.Datum {
  registry.Datum {
    claims_root: root,
    computation_thread_policy_id: test_computation_thread_policy,
  }
}

fn test_registry_output(root: ByteArray, policy_id: PolicyId) -> Output {
  let datum: Data = test_registry_datum(root)
  Output {
    address: address.from_script(test_claim_registry_script),
    value: assets.from_lovelace(2_000_000)
      |> assets.add(policy_id, registry.asset_name, 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn test_registry_input(root: ByteArray, policy_id: PolicyId) -> Input {
  Input {
    output_reference: test_registry_outref(0),
    output: test_registry_output(root, policy_id),
  }
}

fn test_ct_redeemer_entry(
  redeemer: ct.MintRedeemer,
) -> Pair<ScriptPurpose, Redeemer> {
  let data: Data = redeemer
  Pair(Mint(test_computation_thread_policy), data)
}

fn test_open_ct_redeemer() -> ct.MintRedeemer {
  ct.Init {
    first_step_output_index: 1,
    fraud_category_id: test_category_id,
    fraud_category: test_claim_registry_script,
    fraud_category_membership_proof: [],
    fraud_proof_catalogue_ref_input_index: 0,
    inclusion_proof_script_redeemer_index: 0,
    hub_oracle_ref_input_index: 1,
    fraudulent_block_ref_input_index: 2,
  }
}

fn test_live_root() -> ByteArray {
  fixture.adversarial_membership_root(
    test_claim_id(),
    registry.live_claim_value,
    [],
  )
}

fn test_closed_root() -> ByteArray {
  fixture.adversarial_membership_root(
    test_claim_id(),
    registry.closed_claim_value,
    [],
  )
}

fn run_test_mutation(
  current_root: ByteArray,
  next_root: ByteArray,
  mutation: registry.Mutation,
  computation_thread_redeemer: ct.MintRedeemer,
  mint: assets.Value,
  inputs: List<Input>,
  reference_inputs: List<Input>,
) -> Bool {
  spend.spend(
    test_hub_policy,
    Some(test_registry_datum(current_root)),
    registry.Mutate { mutation },
    test_registry_outref(0),
    Transaction {
      ..placeholder,
      inputs,
      outputs: [test_registry_output(next_root, test_hub_policy)],
      reference_inputs,
      redeemers: [test_ct_redeemer_entry(computation_thread_redeemer)],
      mint,
    },
  )
}

test claim_registry_handler_opens_cancels_and_closes_exact_claim() {
  and {
    run_test_mutation(
      registry.empty_claims_root,
      test_live_root(),
      registry.OpenClaim {
        claim_id: test_claim_id(),
        proof: registry.RedeemerCarried([]),
      },
      test_open_ct_redeemer(),
      assets.from_asset(test_computation_thread_policy, test_claim_id(), 1),
      [test_registry_input(registry.empty_claims_root, test_hub_policy)],
      [],
    ),
    run_test_mutation(
      test_live_root(),
      registry.empty_claims_root,
      registry.CancelClaim {
        claim_id: test_claim_id(),
        proof: registry.RedeemerCarried([]),
      },
      ct.BurnForCancellation { burning_token_asset_name: test_claim_id() },
      assets.from_asset(test_computation_thread_policy, test_claim_id(), -1),
      [test_registry_input(test_live_root(), test_hub_policy)],
      [],
    ),
    run_test_mutation(
      test_live_root(),
      test_closed_root(),
      registry.CloseClaim {
        claim_id: test_claim_id(),
        proof: registry.RedeemerCarried([]),
      },
      ct.Success { burning_token_asset_name: test_claim_id() },
      assets.from_asset(test_computation_thread_policy, test_claim_id(), -1),
      [test_registry_input(test_live_root(), test_hub_policy)],
      [],
    ),
  }
}

test claim_registry_handler_rejects_cross_category_thread_coupling() fail {
  run_test_mutation(
    registry.empty_claims_root,
    test_live_root(),
    registry.OpenClaim {
      claim_id: test_claim_id(),
      proof: registry.RedeemerCarried([]),
    },
    test_open_ct_redeemer(),
    assets.from_asset(test_computation_thread_policy, test_other_claim_id(), 1),
    [test_registry_input(registry.empty_claims_root, test_hub_policy)],
    [],
  )
}

test claim_registry_handler_rejects_cross_deployment_singleton() fail {
  run_test_mutation(
    registry.empty_claims_root,
    test_live_root(),
    registry.OpenClaim {
      claim_id: test_claim_id(),
      proof: registry.RedeemerCarried([]),
    },
    test_open_ct_redeemer(),
    assets.from_asset(test_computation_thread_policy, test_claim_id(), 1),
    [
      test_registry_input(
        registry.empty_claims_root,
        #"99999999999999999999999999999999999999999999999999999999",
      ),
    ],
    [],
  )
}

test claim_registry_handler_rejects_duplicate_singleton_inputs() fail {
  run_test_mutation(
    registry.empty_claims_root,
    test_live_root(),
    registry.OpenClaim {
      claim_id: test_claim_id(),
      proof: registry.RedeemerCarried([]),
    },
    test_open_ct_redeemer(),
    assets.from_asset(test_computation_thread_policy, test_claim_id(), 1),
    [
      test_registry_input(registry.empty_claims_root, test_hub_policy),
      Input {
        output_reference: test_registry_outref(1),
        output: test_registry_output(
          registry.empty_claims_root,
          test_hub_policy,
        ),
      },
    ],
    [],
  )
}

test claim_registry_handler_rejects_duplicate_or_closed_open() fail {
  run_test_mutation(
    test_closed_root(),
    test_live_root(),
    registry.OpenClaim {
      claim_id: test_claim_id(),
      proof: registry.RedeemerCarried([]),
    },
    test_open_ct_redeemer(),
    assets.from_asset(test_computation_thread_policy, test_claim_id(), 1),
    [test_registry_input(test_closed_root(), test_hub_policy)],
    [],
  )
}

fn maximum_absence_proof() -> mpf.Proof {
  fixture.adversarial_non_membership_steps(63)
}

fn maximum_membership_proof() -> mpf.Proof {
  fixture.adversarial_branch_steps(64)
}

const maximum_absence_root =
  #"986a0feebec0cae2ee6f90d73a183bd8c3af710d086e4d803ab858e034f39ed9"

const maximum_open_successor =
  #"8bc39247d029beda5e93e3bce48aba55214d9dd81bb6541f24fe5b7d86cd8f84"

const maximum_live_root =
  #"157bb8bacf9181de0df0ebfe7f59ccbbb2b21359bf1d92d063627188beb4b713"

const maximum_close_successor =
  #"9264e7385cf453d6043eaf099b65cd19f14fd9ec25841f9c3a8e568b60ba3d56"

const maximum_cancel_successor =
  #"59166691d4ce75ccacc7e37d1a0995455ec73d70ecc9811576cbeee3a035ba89"

test claim_registry_maximum_root_constants_match_independent_fixtures() {
  and {
    fixture.adversarial_non_membership_root(
      test_claim_id(),
      maximum_absence_proof(),
    ) == maximum_absence_root,
    fixture.adversarial_membership_root(
      test_claim_id(),
      registry.live_claim_value,
      maximum_membership_proof(),
    ) == maximum_live_root,
    fixture.adversarial_membership_root(
      test_claim_id(),
      registry.closed_claim_value,
      maximum_membership_proof(),
    ) == maximum_close_successor,
  }
}

test claim_registry_maximum_open_successor_constant_is_exact() {
  expect Some(successor) = registry.successor_root(
    maximum_absence_root,
    registry.OpenClaim {
      claim_id: test_claim_id(),
      proof: registry.RedeemerCarried(maximum_absence_proof()),
    },
    [],
  )
  successor == maximum_open_successor
}

test claim_registry_handler_maximum_64_step_published_open_fits_release_reserve() {
  let proof = maximum_absence_proof()
  run_test_mutation(
    maximum_absence_root,
    maximum_open_successor,
    registry.OpenClaim {
      claim_id: test_claim_id(),
      proof: registry.PublishedChunks {
        ordered_chunk_reference_input_indices: [0, 1, 2, 3],
      },
    },
    test_open_ct_redeemer(),
    assets.from_asset(test_computation_thread_policy, test_claim_id(), 1),
    [test_registry_input(maximum_absence_root, test_hub_policy)],
    fixture.proof_chunk_reference_inputs(proof),
  )
}

test claim_registry_handler_maximum_64_step_published_close_fits_release_reserve() {
  let proof = maximum_membership_proof()
  run_test_mutation(
    maximum_live_root,
    maximum_close_successor,
    registry.CloseClaim {
      claim_id: test_claim_id(),
      proof: registry.PublishedChunks {
        ordered_chunk_reference_input_indices: [0, 1, 2, 3],
      },
    },
    ct.Success { burning_token_asset_name: test_claim_id() },
    assets.from_asset(test_computation_thread_policy, test_claim_id(), -1),
    [test_registry_input(maximum_live_root, test_hub_policy)],
    fixture.proof_chunk_reference_inputs(proof),
  )
}

test claim_registry_handler_maximum_64_step_published_cancel_fits_release_reserve() {
  let proof = maximum_membership_proof()
  run_test_mutation(
    maximum_live_root,
    maximum_cancel_successor,
    registry.CancelClaim {
      claim_id: test_claim_id(),
      proof: registry.PublishedChunks {
        ordered_chunk_reference_input_indices: [0, 1, 2, 3],
      },
    },
    ct.BurnForCancellation { burning_token_asset_name: test_claim_id() },
    assets.from_asset(test_computation_thread_policy, test_claim_id(), -1),
    [test_registry_input(maximum_live_root, test_hub_policy)],
    fixture.proof_chunk_reference_inputs(proof),
  )
}
