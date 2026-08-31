use aiken/builtin
use aiken/cbor
use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, Redeemer, ScriptPurpose, Spend, Transaction,
  placeholder,
}
use midgard/claim_registry_v1 as registry
use midgard/common/utils
use midgard/computation_thread.{
  BurnForCancellation, Init, MintRedeemer, StepDatum, Success,
}
use midgard/fraud_proof_catalogue as catalogue
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/hub_oracle as hub
use midgard/state_queue

fn claim_registry_mutation(
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  claim_registry_script_hash: ByteArray,
) -> registry.Mutation {
  let claim_registry_address = address.from_script(claim_registry_script_hash)
  expect [Input { output_reference, .. }] = inputs
    |> list.filter(
        fn(Input { output: Output { address: input_address, .. }, .. }) {
          input_address == claim_registry_address
        },
      )
  expect Some(Pair(_, redeemer_data)) = redeemers
    |> list.find(fn(Pair(purpose, _)) { purpose == Spend(output_reference) })
  expect registry.Mutate { mutation }: registry.Redeemer = redeemer_data
  mutation
}

validator mint(
  fraud_proof_catalogue_script_hash: ByteArray,
  hub_oracle_script_hash: PolicyId,
  claim_registry_script_hash: ByteArray,
) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      Init {
        first_step_output_index,
        fraud_category_id,
        fraud_category,
        fraud_category_membership_proof,
        fraud_proof_catalogue_ref_input_index,
        inclusion_proof_script_redeemer_index,
        hub_oracle_ref_input_index,
        fraudulent_block_ref_input_index,
      } -> {
        let
            Transaction {
              outputs,
              inputs,
              reference_inputs,
              redeemers,
              mint,
              extra_signatories,
              ..
            }
          = self

        // 1. Transaction must reference the fraud proof category's authentic
        //    utxo.
        let catalogue_merkle_root = reference_inputs
          |> catalogue.get_datum(
              fraud_proof_catalogue_script_hash,
              fraud_proof_catalogue_ref_input_index,
            )

        // 2. The provided fraud category (i.e. spending script hash for the
        //    first step of the desired fraud proof) must be a member of the
        //    Merkle structure which its root is attached to the referenced
        //    fraud proof category utxo.
        expect
          utils.plutarch_phas(
            merkle_root: catalogue_merkle_root,
            key: builtin.un_b_data(fraud_category_id),
            value: builtin.un_b_data(fraud_category),
            membership_proof: fraud_category_membership_proof,
            redeemers: redeemers,
            withdraw_redeemer_index: inclusion_proof_script_redeemer_index,
          )

        // 3. Transaction must retrieve policy ID of the state queue from the
        //    authentic hub oracle utxo.
        let hub.Datum { state_queue: state_queue_policy_id, .. } = hub.get_datum(
          reference_inputs,
          hub_oracle_script_hash,
          hub_oracle_ref_input_index,
        )

        // 4. Transaction must reference the fraudulent block. Let its key be
        //    `fraudulent_header_hash`.
        let _fraudulent_header, fraudulent_header_hash <- state_queue.get_block_datum_v1(
          reference_inputs,
          state_queue_policy_id,
          fraudulent_block_ref_input_index,
        )

        // 5. Output at the specified index must exist and:
        //    - Go to an address which its payment part is a script
        //    - Have an inline datum attached
        //    - Have no reference script attached
        //    Let this output be `first_step_output`.
        expect
            Some(Output {
              address: Address {
                payment_credential: Script(first_step_output_script_hash),
                ..
              },
              value: first_step_output_value,
              datum: InlineDatum(first_step_output_datum_data),
              reference_script: None,
            })
          = list.at(outputs, first_step_output_index)

        // 6. The script hash of the destination address must be equal to the
        //    proven fraud category.
        expect first_step_output_script_hash == fraud_category

        // 7. `first_step_output` must have only 1 NFT such that:
        //    - Its policy ID is that of the current script (computation thread)
        //    - Its asset name is the concatenation of fraud category's ID, and
        //      `fraudulent_header_hash`
        let expected_asset_name = fraud_category_id
          |> bytearray.concat(fraudulent_header_hash)
        expect registry.OpenClaim { claim_id, .. } = claim_registry_mutation(
          inputs,
          redeemers,
          claim_registry_script_hash,
        )
        expect claim_id == expected_asset_name
        let expected_mint_triplet = (own_policy_id, expected_asset_name, 1)
        expect
          utils.get_single_asset_from_value_apart_from_ada(
            first_step_output_value,
          ) == expected_mint_triplet

        // 8. The datum attached to `first_step_output` must carry the fraud
        //    prover, without any additional data.
        expect StepDatum { fraud_prover, data: None }: StepDatum<Data> = first_step_output_datum_data

        // 9. Transaction must be signed by the specified fraud prover.
        expect fraud_prover |> utils.has_signed(extra_signatories)

        // 10. The NFT in `first_step_output` must be the only asset minted in
        //     the transaction.
        expect utils.get_single_asset_from_value(mint) == expected_mint_triplet

        // Done.
        True
      }
      Success { burning_token_asset_name } -> {
        expect registry.CloseClaim { claim_id, .. } = claim_registry_mutation(
          self.inputs,
          self.redeemers,
          claim_registry_script_hash,
        )
        expect claim_id == burning_token_asset_name
        // 1. Specified token must be burned. The rest of validations
        //    (including the coupling of this endpoint with minting of a
        //    `fraud_proof` token) are done in each fraud proof's last step.
        let own_policy_mint = self.mint
          |> assets.tokens(own_policy_id)
          |> dict.to_pairs
        expect own_policy_mint == [Pair(burning_token_asset_name, -1)]

        // Done.
        True
      }
      BurnForCancellation { burning_token_asset_name } -> {
        expect registry.CancelClaim { claim_id, .. } = claim_registry_mutation(
          self.inputs,
          self.redeemers,
          claim_registry_script_hash,
        )
        expect claim_id == burning_token_asset_name
        // 1. Transaction must burn the specified token, without minting or
        //    burning any other tokens.
        let expected_mint = assets.from_asset(
          own_policy_id,
          burning_token_asset_name,
          -1,
        )
        expect expected_mint == self.mint

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

// ## Q01 shared-machinery selectors (thread lifecycle entry/exit)

const test_catalogue_root =
  #"1212121212121212121212121212121212121212121212121212121212121212"

const test_other_policy =
  #"99999999999999999999999999999999999999999999999999999999"

const test_claim_registry_script_hash =
  #"77777777777777777777777777777777777777777777777777777777"

fn test_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn membership_attestation(
  attested_category: ByteArray,
) -> Pairs<ScriptPurpose, Redeemer> {
  [
    fixture.phas_withdraw_redeemer_entry(
      test_catalogue_root,
      cbor.serialise(fixture.fraud_category_id),
      cbor.serialise(attested_category),
    ),
  ]
}

fn standard_init_references(block: fixture.NativeBlockFixtureV1) -> List<Input> {
  [
    fixture.catalogue_reference_input(test_catalogue_root),
    fixture.hub_reference_input(),
    fixture.state_queue_reference_input(block),
  ]
}

fn init_redeemer() -> MintRedeemer {
  Init {
    first_step_output_index: 0,
    fraud_category_id: fixture.fraud_category_id,
    fraud_category: fixture.step_script_hash,
    fraud_category_membership_proof: [],
    fraud_proof_catalogue_ref_input_index: 0,
    inclusion_proof_script_redeemer_index: 0,
    hub_oracle_ref_input_index: 1,
    fraudulent_block_ref_input_index: 2,
  }
}

fn claim_registry_input() -> Input {
  let datum: Data = registry.Datum {
    claims_root: registry.empty_claims_root,
    computation_thread_policy_id: fixture.computation_thread_policy_id,
  }
  Input {
    output_reference: fixture.own_out_ref(),
    output: Output {
      address: address.from_script(test_claim_registry_script_hash),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(fixture.hub_oracle_script_hash, registry.asset_name, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn claim_registry_redeemer_entry(
  mutation: registry.Mutation,
) -> Pair<ScriptPurpose, Redeemer> {
  let redeemer: Data = registry.Mutate { mutation }
  Pair(Spend(fixture.own_out_ref()), redeemer)
}

fn open_claim_registry_redeemer_entry() -> Pair<ScriptPurpose, Redeemer> {
  claim_registry_redeemer_entry(
    registry.OpenClaim {
      claim_id: fixture.ct_token_asset_name(test_block()),
      proof: registry.RedeemerCarried([]),
    },
  )
}

fn run_init(
  first_step_hash: ByteArray,
  first_step_datum_data: Data,
  output_value: assets.Value,
  minted: assets.Value,
  signatories: List<ByteArray>,
  references: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  mint.mint(
    fixture.catalogue_script_hash,
    fixture.hub_oracle_script_hash,
    test_claim_registry_script_hash,
    init_redeemer(),
    fixture.computation_thread_policy_id,
    Transaction {
      ..placeholder,
      outputs: [
        Output {
          address: address.from_script(first_step_hash),
          value: output_value,
          datum: InlineDatum(first_step_datum_data),
          reference_script: None,
        },
      ],
      inputs: [claim_registry_input()],
      reference_inputs: references,
      redeemers: list.concat(redeemers, [open_claim_registry_redeemer_entry()]),
      mint: minted,
      extra_signatories: signatories,
    },
  )
}

fn bare_step_datum_data() -> Data {
  let step_datum: StepDatum<Data> = StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: None,
  }
  let step_datum_data: Data = step_datum
  step_datum_data
}

fn happy_init(
  first_step_hash: ByteArray,
  first_step_datum_data: Data,
  minted: assets.Value,
  signatories: List<ByteArray>,
  references: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  let block = test_block()
  let asset_name = fixture.ct_token_asset_name(block)
  run_init(
    first_step_hash,
    first_step_datum_data,
    fixture.ct_token_value(asset_name),
    minted,
    signatories,
    references,
    redeemers,
  )
}

fn single_thread_mint() -> assets.Value {
  assets.from_asset(
    fixture.computation_thread_policy_id,
    fixture.ct_token_asset_name(test_block()),
    1,
  )
}

/// Lifecycle positive: an init backed by the authentic catalogue, hub oracle,
/// and registered fraudulent block mints exactly one thread token into a
/// stateless first-step UTxO at the proven fraud-category script.
test ct_init_accepts_authentic_catalogue_backed_thread() {
  happy_init(
    fixture.step_script_hash,
    bare_step_datum_data(),
    single_thread_mint(),
    [fixture.fraud_prover_key_hash],
    standard_init_references(test_block()),
    membership_attestation(fixture.step_script_hash),
  )
}

/// No-skip negative: the thread cannot start at any script other than the
/// catalogued first step of the proven fraud category.
test ct_init_rejects_first_step_not_at_proven_category() fail {
  happy_init(
    fixture.next_step_script_hash,
    bare_step_datum_data(),
    single_thread_mint(),
    [fixture.fraud_prover_key_hash],
    standard_init_references(test_block()),
    membership_attestation(fixture.step_script_hash),
  )
}

/// No-skip negative: a thread cannot be born with pre-loaded step state; the
/// first-step datum must carry `data: None`.
test ct_init_rejects_preloaded_step_state() fail {
  let preloaded_state: Data = "smuggled-mid-thread-state"
  let step_datum: StepDatum<Data> = StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(preloaded_state),
  }
  let step_datum_data: Data = step_datum
  happy_init(
    fixture.step_script_hash,
    step_datum_data,
    single_thread_mint(),
    [fixture.fraud_prover_key_hash],
    standard_init_references(test_block()),
    membership_attestation(fixture.step_script_hash),
  )
}

/// Lifecycle negative: the declared fraud prover must sign the init.
test ct_init_rejects_missing_prover_signature() fail {
  happy_init(
    fixture.step_script_hash,
    bare_step_datum_data(),
    single_thread_mint(),
    [],
    standard_init_references(test_block()),
    membership_attestation(fixture.step_script_hash),
  )
}

/// Duplicate-init negative: minting the same thread token twice in one
/// transaction rejects.
test ct_init_rejects_parallel_thread_mint_in_one_tx() fail {
  let block = test_block()
  let asset_name = fixture.ct_token_asset_name(block)
  run_init(
    fixture.step_script_hash,
    bare_step_datum_data(),
    assets.from_lovelace(2_000_000)
      |> assets.add(fixture.computation_thread_policy_id, asset_name, 2),
    assets.from_asset(fixture.computation_thread_policy_id, asset_name, 2),
    [fixture.fraud_prover_key_hash],
    standard_init_references(block),
    membership_attestation(fixture.step_script_hash),
  )
}

/// Duplicate-init negative: piggybacking a second thread token (any other
/// asset name) onto one init rejects.
test ct_init_rejects_second_thread_token_in_one_tx() fail {
  let extra_minted = single_thread_mint()
    |> assets.add(
        fixture.computation_thread_policy_id,
        #"00000002" |> bytearray.concat(test_block().header_hash),
        1,
      )
  happy_init(
    fixture.step_script_hash,
    bare_step_datum_data(),
    extra_minted,
    [fixture.fraud_prover_key_hash],
    standard_init_references(test_block()),
    membership_attestation(fixture.step_script_hash),
  )
}

/// Catalogue-immutability negative: a catalogue reference authenticated by a
/// foreign NFT policy (a substituted catalogue) rejects.
test ct_init_rejects_substituted_catalogue_reference() fail {
  happy_init(
    fixture.step_script_hash,
    bare_step_datum_data(),
    single_thread_mint(),
    [fixture.fraud_prover_key_hash],
    [
      fixture.catalogue_reference_input_with_nft_policy(
        test_catalogue_root,
        test_other_policy,
      ),
      fixture.hub_reference_input(),
      fixture.state_queue_reference_input(test_block()),
    ],
    membership_attestation(fixture.step_script_hash),
  )
}

/// Catalogue-immutability negative: a fraud category whose catalogue
/// attestation names a different first-step hash rejects.
test ct_init_rejects_uncatalogued_fraud_category() fail {
  happy_init(
    fixture.step_script_hash,
    bare_step_datum_data(),
    single_thread_mint(),
    [fixture.fraud_prover_key_hash],
    standard_init_references(test_block()),
    membership_attestation(test_other_policy),
  )
}

/// Catalogue-immutability negative: an init with no catalogue membership
/// attestation at all rejects.
test ct_init_rejects_absent_membership_attestation() fail {
  happy_init(
    fixture.step_script_hash,
    bare_step_datum_data(),
    single_thread_mint(),
    [fixture.fraud_prover_key_hash],
    standard_init_references(test_block()),
    [],
  )
}

/// Reference-input-discipline negative: a hub oracle authenticated by a
/// foreign NFT policy rejects.
test ct_init_rejects_substituted_hub_oracle() fail {
  happy_init(
    fixture.step_script_hash,
    bare_step_datum_data(),
    single_thread_mint(),
    [fixture.fraud_prover_key_hash],
    [
      fixture.catalogue_reference_input(test_catalogue_root),
      fixture.hub_reference_input_with_nft_policy(test_other_policy),
      fixture.state_queue_reference_input(test_block()),
    ],
    membership_attestation(fixture.step_script_hash),
  )
}

/// Reference-input-discipline negative: a challenged block outside the
/// hub-registered state queue (foreign node policy) rejects.
test ct_init_rejects_unregistered_block_reference() fail {
  happy_init(
    fixture.step_script_hash,
    bare_step_datum_data(),
    single_thread_mint(),
    [fixture.fraud_prover_key_hash],
    [
      fixture.catalogue_reference_input(test_catalogue_root),
      fixture.hub_reference_input(),
      fixture.state_queue_reference_input_with_policy(
        test_block(),
        test_other_policy,
      ),
    ],
    membership_attestation(fixture.step_script_hash),
  )
}

/// Token-coupling positive: `Success` burns the thread token (the fraud-proof
/// mint in the same transaction carries the terminal record).
test ct_success_accepts_thread_token_burn() {
  let asset_name = fixture.ct_token_asset_name(test_block())
  mint.mint(
    fixture.catalogue_script_hash,
    fixture.hub_oracle_script_hash,
    test_claim_registry_script_hash,
    Success { burning_token_asset_name: asset_name },
    fixture.computation_thread_policy_id,
    Transaction {
      ..placeholder,
      inputs: [claim_registry_input()],
      redeemers: [
        claim_registry_redeemer_entry(
          registry.CloseClaim {
            claim_id: asset_name,
            proof: registry.RedeemerCarried([]),
          },
        ),
      ],
      mint: assets.from_asset(
        fixture.computation_thread_policy_id,
        asset_name,
        -1,
      )
        |> assets.add(fixture.fraud_proof_policy_id, asset_name, 1),
    },
  )
}

/// Token-coupling negative: `Success` without actually burning the named
/// thread token rejects.
test ct_success_rejects_missing_thread_token_burn() fail {
  let asset_name = fixture.ct_token_asset_name(test_block())
  mint.mint(
    fixture.catalogue_script_hash,
    fixture.hub_oracle_script_hash,
    test_claim_registry_script_hash,
    Success { burning_token_asset_name: asset_name },
    fixture.computation_thread_policy_id,
    Transaction {
      ..placeholder,
      inputs: [claim_registry_input()],
      redeemers: [
        claim_registry_redeemer_entry(
          registry.CloseClaim {
            claim_id: asset_name,
            proof: registry.RedeemerCarried([]),
          },
        ),
      ],
      mint: assets.from_asset(fixture.fraud_proof_policy_id, asset_name, 1),
    },
  )
}

/// Token-coupling negative: terminal success cannot piggyback an unregistered
/// computation-thread asset under the same policy while burning the real one.
test ct_success_rejects_surplus_own_policy_mint() fail {
  let asset_name = fixture.ct_token_asset_name(test_block())
  mint.mint(
    fixture.catalogue_script_hash,
    fixture.hub_oracle_script_hash,
    test_claim_registry_script_hash,
    Success { burning_token_asset_name: asset_name },
    fixture.computation_thread_policy_id,
    Transaction {
      ..placeholder,
      inputs: [claim_registry_input()],
      redeemers: [
        claim_registry_redeemer_entry(
          registry.CloseClaim {
            claim_id: asset_name,
            proof: registry.RedeemerCarried([]),
          },
        ),
      ],
      mint: assets.from_asset(
        fixture.computation_thread_policy_id,
        asset_name,
        -1,
      )
        |> assets.add(
            fixture.computation_thread_policy_id,
            "unregistered-thread",
            1,
          )
        |> assets.add(fixture.fraud_proof_policy_id, asset_name, 1),
    },
  )
}

/// Token-coupling positive: cancellation burns exactly the thread token and
/// nothing else.
test ct_burn_for_cancellation_accepts_exact_burn() {
  let asset_name = fixture.ct_token_asset_name(test_block())
  mint.mint(
    fixture.catalogue_script_hash,
    fixture.hub_oracle_script_hash,
    test_claim_registry_script_hash,
    BurnForCancellation { burning_token_asset_name: asset_name },
    fixture.computation_thread_policy_id,
    Transaction {
      ..placeholder,
      inputs: [claim_registry_input()],
      redeemers: [
        claim_registry_redeemer_entry(
          registry.CancelClaim {
            claim_id: asset_name,
            proof: registry.RedeemerCarried([]),
          },
        ),
      ],
      mint: assets.from_asset(
        fixture.computation_thread_policy_id,
        asset_name,
        -1,
      ),
    },
  )
}

/// Token-coupling negative: cancellation cannot mint or burn anything beyond
/// the thread token.
test ct_burn_for_cancellation_rejects_extra_mint() fail {
  let asset_name = fixture.ct_token_asset_name(test_block())
  mint.mint(
    fixture.catalogue_script_hash,
    fixture.hub_oracle_script_hash,
    test_claim_registry_script_hash,
    BurnForCancellation { burning_token_asset_name: asset_name },
    fixture.computation_thread_policy_id,
    Transaction {
      ..placeholder,
      inputs: [claim_registry_input()],
      redeemers: [
        claim_registry_redeemer_entry(
          registry.CancelClaim {
            claim_id: asset_name,
            proof: registry.RedeemerCarried([]),
          },
        ),
      ],
      mint: assets.from_asset(
        fixture.computation_thread_policy_id,
        asset_name,
        -1,
      )
        |> assets.add(test_other_policy, "extra", 1),
    },
  )
}
