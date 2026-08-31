//// Q53 regression selector: an Init that does not consume the deployment-
//// bound claim-registry singleton and present the exact Open mutation is no
//// longer admissible. Concurrent valid transactions must consume the same
//// singleton out-ref, so Cardano input uniqueness serializes their claims.

use aiken/cbor
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction, placeholder,
}
use computation_thread as computation_thread_validator
use midgard/computation_thread
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

const catalogue_root =
  #"1212121212121212121212121212121212121212121212121212121212121212"

const claim_registry_script_hash =
  #"77777777777777777777777777777777777777777777777777777777"

fn funding_input(output_index: Int) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: #"abababababababababababababababababababababababababababababababab",
      output_index,
    },
    output: Output {
      address: address.from_verification_key(fixture.fraud_prover_key_hash),
      value: assets.from_lovelace(5_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn duplicate_claim_init(funding_output_index: Int) -> Transaction {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let asset_name = fixture.ct_token_asset_name(block)
  let step_datum_data: Data = computation_thread.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: None,
  }
  Transaction {
    ..placeholder,
    inputs: [funding_input(funding_output_index)],
    outputs: [
      Output {
        address: address.from_script(fixture.step_script_hash),
        value: fixture.ct_token_value(asset_name),
        datum: InlineDatum(step_datum_data),
        reference_script: None,
      },
    ],
    reference_inputs: [
      fixture.catalogue_reference_input(catalogue_root),
      fixture.hub_reference_input(),
      fixture.state_queue_reference_input(block),
    ],
    redeemers: [
      fixture.phas_withdraw_redeemer_entry(
        catalogue_root,
        cbor.serialise(fixture.fraud_category_id),
        cbor.serialise(fixture.step_script_hash),
      ),
    ],
    mint: assets.from_asset(fixture.computation_thread_policy_id, asset_name, 1),
    extra_signatories: [fixture.fraud_prover_key_hash],
  }
}

fn validates_init(tx: Transaction) -> Bool {
  computation_thread_validator.mint.mint(
    fixture.catalogue_script_hash,
    fixture.hub_oracle_script_hash,
    claim_registry_script_hash,
    computation_thread.Init {
      first_step_output_index: 0,
      fraud_category_id: fixture.fraud_category_id,
      fraud_category: fixture.step_script_hash,
      fraud_category_membership_proof: [],
      fraud_proof_catalogue_ref_input_index: 0,
      inclusion_proof_script_redeemer_index: 0,
      hub_oracle_ref_input_index: 1,
      fraudulent_block_ref_input_index: 2,
    },
    fixture.computation_thread_policy_id,
    tx,
  )
}

test q53_init_without_claim_registry_transition_is_rejected() fail {
  validates_init(duplicate_claim_init(0))
}
