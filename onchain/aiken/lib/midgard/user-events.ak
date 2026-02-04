use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken_design_patterns/parameter_validation
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId, Value}
use cardano/transaction.{
  Datum as CardanoDatum, InlineDatum, Input, Output, OutputReference, Redeemer,
  ScriptPurpose, Transaction, ValidityRange,
}
use env
use midgard/common/types.{PosixTime}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/user_events/witness.{
  validate_witness_redeemer, witness_script_prefix,
}

// ## Types

pub type OptimisticDatum<event_type> {
  event: event_type,
  inclusion_time: PosixTime,
  witness: ScriptHash,
  refund_address: Address,
  refund_datum: CardanoDatum,
}

pub type MintRedeemer {
  AuthenticateEvent {
    nonce_input_index: Int,
    event_output_index: Int,
    hub_ref_input_index: Int,
    witness_registration_redeemer_index: Int,
  }
  BurnEventNFT {
    nonce_asset_name: AssetName,
    witness_unregistration_redeemer_index: Int,
  }
}

// ## Utilities & Helpers

pub fn out_ref_to_nonce(out_ref: OutputReference) -> AssetName {
  cbor.serialise(out_ref) |> blake2b_256
}

// ## Validation Abstractions

pub fn validate_mint(
  hub_oracle: ScriptHash,
  event_address_getter: fn(hub.Datum) -> Address,
  redeemer: MintRedeemer,
  own_policy: PolicyId,
  tx: Transaction,
  event_validator: fn(AssetName, Value, Data) -> Bool,
) -> Bool {
  when redeemer is {
    AuthenticateEvent {
      nonce_input_index,
      event_output_index,
      hub_ref_input_index,
      witness_registration_redeemer_index,
    } -> {
      let Transaction {
        inputs,
        outputs,
        reference_inputs,
        validity_range,
        mint,
        redeemers,
        ..
      } = tx

      // Grab hub oracle datum.
      let hub_datum =
        hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)

      let event_address = event_address_getter(hub_datum)

      authenticate_event(
        event_address,
        nonce_input_index,
        event_output_index,
        witness_registration_redeemer_index,
        own_policy,
        inputs,
        outputs,
        validity_range,
        mint,
        redeemers,
        event_validator,
      )
    }
    BurnEventNFT { nonce_asset_name, witness_unregistration_redeemer_index } -> {
      let Transaction { mint, redeemers, .. } = tx
      validate_event_nft_burn(
        nonce_asset_name,
        witness_unregistration_redeemer_index,
        own_policy,
        mint,
        redeemers,
      )
    }
  }
}

pub fn authenticate_event(
  event_address: Address,
  nonce_input_index: Int,
  event_output_index: Int,
  witness_registration_redeemer_index: Int,
  own_policy: PolicyId,
  inputs: List<Input>,
  outputs: List<Output>,
  validity_range: ValidityRange,
  mint: Value,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  event_validator: fn(AssetName, Value, Data) -> Bool,
) -> Bool {
  // Grab validity range's inclusive upper bound.
  // TODO: Is it needed to ensure inclusivity?
  let valid_to = utils.get_inclusive_upper_bound_of_interval(validity_range)

  // Grab output reference of the specified nonce input.
  expect Some(Input { output_reference: nonce_input_out_ref, .. }) =
    inputs |> list.at(nonce_input_index)

  // In order to apply the parameterized witness script, we hash the
  // serialized nonce output reference to get the 32 bytes long nonce.
  let nonce = out_ref_to_nonce(nonce_input_out_ref)

  // 1. This event NFT must be minted with a quantity of 1.
  expect assets.quantity_of(mint, own_policy, nonce) == 1

  // Script hash of the witness script, required to be registered in this
  // transaction.
  let expected_witness_script_hash =
    parameter_validation.apply_prehashed_param(
      version: 3,
      prefix: witness_script_prefix,
      param: nonce,
    )

  // 2. Validate this script hash is being registered.
  // 3. Validate the policy ID passed in its redeemer is correct.
  expect
    validate_witness_redeemer(
      expected_witness_script_hash: expected_witness_script_hash,
      witness_redeemer_index: witness_registration_redeemer_index,
      for_registration: True,
      event_policy_id: own_policy,
      redeemers: redeemers,
    )

  // Event's produced UTxO, with an inline datum and no reference script
  // attached.
  expect Some(Output {
    address: output_event_address,
    value: output_event_value,
    datum: InlineDatum(output_event_datum_data),
    reference_script: None,
  }) = outputs |> list.at(event_output_index)

  // Extracting common data shared between all event datums.
  expect [event_data,
    inclusion_time_data,
    witness_data, ..] = builtin.unconstr_fields(output_event_datum_data)

  let output_event_inclusion_time: PosixTime =
    builtin.un_i_data(inclusion_time_data)

  let output_event_witness: ScriptHash = builtin.un_b_data(witness_data)

  let Pair(event_id, event_info) =
    ledger_state.unsafe_event_to_key_value_pair(event_data)

  let nonce_input_out_ref_data: Data = nonce_input_out_ref

  // 4. Event's ID must be the same as the specified spent input.
  expect event_id == nonce_input_out_ref_data

  and {
    // 5. Event UTxO must be produced at the provided script's address from
    //    hub oracle.
    output_event_address == event_address,
    // 6. Specified inclusion time of the event must be some time in the
    //    future.
    output_event_inclusion_time == valid_to + env.event_wait_duration,
    // 7. Recorded witness must be correct.
    output_event_witness == expected_witness_script_hash,
    // 8. The provided validation function must pass.
    event_validator(nonce, output_event_value, event_info),
  }
}

pub fn validate_event_nft_burn(
  nonce_asset_name: AssetName,
  witness_unregistration_redeemer_index: Int,
  own_policy: PolicyId,
  mint: Value,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  let expected_witness_script_hash =
    parameter_validation.apply_prehashed_param(
      version: 3,
      prefix: witness_script_prefix,
      param: nonce_asset_name,
    )

  and {
    // 1. The specified NFT must be burnt.
    assets.quantity_of(mint, own_policy, nonce_asset_name) == -1,
    // 2. Witness staking script corresponding to the NFT must be getting
    //    unregistered.
    // 3. `PublishRedeemer` for unregistration must be equal to
    //    `own_policy`
    validate_witness_redeemer(
      expected_witness_script_hash: expected_witness_script_hash,
      witness_redeemer_index: witness_unregistration_redeemer_index,
      for_registration: False,
      event_policy_id: own_policy,
      redeemers: redeemers,
    ),
  }
}
