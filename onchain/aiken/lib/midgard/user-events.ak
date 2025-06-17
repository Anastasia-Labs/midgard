use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/interval.{Finite}
use aiken/merkle_patricia_forestry as mpf
use aiken_design_patterns/linked_list/unordered.{NodeKey}
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId, Value}
use cardano/transaction.{
  Datum as CardanoDatum, InlineDatum, Input, Output, OutputReference, Redeemer,
  ScriptPurpose, Transaction, ValidityRange,
}
use env
use midgard/common/parameter_validation
use midgard/common/types.{MerkleRoot, PosixTime}
use midgard/common/utils.{plutarch_pexcludes}
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/settlement_queue
use midgard/user_events/witness.{
  validate_witness_redeemer, witness_script_prefix,
}

// ## Types

pub type Datum<event_type> {
  event: event_type,
  inclusion_time: PosixTime,
  witness: ScriptHash,
  refund_address: Address,
  refund_datum: CardanoDatum,
}

pub type RefundApproach {
  SkippedEvent { non_membership_proof: mpf.Proof }
  EventInSettlementGap { settlement_link_ref_input_index: Int }
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

pub fn validate_refund(
  own_value: Value,
  event_nft_policy_id: PolicyId,
  nonce_asset_name: AssetName,
  output_address: Address,
  output_value: Value,
  output_datum: CardanoDatum,
  refund_address: Address,
  refund_datum: CardanoDatum,
  refund_approach: RefundApproach,
  events_root: MerkleRoot,
  event_id: Data,
  settlement_queue_policy_id: PolicyId,
  settlement_node_link: NodeKey,
  settlement_start_time: PosixTime,
  settlement_end_time: PosixTime,
  inclusion_time: PosixTime,
  reference_inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  expect and {
      // 1. The specified address must be the recepient.
      output_address == refund_address,
      // 2. The whole value must get refunded (excluding the NFT).
      output_value == (
        own_value
          |> assets.add(event_nft_policy_id, nonce_asset_name, -1)
      ),
      // 3. The attached datum must be as specified.
      output_datum == refund_datum,
    }

  when refund_approach is {
    SkippedEvent { non_membership_proof } -> and {
        // 4. The deposit event must be absent from the deposit tree.
        plutarch_pexcludes(
          events_root,
          event_id,
          non_membership_proof,
          redeemers,
        ),
        // 5. Inclusion time of the deposit subject to this refund
        //    logic must fall within the time range of the specified
        //    settlement node.
        //    TODO: Should any of the comparisons be exclusive?
        inclusion_time <= settlement_end_time,
        inclusion_time >= settlement_start_time,
      }
    EventInSettlementGap { settlement_link_ref_input_index } -> {
      // 4. Grab the immediate next settlement node, and ensure
      //    correct linkage (authenticity of the reference UTxO is
      //    checked with `settlement_queue.get_datum`).
      let parsed_settlement_link_datum =
        settlement_queue.get_datum(
          reference_inputs,
          settlement_queue_policy_id,
          settlement_link_ref_input_index,
        )
      expect settlement_node_link == parsed_settlement_link_datum.key

      // 5. Inclusion time of the deposit subject to this refund logic
      //    must fall within the time gap between the two settlement
      //    nodes.
      //    TODO: Should any of the comparisons be inclusive?
      expect settlement_queue.NodeData {
        start_time: settlement_link_start_time,
        ..
      } = parsed_settlement_link_datum.data
      and {
        inclusion_time > settlement_end_time,
        inclusion_time < settlement_link_start_time,
      }
    }
  }
}

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
  // Grab validity range's upper bound.
  expect Finite(valid_to) = validity_range.upper_bound.bound_type

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
  expect Datum {
    event,
    inclusion_time: output_event_inclusion_time,
    witness: output_event_witness,
    ..
  }: Datum<Data> = output_event_datum_data

  let Pair(event_id, event_info) =
    ledger_state.unsafe_event_to_key_value_pair(event)

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
    // 3. `PublishRedeemer` for unregistration must be equal to `own_policy`
    validate_witness_redeemer(
      expected_witness_script_hash: expected_witness_script_hash,
      witness_redeemer_index: witness_unregistration_redeemer_index,
      for_registration: False,
      event_policy_id: own_policy,
      redeemers: redeemers,
    ),
  }
}
