use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction, resolve_input,
}
use midgard/common/utils
use midgard/da_attestation.{
  AddSignatures, ApplyToStateQueue, BurnForStateQueue, Datum, Init, MintRedeemer,
  SpendRedeemer, asset_name, validate_add_signatures,
}
use midgard/da_params_governor as da_params_governor
use midgard/state_queue

fn sorted_unique_vkey_len_at_most(keys: ByteArray, max: Int) -> Int {
  expect max > 0
  let keys_len = bytearray.length(keys)
  expect keys_len >= da_params_governor.verification_key_byte_count
  let first =
    utils.bytearray_slice(
      keys,
      0,
      da_params_governor.verification_key_byte_count,
    )
  sorted_unique_vkey_len_from(
    keys,
    da_params_governor.verification_key_byte_count,
    first,
    1,
    max,
    keys_len,
  )
}

fn sorted_unique_vkey_len_from(
  keys: ByteArray,
  offset: Int,
  previous: ByteArray,
  len: Int,
  max: Int,
  keys_len: Int,
) -> Int {
  if offset == keys_len {
    len
  } else {
    expect offset + da_params_governor.verification_key_byte_count <= keys_len
    let key =
      utils.bytearray_slice(
        keys,
        offset,
        da_params_governor.verification_key_byte_count,
      )
    expect bytearray.compare(previous, key) == Less
    let next_len = len + 1
    expect next_len <= max
    sorted_unique_vkey_len_from(
      keys,
      offset + da_params_governor.verification_key_byte_count,
      key,
      next_len,
      max,
      keys_len,
    )
  }
}

fn get_da_params(
  reference_inputs: List<Input>,
  da_params_policy_id: PolicyId,
  da_params_ref_input_index: Int,
) -> da_params_governor.Datum {
  // 1. The referenced params input must exist and expose an inline datum.
  expect Input {
    output: Output {
      address,
      value,
      datum: InlineDatum(datum_data),
      reference_script: None,
    },
    ..
  } = reference_inputs |> list.expect_at(da_params_ref_input_index)

  // 2. The params input must be locked by the expected governor policy.
  expect Script(params_script_hash) = address.payment_credential
  expect params_script_hash == da_params_policy_id

  // 3. The params input must carry exactly one params NFT.
  expect
    assets.quantity_of(
      value,
      da_params_policy_id,
      da_params_governor.asset_name,
    ) == 1

  // 4. The inline datum must be a params datum with a valid committee.
  expect datum: da_params_governor.Datum = datum_data
  let da_params_governor.Datum { committee, da_threshold, .. } = datum

  // 5. The data availability threshold must be positive.
  expect da_threshold > 0

  // 6. The committee must be a sorted unique packed set with enough members to
  //    satisfy the threshold.
  let committee_len =
    sorted_unique_vkey_len_at_most(
      committee,
      da_params_governor.max_indexed_signer_count,
    )
  expect da_threshold <= committee_len

  datum
}

validator main(da_params_policy_id: PolicyId, state_queue_policy_id: PolicyId) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, tx: Transaction) {
    let Transaction { inputs, outputs, reference_inputs, mint, .. } = tx

    when redeemer is {
      Init {
        output_index,
        da_params_ref_input_index,
        state_queue_ref_input_index,
      } -> {
        // 1. The referenced data availability params must be authentic.
        let da_params_governor.Datum {
          committee: params_committee,
          da_threshold: params_da_threshold,
          ..
        } =
          get_da_params(
            reference_inputs,
            da_params_policy_id,
            da_params_ref_input_index,
          )

        // 2. The referenced state queue node must match a block with no
        //    attached data availability attestation.
        let expected_header_hash =
          state_queue.get_state_queue_node(
            reference_inputs,
            state_queue_policy_id,
            state_queue_ref_input_index,
            fn(state_queue_node, header_hash) {
              expect
                state_queue_node.da_attestation == state_queue.no_da_attestation
              header_hash
            },
          )

        // 3. The attestation output must be locked by this policy with an
        //    inline datum and no reference script.
        expect Output {
          address: attestation_address,
          value: attestation_value,
          datum: InlineDatum(attestation_datum_data),
          reference_script: None,
        } = outputs |> list.expect_at(output_index)
        expect utils.expect_script_hash(attestation_address) == own_policy_id

        // 4. The output datum must initialize attestation for the expected
        //    header with the current params committee and threshold.
        let expected_attestation_datum_data: Data =
          Datum {
            header_hash: expected_header_hash,
            da_threshold: params_da_threshold,
            remaining_signers: params_committee,
            attestation_count: 0,
          }
        expect attestation_datum_data == expected_attestation_datum_data

        // 5. The output must carry the exact header-specific attestation NFT.
        let attestation_asset = asset_name(expected_header_hash)
        expect
          assets.has_nft_strict(
            attestation_value,
            own_policy_id,
            attestation_asset,
          )

        // 6. Exactly one header-specific attestation NFT must be minted.
        expect utils.validate_mint(mint, own_policy_id, attestation_asset, 1)

        True
      }
      ApplyToStateQueue {
        da_attestation_input_index,
        state_queue_input_index,
        state_queue_output_index,
      } -> {
        // 1. The attestation input must be authentic for this policy.
        expect Input {
          output: Output {
            address: attestation_address,
            value: attestation_value,
            datum: InlineDatum(attestation_datum_data),
            reference_script: None,
          },
          ..
        } = inputs |> list.expect_at(da_attestation_input_index)
        expect utils.expect_script_hash(attestation_address) == own_policy_id

        expect attestation_datum: Datum = attestation_datum_data
        let Datum {
          header_hash: attestation_header_hash,
          da_threshold: attestation_da_threshold,
          attestation_count,
          ..
        } = attestation_datum
        let attestation_asset = asset_name(attestation_header_hash)
        expect
          assets.has_nft_strict(
            attestation_value,
            own_policy_id,
            attestation_asset,
          )

        // 2. The attestation threshold must have been met.
        expect attestation_count >= attestation_da_threshold

        // 3. The header-specific attestation NFT must be burnt.
        expect utils.validate_mint(mint, own_policy_id, attestation_asset, -1)

        // 4. The state queue node must attach this attestation to the same
        //    header.
        state_queue.validate_da_attestation_attachment(
          inputs,
          outputs,
          state_queue_policy_id,
          state_queue_input_index,
          state_queue_output_index,
          attestation_header_hash,
          own_policy_id,
        )
      }
    }
  }

  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    let Transaction { inputs, redeemers, .. } = tx

    when redeemer is {
      AddSignatures { output_index, signatures } ->
        validate_add_signatures(datum, output_index, signatures, own_ref, tx)
      BurnForStateQueue { mint_redeemer_index } -> {
        // 1. The attestation spend must correspond to this policy.
        let Output { address: input_address, .. } =
          inputs |> resolve_input(own_ref)
        let own_policy_id = utils.expect_script_hash(input_address)

        // 2. The matching mint redeemer must burn this attestation for state
        //    queue attachment.
        let mint_redeemer_data =
          redeemers
            |> utils.get_redeemer_at(Mint(own_policy_id), mint_redeemer_index)
        expect ApplyToStateQueue { da_attestation_input_index, .. } =
          mint_redeemer_data

        // 3. The mint redeemer's attestation input index must point to this
        //    spent UTxO.
        let Input { output_reference, .. } =
          inputs |> list.expect_at(da_attestation_input_index)
        expect output_reference == own_ref

        True
      }
    }
  }

  else(_) {
    fail
  }
}
