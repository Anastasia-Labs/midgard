use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address.{Script}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Transaction, resolve_input,
}
use midgard/common/utils
use midgard/da_attestation.{
  AddSignatures, ApplyToStateQueue, BurnForStateQueue, Datum, Init, MintRedeemer,
  SpendRedeemer, asset_name, bytearray_slice, expect_script_hash,
  output_at_index, validate_add_signatures,
}
use midgard/da_params_governor as da_params_governor
use midgard/ledger_state.{HeaderHash}
use midgard/state_queue

fn sorted_unique_vkey_len_at_most(keys: ByteArray, max: Int) -> Int {
  expect max > 0
  let keys_len = bytearray.length(keys)
  expect keys_len >= da_params_governor.verification_key_byte_count
  let first =
    bytearray_slice(keys, 0, da_params_governor.verification_key_byte_count)
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
      bytearray_slice(
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

fn validate_committee(datum: da_params_governor.Datum) -> Bool {
  // 1. The data availability threshold must be positive.
  expect datum.da_threshold > 0

  // 2. The committee must be a sorted unique packed set with enough members to
  //    satisfy the threshold.
  let committee_len =
    sorted_unique_vkey_len_at_most(
      datum.committee,
      da_params_governor.max_indexed_signer_count,
    )
  expect datum.da_threshold <= committee_len

  True
}

fn get_da_params(
  reference_inputs: List<Input>,
  da_params_policy_id: PolicyId,
  da_params_ref_input_index: Int,
) -> da_params_governor.Datum {
  // 1. The referenced params input must exist and expose an inline datum.
  expect Some(Input {
    output: Output {
      address,
      value,
      datum: InlineDatum(datum_data),
      reference_script: None,
    },
    ..
  }) = reference_inputs |> list.at(da_params_ref_input_index)

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
  expect validate_committee(datum)
  datum
}

fn expect_script_hash_from_output(output: Output) -> PolicyId {
  let Output { address, .. } = output
  expect_script_hash(address)
}

fn validate_init_output(
  output: Output,
  own_policy_id: PolicyId,
  params: da_params_governor.Datum,
  expected_header_hash: HeaderHash,
) -> AssetName {
  // 1. The attestation output must be locked by this policy with an inline
  //    datum and no reference script.
  expect Output {
    address,
    value,
    datum: InlineDatum(datum_data),
    reference_script: None,
  } = output
  expect expect_script_hash(address) == own_policy_id

  // 2. The output datum must initialize attestation for the expected header
  //    with the current params committee and threshold.
  expect datum: Datum = datum_data
  expect datum.header_hash == expected_header_hash
  expect datum.da_threshold == params.da_threshold
  expect datum.remaining_signers == params.committee
  expect datum.attestation_count == 0

  // 3. The output must carry the exact header-specific attestation NFT.
  let attestation_asset_name = asset_name(datum.header_hash)
  expect assets.has_nft_strict(value, own_policy_id, attestation_asset_name)

  attestation_asset_name
}

fn get_attestation_input_datum(
  inputs: List<Input>,
  input_index: Int,
  own_policy_id: PolicyId,
) -> Datum {
  // 1. The attestation input must exist and expose an inline datum.
  expect Some(Input {
    output: Output {
      address,
      value,
      datum: InlineDatum(datum_data),
      reference_script: None,
    },
    ..
  }) = inputs |> list.at(input_index)

  // 2. The attestation input must be locked by this policy.
  expect expect_script_hash(address) == own_policy_id

  // 3. The input must carry the header-specific attestation NFT.
  expect datum: Datum = datum_data
  let attestation_asset = asset_name(datum.header_hash)
  expect assets.has_nft_strict(value, own_policy_id, attestation_asset)
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
        let params =
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

        // 3. The output at the indicated index must initialize the attestation
        //    for that header and params committee.
        let attestation_asset =
          outputs
            |> output_at_index(output_index)
            |> validate_init_output(own_policy_id, params, expected_header_hash)

        // 4. Exactly one header-specific attestation NFT must be minted.
        expect [Pair(minted_asset_name, minted_quantity)] =
          mint
            |> assets.tokens(own_policy_id)
            |> dict.to_pairs
        expect minted_asset_name == attestation_asset
        expect minted_quantity == 1

        True
      }
      ApplyToStateQueue {
        da_attestation_input_index,
        state_queue_input_index,
        state_queue_output_index,
      } -> {
        // 1. The attestation input must be authentic for this policy.
        let attestation_datum =
          get_attestation_input_datum(
            inputs,
            da_attestation_input_index,
            own_policy_id,
          )

        // 2. The attestation threshold must have been met.
        expect
          attestation_datum.attestation_count >= attestation_datum.da_threshold

        // 3. The header-specific attestation NFT must be burnt.
        let attestation_asset = asset_name(attestation_datum.header_hash)
        expect [Pair(minted_asset_name, minted_quantity)] =
          mint
            |> assets.tokens(own_policy_id)
            |> dict.to_pairs
        expect minted_asset_name == attestation_asset
        expect minted_quantity == -1

        // 4. The state queue node must attach this attestation to the same
        //    header.
        state_queue.validate_da_attestation_attachment(
          inputs,
          outputs,
          state_queue_policy_id,
          state_queue_input_index,
          state_queue_output_index,
          attestation_datum.header_hash,
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
        let own_policy_id =
          inputs
            |> resolve_input(own_ref)
            |> expect_script_hash_from_output

        // 2. The matching mint redeemer must burn this attestation for state
        //    queue attachment.
        let mint_redeemer_data =
          redeemers
            |> utils.get_redeemer_at(Mint(own_policy_id), mint_redeemer_index)
        expect ApplyToStateQueue { da_attestation_input_index, .. }: MintRedeemer =
          mint_redeemer_data

        // 3. The mint redeemer's attestation input index must point to this
        //    spent UTxO.
        expect Some(Input { output_reference, .. }) =
          inputs |> list.at(da_attestation_input_index)
        expect output_reference == own_ref

        True
      }
    }
  }

  else(_) {
    fail
  }
}
