use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId, quantity_of}
use cardano/transaction.{InlineDatum, Input, Output}

/// Deployment-bound singleton serialized with the hub-oracle NFT.  It lives at
/// the dedicated correction-lock validator, outside the state-queue linked-list
/// policy namespace.
pub const asset_name: AssetName = "MIDGARD_CORRECTION_LOCK"

/// Authenticated reason for the correction currently holding the singleton.
/// A permanent fraud-proof asset name includes category id || header hash.
/// Timeout correction has no external proof UTxO.  The availability identity
/// is reserved for Q58's authenticated per-header challenge token.
pub type CorrectionIdentity {
  FraudProof { fraud_proof_asset_name: AssetName }
  AttestationTimeout
  AvailabilityChallenge { challenge_asset_name: AssetName }
}

pub type Datum {
  Idle
  Locked {
    target_header_hash: ByteArray,
    correction_identity: CorrectionIdentity,
  }
}

/// Ordinary corrections reference the authentic hub oracle.  Deinit consumes
/// it, so its index names the spent hub input instead.
pub type Redeemer {
  Correct { hub_oracle_ref_input_index: Int }
  Deinit { hub_oracle_input_index: Int }
}

pub fn output_has_token(output: Output, hub_oracle_policy_id: PolicyId) -> Bool {
  quantity_of(output.value, hub_oracle_policy_id, asset_name) == 1
}

pub fn output_is_dedicated(
  output: Output,
  hub_oracle_policy_id: PolicyId,
  correction_lock_address: Address,
) -> Bool {
  let Output { address, value, reference_script, .. } = output
  and {
    address == correction_lock_address,
    output_has_token(output, hub_oracle_policy_id),
    assets.without_lovelace(value) == assets.from_asset(
      hub_oracle_policy_id,
      asset_name,
      1,
    ),
    reference_script == None,
  }
}

pub fn decode_datum(output: Output) -> Datum {
  expect InlineDatum(datum_data) = output.datum
  expect datum: Datum = datum_data
  datum
}

pub fn unique_input(
  inputs: List<Input>,
  hub_oracle_policy_id: PolicyId,
  correction_lock_address: Address,
) -> Input {
  expect [lock_input] = inputs
    |> list.filter(
        fn(Input { output, .. }) {
          output_has_token(output, hub_oracle_policy_id)
        },
      )
  expect
    output_is_dedicated(
      lock_input.output,
      hub_oracle_policy_id,
      correction_lock_address,
    )
  lock_input
}

pub fn unique_output(
  outputs: List<Output>,
  hub_oracle_policy_id: PolicyId,
  correction_lock_address: Address,
) -> Output {
  expect [lock_output] = outputs
    |> list.filter(
        fn(output) { output_has_token(output, hub_oracle_policy_id) },
      )
  expect
    output_is_dedicated(
      lock_output,
      hub_oracle_policy_id,
      correction_lock_address,
    )
  lock_output
}

/// Append and merge reference the unique Idle singleton.  Spending that UTxO
/// invalidates every already-prepared mutation; a refetched transaction sees
/// Locked and fails here.
pub fn references_idle(
  reference_inputs: List<Input>,
  hub_oracle_policy_id: PolicyId,
  correction_lock_address: Address,
) -> Bool {
  expect [Input { output, .. }] = reference_inputs
    |> list.filter(
        fn(Input { output, .. }) {
          output_has_token(output, hub_oracle_policy_id)
        },
      )
  and {
    output_is_dedicated(output, hub_oracle_policy_id, correction_lock_address),
    decode_datum(output) == Idle,
  }
}

pub fn has_no_output(
  outputs: List<Output>,
  hub_oracle_policy_id: PolicyId,
) -> Bool {
  list.length(
    outputs
      |> list.filter(
          fn(output) { output_has_token(output, hub_oracle_policy_id) },
        ),
  ) == 0
}

/// Exact state machine shared by the dedicated spending validator and its
/// adversarial vectors.  A correction may acquire Idle or resume the same
/// target and identity.  Only the terminal removal clears back to Idle.
pub fn datum_transition_is_valid(
  current: Datum,
  next: Datum,
  expected_locked: Datum,
  terminal: Bool,
) -> Bool {
  let expected_next = if terminal {
    Idle
  } else {
    expected_locked
  }
  and {
    or {
      current == Idle,
      current == expected_locked,
    },
    next == expected_next,
  }
}
