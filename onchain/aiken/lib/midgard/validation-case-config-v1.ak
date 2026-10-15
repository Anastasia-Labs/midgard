//// The validation-case configuration: the script hashes the case validator
//// reads. One configuration UTxO holds them, authenticated by the one-shot
//// configuration NFT and locked for ever at the configuration validator,
//// whose spend always fails.
////
//// The case validator finds the configuration and the hub oracle among the
//// reference inputs by their NFTs, so its redeemers carry no index for them.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/hub_oracle as hub

pub const config_asset_name: AssetName = "MIDGARD_CASE_CONFIG"

pub type CaseConfigV1 {
  source_v1: ScriptHash,
  game_v1: ScriptHash,
  boundary_v1: ScriptHash,
  award_v1: ScriptHash,
  timeout_v1: ScriptHash,
  dispute_bond_v1: ScriptHash,
}

/// The inline datum of the reference input holding the NFT `policy_id.name`.
fn reference_datum_with_nft(
  reference_inputs: List<Input>,
  policy_id: PolicyId,
  name: AssetName,
) -> Data {
  expect Some(Input { output: Output { datum: InlineDatum(datum), .. }, .. }) = list.find(
    reference_inputs,
    fn(input) { assets.quantity_of(input.output.value, policy_id, name) == 1 },
  )
  datum
}

/// The case configuration, read from the reference input holding the
/// configuration NFT.
pub fn get_config(
  reference_inputs: List<Input>,
  config_policy_id: PolicyId,
) -> CaseConfigV1 {
  expect config: CaseConfigV1 = reference_datum_with_nft(
    reference_inputs,
    config_policy_id,
    config_asset_name,
  )
  config
}

/// The hub oracle datum's fields, in order (`hub_oracle.Datum`), read from
/// the reference input holding the hub NFT. The hub datum is governed, so
/// only the fields a caller reads are decoded.
fn hub_fields(
  reference_inputs: List<Input>,
  hub_policy_id: PolicyId,
) -> List<Data> {
  let datum = reference_datum_with_nft(
    reference_inputs,
    hub_policy_id,
    hub.asset_name,
  )
  builtin.unconstr_fields(datum)
}

/// The active-operators policy named by the hub.
pub fn hub_active_operators(
  reference_inputs: List<Input>,
  hub_policy_id: PolicyId,
) -> PolicyId {
  expect Some(policy) = list.at(hub_fields(reference_inputs, hub_policy_id), 1)
  builtin.un_b_data(policy)
}

/// The fraud-proof policy and address named by the hub.
pub fn hub_fraud_proof(
  reference_inputs: List<Input>,
  hub_policy_id: PolicyId,
) -> (PolicyId, Address) {
  let fields = hub_fields(reference_inputs, hub_policy_id)
  expect Some(policy) = list.at(fields, 6)
  expect Some(address_data) = list.at(fields, 18)
  expect address: Address = address_data
  (builtin.un_b_data(policy), address)
}
