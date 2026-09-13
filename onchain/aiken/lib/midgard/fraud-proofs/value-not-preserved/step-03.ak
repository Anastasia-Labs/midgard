//// ## `value-not-preserved` step-03 — outputs, mint and fee
////
//// Completes the claimed asset's conservation fold: subtract every output's
//// claimed-asset quantity (the machine's stage three, restricted to one
//// unit), add the mint field's claimed-asset entries for a token claim (stage
//// four), and subtract the committed fee for an ADA claim (stage five's
//// `lovelace_delta - body.fee`).
////
//// Both fields are opened through the §8.8 door from the thread's
//// `bad_tx_id` anchor and **folded whole** (`fold_opened_field` asserts walk
//// completion), because a zero contribution must be an established absence:
//// skipping an output would hide outflow, and skipping a mint item would turn
//// a minted-and-fully-paid-out unit into a fake inflation fault.
////
//// The mint reading is the canonical one: each field-5 item decodes under
//// `decode_mint_policy_item_cbor`, the exact per-item decoder
//// `decode_midgard_tx_mint_preimage_cbor` is built from, so quantities are
//// non-zero, names bounded and canonically ordered per item — a transaction
//// whose committed mint preimage is non-canonical fails this fold closed
//// (and is separately faultable under canonical-decodability). The one
//// cross-item rule the whole-preimage decoder adds — canonical policy
//// ordering *between* items — is deliberately not re-asserted here: it has
//// no bearing on the per-unit sum other than duplicates, and a duplicated
//// policy in an **accepted** transaction is already canonical-decodability
//// fraud, so summing its entries (as this fold does) can only convict a block
//// that is fraudulent anyway.
////
//// An ADA claim opens no mint field at all: ADA is structurally unmintable
//// (structural-na-q24 — a mint policy id is 28 bytes, the ADA policy is
//// empty), so the ADA conservation equation has no mint term and the step
//// refuses a mint carriage rather than ignoring one.

use aiken/builtin
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/components.{decode_midgard_tx_output_cbor}
use midgard/fraud_proofs/native_tx/preimages.{decode_mint_policy_item_cbor}
use midgard/fraud_proofs/value_not_preserved/step_01.{
  AdaAsset, ClaimedAssetV1, ClaimedImbalanceDirectionV1, TokenAsset,
  unit_matches_claim_v1,
}
use midgard/ledger_state.{MidgardTxId}
use midgard/native_tx_field_access_v1.{FieldCarriageV1}

// ## Thread state

/// The fold's state after every spend input has been accumulated.
pub type State {
  bad_tx_id: MidgardTxId,
  claimed_asset: ClaimedAssetV1,
  claimed_direction: ClaimedImbalanceDirectionV1,
  committed_fee: Int,
  /// `Σ spent-input claimed quantity` — inflow so far.
  claimed_delta: Int,
}

pub type Datum =
  ct.StepDatum<State>

// ## Per-item folds

/// One output's claimed-asset quantity — the machine's stage-three
/// contribution restricted to the claimed unit. The output bytes decode under
/// the canonical output decoder, whose value decoding already enforces
/// non-negative lovelace and strictly positive asset quantities
/// (structural-na-q25), so output contributions can only be outflow.
pub fn output_claimed_quantity_v1(
  output_item_cbor: ByteArray,
  claimed_asset: ClaimedAssetV1,
) -> Int {
  let output = decode_midgard_tx_output_cbor(output_item_cbor)
  when claimed_asset is {
    AdaAsset -> output.value.lovelace
    TokenAsset { .. } ->
      sum_matching_units(output.value.assets, claimed_asset, 0)
  }
}

fn sum_matching_units(
  assets: List<Pair<ByteArray, Int>>,
  claimed_asset: ClaimedAssetV1,
  accumulated: Int,
) -> Int {
  when assets is {
    [] -> accumulated
    [Pair(unit, quantity), ..rest] ->
      sum_matching_units(
        rest,
        claimed_asset,
        if unit_matches_claim_v1(claimed_asset, unit) {
          accumulated + quantity
        } else {
          accumulated
        },
      )
  }
}

/// One field-5 item's claimed-asset quantity — the machine's stage-four
/// contribution restricted to the claimed unit. Quantities may be negative
/// (burns), exactly as the machine folds them. Callable only for a token
/// claim; an ADA claim has no mint term (structural-na-q24).
pub fn mint_item_claimed_quantity_v1(
  mint_item_cbor: ByteArray,
  claimed_asset: ClaimedAssetV1,
) -> Int {
  expect TokenAsset { policy_id, asset_name } = claimed_asset
  let Pair(item_policy_id, asset_pairs) = decode_mint_policy_item_cbor(
    mint_item_cbor,
  )
  if item_policy_id == policy_id {
    sum_matching_mint_assets(asset_pairs, asset_name, 0)
  } else {
    0
  }
}

fn sum_matching_mint_assets(
  asset_pairs: List<Pair<Data, Data>>,
  claimed_asset_name: ByteArray,
  accumulated: Int,
) -> Int {
  when asset_pairs is {
    [] -> accumulated
    [Pair(name_data, quantity_data), ..rest] ->
      sum_matching_mint_assets(
        rest,
        claimed_asset_name,
        if builtin.un_b_data(name_data) == claimed_asset_name {
          accumulated + builtin.un_i_data(quantity_data)
        } else {
          accumulated
        },
      )
  }
}

// ## Step-03 redeemer

pub type Args {
  input_index: Int,
  output_index: Int,
  /// The challenged transaction's compact bytes; the §3 anchor is paid once
  /// and both fields are opened off the same handle.
  native_tx_compact_cbor: ByteArray,
  /// The prover's chosen §8 carriage for the field-2 (outputs) preimage.
  outputs_carriage: FieldCarriageV1,
  /// The prover's chosen §8 carriage for the field-5 (mint) preimage.
  /// `Some` exactly for a token claim; an ADA claim must carry `None`.
  mint_carriage: Option<FieldCarriageV1>,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
