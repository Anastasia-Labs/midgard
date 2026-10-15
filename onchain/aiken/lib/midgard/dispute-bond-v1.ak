//// The challenger bond of a validation case. VerifySource locks it in its own
//// UTxO at `dispute_bond_v1`, apart from the case, and binds it to the case
//// by its case id. It is spent only together with its own case's close:
//// refunded when the case convicts and forfeited to the protocol treasury
//// otherwise. The case validator checks the settling output
//// (`validation_case_v1.challenger_bond_address`); this module checks only
//// that the case is closing with this bond.

use aiken/collection/list
use aiken/collection/pairs
use aiken/crypto.{VerificationKeyHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Spend, Transaction}
use midgard/ledger_state.{HeaderHash}
use midgard/validation_case_v1.{CaseClose, CaseId, CaseRedeemerV1}

pub type DisputeBondV1 {
  challenged_header_hash: HeaderHash,
  fraud_prover: VerificationKeyHash,
  /// The case NFT name of the case this bond backs.
  case_id: CaseId,
}

pub type SpendRedeemer {
  /// The case input at `case_input_index` holds this bond's case NFT and is
  /// spent with a `CaseClose` whose bond input is this one.
  SettleWithCase { case_input_index: Int }
}

/// The bond is spent only by its own case's close.
pub fn spend(
  case_policy_id: PolicyId,
  datum: Option<DisputeBondV1>,
  redeemer: SpendRedeemer,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  expect Some(bond) = datum
  let SettleWithCase { case_input_index } = redeemer
  let Transaction { inputs, redeemers, .. } = tx
  expect Some(case_input) = list.at(inputs, case_input_index)
  expect Some(case_redeemer) = pairs.get_first(
    redeemers,
    Spend(case_input.output_reference),
  )
  expect CaseClose { bond_input_index, .. }: CaseRedeemerV1 = case_redeemer
  expect Some(bond_input) = list.at(inputs, bond_input_index)
  and {
    assets.quantity_of(case_input.output.value, case_policy_id, bond.case_id) == 1,
    bond_input.output_reference == own_out_ref,
  }
}
