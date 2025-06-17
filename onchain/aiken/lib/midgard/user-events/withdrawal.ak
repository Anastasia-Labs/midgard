use aiken/merkle_patricia_forestry as mpf
use cardano/assets.{AssetName}
use midgard/ledger_state
use midgard/user_events

pub type Datum =
  user_events.Datum<ledger_state.WithdrawalEvent>

pub type SpendPurpose {
  TransferToEscrow { membership_proof: mpf.Proof }
  Refund { refund_approach: user_events.RefundApproach }
}

pub type SpendRedeemer {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  burn_redeemer_index: Int,
  escrow_mint_redeemer_index: Int,
  purpose: SpendPurpose,
}

pub type MintRedeemer {
  AuthenticateWithdrawal {
    nonce_input_index: Int,
    event_output_index: Int,
    hub_ref_input_index: Int,
    witness_registration_redeemer_index: Int,
    latest_block_ref_input_index: Int,
    l2_output_data: Data,
    membership_proof: mpf.Proof,
  }
  BurnWithdrawalNFT {
    nonce_asset_name: AssetName,
    witness_unregistration_redeemer_index: Int,
  }
}
