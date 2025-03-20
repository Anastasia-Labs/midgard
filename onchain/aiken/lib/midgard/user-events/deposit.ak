use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Address}
use cardano/assets.{AssetName}
use cardano/transaction.{OutputReference}
use midgard/common/types.{PosixTime}
use midgard/ledger_state

pub type Datum {
  event: ledger_state.DepositEvent,
  inclusion_time: PosixTime,
  witness: ScriptHash,
  refund_address: Address,
  refund_datum: Option<Data>,
}

pub type SpendPurpose {
  FundWithdrawal
  TransferToReserve
  Refund
}

pub type SpendRedeemer {
  hub_ref_input_index: Int,
  settlement_ref_input_index: Int,
  settlement_node_asset_name: AssetName,
  membership_proof: mpf.Proof,
  purpose: SpendPurpose,
}

pub type MintRedeemer {
  AuthenticateDeposit { nonce: OutputReference }
  BurnDepositNFT
}

pub type ObserverRedeemer {
  ObserverRedeemer
}
