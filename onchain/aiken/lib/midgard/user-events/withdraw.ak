use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use cardano/address.{Address}
use cardano/transaction.{OutputReference}
use midgard/common/types.{PosixTime}
use midgard/user_events

pub type Event =
  user_events.Event<Info>

pub type Info {
  l2_out_ref: OutputReference,
  l1_address: Address,
  l1_datum: Option<Data>,
}

pub type Datum {
  event: Event,
  inclusion_time: PosixTime,
  witness: ScriptHash,
  refund_address: Address,
  refund_datum: Option<Data>,
}

pub type SpendPurpose {
  FinalizeWithdrawal
  Refund
}

pub type SpendRedeemer {
  mpf_utxo: OutputReference,
  membership_proof: mpf.Proof,
  purpose: SpendPurpose,
}

pub type MintRedeemer {
  AuthenticateDeposit { nonce: OutputReference }
  BurnWithdrawNFT
}

pub type ObserverRedeemer {
  ObserverRedeemer
}
