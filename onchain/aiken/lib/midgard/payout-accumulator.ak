use cardano/address.{Address}
use cardano/transaction.{Datum as CardanoDatum, OutputReference}
use midgard/common/types.{ValuePairs}

pub type Datum {
  l2_value: ValuePairs,
  bridged_tokens: ValuePairs,
  l1_address: Address,
  l1_datum: CardanoDatum,
}

pub type SpendRedeemer {
  AddFunds
  ConcludeWithdrawal
}

pub type MintRedeemer {
  withdrawal_utxo_out_ref: OutputReference,
}
