use cardano/address.{Address}
use cardano/transaction.{Datum as CardanoDatum, OutputReference}
use midgard/common/types.{ValuePairs}

pub type Datum {
  l2_value: ValuePairs,
  l1_address: Address,
  l1_datum: CardanoDatum,
}

pub type SpendRedeemer {
  AddFunds {
    input_index: Int,
    output_index: Int,
    reserve_input_index: Int,
    hub_ref_input_index: Int,
  }
  ConcludeWithdrawal
}

pub type MintRedeemer {
  withdrawal_utxo_out_ref: OutputReference,
}
