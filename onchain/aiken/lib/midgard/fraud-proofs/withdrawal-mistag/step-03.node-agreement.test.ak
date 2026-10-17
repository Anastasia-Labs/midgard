//// Node-agreement vectors for the withdrawal value rule.
////
//// Each `agrees_<case>` test judges a requested `l2_value` (raw CBOR, in the
//// order admission accepted it) against a ledger output's descriptor facts
//// (lovelace, asset count, asset frontier commitment) with step-03's
//// `output_value_matches_v1`, and pins the verdict. The TypeScript twin,
//// demo/midgard-fault-proofs/tests/withdrawal-mistag.node-agreement.test.ts,
//// rebuilds each case's output and value, checks these bytes and facts are
//// that case's, and checks the node's `classifyWithdrawalFromLedger` reaches
//// the same verdict.

use aiken/cbor
use cardano/address
use cardano/assets
use cardano/transaction.{NoDatum, OutputReference}
use midgard/fraud_proofs/withdrawal_mistag/step_03
use midgard/ledger_state

fn chain_valid(
  l2_value_cbor: ByteArray,
  output_lovelace: Int,
  output_asset_count: Int,
  output_asset_frontier_commitment: ByteArray,
) -> Bool {
  expect Some(data) = cbor.deserialise(l2_value_cbor)
  expect l2_value: ledger_state.MidgardValue = data
  let body = ledger_state.WithdrawalBody {
    l2_outref: OutputReference { transaction_id: #"", output_index: 0 },
    l2_owner: #"",
    l2_value,
    l1_address: address.from_verification_key(#""),
    l1_datum: NoDatum,
  }
  step_03.output_value_matches_v1(
    body,
    assets.from_asset_list(l2_value),
    output_lovelace,
    output_asset_count,
    output_asset_frontier_commitment,
  )
}

test agrees_ada_only() {
  chain_valid(
    #"bf40bf401a006acfc0ffff",
    7_000_000,
    0,
    #"b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd",
  ) == True
}

test agrees_wrong_lovelace() {
  chain_valid(
    #"bf40bf401a006acfbfffff",
    7_000_000,
    0,
    #"b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd",
  ) == False
}

test agrees_policies_out_of_order() {
  chain_valid(
    #"a3581cababababababababababababababababababababababababababababa141ff0140a1401a006acfc0581c0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0aa1410101",
    7_000_000,
    2,
    #"19097f6fee0ac8f5ee93e477e565ea087940bfb3e6f2eb4161043bf561a3fc63",
  ) == True
}

test agrees_mixed_width_names() {
  chain_valid(
    #"bf40bf401a006acfc0ff581c0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0abf4200000241ff01ffff",
    7_000_000,
    2,
    #"b2ebaf96a59c5228342bcaf4c1f3d2b371a0bc97b45fed6e740f96a2a09a4e37",
  ) == True
}

test agrees_changed_quantity() {
  chain_valid(
    #"a240a1401a006acfc0581c0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0aa24200000241ff02",
    7_000_000,
    2,
    #"b2ebaf96a59c5228342bcaf4c1f3d2b371a0bc97b45fed6e740f96a2a09a4e37",
  ) == False
}

test agrees_missing_asset() {
  chain_valid(
    #"a240a1401a006acfc0581c0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0aa141ff01",
    7_000_000,
    2,
    #"b2ebaf96a59c5228342bcaf4c1f3d2b371a0bc97b45fed6e740f96a2a09a4e37",
  ) == False
}

test agrees_extra_requested_asset() {
  chain_valid(
    #"a240a1401a006acfc0581c0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0aa141ff01",
    7_000_000,
    0,
    #"b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd",
  ) == False
}

test agrees_second_ada_name() {
  chain_valid(
    #"a140a2401a006acfc0410101",
    7_000_000,
    0,
    #"b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd",
  ) == False
}

test agrees_short_policy() {
  chain_valid(
    #"a240a1401a006acfc0581b0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0aa141ff01",
    7_000_000,
    0,
    #"b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd",
  ) == False
}

test agrees_long_asset_name() {
  chain_valid(
    #"a240a1401a006acfc0581c0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0aa15821cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc01",
    7_000_000,
    0,
    #"b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd",
  ) == False
}

test agrees_hundred_assets() {
  chain_valid(
    #"bf40bf401a02faf080ff581c0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0abf4200000142000101420002014200030142000401420005014200060142000701420008014200090142000a0142000b0142000c0142000d0142000e0142000f014200100142001101420012014200130142001401420015014200160142001701420018014200190142001a0142001b0142001c0142001d0142001e0142001f014200200142002101420022014200230142002401420025014200260142002701420028014200290142002a0142002b0142002c0142002d0142002e0142002f014200300142003101420032014200330142003401420035014200360142003701420038014200390142003a0142003b0142003c0142003d0142003e0142003f014200400142004101420042014200430142004401420045014200460142004701420048014200490142004a0142004b0142004c0142004d0142004e0142004f014200500142005101420052014200530142005401420055014200560142005701420058014200590142005a0142005b0142005c0142005d0142005e0142005f0142006001420061014200620142006301ffff",
    50_000_000,
    100,
    #"29f24bc1549a248312c8b7204c9ab733f135312ea22e5dfd78453534f64a426d",
  ) == True
}

test agrees_hundred_and_one_assets() {
  chain_valid(
    #"bf40bf401a02faf080ff581c0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0abf4200000142000101420002014200030142000401420005014200060142000701420008014200090142000a0142000b0142000c0142000d0142000e0142000f014200100142001101420012014200130142001401420015014200160142001701420018014200190142001a0142001b0142001c0142001d0142001e0142001f014200200142002101420022014200230142002401420025014200260142002701420028014200290142002a0142002b0142002c0142002d0142002e0142002f014200300142003101420032014200330142003401420035014200360142003701420038014200390142003a0142003b0142003c0142003d0142003e0142003f014200400142004101420042014200430142004401420045014200460142004701420048014200490142004a0142004b0142004c0142004d0142004e0142004f014200500142005101420052014200530142005401420055014200560142005701420058014200590142005a0142005b0142005c0142005d0142005e0142005f014200600142006101420062014200630142006401ffff",
    50_000_000,
    101,
    #"72fa0b94ee5f6bcc6ea5487e91a6c13502e5caf5fd4b66e4b0438e661c5c0066",
  ) == False
}
