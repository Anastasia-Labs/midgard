use aiken/builtin
use aiken/cbor
use cardano/address.{Address, Script}
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Spend, Transaction,
}
use env
use midgard/dispute_bond_v1.{DisputeBondV1, SettleWithCase}
use midgard/validation_case_v1.{CaseClose, CaseRedeemerV1, ThreadClose}
use midgard/validation_dispute_v1

const case_policy_id =
  #"c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0"

const bond_script_hash =
  #"b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0"

const prover = #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1"

const header_hash = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const case_id =
  #"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd"

const other_case_id =
  #"cececececececececececececececececececececececececececececececece"

const bond_lovelace = 10_000_000_000

const case_out_ref =
  OutputReference {
    transaction_id: #"0101010101010101010101010101010101010101010101010101010101010101",
    output_index: 0,
  }

const bond_out_ref =
  OutputReference {
    transaction_id: #"0101010101010101010101010101010101010101010101010101010101010101",
    output_index: 1,
  }

const second_bond_out_ref =
  OutputReference {
    transaction_id: #"0101010101010101010101010101010101010101010101010101010101010101",
    output_index: 2,
  }

fn bond_datum() -> DisputeBondV1 {
  DisputeBondV1 {
    challenged_header_hash: header_hash,
    fraud_prover: prover,
    case_id,
  }
}

fn bond_input(out_ref: OutputReference) -> Input {
  let datum: Data = bond_datum()
  Input {
    output_reference: out_ref,
    output: Output {
      address: address.from_script(bond_script_hash),
      value: assets.from_lovelace(bond_lovelace),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

/// The case UTxO holding the case NFT `nft`; its datum is not read by the bond.
fn case_input(nft: ByteArray) -> Input {
  Input {
    output_reference: case_out_ref,
    output: Output {
      address: address.from_script(case_policy_id),
      value: assets.from_lovelace(5_000_000)
        |> assets.add(case_policy_id, nft, 1),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn close_at(bond_input_index: Int) -> CaseRedeemerV1 {
  CaseClose {
    bond_input_index,
    bond_output_index: 0,
    remainder_output_index: 1,
  }
}

/// A transaction spending `inputs`, with the case input spent by `case_redeemer`.
fn tx_with(inputs: List<Input>, case_redeemer: Data) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs,
    redeemers: [Pair(Spend(case_out_ref), case_redeemer)],
  }
}

fn settles(
  datum: Option<DisputeBondV1>,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  dispute_bond_v1.spend(
    case_policy_id,
    datum,
    SettleWithCase { case_input_index: 0 },
    own_out_ref,
    tx,
  )
}

fn honest_tx() -> Transaction {
  let redeemer: Data = close_at(1)
  tx_with([case_input(case_id), bond_input(bond_out_ref)], redeemer)
}

// ## Constants and wire layout (shared with the SDK schemas)

/// The treasury script hash is blake2b_224(0x00 || CBOR), the ledger's native
/// script hash. The vector is a 2-of-3 `atLeast` script, hashed independently.
test treasury_script_hash_is_the_native_script_hash() {
  builtin.blake2b_224(
    builtin.cons_bytearray(
      0,
      #"830302838200581c706c616365686f6c6465723a747265730000000000000000000000018200581c706c616365686f6c6465723a747265730000000000000000000000028200581c706c616365686f6c6465723a74726573000000000000000000000003",
    ),
  ) == #"ab68ab329de629beaa7f6b56e67ed93ead3ccb7c82571c60f798fdcd"
}

test treasury_address_is_the_unstaked_treasury_script() {
  validation_case_v1.treasury_address == Address {
    payment_credential: Script(
      builtin.blake2b_224(
        builtin.cons_bytearray(0, env.protocol_treasury_native_script_cbor),
      ),
    ),
    stake_credential: None,
  }
}

test bond_datum_wire_layout() {
  cbor.serialise(bond_datum()) == #"d8799f581caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa581ca1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a15820cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdff"
}

test bond_redeemer_wire_layout() {
  cbor.serialise(SettleWithCase { case_input_index: 1 }) == #"d8799f01ff"
}

// ## Settling with the case close

test bond_settles_with_its_own_case_close() {
  settles(Some(bond_datum()), bond_out_ref, honest_tx())
}

test bond_refuses_a_case_holding_another_case_nft() fail {
  let redeemer: Data = close_at(1)
  settles(
    Some(bond_datum()),
    bond_out_ref,
    tx_with([case_input(other_case_id), bond_input(bond_out_ref)], redeemer),
  )
}

test bond_refuses_an_input_holding_no_case_nft() fail {
  let redeemer: Data = close_at(1)
  let wallet = Input {
    ..case_input(case_id),
    output: Output {
      ..case_input(case_id).output,
      value: assets.from_lovelace(5_000_000),
    },
  }
  settles(
    Some(bond_datum()),
    bond_out_ref,
    tx_with([wallet, bond_input(bond_out_ref)], redeemer),
  )
}

/// The case close settles one bond: a second bond of the same case spent
/// beside it is not the one the close names.
test bond_refuses_a_case_close_naming_another_bond() fail {
  let redeemer: Data = close_at(1)
  settles(
    Some(bond_datum()),
    second_bond_out_ref,
    tx_with(
      [
        case_input(case_id),
        bond_input(bond_out_ref),
        bond_input(second_bond_out_ref),
      ],
      redeemer,
    ),
  )
}

test bond_refuses_a_thread_close() fail {
  let redeemer: Data = ThreadClose {
    case_output_index: 0,
    thread_input_index: 1,
    outcome: validation_dispute_v1.ChallengerWins,
  }
  settles(
    Some(bond_datum()),
    bond_out_ref,
    tx_with([case_input(case_id), bond_input(bond_out_ref)], redeemer),
  )
}

test bond_refuses_a_case_input_spent_by_no_redeemer() fail {
  settles(
    Some(bond_datum()),
    bond_out_ref,
    Transaction { ..honest_tx(), redeemers: [] },
  )
}

test bond_refuses_a_missing_datum() fail {
  settles(None, bond_out_ref, honest_tx())
}
