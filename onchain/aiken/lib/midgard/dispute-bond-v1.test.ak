use aiken/builtin
use aiken/cbor
use aiken/interval
use cardano/address.{Address, Inline, Script, VerificationKey}
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction,
}
use env
use midgard/dispute_bond_v1.{DisputeBondV1, Forfeit, Refund}
use midgard/fraud_proof
use midgard/ledger_state.{block_maturity_duration_v1}
use midgard/validation_dispute_v1

const fraud_proof_policy_id =
  #"f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0"

const bond_script_hash =
  #"b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0"

const prover = #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1"

const other_key = #"c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2"

const header_hash = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const other_header_hash =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const bond_lovelace = 10_000_000_000

const forfeit_after = 1_800_000_000_000

const fee = 300_000

const bond_out_ref =
  OutputReference {
    transaction_id: #"0101010101010101010101010101010101010101010101010101010101010101",
    output_index: 0,
  }

const second_bond_out_ref =
  OutputReference {
    transaction_id: #"0101010101010101010101010101010101010101010101010101010101010101",
    output_index: 1,
  }

const wallet_out_ref =
  OutputReference {
    transaction_id: #"0202020202020202020202020202020202020202020202020202020202020202",
    output_index: 0,
  }

fn bond_datum() -> DisputeBondV1 {
  DisputeBondV1 {
    challenged_header_hash: header_hash,
    fraud_prover: prover,
    forfeit_after,
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

fn wallet_input() -> Input {
  Input {
    output_reference: wallet_out_ref,
    output: Output {
      address: address.from_verification_key(other_key),
      value: assets.from_lovelace(5_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

/// The permanent fraud record of `category ++ convicted_header`.
fn fraud_proof_reference(
  category: ByteArray,
  convicted_header: ByteArray,
) -> Input {
  let datum: Data = fraud_proof.Datum { fraud_prover: prover }
  Input {
    output_reference: OutputReference {
      transaction_id: #"0303030303030303030303030303030303030303030303030303030303030303",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(
        #"d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0",
      ),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(
            fraud_proof_policy_id,
            builtin.append_bytearray(category, convicted_header),
            1,
          ),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn prover_address() -> Address {
  address.from_verification_key(prover)
}

fn paying(destination: Address, lovelace: Int, tag: OutputReference) -> Output {
  let tag_data: Data = tag
  Output {
    address: destination,
    value: assets.from_lovelace(lovelace),
    datum: InlineDatum(tag_data),
    reference_script: None,
  }
}

fn tx(
  inputs: List<Input>,
  reference_inputs: List<Input>,
  outputs: List<Output>,
  lower: Int,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs,
    reference_inputs,
    outputs,
    fee,
    validity_range: interval.between(lower, lower + 60_000),
  }
}

fn single_input_floor() -> Int {
  bond_lovelace - fee
}

fn refund(transaction: Transaction) -> Bool {
  dispute_bond_v1.spend(
    fraud_proof_policy_id,
    Some(bond_datum()),
    Refund { fraud_proof_ref_input_index: 0, output_index: 0 },
    bond_out_ref,
    transaction,
  )
}

fn forfeit(transaction: Transaction) -> Bool {
  dispute_bond_v1.spend(
    fraud_proof_policy_id,
    Some(bond_datum()),
    Forfeit { output_index: 0 },
    bond_out_ref,
    transaction,
  )
}

fn convicted() -> List<Input> {
  [fraud_proof_reference(#"00000006", header_hash)]
}

// ## Parameters

test fee_fixture_is_within_the_move_fee_cap() {
  fee <= env.max_dispute_move_fee_lovelace
}

test forfeit_after_is_maturity_plus_two_clocks_plus_resolution() {
  dispute_bond_v1.forfeit_after(1_000) == 1_000 + block_maturity_duration_v1 + 2 * validation_dispute_v1.dispute_side_clock_milliseconds + validation_dispute_v1.dispute_resolution_allowance_milliseconds
}

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
  dispute_bond_v1.treasury_address == Address {
    payment_credential: Script(
      builtin.blake2b_224(
        builtin.cons_bytearray(0, env.protocol_treasury_native_script_cbor),
      ),
    ),
    stake_credential: None,
  }
}

// ## Wire layout (shared with the SDK schemas)

test bond_datum_wire_layout() {
  cbor.serialise(bond_datum()) == #"d8799f581caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa581ca1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a11b000001a3185c5000ff"
}

test bond_redeemer_wire_layout() {
  and {
    cbor.serialise(Refund { fraud_proof_ref_input_index: 1, output_index: 2 }) == #"d8799f0102ff",
    cbor.serialise(Forfeit { output_index: 3 }) == #"d87a9f03ff",
  }
}

// ## Refund

test refund_pays_the_prover_on_conviction() {
  refund(
    tx(
      [bond_input(bond_out_ref)],
      convicted(),
      [paying(prover_address(), single_input_floor(), bond_out_ref)],
      0,
    ),
  )
}

/// Any category's conviction of the block refunds its challengers.
test refund_accepts_a_conviction_in_another_category() {
  refund(
    tx(
      [bond_input(bond_out_ref)],
      [fraud_proof_reference(#"00000001", header_hash)],
      [paying(prover_address(), single_input_floor(), bond_out_ref)],
      0,
    ),
  )
}

test refund_rejects_a_conviction_of_another_block() {
  !refund(
    tx(
      [bond_input(bond_out_ref)],
      [fraud_proof_reference(#"00000006", other_header_hash)],
      [paying(prover_address(), single_input_floor(), bond_out_ref)],
      0,
    ),
  )
}

test refund_rejects_without_the_fraud_proof_reference() fail {
  refund(
    tx(
      [bond_input(bond_out_ref)],
      [wallet_input()],
      [paying(prover_address(), single_input_floor(), bond_out_ref)],
      0,
    ),
  )
}

/// A token of another policy carrying the same name is not a conviction.
test refund_rejects_a_token_of_another_policy() fail {
  let forged = Input {
    ..wallet_input(),
    output: Output {
      ..wallet_input().output,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(
            bond_script_hash,
            builtin.append_bytearray(#"00000006", header_hash),
            1,
          ),
    },
  }
  refund(
    tx(
      [bond_input(bond_out_ref)],
      [forged],
      [paying(prover_address(), single_input_floor(), bond_out_ref)],
      0,
    ),
  )
}

test refund_rejects_paying_another_key() {
  !refund(
    tx(
      [bond_input(bond_out_ref)],
      convicted(),
      [
        paying(
          address.from_verification_key(other_key),
          single_input_floor(),
          bond_out_ref,
        ),
      ],
      0,
    ),
  )
}

/// The refund goes to the prover's enterprise address, not a staked one.
test refund_rejects_a_staked_prover_address() {
  !refund(
    tx(
      [bond_input(bond_out_ref)],
      convicted(),
      [
        paying(
          prover_address() |> address.with_delegation_key(other_key),
          single_input_floor(),
          bond_out_ref,
        ),
      ],
      0,
    ),
  )
}

// ## Forfeit

test forfeit_pays_the_treasury_after_forfeit_after() {
  forfeit(
    tx(
      [bond_input(bond_out_ref)],
      [],
      [
        paying(
          dispute_bond_v1.treasury_address,
          single_input_floor(),
          bond_out_ref,
        ),
      ],
      forfeit_after + 1,
    ),
  )
}

test forfeit_rejects_at_forfeit_after() {
  !forfeit(
    tx(
      [bond_input(bond_out_ref)],
      [],
      [
        paying(
          dispute_bond_v1.treasury_address,
          single_input_floor(),
          bond_out_ref,
        ),
      ],
      forfeit_after,
    ),
  )
}

test forfeit_rejects_an_unbounded_lower_bound() fail {
  forfeit(
    Transaction {
      ..tx(
        [bond_input(bond_out_ref)],
        [],
        [
          paying(
            dispute_bond_v1.treasury_address,
            single_input_floor(),
            bond_out_ref,
          ),
        ],
        0,
      ),
      validity_range: interval.before(forfeit_after + 60_000),
    },
  )
}

test forfeit_rejects_paying_the_prover() {
  !forfeit(
    tx(
      [bond_input(bond_out_ref)],
      [],
      [paying(prover_address(), single_input_floor(), bond_out_ref)],
      forfeit_after + 1,
    ),
  )
}

/// Exactly the treasury address: the treasury script with a stake credential
/// is another address.
test forfeit_rejects_a_staked_treasury_address() {
  !forfeit(
    tx(
      [bond_input(bond_out_ref)],
      [],
      [
        paying(
          Address {
            ..dispute_bond_v1.treasury_address,
            stake_credential: Some(Inline(VerificationKey(other_key))),
          },
          single_input_floor(),
          bond_out_ref,
        ),
      ],
      forfeit_after + 1,
    ),
  )
}

/// Forfeit checks only time: a convicted block's unrefunded bond is still
/// forfeited after `forfeit_after` (accepted behaviour).
test forfeit_takes_a_convicted_blocks_unrefunded_bond() {
  forfeit(
    tx(
      [bond_input(bond_out_ref)],
      convicted(),
      [
        paying(
          dispute_bond_v1.treasury_address,
          single_input_floor(),
          bond_out_ref,
        ),
      ],
      forfeit_after + 1,
    ),
  )
}

// ## Double-satisfaction tag

test refund_rejects_an_untagged_output() {
  !refund(
    tx(
      [bond_input(bond_out_ref)],
      convicted(),
      [
        Output {
          ..paying(prover_address(), single_input_floor(), bond_out_ref),
          datum: NoDatum,
        },
      ],
      0,
    ),
  )
}

test forfeit_rejects_another_bonds_tag() {
  !forfeit(
    tx(
      [bond_input(bond_out_ref)],
      [],
      [
        paying(
          dispute_bond_v1.treasury_address,
          single_input_floor(),
          second_bond_out_ref,
        ),
      ],
      forfeit_after + 1,
    ),
  )
}

test spend_rejects_a_missing_datum() fail {
  dispute_bond_v1.spend(
    fraud_proof_policy_id,
    None,
    Forfeit { output_index: 0 },
    bond_out_ref,
    tx(
      [bond_input(bond_out_ref)],
      [],
      [
        paying(
          dispute_bond_v1.treasury_address,
          single_input_floor(),
          bond_out_ref,
        ),
      ],
      forfeit_after + 1,
    ),
  )
}

// ## Single-input fee rule

test refund_rejects_one_lovelace_below_the_single_input_floor() {
  !refund(
    tx(
      [bond_input(bond_out_ref)],
      convicted(),
      [paying(prover_address(), single_input_floor() - 1, bond_out_ref)],
      0,
    ),
  )
}

/// The bond pays at most the cap, however large the fee.
test forfeit_caps_the_single_input_fee() {
  let capped = bond_lovelace - env.max_dispute_move_fee_lovelace
  let transaction = Transaction {
    ..tx(
      [bond_input(bond_out_ref)],
      [],
      [paying(dispute_bond_v1.treasury_address, capped, bond_out_ref)],
      forfeit_after + 1,
    ),
    fee: env.max_dispute_move_fee_lovelace + 1,
  }
  and {
    forfeit(transaction),
    !forfeit(
      Transaction {
        ..transaction,
        outputs: [
          paying(dispute_bond_v1.treasury_address, capped - 1, bond_out_ref),
        ],
      },
    ),
  }
}

/// A batched refund of two bonds cannot pay the fee from each bond.
test refund_rejects_a_fee_from_a_batched_bond() {
  !refund(
    tx(
      [bond_input(bond_out_ref), bond_input(second_bond_out_ref)],
      convicted(),
      [
        paying(prover_address(), single_input_floor(), bond_out_ref),
        paying(prover_address(), single_input_floor(), second_bond_out_ref),
      ],
      0,
    ),
  )
}

/// A fee paid from a wallet input leaves the bond whole: accepted.
test refund_accepts_a_whole_bond_with_a_wallet_fee() {
  refund(
    tx(
      [bond_input(bond_out_ref), wallet_input()],
      convicted(),
      [paying(prover_address(), bond_lovelace, bond_out_ref)],
      0,
    ),
  )
}

test forfeit_rejects_a_short_bond_with_a_wallet_input() {
  !forfeit(
    tx(
      [bond_input(bond_out_ref), wallet_input()],
      [],
      [
        paying(
          dispute_bond_v1.treasury_address,
          bond_lovelace - 1,
          bond_out_ref,
        ),
      ],
      forfeit_after + 1,
    ),
  )
}
