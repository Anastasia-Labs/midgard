//// ## `value-not-preserved` family test fixtures
////
//// Test-fixture support for this family's step selectors, in the same spirit
//// as `midgard/fraud_proofs/native_binding_fixture_v1` (whose block, hub,
//// state-queue and thread builders these extend): not reachable from any
//// on-chain validator entry point. Every challenged block here is a genuine
//// single-transaction native-V1 block whose header additionally commits a
//// **real** single-leaf pre-state ledger MPF — the spent input's out-ref key
//// mapped to the exact `LedgerOutputCommitmentV1` bytes
//// `ledger_output_descriptor_v1.build_v1` derives for the spent output — so
//// every membership witness a selector feeds the family is one the protocol's
//// own ledger reading reconstructs, byte for byte.

use aiken/builtin
use aiken/merkle_patricia_forestry as mpf
use cardano/address
use cardano/transaction.{InlineDatum, Output, Transaction, placeholder}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_tx_input, encode_midgard_tx_output,
}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/value_not_preserved/step_01.{
  AdaAsset, ClaimedAssetV1, TokenAsset, value_not_preserved_fraud_category_id,
}
use midgard/fraud_proofs/value_not_preserved/step_02.{
  AssetLeafOpeningV1, SpentInputValueWitnessV1,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_descriptor_v1
use midgard/ledger_state
use midgard/validation_merkle_v1

// ## The claimed and decoy units

pub const claimed_policy_id: ByteArray =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

pub const claimed_asset_name: ByteArray = #"56414c"

pub const decoy_policy_id: ByteArray =
  #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

pub const decoy_asset_name: ByteArray = #"444543"

pub fn claimed_token_v1() -> ClaimedAssetV1 {
  TokenAsset { policy_id: claimed_policy_id, asset_name: claimed_asset_name }
}

pub fn decoy_token_v1() -> ClaimedAssetV1 {
  TokenAsset { policy_id: decoy_policy_id, asset_name: decoy_asset_name }
}

pub fn claimed_unit_v1() -> ByteArray {
  claimed_policy_id |> builtin.append_bytearray(claimed_asset_name)
}

pub fn decoy_unit_v1() -> ByteArray {
  decoy_policy_id |> builtin.append_bytearray(decoy_asset_name)
}

// ## The spent pre-state output

/// The out-ref every challenged transaction spends.
pub fn spent_input_v1() -> native_types.MidgardTxInput {
  native_types.MidgardTxInput {
    tx_id: #"7777777777777777777777777777777777777777777777777777777777777777",
    output_index: 2,
  }
}

fn output_at_sample_address(
  lovelace: Int,
  assets: List<Pair<ByteArray, Int>>,
) -> native_types.MidgardTxOutput {
  native_types.MidgardTxOutput {
    ..fixture.sample_native_output(),
    value: native_types.MidgardValue { lovelace, assets },
  }
}

/// The pre-state value of `spent_input_v1()` in the token scenarios:
/// 10 ADA, 40 of the claimed unit and 7 of a decoy unit.
pub fn spent_output_with_tokens_v1() -> native_types.MidgardTxOutput {
  output_at_sample_address(
    10_000_000,
    [Pair(claimed_unit_v1(), 40), Pair(decoy_unit_v1(), 7)],
  )
}

/// The pre-state value in the ADA and mint scenarios: 10 ADA, no assets.
pub fn spent_output_ada_only_v1() -> native_types.MidgardTxOutput {
  output_at_sample_address(10_000_000, [])
}

// ## Challenged transactions
//
// All are `valid_native_tx_v1` with the spend input replaced by
// `spent_input_v1()` and the outputs/fee/mint/validity chosen per scenario.
// The claimed-token conservation equation over `spent_output_with_tokens_v1`
// is `40 + mint − Σ_out == 0`.

pub fn challenged_tx_v1(
  outputs: List<native_types.MidgardTxOutput>,
  mint: Data,
  fee: Int,
  validity: native_types.MidgardTxValidity,
) -> native_types.MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  native_types.MidgardTransaction {
    ..base,
    validity,
    body: native_types.MidgardTransactionBody {
      ..base.body,
      inputs: [spent_input_v1()],
      outputs,
      fee,
      mint,
    },
  }
}

pub fn empty_mint_v1() -> Data {
  let empty: Data = []
  empty
}

/// `{ policy: { name: quantity } }` in the shape
/// `preimages.encode_mint_preimage` folds.
pub fn single_entry_mint_v1(
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
) -> Data {
  builtin.map_data(
    [
      Pair(
        builtin.b_data(policy_id),
        builtin.map_data(
          [Pair(builtin.b_data(asset_name), builtin.i_data(quantity))],
        ),
      ),
    ],
  )
}

/// Pays out 50 of the claimed unit against 40 sourced: `delta = 40 − 50 =
/// −10 < 0`, the **inflated** fault (value created from nothing).
pub fn token_inflated_tx_v1() -> native_types.MidgardTransaction {
  challenged_tx_v1(
    [output_at_sample_address(4_000_000, [Pair(claimed_unit_v1(), 50)])],
    empty_mint_v1(),
    1_000_000,
    native_types.TxIsValid,
  )
}

/// Pays out 30 of the claimed unit against 40 sourced with no burn:
/// `delta = +10 > 0`, the **deflated** fault (value destroyed).
pub fn token_deflated_tx_v1() -> native_types.MidgardTransaction {
  challenged_tx_v1(
    [output_at_sample_address(4_000_000, [Pair(claimed_unit_v1(), 30)])],
    empty_mint_v1(),
    1_000_000,
    native_types.TxIsValid,
  )
}

/// Pays out exactly the 40 sourced (and the decoy's 7): balanced in every
/// unit — the transaction this family must never convict.
pub fn token_balanced_tx_v1() -> native_types.MidgardTransaction {
  challenged_tx_v1(
    [
      output_at_sample_address(
        9_000_000,
        [Pair(claimed_unit_v1(), 40), Pair(decoy_unit_v1(), 7)],
      ),
    ],
    empty_mint_v1(),
    1_000_000,
    native_types.TxIsValid,
  )
}

/// Balanced in the claimed unit (40 in, 40 out) but inflated in the decoy
/// unit (7 in, 9 out): the wrong-asset-claim scenario, in which a claim
/// naming the *claimed* unit folds to zero and must refuse even though the
/// transaction really is fraudulent in another unit.
pub fn decoy_inflated_tx_v1() -> native_types.MidgardTransaction {
  challenged_tx_v1(
    [
      output_at_sample_address(
        9_000_000,
        [Pair(claimed_unit_v1(), 40), Pair(decoy_unit_v1(), 9)],
      ),
    ],
    empty_mint_v1(),
    1_000_000,
    native_types.TxIsValid,
  )
}

/// Mints 25 of the claimed unit and pays all 25 out (sources hold none):
/// `delta = 0 + 25 − 25 = 0`. The minted-and-fully-paid-out corner — the
/// mint fold is what keeps this balanced transaction unconvictable.
pub fn minted_paid_out_tx_v1() -> native_types.MidgardTransaction {
  challenged_tx_v1(
    [output_at_sample_address(4_000_000, [Pair(claimed_unit_v1(), 25)])],
    single_entry_mint_v1(claimed_policy_id, claimed_asset_name, 25),
    1_000_000,
    native_types.TxIsValid,
  )
}

/// Pays out 9.5 ADA plus a 1 ADA fee against 10 ADA sourced:
/// `delta = 10_000_000 − 9_500_000 − 1_000_000 = −500_000 < 0`, the inflated
/// ADA fault.
pub fn ada_inflated_tx_v1() -> native_types.MidgardTransaction {
  challenged_tx_v1(
    [output_at_sample_address(9_500_000, [])],
    empty_mint_v1(),
    1_000_000,
    native_types.TxIsValid,
  )
}

/// Pays out 9 ADA plus a 1 ADA fee against 10 ADA sourced: `delta = 0`,
/// balanced.
pub fn ada_balanced_tx_v1() -> native_types.MidgardTransaction {
  challenged_tx_v1(
    [output_at_sample_address(9_000_000, [])],
    empty_mint_v1(),
    1_000_000,
    native_types.TxIsValid,
  )
}

/// The inflated token transaction honestly recorded as a **no-op**
/// (`TxIsInvalid`): correct block production, which the acceptance gate must
/// refuse to adjudicate.
pub fn rejected_token_inflated_tx_v1() -> native_types.MidgardTransaction {
  native_types.MidgardTransaction {
    ..token_inflated_tx_v1(),
    validity: native_types.TxIsInvalid,
  }
}

// ## Challenged blocks with a real pre-state ledger

/// A single-transaction native-V1 block whose header commits the spent
/// output's descriptor as the single leaf of a real pre-state ledger MPF.
pub type ValueBlockV1 {
  block: fixture.NativeBlockFixtureV1,
  spent_output: native_types.MidgardTxOutput,
  descriptor_cbor: ByteArray,
}

pub fn value_block_v1(
  tx: native_types.MidgardTransaction,
  spent_output: native_types.MidgardTxOutput,
) -> ValueBlockV1 {
  let base = fixture.native_block_fixture_v1(tx)
  expect [spend_input] = tx.body.inputs
  expect Some(descriptor_cbor) = ledger_output_descriptor_v1.ledger_value_v1(
    spend_input.output_index,
    encode_midgard_tx_output(spent_output),
  )
  let prev_utxos_root = mpf.root(
    mpf.insert(
      mpf.empty,
      encode_midgard_tx_input(spend_input),
      descriptor_cbor,
      [],
    ),
  )
  let header = ledger_state.HeaderV1 { ..base.header, prev_utxos_root }
  let header_data: Data = header
  ValueBlockV1 {
    block: fixture.NativeBlockFixtureV1 {
      ..base,
      header,
      header_hash: utils.serialise_and_hash_28(header_data),
    },
    spent_output,
    descriptor_cbor,
  }
}

/// The thread token asset name of a value-not-preserved thread against
/// `block`.
pub fn thread_asset_name_v1(block: ValueBlockV1) -> ByteArray {
  value_not_preserved_fraud_category_id
    |> builtin.append_bytearray(block.block.header_hash)
}

/// `fixture.continue_step_l1_tx_v1` under this family's thread asset name:
/// thread input and next-step output, hub-oracle and state-queue reference
/// inputs, and the delegated `phas` membership redeemer over the block's
/// committed transaction.
pub fn continue_step_01_l1_tx_v1(
  block: ValueBlockV1,
  output_state_data: Data,
) -> Transaction {
  let asset_name = thread_asset_name_v1(block)
  Transaction {
    ..fixture.continue_mid_step_l1_tx_v1(asset_name, output_state_data),
    reference_inputs: [
      fixture.hub_reference_input(),
      fixture.state_queue_reference_input(block.block),
    ],
    redeemers: [
      fixture.phas_withdraw_redeemer_entry(
        block.block.transactions_phas_root,
        block.block.tx_id,
        // The raw transactions trie commits `Data(L2TransactionSourceV1)`.
        block.block.source_cbor,
      ),
    ],
  }
}

/// L1 transaction of a step-02 **self-loop** iteration: the thread token
/// returns to the *same* step script rather than moving to the next one.
pub fn self_loop_l1_tx_v1(
  asset_name: ByteArray,
  output_state_data: Data,
) -> Transaction {
  let output_datum: ct.StepDatum<Data> = ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(output_state_data),
  }
  let output_datum_data: Data = output_datum
  Transaction {
    ..placeholder,
    inputs: [fixture.ct_thread_input(asset_name)],
    outputs: [
      Output {
        address: address.from_script(fixture.step_script_hash),
        value: fixture.ct_token_value(asset_name),
        datum: InlineDatum(output_datum_data),
        reference_script: None,
      },
    ],
  }
}

// ## Spent-input value witnesses

fn asset_leaf_hashes(assets: List<Pair<ByteArray, Int>>) -> List<ByteArray> {
  when assets is {
    [] -> []
    [Pair(unit, quantity), ..rest] ->
      [
        ledger_output_commitment_v1.asset_leaf_hash(
          builtin.slice_bytearray(0, 28, unit),
          builtin.slice_bytearray(28, 32, unit),
          quantity,
        ),
        ..asset_leaf_hashes(rest)
      ]
  }
}

fn openings_for(
  assets: List<Pair<ByteArray, Int>>,
  leaves: List<ByteArray>,
) -> List<AssetLeafOpeningV1> {
  when assets is {
    [] -> []
    [Pair(unit, quantity)] ->
      [
        AssetLeafOpeningV1 {
          policy_id: builtin.slice_bytearray(0, 28, unit),
          asset_name: builtin.slice_bytearray(28, 32, unit),
          quantity,
          siblings: [],
        },
      ]
    [Pair(unit_0, quantity_0), Pair(unit_1, quantity_1)] -> {
      expect [leaf_0, leaf_1] = leaves
      [
        AssetLeafOpeningV1 {
          policy_id: builtin.slice_bytearray(0, 28, unit_0),
          asset_name: builtin.slice_bytearray(28, 32, unit_0),
          quantity: quantity_0,
          siblings: [leaf_1],
        },
        AssetLeafOpeningV1 {
          policy_id: builtin.slice_bytearray(0, 28, unit_1),
          asset_name: builtin.slice_bytearray(28, 32, unit_1),
          quantity: quantity_1,
          siblings: [leaf_0],
        },
      ]
    }
    _ -> fail @"fixture supports at most two asset leaves"
  }
}

/// The genuine pre-state value witness of the block's single spent input: the
/// committed descriptor bytes, the (single-leaf) ledger membership proof, and
/// — for a token claim — the full authenticated asset-leaf walk.
pub fn spent_input_value_witness_v1(
  block: ValueBlockV1,
  claimed_asset: ClaimedAssetV1,
) -> SpentInputValueWitnessV1 {
  when claimed_asset is {
    AdaAsset ->
      SpentInputValueWitnessV1 {
        descriptor_cbor: block.descriptor_cbor,
        ledger_membership_proof: [],
        asset_peaks: [],
        asset_openings: [],
      }
    TokenAsset { .. } -> {
      let assets = block.spent_output.value.assets
      let leaves = asset_leaf_hashes(assets)
      let built = validation_merkle_v1.build_frontier(leaves)
      SpentInputValueWitnessV1 {
        descriptor_cbor: block.descriptor_cbor,
        ledger_membership_proof: [],
        asset_peaks: built.peaks,
        asset_openings: openings_for(assets, leaves),
      }
    }
  }
}
