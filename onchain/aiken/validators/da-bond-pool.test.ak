//// Executable handler coverage for `validators/da-bond-pool.ak` (#687).
////
//// Every test drives the real `da_bond_pool.da_bond_pool.mint` / `.spend` /
//// `.else` handlers. Fixture convention follows
//// `validators/settlement-handlers.test.ak`: named constants for every script
//// hash, one `honest_*` builder per action returning a fully populated
//// `Transaction`, and one `fail` test per guard whose body is the honest
//// fixture with a single minimal mutation. The honest control named beside
//// each negative passes, so the mutated conjunct is the one that refuses.
//// Timing is computed from `env` (never a profile literal), so the module
//// holds under every `--env`.
////
//// GUARD -> TEST MAP
////
//// mint / InitPool
////   honest -> da_bond_pool_init_accepts_honest_pool
////   own-policy mint is exactly [(MIDGARD_DA_BOND_POOL, 1)]
////     -> ..._init_rejects_burning_the_nft
////     -> ..._init_rejects_minting_two_nfts
////     -> ..._init_rejects_a_second_asset_name
////   the tx spends `init_ref` (so a second InitPool is impossible)
////     -> ..._init_rejects_without_spending_init_ref
////   output: inline datum
////     -> ..._init_rejects_datum_hash
////   output: no reference script
////     -> ..._init_rejects_reference_script
////   output address == Address { Script(own), None }
////     -> ..._init_rejects_stake_credential
////     -> ..._init_rejects_other_address
////   output datum == Bonded
////     -> ..._init_rejects_wrong_datum
////   output value is lovelace + the NFT only
////     -> ..._init_rejects_another_token
////   output lovelace >= da_bond_pool_floor_lovelace
////     -> ..._init_rejects_below_the_floor
////
//// spend / common (every arm)
////   exactly one input at `Script(own)`
////     -> da_bond_pool_spend_rejects_a_second_pool_input  (control: top-up)
////   continuing output carries an inline datum (G1)
////     -> da_bond_pool_top_up_rejects_datum_hash  (control: top-up)
////   script purposes other than mint / spend
////     -> da_bond_pool_else_rejects_other_purpose
////
//// spend / TopUp   (controls: ..._top_up_accepts_honest_bonded,
////                  ..._top_up_accepts_honest_withdrawing,
////                  ..._top_up_accepts_exact_minimum)
////   increase >= da_bond_min_top_up_lovelace
////     -> ..._top_up_rejects_below_minimum
////   continuing value is lovelace + the NFT only
////     -> ..._top_up_rejects_non_ada_token
////   continuing datum byte-equal to the input's
////     -> ..._top_up_rejects_changed_datum
////   continuing address == pool_in.address
////     -> ..._top_up_rejects_moved_address
////
//// spend / Slash   (controls: ..._slash_accepts_honest_bonded,
////                  ..._slash_accepts_withdrawing_and_keeps_unlock_at,
////                  ..._slash_takes_exactly_the_backing_when_short,
////                  ..._slash_takes_zero_below_the_floor)
////   hub oracle reference input is authentic
////     -> ..._slash_rejects_unauthentic_hub
////   Mint(state_queue_policy) redeemer at the named index
////     -> ..._slash_rejects_without_state_queue_redeemer
////     -> ..._slash_rejects_remove_redeemer_under_a_foreign_policy
////   its constructor index == 5 (RemoveUnavailableBlockAfterTimeout)
////     -> ..._slash_rejects_other_state_queue_redeemer
////   (B4) the input at the named index holds the correction-lock token
////     -> ..._slash_rejects_without_correction_lock_input
////     -> ..._slash_rejects_idle_datum_without_lock_token
////   (B4) the correction lock is Idle
////     -> ..._slash_rejects_resume_step_locked_correction_lock
////   continuing datum byte-equal to the input's
////     -> ..._slash_rejects_changed_datum
////   continuing address == pool_in.address (payment and stake)
////     -> ..._slash_rejects_changed_stake_credential
////   continuing output has no reference script
////     -> ..._slash_rejects_reference_script
////   continuing value is lovelace + the NFT
////     -> ..._slash_rejects_dropped_nft
////   lovelace == input - min(da_bond, backing)
////     -> ..._slash_rejects_taking_more
////     -> ..._slash_rejects_taking_less
////
//// spend / BeginWithdraw, CancelWithdraw, CompleteWithdraw: owner quorum
////   (controls: ..._begin_withdraw_accepts_honest,
////              ..._cancel_withdraw_accepts_honest,
////              ..._complete_withdraw_accepts_honest)
////   `owner_quorum_met` over the params owners
////     -> ..._{begin,cancel,complete}_withdraw_rejects_below_threshold
////     -> ..._{begin,cancel,complete}_withdraw_rejects_non_owner_signers
////   `get_da_params` NFT quantity 1
////     -> ..._{begin,cancel,complete}_withdraw_rejects_params_without_nft
////   `get_da_params` payment credential Script(da_params_policy_id)
////     -> ..._{begin,cancel,complete}_withdraw_rejects_params_at_other_credential
////
//// spend / BeginWithdraw
////   input is Bonded
////     -> ..._begin_withdraw_rejects_from_withdrawing
////   output == Withdrawing { upper + da_bond_withdraw_delay_ms_v1 }
////     -> ..._begin_withdraw_rejects_wrong_unlock_at
////   value unchanged
////     -> ..._begin_withdraw_rejects_changed_value
////
//// spend / CancelWithdraw
////   input is Withdrawing
////     -> ..._cancel_withdraw_rejects_from_bonded
////   output datum == Bonded (else a rewritten unlock_at skips the delay)
////     -> ..._cancel_withdraw_rejects_output_not_bonded
////   value unchanged
////     -> ..._cancel_withdraw_rejects_changed_value
////
//// spend / CompleteWithdraw   (extra controls:
////                  ..._complete_withdraw_accepts_at_unlock_boundary,
////                  ..._complete_withdraw_accepts_the_whole_backing)
////   input is Withdrawing
////     -> ..._complete_withdraw_rejects_from_bonded
////   inclusive lower bound >= unlock_at
////     -> ..._complete_withdraw_rejects_before_unlock
////   amount > 0
////     -> ..._complete_withdraw_rejects_zero
////   amount <= backing
////     -> ..._complete_withdraw_rejects_more_than_backing
////   output datum == Bonded
////     -> ..._complete_withdraw_rejects_output_not_bonded
////   output lovelace == input - amount
////     -> ..._complete_withdraw_rejects_output_not_input_minus_amount

use aiken/cbor
use aiken/crypto.{VerificationKeyHash, blake2b_256}
use aiken/interval.{Interval}
use aiken/primitive/bytearray
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{PolicyId, Value}
use cardano/script_context.{ScriptContext, Withdrawing}
use cardano/transaction.{
  DatumHash, InlineDatum, Input, Mint, NoDatum, Output, OutputReference,
  Redeemer, ScriptPurpose, Spend, Transaction, placeholder,
}
use da_bond_pool as pool_validator
use env
use midgard/availability_challenge as availability
use midgard/correction_lock
use midgard/da_attestation_types.{DaParamsDatum, da_params_asset_name}
use midgard/da_bond_pool as pool
use midgard/hub_oracle as hub
use midgard/state_queue

// ---------------------------------------------------------------------------
// Named script hashes, keys and amounts
// ---------------------------------------------------------------------------

const pool_policy: PolicyId =
  #"d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0"

const hub_policy: PolicyId =
  #"e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0"

const da_params_policy: PolicyId =
  #"e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1e1"

const state_queue_policy: PolicyId =
  #"e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2"

const correction_lock_script: PolicyId =
  #"e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3e3"

const foreign_policy: PolicyId =
  #"efefefefefefefefefefefefefefefefefefefefefefefefefefefef"

const owner_1: VerificationKeyHash =
  #"c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1"

const owner_2: VerificationKeyHash =
  #"c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2"

const owner_3: VerificationKeyHash =
  #"c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3c3"

const non_owner_1: VerificationKeyHash =
  #"c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8"

const non_owner_2: VerificationKeyHash =
  #"c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9c9"

const committee_key: ByteArray =
  #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1"

const da_bond: Int = 500_000_000

const pool_floor: Int = 5_000_000

const min_top_up: Int = 5_000_000

/// Backing 600 ADA: more than one bond.
const pool_lovelace: Int = 605_000_000

const withdraw_amount: Int = 100_000_000

const begin_lower: Int = 1_000_000_000

const withdrawing_unlock_at: Int = 2_000_000_000

fn parameters() -> availability.ParametersV1 {
  availability.ParametersV1 {
    response_geometry: availability.ResponseGeometryV1 {
      chunk_byte_length: 4_096,
      tranche_byte_length: 4 * 1024 * 1024,
      max_tranche_count: 16,
    },
    da_bond_lovelace: da_bond,
    challenger_bond_lovelace: 10_000_000_000,
    max_open_fee_lovelace: 1,
    max_publication_fee_lovelace: 1,
    max_settlement_fee_lovelace: 1,
    max_close_fee_lovelace: 1,
    max_timeout_fee_lovelace: 1,
    da_slash_penalty_lovelace: 100_000_000,
    da_bond_min_top_up_lovelace: min_top_up,
    da_bond_pool_floor_lovelace: pool_floor,
    challenge_record_lovelace: 27_000_000,
  }
}

// ---------------------------------------------------------------------------
// Small builders
// ---------------------------------------------------------------------------

fn outref(seed: Int, index: Int) -> OutputReference {
  OutputReference {
    transaction_id: bytearray.push(
      #"00000000000000000000000000000000000000000000000000000000000000",
      seed,
    ),
    output_index: index,
  }
}

const init_ref_seed: Int = 1

const pool_ref_seed: Int = 2

fn init_ref() -> OutputReference {
  outref(init_ref_seed, 0)
}

fn pool_ref() -> OutputReference {
  outref(pool_ref_seed, 0)
}

fn pool_address() -> Address {
  Address { payment_credential: Script(pool_policy), stake_credential: None }
}

fn pool_nft() -> Value {
  assets.from_asset(pool_policy, pool.da_bond_pool_asset_name, 1)
}

fn pool_value(lovelace: Int) -> Value {
  assets.from_lovelace(lovelace) |> assets.merge(pool_nft())
}

fn bonded() -> Data {
  let datum: Data = pool.Bonded
  datum
}

fn withdrawing(unlock_at: Int) -> Data {
  let datum: Data = pool.Withdrawing { unlock_at }
  datum
}

fn pool_output(datum: Data, lovelace: Int) -> Output {
  Output {
    address: pool_address(),
    value: pool_value(lovelace),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn pool_input(datum: Data, lovelace: Int) -> Input {
  Input { output_reference: pool_ref(), output: pool_output(datum, lovelace) }
}

fn wallet_input(seed: Int) -> Input {
  Input {
    output_reference: outref(seed, 0),
    output: Output {
      address: Address {
        payment_credential: VerificationKey(owner_1),
        stake_credential: None,
      },
      value: assets.from_lovelace(700_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn pool_tx(
  inputs: List<Input>,
  reference_inputs: List<Input>,
  outputs: List<Output>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  extra_signatories: List<VerificationKeyHash>,
  validity_range: Interval,
) -> Transaction {
  Transaction {
    ..placeholder,
    inputs,
    reference_inputs,
    outputs,
    redeemers,
    extra_signatories,
    validity_range,
  }
}

fn with_pool_output(tx: Transaction, output: Output) -> Transaction {
  Transaction { ..tx, outputs: [output] }
}

fn run_spend(redeemer: pool.SpendRedeemer, tx: Transaction) -> Bool {
  pool_validator.da_bond_pool.spend(
    init_ref(),
    hub_policy,
    da_params_policy,
    parameters(),
    None,
    redeemer,
    pool_ref(),
    tx,
  )
}

fn run_mint(tx: Transaction) -> Bool {
  pool_validator.da_bond_pool.mint(
    init_ref(),
    hub_policy,
    da_params_policy,
    parameters(),
    pool.InitPool { output_index: 0 },
    pool_policy,
    tx,
  )
}

// ---------------------------------------------------------------------------
// mint / InitPool
// ---------------------------------------------------------------------------

fn init_output(lovelace: Int) -> Output {
  pool_output(bonded(), lovelace)
}

fn honest_init_tx() -> Transaction {
  Transaction {
    ..placeholder,
    inputs: [
      Input { output_reference: init_ref(), output: wallet_input(9).output },
    ],
    outputs: [init_output(pool_floor)],
    mint: pool_nft(),
  }
}

test da_bond_pool_init_accepts_honest_pool() {
  run_mint(honest_init_tx())
}

test da_bond_pool_init_rejects_burning_the_nft() fail {
  run_mint(
    Transaction {
      ..honest_init_tx(),
      mint: assets.from_asset(pool_policy, pool.da_bond_pool_asset_name, -1),
    },
  )
}

test da_bond_pool_init_rejects_minting_two_nfts() fail {
  run_mint(
    Transaction {
      ..honest_init_tx(),
      mint: assets.from_asset(pool_policy, pool.da_bond_pool_asset_name, 2),
    },
  )
}

test da_bond_pool_init_rejects_a_second_asset_name() fail {
  run_mint(
    Transaction {
      ..honest_init_tx(),
      mint: pool_nft() |> assets.add(pool_policy, "MIDGARD_DA_BOND_POOL_2", 1),
    },
  )
}

test da_bond_pool_init_rejects_without_spending_init_ref() fail {
  run_mint(Transaction { ..honest_init_tx(), inputs: [wallet_input(9)] })
}

test da_bond_pool_init_rejects_datum_hash() fail {
  run_mint(
    with_pool_output(
      honest_init_tx(),
      Output { ..init_output(pool_floor), datum: DatumHash(blake2b_256(#"00")) },
    ),
  )
}

test da_bond_pool_init_rejects_reference_script() fail {
  run_mint(
    with_pool_output(
      honest_init_tx(),
      Output {
        ..init_output(pool_floor),
        reference_script: Some(foreign_policy),
      },
    ),
  )
}

test da_bond_pool_init_rejects_stake_credential() fail {
  run_mint(
    with_pool_output(
      honest_init_tx(),
      Output {
        ..init_output(pool_floor),
        address: Address {
          payment_credential: Script(pool_policy),
          stake_credential: Some(address.Inline(VerificationKey(owner_1))),
        },
      },
    ),
  )
}

test da_bond_pool_init_rejects_other_address() fail {
  run_mint(
    with_pool_output(
      honest_init_tx(),
      Output {
        ..init_output(pool_floor),
        address: address.from_script(foreign_policy),
      },
    ),
  )
}

test da_bond_pool_init_rejects_wrong_datum() fail {
  run_mint(
    with_pool_output(
      honest_init_tx(),
      Output { ..init_output(pool_floor), datum: InlineDatum(withdrawing(0)) },
    ),
  )
}

test da_bond_pool_init_rejects_another_token() fail {
  run_mint(
    with_pool_output(
      honest_init_tx(),
      Output {
        ..init_output(pool_floor),
        value: pool_value(pool_floor) |> assets.add(foreign_policy, "x", 1),
      },
    ),
  )
}

test da_bond_pool_init_rejects_below_the_floor() fail {
  run_mint(with_pool_output(honest_init_tx(), init_output(pool_floor - 1)))
}

// ---------------------------------------------------------------------------
// spend / TopUp
// ---------------------------------------------------------------------------

fn top_up() -> pool.SpendRedeemer {
  pool.TopUp { output_index: 0 }
}

fn honest_top_up_tx(datum: Data, increase: Int) -> Transaction {
  pool_tx(
    [pool_input(datum, pool_lovelace), wallet_input(9)],
    [],
    [pool_output(datum, pool_lovelace + increase)],
    [],
    [],
    interval.everything,
  )
}

test da_bond_pool_top_up_accepts_honest_bonded() {
  run_spend(top_up(), honest_top_up_tx(bonded(), 50_000_000))
}

test da_bond_pool_top_up_accepts_honest_withdrawing() {
  run_spend(
    top_up(),
    honest_top_up_tx(withdrawing(withdrawing_unlock_at), 50_000_000),
  )
}

test da_bond_pool_top_up_accepts_exact_minimum() {
  run_spend(top_up(), honest_top_up_tx(bonded(), min_top_up))
}

test da_bond_pool_top_up_rejects_below_minimum() fail {
  run_spend(top_up(), honest_top_up_tx(bonded(), min_top_up - 1))
}

test da_bond_pool_top_up_rejects_non_ada_token() fail {
  run_spend(
    top_up(),
    with_pool_output(
      honest_top_up_tx(bonded(), 50_000_000),
      Output {
        ..pool_output(bonded(), pool_lovelace + 50_000_000),
        value: pool_value(pool_lovelace + 50_000_000)
          |> assets.add(foreign_policy, "x", 1),
      },
    ),
  )
}

test da_bond_pool_top_up_rejects_changed_datum() fail {
  run_spend(
    top_up(),
    with_pool_output(
      honest_top_up_tx(bonded(), 50_000_000),
      pool_output(
        withdrawing(withdrawing_unlock_at),
        pool_lovelace + 50_000_000,
      ),
    ),
  )
}

test da_bond_pool_top_up_rejects_moved_address() fail {
  run_spend(
    top_up(),
    with_pool_output(
      honest_top_up_tx(bonded(), 50_000_000),
      Output {
        ..pool_output(bonded(), pool_lovelace + 50_000_000),
        address: address.from_script(foreign_policy),
      },
    ),
  )
}

test da_bond_pool_top_up_rejects_datum_hash() fail {
  run_spend(
    top_up(),
    with_pool_output(
      honest_top_up_tx(bonded(), 50_000_000),
      Output {
        ..pool_output(bonded(), pool_lovelace + 50_000_000),
        datum: DatumHash(blake2b_256(bonded() |> as_cbor)),
      },
    ),
  )
}

fn as_cbor(data: Data) -> ByteArray {
  cbor.serialise(data)
}

// ---------------------------------------------------------------------------
// spend / common
// ---------------------------------------------------------------------------

test da_bond_pool_spend_rejects_a_second_pool_input() fail {
  let honest = honest_top_up_tx(bonded(), 50_000_000)
  let stray = Input {
    output_reference: outref(7, 0),
    output: Output {
      address: pool_address(),
      value: assets.from_lovelace(2_000_000),
      datum: InlineDatum(bonded()),
      reference_script: None,
    },
  }
  run_spend(
    top_up(),
    Transaction { ..honest, inputs: [stray, ..honest.inputs] },
  )
}

test da_bond_pool_else_rejects_other_purpose() fail {
  pool_validator.da_bond_pool.else(
    init_ref(),
    hub_policy,
    da_params_policy,
    parameters(),
    ScriptContext {
      transaction: honest_top_up_tx(bonded(), 50_000_000),
      redeemer: {
        let redeemer: Data = top_up()
        redeemer
      },
      info: Withdrawing(Script(pool_policy)),
    },
  )
}

// ---------------------------------------------------------------------------
// spend / Slash
// ---------------------------------------------------------------------------

const challenged_header_hash: ByteArray =
  #"0101010101010101010101010101010101010101010101010101010101010101"

const challenge_asset_name: ByteArray =
  #"4441434802020202020202020202020202020202020202020202020202020202"

fn hub_datum() -> hub.Datum {
  let other = address.from_script(foreign_policy)
  hub.Datum {
    registered_operators: foreign_policy,
    active_operators: foreign_policy,
    retired_operators: foreign_policy,
    scheduler: foreign_policy,
    state_queue: state_queue_policy,
    fraud_proof_catalogue: foreign_policy,
    fraud_proof: foreign_policy,
    deposit: foreign_policy,
    withdrawal: foreign_policy,
    tx_order: foreign_policy,
    settlement: foreign_policy,
    payout: foreign_policy,
    registered_operators_addr: other,
    active_operators_addr: other,
    retired_operators_addr: other,
    scheduler_addr: other,
    state_queue_addr: address.from_script(state_queue_policy),
    fraud_proof_catalogue_addr: other,
    fraud_proof_addr: other,
    deposit_addr: other,
    withdrawal_addr: other,
    tx_order_addr: other,
    settlement_addr: other,
    reserve_addr: other,
    payout_addr: other,
    reserve_observer: foreign_policy,
  }
}

fn hub_ref_input(nft_policy: PolicyId) -> Input {
  Input {
    output_reference: outref(3, 0),
    output: Output {
      address: address.from_script(hub_policy),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(nft_policy, hub.asset_name, 1),
      datum: InlineDatum(hub_datum()),
      reference_script: None,
    },
  }
}

fn correction_lock_input(datum: correction_lock.Datum) -> Input {
  Input {
    output_reference: outref(4, 0),
    output: Output {
      address: address.from_script(correction_lock_script),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(hub_policy, correction_lock.asset_name, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn remove_unavailable_redeemer() -> Data {
  let redeemer: state_queue.MintRedeemer = state_queue.RemoveUnavailableBlockAfterTimeout {
    yield_to_ref_input_index: 1,
    unavailable_header_hash: challenged_header_hash,
    challenge_asset_name,
    removal_approach: state_queue.RemoveTimedOutHead {
      confirmed_state_input_outref: outref(5, 0),
      confirmed_state_output_index: 1,
    },
  }
  let redeemer_data: Data = redeemer
  redeemer_data
}

fn slash() -> pool.SpendRedeemer {
  pool.Slash {
    hub_oracle_ref_input_index: 0,
    state_queue_mint_redeemer_index: 1,
    correction_lock_input_index: 0,
    output_index: 0,
  }
}

/// The first step of an unavailable-block removal: the Idle correction lock is
/// spent under `RemoveUnavailableBlockAfterTimeout`, and the pool gives up
/// `min(da_bond, backing)`. Only the pool-side shape is modelled; the
/// state-queue, correction-lock and availability scripts are not run here.
fn honest_slash_tx(
  datum: Data,
  in_lovelace: Int,
  out_lovelace: Int,
) -> Transaction {
  let slash_data: Data = slash()
  pool_tx(
    [
      correction_lock_input(correction_lock.Idle),
      pool_input(datum, in_lovelace),
    ],
    [hub_ref_input(hub_policy)],
    [pool_output(datum, out_lovelace)],
    [
      Pair(Spend(pool_ref()), slash_data),
      Pair(Mint(state_queue_policy), remove_unavailable_redeemer()),
    ],
    [],
    interval.everything,
  )
}

fn honest_bonded_slash_tx() -> Transaction {
  honest_slash_tx(bonded(), pool_lovelace, pool_lovelace - da_bond)
}

test da_bond_pool_slash_accepts_honest_bonded() {
  run_spend(slash(), honest_bonded_slash_tx())
}

/// G2: a Withdrawing pool stays slashable and keeps its `unlock_at`.
test da_bond_pool_slash_accepts_withdrawing_and_keeps_unlock_at() {
  run_spend(
    slash(),
    honest_slash_tx(
      withdrawing(withdrawing_unlock_at),
      pool_lovelace,
      pool_lovelace - da_bond,
    ),
  )
}

/// Backing 300 ADA < one bond: exactly the backing is taken, leaving the floor.
test da_bond_pool_slash_takes_exactly_the_backing_when_short() {
  run_spend(
    slash(),
    honest_slash_tx(bonded(), pool_floor + 300_000_000, pool_floor),
  )
}

/// G3: at or below the floor the backing is 0, so nothing is taken.
test da_bond_pool_slash_takes_zero_below_the_floor() {
  and {
    run_spend(slash(), honest_slash_tx(bonded(), pool_floor, pool_floor)),
    run_spend(
      slash(),
      honest_slash_tx(bonded(), pool_floor - 1_000_000, pool_floor - 1_000_000),
    ),
  }
}

test da_bond_pool_slash_rejects_unauthentic_hub() fail {
  let honest = honest_bonded_slash_tx()
  run_spend(
    slash(),
    Transaction { ..honest, reference_inputs: [hub_ref_input(foreign_policy)] },
  )
}

test da_bond_pool_slash_rejects_without_state_queue_redeemer() fail {
  let honest = honest_bonded_slash_tx()
  let slash_data: Data = slash()
  run_spend(
    slash(),
    Transaction { ..honest, redeemers: [Pair(Spend(pool_ref()), slash_data)] },
  )
}

test da_bond_pool_slash_rejects_remove_redeemer_under_a_foreign_policy() fail {
  let honest = honest_bonded_slash_tx()
  let slash_data: Data = slash()
  run_spend(
    slash(),
    Transaction {
      ..honest,
      redeemers: [
        Pair(Spend(pool_ref()), slash_data),
        Pair(Mint(foreign_policy), remove_unavailable_redeemer()),
      ],
    },
  )
}

test da_bond_pool_slash_rejects_other_state_queue_redeemer() fail {
  let honest = honest_bonded_slash_tx()
  let slash_data: Data = slash()
  let unattested: state_queue.MintRedeemer = state_queue.RemoveUnattestedBlockAfterTimeout {
    yield_to_ref_input_index: 1,
    timed_out_header_hash: challenged_header_hash,
    removal_approach: state_queue.RemoveLastUnattestedBlock {
      predecessor_input_outref: outref(5, 0),
      predecessor_output_index: 1,
    },
  }
  let unattested_data: Data = unattested
  run_spend(
    slash(),
    Transaction {
      ..honest,
      redeemers: [
        Pair(Spend(pool_ref()), slash_data),
        Pair(Mint(state_queue_policy), unattested_data),
      ],
    },
  )
}

/// B4: a resume step of a multi-step removal carries the same state-queue
/// redeemer but runs on a Locked correction lock, with no availability
/// redeemer and so no timeout arithmetic. Slash must refuse it.
test da_bond_pool_slash_rejects_resume_step_locked_correction_lock() fail {
  let honest = honest_bonded_slash_tx()
  run_spend(
    slash(),
    Transaction {
      ..honest,
      inputs: [
        correction_lock_input(
          correction_lock.Locked {
            target_header_hash: challenged_header_hash,
            correction_identity: correction_lock.AvailabilityChallenge {
              challenge_asset_name,
            },
          },
        ),
        pool_input(bonded(), pool_lovelace),
      ],
    },
  )
}

/// B4: with no correction-lock input the named index lands on the pool input,
/// which holds no lock token.
test da_bond_pool_slash_rejects_without_correction_lock_input() fail {
  let honest = honest_bonded_slash_tx()
  run_spend(
    slash(),
    Transaction { ..honest, inputs: [pool_input(bonded(), pool_lovelace)] },
  )
}

/// B4: an input with an Idle correction-lock datum but no lock token is not
/// the singleton.
test da_bond_pool_slash_rejects_idle_datum_without_lock_token() fail {
  let honest = honest_bonded_slash_tx()
  let counterfeit = correction_lock_input(correction_lock.Idle)
  run_spend(
    slash(),
    Transaction {
      ..honest,
      inputs: [
        Input {
          ..counterfeit,
          output: Output {
            ..counterfeit.output,
            value: assets.from_lovelace(2_000_000),
          },
        },
        pool_input(bonded(), pool_lovelace),
      ],
    },
  )
}

test da_bond_pool_slash_rejects_changed_datum() fail {
  run_spend(
    slash(),
    with_pool_output(
      honest_bonded_slash_tx(),
      pool_output(withdrawing(withdrawing_unlock_at), pool_lovelace - da_bond),
    ),
  )
}

test da_bond_pool_slash_rejects_changed_stake_credential() fail {
  run_spend(
    slash(),
    with_pool_output(
      honest_bonded_slash_tx(),
      Output {
        ..pool_output(bonded(), pool_lovelace - da_bond),
        address: Address {
          payment_credential: Script(pool_policy),
          stake_credential: Some(address.Inline(VerificationKey(owner_1))),
        },
      },
    ),
  )
}

test da_bond_pool_slash_rejects_reference_script() fail {
  run_spend(
    slash(),
    with_pool_output(
      honest_bonded_slash_tx(),
      Output {
        ..pool_output(bonded(), pool_lovelace - da_bond),
        reference_script: Some(foreign_policy),
      },
    ),
  )
}

test da_bond_pool_slash_rejects_dropped_nft() fail {
  run_spend(
    slash(),
    with_pool_output(
      honest_bonded_slash_tx(),
      Output {
        ..pool_output(bonded(), pool_lovelace - da_bond),
        value: assets.from_lovelace(pool_lovelace - da_bond),
      },
    ),
  )
}

test da_bond_pool_slash_rejects_taking_more() fail {
  run_spend(
    slash(),
    honest_slash_tx(bonded(), pool_lovelace, pool_lovelace - da_bond - 1),
  )
}

test da_bond_pool_slash_rejects_taking_less() fail {
  run_spend(
    slash(),
    honest_slash_tx(bonded(), pool_lovelace, pool_lovelace - da_bond + 1),
  )
}

// ---------------------------------------------------------------------------
// Owner quorum fixtures
// ---------------------------------------------------------------------------

fn da_params() -> DaParamsDatum {
  DaParamsDatum {
    committee: committee_key,
    committee_signers_hash: blake2b_256(committee_key),
    da_threshold: 1,
    owners: [owner_1, owner_2, owner_3],
    update_threshold: 2,
  }
}

fn da_params_ref_input() -> Input {
  Input {
    output_reference: outref(6, 0),
    output: Output {
      address: address.from_script(da_params_policy),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(da_params_policy, da_params_asset_name, 1),
      datum: InlineDatum(da_params()),
      reference_script: None,
    },
  }
}

fn quorum() -> List<VerificationKeyHash> {
  [owner_1, owner_3]
}

fn with_signers(
  tx: Transaction,
  extra_signatories: List<VerificationKeyHash>,
) -> Transaction {
  Transaction { ..tx, extra_signatories }
}

fn with_params_output(tx: Transaction, output: Output) -> Transaction {
  let honest = da_params_ref_input()
  Transaction { ..tx, reference_inputs: [Input { ..honest, output }] }
}

fn params_without_nft(tx: Transaction) -> Transaction {
  with_params_output(
    tx,
    Output {
      ..da_params_ref_input().output,
      value: assets.from_lovelace(2_000_000),
    },
  )
}

fn params_at_other_credential(tx: Transaction) -> Transaction {
  with_params_output(
    tx,
    Output {
      ..da_params_ref_input().output,
      address: address.from_script(foreign_policy),
    },
  )
}

fn quorum_tx(
  in_datum: Data,
  out_datum: Data,
  out_lovelace: Int,
  validity_range: Interval,
) -> Transaction {
  pool_tx(
    [pool_input(in_datum, pool_lovelace)],
    [da_params_ref_input()],
    [pool_output(out_datum, out_lovelace)],
    [],
    quorum(),
    validity_range,
  )
}

// ---------------------------------------------------------------------------
// spend / BeginWithdraw
// ---------------------------------------------------------------------------

fn begin_upper() -> Int {
  begin_lower + env.max_validity_range_length
}

fn begin_range() -> Interval {
  interval.between(begin_lower, begin_upper())
}

fn begin_unlock_at() -> Int {
  begin_upper() + env.da_bond_withdraw_delay_ms_v1
}

fn begin() -> pool.SpendRedeemer {
  pool.BeginWithdraw { da_params_ref_input_index: 0, output_index: 0 }
}

fn honest_begin_tx() -> Transaction {
  quorum_tx(
    bonded(),
    withdrawing(begin_unlock_at()),
    pool_lovelace,
    begin_range(),
  )
}

test da_bond_pool_begin_withdraw_accepts_honest() {
  run_spend(begin(), honest_begin_tx())
}

test da_bond_pool_begin_withdraw_rejects_below_threshold() fail {
  run_spend(begin(), with_signers(honest_begin_tx(), [owner_1]))
}

test da_bond_pool_begin_withdraw_rejects_non_owner_signers() fail {
  run_spend(
    begin(),
    with_signers(honest_begin_tx(), [non_owner_1, non_owner_2]),
  )
}

test da_bond_pool_begin_withdraw_rejects_params_without_nft() fail {
  run_spend(begin(), params_without_nft(honest_begin_tx()))
}

test da_bond_pool_begin_withdraw_rejects_params_at_other_credential() fail {
  run_spend(begin(), params_at_other_credential(honest_begin_tx()))
}

test da_bond_pool_begin_withdraw_rejects_from_withdrawing() fail {
  run_spend(
    begin(),
    quorum_tx(
      withdrawing(withdrawing_unlock_at),
      withdrawing(begin_unlock_at()),
      pool_lovelace,
      begin_range(),
    ),
  )
}

test da_bond_pool_begin_withdraw_rejects_wrong_unlock_at() fail {
  run_spend(
    begin(),
    quorum_tx(
      bonded(),
      withdrawing(begin_unlock_at() - 1),
      pool_lovelace,
      begin_range(),
    ),
  )
}

test da_bond_pool_begin_withdraw_rejects_changed_value() fail {
  run_spend(
    begin(),
    quorum_tx(
      bonded(),
      withdrawing(begin_unlock_at()),
      pool_lovelace - 1,
      begin_range(),
    ),
  )
}

// ---------------------------------------------------------------------------
// spend / CancelWithdraw
// ---------------------------------------------------------------------------

fn cancel() -> pool.SpendRedeemer {
  pool.CancelWithdraw { da_params_ref_input_index: 0, output_index: 0 }
}

fn honest_cancel_tx() -> Transaction {
  quorum_tx(
    withdrawing(withdrawing_unlock_at),
    bonded(),
    pool_lovelace,
    interval.everything,
  )
}

test da_bond_pool_cancel_withdraw_accepts_honest() {
  run_spend(cancel(), honest_cancel_tx())
}

test da_bond_pool_cancel_withdraw_rejects_below_threshold() fail {
  run_spend(cancel(), with_signers(honest_cancel_tx(), [owner_2]))
}

test da_bond_pool_cancel_withdraw_rejects_non_owner_signers() fail {
  run_spend(
    cancel(),
    with_signers(honest_cancel_tx(), [non_owner_1, non_owner_2]),
  )
}

test da_bond_pool_cancel_withdraw_rejects_params_without_nft() fail {
  run_spend(cancel(), params_without_nft(honest_cancel_tx()))
}

test da_bond_pool_cancel_withdraw_rejects_params_at_other_credential() fail {
  run_spend(cancel(), params_at_other_credential(honest_cancel_tx()))
}

test da_bond_pool_cancel_withdraw_rejects_from_bonded() fail {
  run_spend(
    cancel(),
    quorum_tx(bonded(), bonded(), pool_lovelace, interval.everything),
  )
}

test da_bond_pool_cancel_withdraw_rejects_output_not_bonded() fail {
  run_spend(
    cancel(),
    quorum_tx(
      withdrawing(withdrawing_unlock_at),
      withdrawing(0),
      pool_lovelace,
      interval.everything,
    ),
  )
}

test da_bond_pool_cancel_withdraw_rejects_changed_value() fail {
  run_spend(
    cancel(),
    quorum_tx(
      withdrawing(withdrawing_unlock_at),
      bonded(),
      pool_lovelace + 1,
      interval.everything,
    ),
  )
}

// ---------------------------------------------------------------------------
// spend / CompleteWithdraw
// ---------------------------------------------------------------------------

fn complete(amount: Int) -> pool.SpendRedeemer {
  pool.CompleteWithdraw {
    amount,
    da_params_ref_input_index: 0,
    output_index: 0,
  }
}

fn range_from(lower: Int) -> Interval {
  interval.between(lower, lower + env.max_validity_range_length)
}

fn complete_tx(
  in_datum: Data,
  out_datum: Data,
  out_lovelace: Int,
  lower: Int,
) -> Transaction {
  quorum_tx(in_datum, out_datum, out_lovelace, range_from(lower))
}

fn honest_complete_tx() -> Transaction {
  complete_tx(
    withdrawing(withdrawing_unlock_at),
    bonded(),
    pool_lovelace - withdraw_amount,
    withdrawing_unlock_at + env.max_validity_range_length,
  )
}

test da_bond_pool_complete_withdraw_accepts_honest() {
  run_spend(complete(withdraw_amount), honest_complete_tx())
}

test da_bond_pool_complete_withdraw_accepts_at_unlock_boundary() {
  run_spend(
    complete(withdraw_amount),
    complete_tx(
      withdrawing(withdrawing_unlock_at),
      bonded(),
      pool_lovelace - withdraw_amount,
      withdrawing_unlock_at,
    ),
  )
}

/// The whole backing may leave; the floor stays behind as a Bonded pool.
test da_bond_pool_complete_withdraw_accepts_the_whole_backing() {
  run_spend(
    complete(pool_lovelace - pool_floor),
    complete_tx(
      withdrawing(withdrawing_unlock_at),
      bonded(),
      pool_floor,
      withdrawing_unlock_at,
    ),
  )
}

test da_bond_pool_complete_withdraw_rejects_below_threshold() fail {
  run_spend(
    complete(withdraw_amount),
    with_signers(honest_complete_tx(), [owner_3]),
  )
}

test da_bond_pool_complete_withdraw_rejects_non_owner_signers() fail {
  run_spend(
    complete(withdraw_amount),
    with_signers(honest_complete_tx(), [non_owner_1, non_owner_2]),
  )
}

test da_bond_pool_complete_withdraw_rejects_params_without_nft() fail {
  run_spend(complete(withdraw_amount), params_without_nft(honest_complete_tx()))
}

test da_bond_pool_complete_withdraw_rejects_params_at_other_credential() fail {
  run_spend(
    complete(withdraw_amount),
    params_at_other_credential(honest_complete_tx()),
  )
}

test da_bond_pool_complete_withdraw_rejects_before_unlock() fail {
  run_spend(
    complete(withdraw_amount),
    complete_tx(
      withdrawing(withdrawing_unlock_at),
      bonded(),
      pool_lovelace - withdraw_amount,
      withdrawing_unlock_at - 1,
    ),
  )
}

test da_bond_pool_complete_withdraw_rejects_more_than_backing() fail {
  let amount = pool_lovelace - pool_floor + 1
  run_spend(
    complete(amount),
    complete_tx(
      withdrawing(withdrawing_unlock_at),
      bonded(),
      pool_lovelace - amount,
      withdrawing_unlock_at,
    ),
  )
}

test da_bond_pool_complete_withdraw_rejects_zero() fail {
  run_spend(
    complete(0),
    complete_tx(
      withdrawing(withdrawing_unlock_at),
      bonded(),
      pool_lovelace,
      withdrawing_unlock_at,
    ),
  )
}

test da_bond_pool_complete_withdraw_rejects_from_bonded() fail {
  run_spend(
    complete(withdraw_amount),
    complete_tx(
      bonded(),
      bonded(),
      pool_lovelace - withdraw_amount,
      withdrawing_unlock_at,
    ),
  )
}

test da_bond_pool_complete_withdraw_rejects_output_not_bonded() fail {
  run_spend(
    complete(withdraw_amount),
    complete_tx(
      withdrawing(withdrawing_unlock_at),
      withdrawing(withdrawing_unlock_at),
      pool_lovelace - withdraw_amount,
      withdrawing_unlock_at,
    ),
  )
}

test da_bond_pool_complete_withdraw_rejects_output_not_input_minus_amount() fail {
  run_spend(
    complete(withdraw_amount),
    complete_tx(
      withdrawing(withdrawing_unlock_at),
      bonded(),
      pool_lovelace - withdraw_amount - 1,
      withdrawing_unlock_at,
    ),
  )
}
