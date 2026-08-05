//// Executable handler coverage for `validators/settlement.ak` (#536).
////
//// Before this module the settlement `spend` and `mint` handlers were executed
//// by zero tests: the only settlement tests were the redeemer-decoder unit
//// tests in `settlement.ak` and the four `stdlib_token_shape_*` tests, which
//// exercise the Aiken stdlib rather than any settlement code (see V-22 in
//// #532). Those tests stay where they are; this module is the real coverage.
////
//// Fixture convention is copied from
//// `lib/midgard/fraud-proofs/transition-trace/proof.test.ak` and
//// `validators/state-queue-merge.test.ak`: named constants for every script
//// hash and root, one `honest_*` builder per redeemer variant that returns a
//// fully-populated `Transaction`, and one `fail` test per guard whose body is
//// the honest fixture with a single field mutated. Each `fail` test is named
//// for the guard it fires; the guard is identified from the crash trace
//// emitted by the compiler (`aiken check` prints the failing `expect` source
//// line for a crashed test), and the mutation is minimal so that exactly one
//// conjunct of the handler flips.
////
//// GUARD -> TEST MAP (see the report in #536 for the same table):
////
//// spend / prologue
////   `expect Some(datum) = m_datum`
////     -> settlement_handler_spend_rejects_missing_datum
////
//// spend / AttachResolutionClaim
////   1. `resolution_claim == None`
////     -> ..._attach_rejects_preexisting_resolution_claim
////   2. `operator |> utils.has_signed(...)`
////     -> ..._attach_rejects_missing_operator_signature
////   3. `singular_utxo_indexer.one_to_one` own-ref binding
////     -> ..._attach_rejects_input_index_not_the_spent_utxo
////   3'. continuing-output authentication (address, NFT, no reference script)
////     -> ..._attach_rejects_output_at_a_foreign_address
////     -> ..._attach_rejects_output_without_the_settlement_nft
////     -> ..._attach_rejects_output_carrying_a_reference_script
////   4/5. hub oracle reference input authenticity
////     -> ..._attach_rejects_unauthentic_hub_oracle_reference_input
////   6. active-operators spending redeemer route + constructor
////     -> ..._attach_rejects_wrong_active_operators_redeemer_constructor
////     -> ..._attach_rejects_active_operators_input_at_a_foreign_address
////   6'. `operator == active_operator`
////     -> ..._attach_rejects_operator_mismatch_with_active_operators_redeemer
////   7. continuing datum == datum with the claim attached
////     -> ..._attach_rejects_resolution_time_not_from_active_operators_redeemer
////     -> ..._attach_rejects_mutated_deposits_root_in_continuing_datum
////   8. scheduler reference input datum shape and operator
////     -> ..._attach_rejects_scheduler_without_an_active_operator
////     -> ..._attach_rejects_operator_that_is_not_the_current_scheduler_operator
////   honest -> settlement_handler_spend_attach_accepts_honest_claim
////
//// spend / DisproveResolutionClaim
////   1. `Some(ResolutionClaim { .. }) = resolution_claim`
////     -> ..._disprove_rejects_settlement_without_a_resolution_claim
////   1'. `operator == resolution_operator`
////     -> ..._disprove_rejects_operator_mismatch_with_the_claim
////   2. one_to_one own-ref binding
////     -> ..._disprove_rejects_input_index_not_the_spent_utxo
////      (the continuing-output authentication helper this arm shares with
////       AttachResolutionClaim is mutated in the attach tests above)
////   3. continuing datum must drop the claim
////     -> ..._disprove_rejects_continuing_datum_that_keeps_the_claim
////   4/5. hub oracle reference input authenticity
////     -> ..._disprove_rejects_unauthentic_hub_oracle_reference_input
////   6. `settlement.valid_event_inclusion` (Deposit route)
////     -> ..._disprove_rejects_event_reference_input_of_the_wrong_script
////     -> ..._disprove_rejects_membership_witness_for_a_foreign_root
////     -> ..._disprove_rejects_phas_withdraw_redeemer_that_does_not_match
////   7. `valid_to < resolution_time`
////     -> ..._disprove_rejects_upper_bound_at_or_after_the_resolution_time
////   8. slashing redeemer route, `operator_is_active == True` branch
////     -> ..._disprove_rejects_slashing_redeemer_for_another_operator
////   8'. slashing redeemer route, `operator_is_active == False` branch
////     -> ..._disprove_rejects_retired_slashing_redeemer_for_another_operator
////   9. `SlashOperatorForBadSettlement`
////     -> ..._disprove_rejects_slashing_for_a_reason_other_than_settlement
////   honest -> ..._disprove_accepts_honest_active_operator_disproof
////          -> ..._disprove_accepts_honest_retired_operator_disproof
////
//// spend / Resolve
////   1. `assets.quantity_of(tx.mint, policy, id) == -1`
////     -> ..._resolve_rejects_absent_burn
////     -> ..._resolve_rejects_burn_of_a_different_settlement_id
////     -> ..._resolve_rejects_burn_of_the_wrong_quantity
////   honest -> settlement_handler_spend_resolve_accepts_exact_burn
////
//// mint / Spawn
////   1/2. hub oracle reference input authenticity
////     -> ..._spawn_rejects_unauthentic_hub_oracle_reference_input
////   3. state-queue merge redeemer route
////     -> ..._spawn_rejects_merge_redeemer_at_a_foreign_policy
////   3'. constructor tag == 4
////     -> ..._spawn_rejects_non_merge_state_queue_redeemer
////   3''. arity == 18
////     -> ..._spawn_rejects_merge_redeemer_of_the_wrong_arity
////   3'''. `header_node_key == settlement_id`
////     -> ..._spawn_rejects_settlement_id_that_is_not_the_merged_header_hash
////   3''''. `m_settlement_redeemer_index` must be `Some`
////     -> ..._spawn_rejects_merge_without_a_settlement_redeemer_index
////   4. authentic settlement output (address, NFT, no reference script)
////     -> ..._spawn_rejects_output_at_a_foreign_address
////     -> ..._spawn_rejects_output_without_the_settlement_nft
////     -> ..._spawn_rejects_output_carrying_a_reference_script
////   5/6. produced datum must mirror the merged block and carry no claim
////     -> ..._spawn_rejects_deposits_root_not_from_the_merged_block
////     -> ..._spawn_rejects_produced_datum_with_a_resolution_claim
////   7. only the new settlement NFT may be minted under this policy
////     -> ..._spawn_rejects_a_second_token_under_the_settlement_policy
////     -> ..._spawn_rejects_a_mint_quantity_other_than_one
////   honest -> settlement_handler_mint_spawn_accepts_honest_merge
////
//// mint / Remove
////   1. authentic settlement input with the NFT at the given index
////     -> ..._remove_rejects_input_without_the_settlement_nft
////   2. spending redeemer at the route must be `Resolve`
////     -> ..._remove_rejects_spend_redeemer_that_is_not_resolve
////     -> ..._remove_rejects_spend_redeemer_at_a_foreign_purpose
////   3. `settlement_id` agreement between burn and spend redeemers
////     -> ..._remove_rejects_settlement_id_mismatch_across_redeemers
////   4. removed settlement must carry a resolution claim
////     -> ..._remove_rejects_settlement_without_a_resolution_claim
////   5. operator signature
////     -> ..._remove_rejects_missing_operator_signature
////   6. `resolution_time <= valid_from`
////     -> ..._remove_rejects_lower_bound_before_the_resolution_time
////   honest -> settlement_handler_mint_remove_accepts_resolved_settlement
////
//// STATED GAPS (not closed here):
////   * `spend`'s `else(_) { fail }` and `mint`'s `else(_) { fail }` fallbacks
////     are not reachable from a direct handler call — the handler functions are
////     what a test can invoke, and the fallback is selected by the ledger from
////     the script purpose. Covering them needs a script-context-level harness
////     this repository does not have.
////   * `DisproveResolutionClaim` is covered on the `Deposit` event route only.
////     The `Withdrawal` and `TxOrder` routes of
////     `midgard/settlement.valid_event_inclusion` differ in the event datum
////     decoded and the validity override applied; they are exercised by the
////     `event_type` dispatch here only insofar as the `Deposit` arm is taken.
////     Their own membership arithmetic is untested by this module.
////   * `mint`'s `Spawn` route trusts the state-queue merge redeemer by design
////     ("we expect it to check them"). These tests pin the ABI decode of that
////     redeemer, not the state-queue validator's own checks.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/interval
use cardano/address.{Address, Script}
use cardano/assets.{AssetName, PolicyId, Value}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Redeemer, ScriptPurpose,
  Spend, Transaction, Withdraw,
}
use env
use midgard/common/types.{Proof}
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/operator_directory
use midgard/operator_directory/active_operators
use midgard/operator_directory/retired_operators
use midgard/scheduler
use midgard/settlement
use midgard/state_queue
use midgard/transition_trace
use settlement as settlement_validator

// ---------------------------------------------------------------------------
// Named script hashes and payloads
// ---------------------------------------------------------------------------

const hub_oracle_hash: ScriptHash =
  #"a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0"

const settlement_policy: PolicyId =
  #"a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1"

const active_operators_policy: PolicyId =
  #"a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2"

const retired_operators_policy: PolicyId =
  #"a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3a3"

const scheduler_policy: PolicyId =
  #"a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4"

const state_queue_policy: PolicyId =
  #"a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5"

const deposit_policy: PolicyId =
  #"a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6a6"

const withdrawal_policy: PolicyId =
  #"a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7"

const tx_order_policy: PolicyId =
  #"a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8a8"

const registered_operators_policy: PolicyId =
  #"a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9a9"

const misc_policy: PolicyId =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const foreign_policy: PolicyId =
  #"abababababababababababababababababababababababababababab"

const operator_vkey: VerificationKeyHash =
  #"b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0"

const other_operator_vkey: VerificationKeyHash =
  #"b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1b1"

const settlement_id: AssetName =
  #"c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0"

const other_settlement_id: AssetName =
  #"c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1"

const withdrawals_root: ByteArray =
  #"d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1"

const forced_transactions_root: ByteArray =
  #"d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2"

const transactions_root: ByteArray =
  #"d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3"

const deposits_phas_root: ByteArray =
  #"d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4"

const foreign_root: ByteArray =
  #"dfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdfdf"

const resolution_time: Int = 1_000_000

// ---------------------------------------------------------------------------
// Small builders
// ---------------------------------------------------------------------------

fn script_address(hash: ScriptHash) -> Address {
  address.from_script(hash)
}

fn outref(seed: ByteArray, index: Int) -> OutputReference {
  OutputReference { transaction_id: seed, output_index: index }
}

fn nft_value(policy: PolicyId, name: AssetName) -> Value {
  assets.from_lovelace(2_000_000)
    |> assets.merge(assets.from_asset(policy, name, 1))
}

fn settlement_outref() -> OutputReference {
  outref(#"1111111111111111111111111111111111111111111111111111111111111111", 0)
}

fn active_operators_outref() -> OutputReference {
  outref(#"2222222222222222222222222222222222222222222222222222222222222222", 0)
}

fn hub_datum() -> hub.Datum {
  hub.Datum {
    registered_operators: registered_operators_policy,
    active_operators: active_operators_policy,
    retired_operators: retired_operators_policy,
    scheduler: scheduler_policy,
    state_queue: state_queue_policy,
    fraud_proof_catalogue: misc_policy,
    fraud_proof: misc_policy,
    deposit: deposit_policy,
    withdrawal: withdrawal_policy,
    tx_order: tx_order_policy,
    settlement: settlement_policy,
    payout: misc_policy,
    registered_operators_addr: script_address(registered_operators_policy),
    active_operators_addr: script_address(active_operators_policy),
    retired_operators_addr: script_address(retired_operators_policy),
    scheduler_addr: script_address(scheduler_policy),
    state_queue_addr: script_address(state_queue_policy),
    fraud_proof_catalogue_addr: script_address(misc_policy),
    fraud_proof_addr: script_address(misc_policy),
    deposit_addr: script_address(deposit_policy),
    withdrawal_addr: script_address(withdrawal_policy),
    tx_order_addr: script_address(tx_order_policy),
    settlement_addr: script_address(settlement_policy),
    reserve_addr: script_address(misc_policy),
    payout_addr: script_address(misc_policy),
    reserve_observer: misc_policy,
  }
}

fn hub_ref_input() -> Input {
  Input {
    output_reference: outref(
      #"3333333333333333333333333333333333333333333333333333333333333333",
      0,
    ),
    output: Output {
      address: script_address(hub_oracle_hash),
      value: nft_value(hub_oracle_hash, hub.asset_name),
      datum: InlineDatum(hub_datum()),
      reference_script: None,
    },
  }
}

fn scheduler_ref_input(datum: scheduler.SchedDatum) -> Input {
  Input {
    output_reference: outref(
      #"4444444444444444444444444444444444444444444444444444444444444444",
      0,
    ),
    output: Output {
      address: script_address(scheduler_policy),
      value: nft_value(scheduler_policy, scheduler.asset_name),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn settlement_datum(
  claim: Option<settlement.ResolutionClaim>,
) -> settlement.Datum {
  settlement.Datum {
    deposits_root: deposits_counted_root(),
    withdrawals_root,
    forced_transactions_root,
    transactions_root,
    resolution_claim: claim,
  }
}

fn settlement_input(datum: settlement.Datum) -> Input {
  Input {
    output_reference: settlement_outref(),
    output: Output {
      address: script_address(settlement_policy),
      value: nft_value(settlement_policy, settlement_id),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

fn settlement_output(datum: settlement.Datum) -> Output {
  Output {
    address: script_address(settlement_policy),
    value: nft_value(settlement_policy, settlement_id),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

fn active_operators_input() -> Input {
  Input {
    output_reference: active_operators_outref(),
    output: Output {
      address: script_address(active_operators_policy),
      value: nft_value(active_operators_policy, #"6e6f6465"),
      datum: InlineDatum(0),
      reference_script: None,
    },
  }
}

fn bond_hold_redeemer(
  active_operator: VerificationKeyHash,
  claim_time: Int,
) -> Redeemer {
  let redeemer: active_operators.SpendRedeemer = active_operators.UpdateBondHoldNewSettlement {
    active_operator,
    active_node_input_index: 1,
    active_node_output_index: 1,
    hub_oracle_ref_input_index: 0,
    settlement_input_index: 0,
    settlement_redeemer_index: 0,
    resolution_time: claim_time,
  }
  let redeemer_data: Data = redeemer
  redeemer_data
}

// ---------------------------------------------------------------------------
// spend / AttachResolutionClaim
// ---------------------------------------------------------------------------

fn attach_redeemer() -> settlement.SpendRedeemer {
  settlement.AttachResolutionClaim {
    settlement_input_index: 0,
    settlement_output_index: 0,
    hub_ref_input_index: 0,
    active_operators_node_input_index: 1,
    active_operators_redeemer_index: 0,
    operator: operator_vkey,
    scheduler_ref_input_index: 1,
  }
}

fn attach_tx(
  inputs: List<Input>,
  reference_inputs: List<Input>,
  outputs: List<Output>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  extra_signatories: List<VerificationKeyHash>,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs,
    reference_inputs,
    outputs,
    redeemers,
    extra_signatories,
  }
}

fn honest_attach_tx() -> Transaction {
  attach_tx(
    [settlement_input(settlement_datum(None)), active_operators_input()],
    [
      hub_ref_input(),
      scheduler_ref_input(
        scheduler.ActiveOperator { operator: operator_vkey, start_time: 0 },
      ),
    ],
    [
      settlement_output(
        settlement_datum(
          Some(
            settlement.ResolutionClaim {
              resolution_time,
              operator: operator_vkey,
            },
          ),
        ),
      ),
    ],
    [
      Pair(
        Spend(active_operators_outref()),
        bond_hold_redeemer(operator_vkey, resolution_time),
      ),
    ],
    [operator_vkey],
  )
}

fn run_spend(
  m_datum: Option<settlement.Datum>,
  redeemer: settlement.SpendRedeemer,
  own_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  settlement_validator.spend.spend(
    hub_oracle_hash,
    settlement_policy,
    m_datum,
    redeemer,
    own_ref,
    tx,
  )
}

fn run_attach(tx: Transaction) -> Bool {
  expect
      Some(Input { output: Output { datum: InlineDatum(datum_data), .. }, .. })
    = tx.inputs |> list.at(0)
  expect datum: settlement.Datum = datum_data
  run_spend(Some(datum), attach_redeemer(), settlement_outref(), tx)
}

test settlement_handler_spend_attach_accepts_honest_claim() {
  run_attach(honest_attach_tx())
}

test settlement_handler_spend_rejects_missing_datum() fail {
  run_spend(None, attach_redeemer(), settlement_outref(), honest_attach_tx())
}

test settlement_handler_spend_attach_rejects_preexisting_resolution_claim() fail {
  let tx = honest_attach_tx()
  let mutated = Transaction {
    ..tx,
    inputs: [
      settlement_input(
        settlement_datum(
          Some(
            settlement.ResolutionClaim {
              resolution_time,
              operator: operator_vkey,
            },
          ),
        ),
      ),
      active_operators_input(),
    ],
  }
  run_attach(mutated)
}

test settlement_handler_spend_attach_rejects_missing_operator_signature() fail {
  let tx = honest_attach_tx()
  run_attach(Transaction { ..tx, extra_signatories: [] })
}

test settlement_handler_spend_attach_rejects_input_index_not_the_spent_utxo() fail {
  run_spend(
    Some(settlement_datum(None)),
    attach_redeemer(),
    // The spent output reference is not the one at `settlement_input_index`.
    outref(
      #"9999999999999999999999999999999999999999999999999999999999999999",
      0,
    ),
    honest_attach_tx(),
  )
}

test settlement_handler_spend_attach_rejects_output_at_a_foreign_address() fail {
  let tx = honest_attach_tx()
  expect Some(Output { value: v, datum: d, .. }) = tx.outputs |> list.at(0)
  run_attach(
    Transaction {
      ..tx,
      outputs: [
        Output {
          address: script_address(foreign_policy),
          value: v,
          datum: d,
          reference_script: None,
        },
      ],
    },
  )
}

test settlement_handler_spend_attach_rejects_output_without_the_settlement_nft() fail {
  let tx = honest_attach_tx()
  expect Some(Output { address: a, datum: d, .. }) = tx.outputs |> list.at(0)
  run_attach(
    Transaction {
      ..tx,
      outputs: [
        Output {
          address: a,
          value: nft_value(settlement_policy, other_settlement_id),
          datum: d,
          reference_script: None,
        },
      ],
    },
  )
}

test settlement_handler_spend_attach_rejects_output_carrying_a_reference_script() fail {
  let tx = honest_attach_tx()
  expect Some(Output { address: a, value: v, datum: d, .. }) = tx.outputs
    |> list.at(0)
  run_attach(
    Transaction {
      ..tx,
      outputs: [
        Output {
          address: a,
          value: v,
          datum: d,
          reference_script: Some(misc_policy),
        },
      ],
    },
  )
}

test settlement_handler_spend_attach_rejects_unauthentic_hub_oracle_reference_input() fail {
  let tx = honest_attach_tx()
  let hub_input = hub_ref_input()
  let unauthentic = Input {
    ..hub_input,
    output: Output {
      ..hub_input.output,
      value: nft_value(foreign_policy, hub.asset_name),
    },
  }
  expect Some(sched) = tx.reference_inputs |> list.at(1)
  run_attach(Transaction { ..tx, reference_inputs: [unauthentic, sched] })
}

test settlement_handler_spend_attach_rejects_wrong_active_operators_redeemer_constructor() fail {
  let tx = honest_attach_tx()
  let wrong: active_operators.SpendRedeemer = active_operators.ListStateTransition
  let wrong_data: Data = wrong
  run_attach(
    Transaction {
      ..tx,
      redeemers: [Pair(Spend(active_operators_outref()), wrong_data)],
    },
  )
}

test settlement_handler_spend_attach_rejects_active_operators_input_at_a_foreign_address() fail {
  let tx = honest_attach_tx()
  let node = active_operators_input()
  let foreign = Input {
    ..node,
    output: Output { ..node.output, address: script_address(foreign_policy) },
  }
  expect Some(settlement_in) = tx.inputs |> list.at(0)
  run_attach(Transaction { ..tx, inputs: [settlement_in, foreign] })
}

test settlement_handler_spend_attach_rejects_operator_mismatch_with_active_operators_redeemer() fail {
  let tx = honest_attach_tx()
  run_attach(
    Transaction {
      ..tx,
      redeemers: [
        Pair(
          Spend(active_operators_outref()),
          bond_hold_redeemer(other_operator_vkey, resolution_time),
        ),
      ],
    },
  )
}

test settlement_handler_spend_attach_rejects_resolution_time_not_from_active_operators_redeemer() fail {
  let tx = honest_attach_tx()
  run_attach(
    Transaction {
      ..tx,
      outputs: [
        settlement_output(
          settlement_datum(
            Some(
              settlement.ResolutionClaim {
                resolution_time: resolution_time + 1,
                operator: operator_vkey,
              },
            ),
          ),
        ),
      ],
    },
  )
}

test settlement_handler_spend_attach_rejects_mutated_deposits_root_in_continuing_datum() fail {
  let tx = honest_attach_tx()
  let claimed = settlement_datum(
    Some(
      settlement.ResolutionClaim { resolution_time, operator: operator_vkey },
    ),
  )
  run_attach(
    Transaction {
      ..tx,
      outputs: [
        settlement_output(
          settlement.Datum { ..claimed, deposits_root: foreign_root },
        ),
      ],
    },
  )
}

test settlement_handler_spend_attach_rejects_scheduler_without_an_active_operator() fail {
  let tx = honest_attach_tx()
  run_attach(
    Transaction {
      ..tx,
      reference_inputs: [
        hub_ref_input(),
        scheduler_ref_input(scheduler.NoActiveOperators),
      ],
    },
  )
}

test settlement_handler_spend_attach_rejects_operator_that_is_not_the_current_scheduler_operator() fail {
  let tx = honest_attach_tx()
  run_attach(
    Transaction {
      ..tx,
      reference_inputs: [
        hub_ref_input(),
        scheduler_ref_input(
          scheduler.ActiveOperator {
            operator: other_operator_vkey,
            start_time: 0,
          },
        ),
      ],
    },
  )
}

// ---------------------------------------------------------------------------
// spend / DisproveResolutionClaim
// ---------------------------------------------------------------------------

fn deposits_counted_root() -> ByteArray {
  transition_trace.commit_counted_root(
    transition_trace.DepositsRootDomain,
    deposits_phas_root,
    1,
  )
}

fn deposit_id() -> ledger_state.DepositId {
  outref(#"5555555555555555555555555555555555555555555555555555555555555555", 0)
}

fn deposit_info() -> ledger_state.DepositInfo {
  ledger_state.DepositInfo {
    l2_address: script_address(misc_policy),
    l2_network_id: 0,
    l2_datum: None,
  }
}

const unresolved_event_asset_name: AssetName =
  #"e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0"

fn deposit_ref_input(policy: PolicyId) -> Input {
  let event = ledger_state.DepositEvent {
    id: deposit_id(),
    info: deposit_info(),
  }
  Input {
    output_reference: outref(
      #"6666666666666666666666666666666666666666666666666666666666666666",
      0,
    ),
    output: Output {
      address: script_address(policy),
      value: nft_value(policy, unresolved_event_asset_name),
      datum: InlineDatum(event),
      reference_script: None,
    },
  }
}

fn deposit_key_bytes() -> ByteArray {
  cbor.serialise(deposit_id())
}

fn deposit_value_bytes() -> ByteArray {
  cbor.serialise(deposit_info())
}

fn empty_proof() -> Proof {
  []
}

fn deposit_witness(
  root: ByteArray,
  phas_root: ByteArray,
) -> transition_trace.RootMembershipProof<ByteArray, ByteArray> {
  transition_trace.RootMembershipProof {
    domain: transition_trace.DepositsRootDomain,
    root,
    phas_root,
    count: 1,
    key: deposit_key_bytes(),
    value: deposit_value_bytes(),
    proof: empty_proof(),
  }
}

fn phas_withdraw_redeemer(
  phas_root: ByteArray,
  key_bytes: ByteArray,
  value_bytes: ByteArray,
) -> Redeemer {
  let proof_data: Data = empty_proof()
  builtin.list_data(
    [
      builtin.b_data(phas_root),
      builtin.b_data(key_bytes),
      builtin.b_data(value_bytes),
      proof_data,
    ],
  )
}

fn slashing_arguments(
  slashed_operator: VerificationKeyHash,
  reason: operator_directory.SlashingReason,
) -> operator_directory.SlashingArguments {
  operator_directory.SlashingArguments {
    slashed_operator,
    hub_oracle_ref_input_index: 0,
    slashed_operator_anchor_element_input_outref: active_operators_outref(),
    slashed_operator_anchor_element_output_index: 1,
    slashing_reason: reason,
  }
}

fn bad_settlement_reason() -> operator_directory.SlashingReason {
  operator_directory.SlashOperatorForBadSettlement {
    settlement_input_index: 0,
    settlement_redeemer_index: 0,
  }
}

fn active_slash_redeemer(
  slashed_operator: VerificationKeyHash,
  reason: operator_directory.SlashingReason,
) -> Redeemer {
  let redeemer: active_operators.MintRedeemer = active_operators.SlashOperator {
    slashing_arguments: slashing_arguments(slashed_operator, reason),
    operator_removal_scheduler_sync: active_operators.ShowOperatorIsInactive {
      scheduler_ref_input_index: 1,
    },
  }
  let redeemer_data: Data = redeemer
  redeemer_data
}

fn retired_slash_redeemer(
  slashed_operator: VerificationKeyHash,
  reason: operator_directory.SlashingReason,
) -> Redeemer {
  let redeemer: retired_operators.MintRedeemer = retired_operators.SlashOperator {
    slashing_arguments: slashing_arguments(slashed_operator, reason),
  }
  let redeemer_data: Data = redeemer
  redeemer_data
}

fn disprove_redeemer(
  operator_is_active: Bool,
  witness_root: ByteArray,
  phas_root: ByteArray,
) -> settlement.SpendRedeemer {
  settlement.DisproveResolutionClaim {
    settlement_input_index: 0,
    settlement_output_index: 0,
    hub_ref_input_index: 0,
    operators_redeemer_index: 0,
    operator: operator_vkey,
    operator_is_active,
    unresolved_event_ref_input_index: 1,
    unresolved_event_asset_name,
    event_type: settlement.Deposit,
    membership_proof: settlement.DepositMembership {
      witness: deposit_witness(witness_root, phas_root),
    },
    inclusion_proof_script_withdraw_redeemer_index: 1,
  }
}

fn disprove_tx(
  operators_redeemer: Redeemer,
  event_policy: PolicyId,
  phas_redeemer: Redeemer,
  claim: Option<settlement.ResolutionClaim>,
  output_datum: settlement.Datum,
  upper_bound: Int,
  operators_purpose: ScriptPurpose,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [settlement_input(settlement_datum(claim))],
    reference_inputs: [hub_ref_input(), deposit_ref_input(event_policy)],
    outputs: [settlement_output(output_datum)],
    redeemers: [
      Pair(operators_purpose, operators_redeemer),
      Pair(Withdraw(Script(env.plutarch_phas_validator_hash)), phas_redeemer),
    ],
    validity_range: interval.before(upper_bound),
  }
}

fn honest_disprove_tx() -> Transaction {
  disprove_tx(
    active_slash_redeemer(operator_vkey, bad_settlement_reason()),
    deposit_policy,
    phas_withdraw_redeemer(
      deposits_phas_root,
      deposit_key_bytes(),
      deposit_value_bytes(),
    ),
    Some(
      settlement.ResolutionClaim { resolution_time, operator: operator_vkey },
    ),
    settlement_datum(None),
    resolution_time - 1,
    Mint(active_operators_policy),
  )
}

fn run_disprove(tx: Transaction, operator_is_active: Bool) -> Bool {
  expect
      Some(Input { output: Output { datum: InlineDatum(datum_data), .. }, .. })
    = tx.inputs |> list.at(0)
  expect datum: settlement.Datum = datum_data
  run_spend(
    Some(datum),
    disprove_redeemer(
      operator_is_active,
      deposits_counted_root(),
      deposits_phas_root,
    ),
    settlement_outref(),
    tx,
  )
}

test settlement_handler_spend_disprove_accepts_honest_active_operator_disproof() {
  run_disprove(honest_disprove_tx(), True)
}

test settlement_handler_spend_disprove_accepts_honest_retired_operator_disproof() {
  let tx = honest_disprove_tx()
  run_disprove(
    Transaction {
      ..tx,
      redeemers: [
        Pair(
          Mint(retired_operators_policy),
          retired_slash_redeemer(operator_vkey, bad_settlement_reason()),
        ),
        Pair(
          Withdraw(Script(env.plutarch_phas_validator_hash)),
          phas_withdraw_redeemer(
            deposits_phas_root,
            deposit_key_bytes(),
            deposit_value_bytes(),
          ),
        ),
      ],
    },
    False,
  )
}

test settlement_handler_spend_disprove_rejects_settlement_without_a_resolution_claim() fail {
  let tx = honest_disprove_tx()
  run_disprove(
    Transaction { ..tx, inputs: [settlement_input(settlement_datum(None))] },
    True,
  )
}

test settlement_handler_spend_disprove_rejects_operator_mismatch_with_the_claim() fail {
  let tx = honest_disprove_tx()
  run_disprove(
    Transaction {
      ..tx,
      inputs: [
        settlement_input(
          settlement_datum(
            Some(
              settlement.ResolutionClaim {
                resolution_time,
                operator: other_operator_vkey,
              },
            ),
          ),
        ),
      ],
    },
    True,
  )
}

test settlement_handler_spend_disprove_rejects_input_index_not_the_spent_utxo() fail {
  let tx = honest_disprove_tx()
  run_spend(
    Some(
      settlement_datum(
        Some(
          settlement.ResolutionClaim {
            resolution_time,
            operator: operator_vkey,
          },
        ),
      ),
    ),
    disprove_redeemer(True, deposits_counted_root(), deposits_phas_root),
    outref(
      #"9999999999999999999999999999999999999999999999999999999999999999",
      0,
    ),
    tx,
  )
}

test settlement_handler_spend_disprove_rejects_continuing_datum_that_keeps_the_claim() fail {
  let tx = honest_disprove_tx()
  run_disprove(
    Transaction {
      ..tx,
      outputs: [
        settlement_output(
          settlement_datum(
            Some(
              settlement.ResolutionClaim {
                resolution_time,
                operator: operator_vkey,
              },
            ),
          ),
        ),
      ],
    },
    True,
  )
}

test settlement_handler_spend_disprove_rejects_unauthentic_hub_oracle_reference_input() fail {
  let tx = honest_disprove_tx()
  let hub_input = hub_ref_input()
  run_disprove(
    Transaction {
      ..tx,
      reference_inputs: [
        Input {
          ..hub_input,
          output: Output {
            ..hub_input.output,
            value: nft_value(foreign_policy, hub.asset_name),
          },
        },
        deposit_ref_input(deposit_policy),
      ],
    },
    True,
  )
}

test settlement_handler_spend_disprove_rejects_event_reference_input_of_the_wrong_script() fail {
  let tx = honest_disprove_tx()
  run_disprove(
    Transaction {
      ..tx,
      reference_inputs: [hub_ref_input(), deposit_ref_input(withdrawal_policy)],
    },
    True,
  )
}

test settlement_handler_spend_disprove_rejects_membership_witness_for_a_foreign_root() fail {
  let tx = honest_disprove_tx()
  expect
      Some(Input { output: Output { datum: InlineDatum(datum_data), .. }, .. })
    = tx.inputs |> list.at(0)
  expect datum: settlement.Datum = datum_data
  run_spend(
    Some(datum),
    disprove_redeemer(True, foreign_root, deposits_phas_root),
    settlement_outref(),
    tx,
  )
}

test settlement_handler_spend_disprove_rejects_phas_withdraw_redeemer_that_does_not_match() fail {
  let tx = honest_disprove_tx()
  run_disprove(
    Transaction {
      ..tx,
      redeemers: [
        Pair(
          Mint(active_operators_policy),
          active_slash_redeemer(operator_vkey, bad_settlement_reason()),
        ),
        Pair(
          Withdraw(Script(env.plutarch_phas_validator_hash)),
          phas_withdraw_redeemer(
            foreign_root,
            deposit_key_bytes(),
            deposit_value_bytes(),
          ),
        ),
      ],
    },
    True,
  )
}

test settlement_handler_spend_disprove_rejects_upper_bound_at_or_after_the_resolution_time() fail {
  let tx = honest_disprove_tx()
  run_disprove(
    Transaction { ..tx, validity_range: interval.before(resolution_time) },
    True,
  )
}

test settlement_handler_spend_disprove_rejects_slashing_redeemer_for_another_operator() fail {
  let tx = honest_disprove_tx()
  run_disprove(
    Transaction {
      ..tx,
      redeemers: [
        Pair(
          Mint(active_operators_policy),
          active_slash_redeemer(other_operator_vkey, bad_settlement_reason()),
        ),
        Pair(
          Withdraw(Script(env.plutarch_phas_validator_hash)),
          phas_withdraw_redeemer(
            deposits_phas_root,
            deposit_key_bytes(),
            deposit_value_bytes(),
          ),
        ),
      ],
    },
    True,
  )
}

test settlement_handler_spend_disprove_rejects_retired_slashing_redeemer_for_another_operator() fail {
  let tx = honest_disprove_tx()
  run_disprove(
    Transaction {
      ..tx,
      redeemers: [
        Pair(
          Mint(retired_operators_policy),
          retired_slash_redeemer(other_operator_vkey, bad_settlement_reason()),
        ),
        Pair(
          Withdraw(Script(env.plutarch_phas_validator_hash)),
          phas_withdraw_redeemer(
            deposits_phas_root,
            deposit_key_bytes(),
            deposit_value_bytes(),
          ),
        ),
      ],
    },
    False,
  )
}

test settlement_handler_spend_disprove_rejects_slashing_for_a_reason_other_than_settlement() fail {
  let tx = honest_disprove_tx()
  run_disprove(
    Transaction {
      ..tx,
      redeemers: [
        Pair(
          Mint(active_operators_policy),
          active_slash_redeemer(
            operator_vkey,
            operator_directory.SlashOperatorForBadState {
              state_queue_redeemer_index: 0,
            },
          ),
        ),
        Pair(
          Withdraw(Script(env.plutarch_phas_validator_hash)),
          phas_withdraw_redeemer(
            deposits_phas_root,
            deposit_key_bytes(),
            deposit_value_bytes(),
          ),
        ),
      ],
    },
    True,
  )
}

// ---------------------------------------------------------------------------
// spend / Resolve
// ---------------------------------------------------------------------------

fn resolve_tx(mint_value: Value) -> Transaction {
  Transaction { ..transaction.placeholder, mint: mint_value }
}

test settlement_handler_spend_resolve_accepts_exact_burn() {
  run_spend(
    Some(settlement_datum(None)),
    settlement.Resolve { settlement_id },
    settlement_outref(),
    resolve_tx(assets.from_asset(settlement_policy, settlement_id, -1)),
  )
}

test settlement_handler_spend_resolve_rejects_absent_burn() fail {
  run_spend(
    Some(settlement_datum(None)),
    settlement.Resolve { settlement_id },
    settlement_outref(),
    resolve_tx(assets.zero),
  )
}

test settlement_handler_spend_resolve_rejects_burn_of_a_different_settlement_id() fail {
  run_spend(
    Some(settlement_datum(None)),
    settlement.Resolve { settlement_id },
    settlement_outref(),
    resolve_tx(assets.from_asset(settlement_policy, other_settlement_id, -1)),
  )
}

test settlement_handler_spend_resolve_rejects_burn_of_the_wrong_quantity() fail {
  run_spend(
    Some(settlement_datum(None)),
    settlement.Resolve { settlement_id },
    settlement_outref(),
    resolve_tx(assets.from_asset(settlement_policy, settlement_id, -2)),
  )
}

// ---------------------------------------------------------------------------
// mint / Spawn
// ---------------------------------------------------------------------------

fn merge_redeemer(header_node_key: ByteArray, m_index: Option<Int>) -> Redeemer {
  let redeemer: state_queue.MintRedeemer = state_queue.MergeToConfirmedStateV1 {
    header_node_key,
    confirmed_state_input_outref: outref(
      #"7777777777777777777777777777777777777777777777777777777777777777",
      0,
    ),
    confirmed_state_output_index: 1,
    m_settlement_redeemer_index: m_index,
    merged_block_withdrawals_root: withdrawals_root,
    merged_block_forced_transactions_root: forced_transactions_root,
    merged_block_transactions_root: transactions_root,
    merged_block_deposits_root: deposits_counted_root(),
    merged_block_transition_trace_root: foreign_root,
    merged_block_event_to_step_root: foreign_root,
    merged_block_validation_traces_root: foreign_root,
    merged_block_withdrawal_count: 1,
    merged_block_forced_transaction_count: 1,
    merged_block_l2_transaction_count: 1,
    merged_block_deposit_count: 1,
    merged_block_total_event_count: 4,
    merged_block_transition_step_count: 4,
    merged_block_validation_trace_count: 2,
  }
  let redeemer_data: Data = redeemer
  redeemer_data
}

fn spawn_redeemer() -> Data {
  let redeemer: settlement.MintRedeemer = settlement.Spawn {
    settlement_id,
    output_index: 0,
    state_queue_merge_redeemer_index: 0,
    hub_ref_input_index: 0,
  }
  let redeemer_data: Data = redeemer
  redeemer_data
}

fn spawn_tx(
  reference_inputs: List<Input>,
  outputs: List<Output>,
  mint_value: Value,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    reference_inputs,
    outputs,
    mint: mint_value,
    redeemers,
  }
}

fn honest_spawn_tx() -> Transaction {
  spawn_tx(
    [hub_ref_input()],
    [settlement_output(settlement_datum(None))],
    assets.from_asset(settlement_policy, settlement_id, 1),
    [Pair(Mint(state_queue_policy), merge_redeemer(settlement_id, Some(1)))],
  )
}

fn run_mint(redeemer: Data, tx: Transaction) -> Bool {
  settlement_validator.mint.mint(
    hub_oracle_hash,
    redeemer,
    settlement_policy,
    tx,
  )
}

test settlement_handler_mint_spawn_accepts_honest_merge() {
  run_mint(spawn_redeemer(), honest_spawn_tx())
}

test settlement_handler_mint_spawn_rejects_unauthentic_hub_oracle_reference_input() fail {
  let tx = honest_spawn_tx()
  let hub_input = hub_ref_input()
  run_mint(
    spawn_redeemer(),
    Transaction {
      ..tx,
      reference_inputs: [
        Input {
          ..hub_input,
          output: Output {
            ..hub_input.output,
            value: nft_value(foreign_policy, hub.asset_name),
          },
        },
      ],
    },
  )
}

test settlement_handler_mint_spawn_rejects_merge_redeemer_at_a_foreign_policy() fail {
  let tx = honest_spawn_tx()
  run_mint(
    spawn_redeemer(),
    Transaction {
      ..tx,
      redeemers: [
        Pair(Mint(foreign_policy), merge_redeemer(settlement_id, Some(1))),
      ],
    },
  )
}

test settlement_handler_mint_spawn_rejects_non_merge_state_queue_redeemer() fail {
  let tx = honest_spawn_tx()
  let not_merge: state_queue.MintRedeemer = state_queue.InitV1 {
    output_index: 0,
  }
  let not_merge_data: Data = not_merge
  run_mint(
    spawn_redeemer(),
    Transaction {
      ..tx,
      redeemers: [Pair(Mint(state_queue_policy), not_merge_data)],
    },
  )
}

test settlement_handler_mint_spawn_rejects_merge_redeemer_of_the_wrong_arity() fail {
  let tx = honest_spawn_tx()
  // Constructor tag 4 (`MergeToConfirmedStateV1`) with 17 fields instead of 18.
  let short_merge = builtin.constr_data(4, list.repeat(builtin.i_data(0), 17))
  run_mint(
    spawn_redeemer(),
    Transaction {
      ..tx,
      redeemers: [Pair(Mint(state_queue_policy), short_merge)],
    },
  )
}

test settlement_handler_mint_spawn_rejects_settlement_id_that_is_not_the_merged_header_hash() fail {
  let tx = honest_spawn_tx()
  run_mint(
    spawn_redeemer(),
    Transaction {
      ..tx,
      redeemers: [
        Pair(
          Mint(state_queue_policy),
          merge_redeemer(other_settlement_id, Some(1)),
        ),
      ],
    },
  )
}

test settlement_handler_mint_spawn_rejects_merge_without_a_settlement_redeemer_index() fail {
  let tx = honest_spawn_tx()
  run_mint(
    spawn_redeemer(),
    Transaction {
      ..tx,
      redeemers: [
        Pair(Mint(state_queue_policy), merge_redeemer(settlement_id, None)),
      ],
    },
  )
}

test settlement_handler_mint_spawn_rejects_output_at_a_foreign_address() fail {
  let tx = honest_spawn_tx()
  run_mint(
    spawn_redeemer(),
    Transaction {
      ..tx,
      outputs: [
        Output {
          address: script_address(foreign_policy),
          value: nft_value(settlement_policy, settlement_id),
          datum: InlineDatum(settlement_datum(None)),
          reference_script: None,
        },
      ],
    },
  )
}

test settlement_handler_mint_spawn_rejects_output_without_the_settlement_nft() fail {
  let tx = honest_spawn_tx()
  run_mint(
    spawn_redeemer(),
    Transaction {
      ..tx,
      outputs: [
        Output {
          address: script_address(settlement_policy),
          value: nft_value(settlement_policy, other_settlement_id),
          datum: InlineDatum(settlement_datum(None)),
          reference_script: None,
        },
      ],
    },
  )
}

test settlement_handler_mint_spawn_rejects_output_carrying_a_reference_script() fail {
  let tx = honest_spawn_tx()
  run_mint(
    spawn_redeemer(),
    Transaction {
      ..tx,
      outputs: [
        Output {
          address: script_address(settlement_policy),
          value: nft_value(settlement_policy, settlement_id),
          datum: InlineDatum(settlement_datum(None)),
          reference_script: Some(misc_policy),
        },
      ],
    },
  )
}

test settlement_handler_mint_spawn_rejects_deposits_root_not_from_the_merged_block() fail {
  let tx = honest_spawn_tx()
  run_mint(
    spawn_redeemer(),
    Transaction {
      ..tx,
      outputs: [
        settlement_output(
          settlement.Datum {
            ..settlement_datum(None),
            deposits_root: foreign_root,
          },
        ),
      ],
    },
  )
}

test settlement_handler_mint_spawn_rejects_produced_datum_with_a_resolution_claim() fail {
  let tx = honest_spawn_tx()
  run_mint(
    spawn_redeemer(),
    Transaction {
      ..tx,
      outputs: [
        settlement_output(
          settlement_datum(
            Some(
              settlement.ResolutionClaim {
                resolution_time,
                operator: operator_vkey,
              },
            ),
          ),
        ),
      ],
    },
  )
}

test settlement_handler_mint_spawn_rejects_a_second_token_under_the_settlement_policy() fail {
  let tx = honest_spawn_tx()
  run_mint(
    spawn_redeemer(),
    Transaction {
      ..tx,
      mint: assets.from_asset(settlement_policy, settlement_id, 1)
        |> assets.merge(
            assets.from_asset(settlement_policy, other_settlement_id, 1),
          ),
    },
  )
}

test settlement_handler_mint_spawn_rejects_a_mint_quantity_other_than_one() fail {
  let tx = honest_spawn_tx()
  run_mint(
    spawn_redeemer(),
    Transaction {
      ..tx,
      mint: assets.from_asset(settlement_policy, settlement_id, 2),
    },
  )
}

// ---------------------------------------------------------------------------
// mint / Remove
// ---------------------------------------------------------------------------

fn remove_redeemer(id: AssetName) -> Data {
  let redeemer: settlement.MintRedeemer = settlement.Remove {
    settlement_id: id,
    input_index: 0,
    spend_redeemer_index: 0,
  }
  let redeemer_data: Data = redeemer
  redeemer_data
}

fn resolve_spend_redeemer(id: AssetName) -> Redeemer {
  let redeemer: settlement.SpendRedeemer = settlement.Resolve {
    settlement_id: id,
  }
  let redeemer_data: Data = redeemer
  redeemer_data
}

fn remove_tx(
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  extra_signatories: List<VerificationKeyHash>,
  lower_bound: Int,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs,
    redeemers,
    extra_signatories,
    validity_range: interval.after(lower_bound),
  }
}

fn resolved_settlement_input() -> Input {
  settlement_input(
    settlement_datum(
      Some(
        settlement.ResolutionClaim { resolution_time, operator: operator_vkey },
      ),
    ),
  )
}

fn honest_remove_tx() -> Transaction {
  remove_tx(
    [resolved_settlement_input()],
    [Pair(Spend(settlement_outref()), resolve_spend_redeemer(settlement_id))],
    [operator_vkey],
    resolution_time,
  )
}

test settlement_handler_mint_remove_accepts_resolved_settlement() {
  run_mint(remove_redeemer(settlement_id), honest_remove_tx())
}

test settlement_handler_mint_remove_rejects_input_without_the_settlement_nft() fail {
  let tx = honest_remove_tx()
  let input = resolved_settlement_input()
  run_mint(
    remove_redeemer(settlement_id),
    Transaction {
      ..tx,
      inputs: [
        Input {
          ..input,
          output: Output {
            ..input.output,
            value: nft_value(settlement_policy, other_settlement_id),
          },
        },
      ],
    },
  )
}

test settlement_handler_mint_remove_rejects_spend_redeemer_that_is_not_resolve() fail {
  let tx = honest_remove_tx()
  let not_resolve: settlement.SpendRedeemer = attach_redeemer()
  let not_resolve_data: Data = not_resolve
  run_mint(
    remove_redeemer(settlement_id),
    Transaction {
      ..tx,
      redeemers: [Pair(Spend(settlement_outref()), not_resolve_data)],
    },
  )
}

test settlement_handler_mint_remove_rejects_spend_redeemer_at_a_foreign_purpose() fail {
  let tx = honest_remove_tx()
  run_mint(
    remove_redeemer(settlement_id),
    Transaction {
      ..tx,
      redeemers: [
        Pair(
          Spend(active_operators_outref()),
          resolve_spend_redeemer(settlement_id),
        ),
      ],
    },
  )
}

test settlement_handler_mint_remove_rejects_settlement_id_mismatch_across_redeemers() fail {
  let tx = honest_remove_tx()
  run_mint(
    remove_redeemer(settlement_id),
    Transaction {
      ..tx,
      redeemers: [
        Pair(
          Spend(settlement_outref()),
          resolve_spend_redeemer(other_settlement_id),
        ),
      ],
    },
  )
}

test settlement_handler_mint_remove_rejects_settlement_without_a_resolution_claim() fail {
  let tx = honest_remove_tx()
  run_mint(
    remove_redeemer(settlement_id),
    Transaction { ..tx, inputs: [settlement_input(settlement_datum(None))] },
  )
}

test settlement_handler_mint_remove_rejects_missing_operator_signature() fail {
  let tx = honest_remove_tx()
  run_mint(
    remove_redeemer(settlement_id),
    Transaction { ..tx, extra_signatories: [] },
  )
}

test settlement_handler_mint_remove_rejects_lower_bound_before_the_resolution_time() fail {
  let tx = honest_remove_tx()
  run_mint(
    remove_redeemer(settlement_id),
    Transaction { ..tx, validity_range: interval.after(resolution_time - 1) },
  )
}
