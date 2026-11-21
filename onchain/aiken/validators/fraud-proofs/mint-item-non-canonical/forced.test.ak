//// Validator-level cases for a forced leaf naming this family's reason,
//// `MintItemMalformed`. No machine step emits that reason, so step 01 hands a
//// forced rejection carrying it to the terminal as an unreachable claim, and
//// step 04 contradicts the rejection.

use aiken/builtin
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use cardano/transaction.{OutputReference}
use fraud_proofs/mint_item_non_canonical/step_01 as step_01_validator
use fraud_proofs/mint_item_non_canonical/step_04 as step_04_validator
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/mint_item_non_canonical/rule
use midgard/fraud_proofs/mint_item_non_canonical/step_01.{
  Args as Step01Args, ForcedSource,
}
use midgard/fraud_proofs/mint_item_non_canonical/step_04.{Args as Step04Args}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/rejection_reason_v1.{
  ForcedTxInvalid, ForcedTxValid, MintDeclaredAssetLimit, MintItemMalformed,
  OperatorVerdictV1,
}
use midgard/transition_trace

const forced_order_key_transaction_id =
  #"7777777777777777777777777777777777777777777777777777777777777777"

fn forced_leaf(verdict: OperatorVerdictV1) -> ledger_state.ForcedInclusionTxV1 {
  let tx = fixture.valid_native_tx_v1()
  ledger_state.ForcedInclusionTxV1 {
    tx_id: fixture.native_block_fixture_v1(tx).tx_id,
    submitted_source: forced_fixture.from_native_transaction(tx),
    verdict,
  }
}

fn forced_membership(
  leaf: ledger_state.ForcedInclusionTxV1,
) -> transition_trace.ForcedLeafMembershipV1<
  ledger_state.TxOrderId,
  ledger_state.ForcedInclusionTxV1,
> {
  let key = OutputReference {
    transaction_id: forced_order_key_transaction_id,
    output_index: 0,
  }
  let phas_root = mpf.root(
    mpf.insert(
      mpf.empty,
      builtin.serialise_data(key),
      builtin.serialise_data(leaf),
      [],
    ),
  )
  transition_trace.ForcedLeafMembershipV1 {
    phas_root,
    key,
    value: leaf,
    proof: [],
  }
}

fn forced_header(phas_root: ByteArray) -> ledger_state.HeaderV1 {
  let base = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  ledger_state.HeaderV1 {
    ..base.header,
    forced_transactions_root: transition_trace.commit_counted_root(
      transition_trace.ForcedTransactionsV1RootDomain,
      phas_root,
      1,
    ),
    forced_transaction_count: 1,
  }
}

/// The subject the substrate opens for a forced leaf carrying `verdict`.
fn opened_subject(verdict: OperatorVerdictV1) -> substrate.VerdictSubjectV1 {
  let leaf = forced_leaf(verdict)
  let membership = forced_membership(leaf)
  let header = forced_header(membership.phas_root)
  let Pair(subject, _) = substrate.open_forced_subject_v1(
    thread_name(header),
    header,
    membership,
  )
  subject
}

fn thread_name(header: ledger_state.HeaderV1) -> ByteArray {
  let header_data: Data = header
  bytearray.concat(
    fixture.fraud_category_id,
    utils.serialise_and_hash_28(header_data),
  )
}

/// Step 01 over a forced leaf carrying `verdict`, writing `written` to step
/// 04 when `to_terminal` and to step 02 otherwise.
fn run_forced_step_01(
  verdict: OperatorVerdictV1,
  written: Data,
  to_terminal: Bool,
) -> Bool {
  let leaf = forced_leaf(verdict)
  let membership = forced_membership(leaf)
  let header = forced_header(membership.phas_root)
  let Pair(step_02_hash, step_04_hash) = if to_terminal {
    Pair(other_successor_hash, fixture.next_step_script_hash)
  } else {
    Pair(fixture.next_step_script_hash, other_successor_hash)
  }
  step_01_validator.main.spend(
    step_02_hash,
    step_04_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Step01Args {
        source: ForcedSource {
          input_index: 0,
          output_index: 0,
          header,
          membership,
        },
        item_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(thread_name(header), written),
  )
}

/// The step-01 successor a case does not hand over to.
const other_successor_hash =
  #"7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c7c"

fn malformed() -> OperatorVerdictV1 {
  ForcedTxInvalid { reason: MintItemMalformed { policy_index: 0 } }
}

fn unreachable(verdict: OperatorVerdictV1) -> Data {
  let state: Data = rule.unreachable_claim_state_v1(opened_subject(verdict))
  state
}

test step_01_hands_a_forced_mint_item_malformed_rejection_to_the_terminal() {
  run_forced_step_01(malformed(), unreachable(malformed()), True)
}

test step_01_hands_a_negative_mint_item_malformed_claim_to_the_terminal() {
  let verdict = ForcedTxInvalid {
    reason: MintItemMalformed { policy_index: -1 },
  }
  run_forced_step_01(verdict, unreachable(verdict), True)
}

test step_01_refuses_the_item_scan_for_a_forced_mint_item_malformed_rejection() fail {
  let subject = opened_subject(malformed())
  let bound: Data = rule.BoundItemV1 {
    subject,
    item_index: 0,
    field_cursor: None,
  }
  run_forced_step_01(malformed(), bound, False)
}

test step_01_refuses_the_terminal_for_another_forced_reason() fail {
  let verdict = ForcedTxInvalid {
    reason: MintDeclaredAssetLimit { policy_index: 0 },
  }
  run_forced_step_01(verdict, unreachable(verdict), True)
}

test step_01_refuses_the_terminal_for_an_accepted_forced_leaf() fail {
  run_forced_step_01(ForcedTxValid, unreachable(ForcedTxValid), True)
}

test step_01_binds_an_accepted_forced_leaf_for_the_item_scan() {
  let subject = opened_subject(ForcedTxValid)
  let bound: Data = rule.BoundItemV1 {
    subject,
    item_index: 0,
    field_cursor: None,
  }
  run_forced_step_01(ForcedTxValid, bound, False)
}

fn run_step_04(verdict: OperatorVerdictV1, state: rule.MintScanStateV1) -> Bool {
  let leaf = forced_leaf(verdict)
  let header = forced_header(forced_membership(leaf).phas_root)
  step_04_validator.main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      Step04Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(thread_name(header)),
  )
}

test step_04_convicts_a_forced_mint_item_malformed_rejection() {
  run_step_04(
    malformed(),
    rule.unreachable_claim_state_v1(opened_subject(malformed())),
  )
}

test step_04_refuses_an_unreachable_claim_state_over_an_acceptance() fail {
  run_step_04(
    ForcedTxValid,
    rule.unreachable_claim_state_v1(opened_subject(ForcedTxValid)),
  )
}
