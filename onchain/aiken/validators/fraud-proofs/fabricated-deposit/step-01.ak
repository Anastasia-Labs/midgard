//// ## Fabricated Deposit Fraud Proof — step-01 (`GOAL_SPEC.md` §9.1 / Q39)
////
//// Authenticates the challenged block header out of the state-queue node the
//// computation thread names, and proves that the header's counted
//// `deposits_root`/`deposit_count` pair really does commit the
//// `(DepositId, DepositInfo)` leaf the rest of the family refutes.
////
//// The membership machinery is the transition-trace family's, not a second one:
//// `midgard/fraud_proofs/fabricated_deposit/step_01`'s
//// `verify_committed_deposit_membership_v1` is a direct call into
//// `transition_trace.verify_root_membership_with_bytes` under
//// `DepositsRootDomain`, the same call `transition_trace/proof`'s
//// `verify_source_membership` makes for a committed deposit and the same domain
//// `validators/user-events/deposit.ak` keys on the settlement side.

use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/fabricated_deposit/step_01.{
  Args, Datum, SpendRedeemer, committed_deposit_info_hash_v1,
  verify_challenged_header_v1, verify_committed_deposit_membership_v1,
}
use midgard/fraud_proofs/fabricated_deposit/step_02
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/ledger_state
use midgard/transition_trace

validator main(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        hub_ref_input_index,
        state_queue_node_ref_input_index,
        committed_deposit,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced.
        let
            _own_script_hash,
            computation_thread_token_asset_name,
            _fraud_prover,
            _m_input_state_data,
            output_script_hash,
            output_state_data
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )

        // 2. The thread must belong to this family's catalogue category, and the
        //    header must be the authentic state-queue block the thread token
        //    names.
        let header, header_hash, _hub_datum <- verify_challenged_header_v1(
          computation_thread_token_asset_name,
          hub_oracle,
          hub_ref_input_index,
          state_queue_node_ref_input_index,
          reference_inputs,
        )

        // 3. The challenged header must genuinely commit the deposit leaf under
        //    dispute. Without this the family would adjudicate a leaf no block
        //    ever claimed.
        expect verify_committed_deposit_membership_v1(header, committed_deposit)

        // 4. The committed identity, the committed content's commitment and the
        //    header's inclusion window must be passed to the next step. The
        //    window travels because step-04 has to know whether the authentic
        //    event was due for *this* block before it convicts.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          challenged_header_hash: header_hash,
          header_start_time: header.start_time,
          header_end_time: header.end_time,
          committed_deposit_id: committed_deposit.key,
          committed_deposit_info_hash: committed_deposit_info_hash_v1(
            committed_deposit,
          ),
        }
        expect output_state_data == expected_output_state

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)
//
// Every selector drives the real `main.spend` handler above on a genuine block
// fixture built by `native_binding_fixture_v1`, extended with a real
// single-leaf deposits MPF and the counted `deposits_root` it reconstructs.

fn run_step_01(
  block: step_01.FabricatedDepositBlockV1,
  membership: step_01.CommittedDepositSourceProofV1,
  thread_asset_name: AssetName,
) -> Bool {
  let expected_output_state: Data = step_02.State {
    challenged_header_hash: block.block.header_hash,
    header_start_time: block.block.header.start_time,
    header_end_time: block.block.header.end_time,
    committed_deposit_id: membership.key,
    committed_deposit_info_hash: committed_deposit_info_hash_v1(membership),
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        hub_ref_input_index: 0,
        state_queue_node_ref_input_index: 1,
        committed_deposit: membership,
      },
    ),
    fixture.own_out_ref(),
    step_01.continue_step_l1_tx_v1(
      thread_asset_name,
      expected_output_state,
      step_01.block_reference_inputs_v1(block),
    ),
  )
}

/// Positive selector: the challenged header's counted `deposits_root` really
/// commits the disputed `(DepositId, DepositInfo)` leaf, so step-01 forwards the
/// committed identity, a 32-byte commitment to the committed content, and the
/// header's inclusion window.
test accepts_committed_deposit_membership() {
  let block = step_01.authentic_deposit_block_v1()
  run_step_01(
    block,
    step_01.committed_deposit_membership_v1(block),
    step_01.block_thread_asset_name_v1(block),
  )
}

/// Valid-block negative selector: a leaf the challenged header does not commit
/// cannot enter the family. Fabricating a deposit against a valid block means
/// exhibiting a deposits MPF that is not the one the header committed — here a
/// tree over a different `DepositId`, carried with its own counted root — and
/// the counted-root equality against `header.deposits_root` refuses it.
test rejects_forged_deposits_root() fail {
  let block = step_01.authentic_deposit_block_v1()
  let forged = step_01.fabricated_deposit_block_v1(
    step_01.foreign_deposit_id_v1(),
    step_01.authentic_deposit_info_v1(),
  )
  run_step_01(
    block,
    step_01.committed_deposit_membership_v1(forged),
    step_01.block_thread_asset_name_v1(block),
  )
}

/// The witness's restated `count` must be the header's `deposit_count`. The
/// counted-root wrapper is what makes this load-bearing in both directions: the
/// count is hashed into `deposits_root`, so a witness cannot keep the raw MPF
/// root and quietly change the cardinality the block committed to.
test rejects_wrong_deposit_count() fail {
  let block = step_01.authentic_deposit_block_v1()
  let real = step_01.committed_deposit_membership_v1(block)
  run_step_01(
    block,
    transition_trace.RootMembershipProof { ..real, count: real.count + 1 },
    step_01.block_thread_asset_name_v1(block),
  )
}

/// `transition_trace`'s catalogue category id: index 4 of the append-only
/// catalogue order. Restated here because
/// `midgard/fraud_proofs/transition_trace/proof` keeps its own copy private.
const transition_trace_category_id: ByteArray = #"00000004"

fn foreign_category_thread_asset_name(
  challenged_header_hash: ledger_state.HeaderHash,
) -> AssetName {
  transition_trace_category_id
    |> builtin.append_bytearray(challenged_header_hash)
}

/// A thread minted under another family's catalogue category cannot be run
/// here: its fraud-proof token would be filed under that other category's rule.
/// The substituted prefix is `transition_trace`'s own `#"00000004"`, the family
/// whose deposit arm this one completes.
test rejects_wrong_category() fail {
  let block = step_01.authentic_deposit_block_v1()
  run_step_01(
    block,
    step_01.committed_deposit_membership_v1(block),
    foreign_category_thread_asset_name(block.block.header_hash),
  )
}
