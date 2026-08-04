use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  OutputReference, Redeemer, ScriptPurpose, Transaction, Withdraw,
}
use env
use midgard/common/types.{Proof}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput} as native_tx_types
use midgard/fraud_proofs/no_input/step_03.{Args, Datum, SpendRedeemer, State}
use midgard/fraud_proofs/no_input/step_04

validator main(
  step_04_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
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
        non_membership_proof_in_ledger,
        non_membership_proof_script_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data,
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

        expect
            Some(State {
              missing_input,
              blocks_prev_utxos_root,
              blocks_transactions_root,
            })
          = m_input_state_data

        // 2. Target input must be absent from the verified block's prev ledger.
        //    The ledger MPF is keyed by the node's `CML.TransactionInput` CBOR
        //    (a definite 2-element array), so the non-membership key must be
        //    `encode_midgard_tx_input`, NOT `cbor.serialise` of an aiken
        //    `OutputReference` (which serialises as a constructor).
        expect
          utils.plutarch_pexcludes_raw(
            merkle_root: blocks_prev_utxos_root,
            key_bytes: encode_midgard_tx_input(missing_input),
            non_membership_proof: non_membership_proof_in_ledger,
            redeemers: redeemers,
            withdraw_redeemer_index: non_membership_proof_script_redeemer_index,
          )

        // 3. Referenced transaction ID, plus the block's transactions root,
        //    must be passed to the next step.
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data = step_04.State {
          missing_input_tx_id: missing_input.tx_id,
          blocks_transactions_root,
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
// Step-03 is the first of the two non-existence checks: the challenged input
// must be absent from the block's initial ledger (`prev_utxos_root`). The
// delegated `pexcludes` claim is bound to the exact authenticated root and to
// the canonical `MidgardTxInput` CBOR key of the forwarded input, so neither the
// root nor the key can be swapped for a convenient one.

/// The block whose header authenticated the challenged transaction at step-01.
/// Both roots this step consumes are the block's real values, so the delegated
/// non-membership claim is bound to genuine roots rather than placeholders.
fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn prev_utxos_root() -> ByteArray {
  challenged_block().header.prev_utxos_root
}

fn transactions_root() -> ByteArray {
  challenged_block().transactions_phas_root
}

fn pexcludes_withdraw_redeemer_entry(
  merkle_root: ByteArray,
  key_bytes: ByteArray,
) -> Pair<ScriptPurpose, Redeemer> {
  let non_membership_proof: Proof = []
  let proof_data: Data = non_membership_proof
  let payload: List<Data> = [
    builtin.b_data(merkle_root),
    builtin.b_data(key_bytes),
    proof_data,
  ]
  Pair(
    Withdraw(Script(env.plutarch_pexcludes_validator_hash)),
    builtin.list_data(payload),
  )
}

fn challenged_input() -> MidgardTxInput {
  MidgardTxInput {
    tx_id: #"3131313131313131313131313131313131313131313131313131313131313131",
    output_index: 3,
  }
}

fn run_step_03(
  missing_input: MidgardTxInput,
  claimed_root: ByteArray,
  claimed_key: ByteArray,
) -> Bool {
  let expected_output_state: Data = step_04.State {
    missing_input_tx_id: missing_input.tx_id,
    blocks_transactions_root: transactions_root(),
  }
  let asset_name = fixture.ct_token_asset_name(challenged_block())
  let l1_tx = Transaction {
    ..transaction.placeholder,
    inputs: [fixture.ct_thread_input(asset_name)],
    outputs: [fixture.next_step_output(asset_name, expected_output_state)],
    redeemers: [pexcludes_withdraw_redeemer_entry(claimed_root, claimed_key)],
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            missing_input,
            blocks_prev_utxos_root: prev_utxos_root(),
            blocks_transactions_root: transactions_root(),
          },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        non_membership_proof_in_ledger: [],
        non_membership_proof_script_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Positive selector: the delegated non-membership claim is made against the
/// authenticated `prev_utxos_root` under the canonical `MidgardTxInput` CBOR key
/// of the forwarded input, so the transaction id reaches step-04.
test no_input_step_03_excludes_the_input_from_the_initial_ledger() {
  let missing_input = challenged_input()
  run_step_03(
    missing_input,
    prev_utxos_root(),
    encode_midgard_tx_input(missing_input),
  )
}

/// Valid-block negative selector: in a valid block the spent input really is in
/// the initial ledger, so the only non-membership claim a prover can obtain is
/// for some other key. Binding the claim to a key that is not the forwarded
/// input's canonical CBOR is rejected here.
test no_input_step_03_rejects_a_non_membership_claim_for_another_key() fail {
  let missing_input = challenged_input()
  let other_key = encode_midgard_tx_input(
    MidgardTxInput { ..missing_input, output_index: 4 },
  )
  run_step_03(missing_input, prev_utxos_root(), other_key)
}
