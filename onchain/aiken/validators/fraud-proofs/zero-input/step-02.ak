//// ## Zero Input Fraud Proof (Section 5.1.1)
////
//// Concludes the proof that the transaction carried through step-01 spends no
//// inputs.

use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/bounded_collection_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/zero_input/step_02.{Args, Datum, SpendRedeemer, State}
use midgard/ledger_state.{MidgardInputsHash}

/// The `spend_inputs_hash` a native transaction body carries when it spends
/// nothing: the canonical V1 bounded collection commitment for field zero.
const empty_spend_inputs_hash: MidgardInputsHash =
  bounded_collection_v1.from_items(0, [])

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
        fraud_proof_mint_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced at the
        //    fraud proof's spending address.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data
          <- finalize(
          computation_thread_token_policy_id,
          fraud_proof_token_policy_id,
          fraud_proof_token_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_out_ref,
          inputs,
          outputs,
          redeemers,
        )

        expect Some(State { bad_tx_spend_inputs_hash }) = m_input_state_data

        // 2. The spend inputs list must be empty.
        expect bad_tx_spend_inputs_hash == empty_spend_inputs_hash

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

test empty_spend_inputs_hash_matches_the_canonical_native_v1_commitment() {
  empty_spend_inputs_hash == bounded_collection_v1.from_items(0, [])
}

/// Pins the exact cross-language vector used by the SDK.
test empty_spend_inputs_hash_matches_the_offchain_constant() {
  empty_spend_inputs_hash == #"eb25ed4ae02426602eee44b29d93e9dcd0be514b2087eda02f398b16fbb0ec76"
}

test empty_spend_inputs_hash_rejects_a_non_empty_bounded_collection() {
  empty_spend_inputs_hash != bounded_collection_v1.from_items(
    0,
    [#"d8799f581c00000000000000000000000000000000000000000000000000000000ff"],
  )
}

type TestFraudProofDatum {
  fraud_prover: ByteArray,
}

type TestFraudProofMintRedeemer {
  computation_thread_token_asset_name: ByteArray,
  computation_thread_mint_redeemer_index: Int,
}

const test_fraud_proof_policy: PolicyId =
  #"11111111111111111111111111111111111111111111111111111111"

const test_computation_thread_policy: PolicyId =
  #"22222222222222222222222222222222222222222222222222222222"

const test_step_script_hash =
  #"33333333333333333333333333333333333333333333333333333333"

const test_fraud_prover =
  #"44444444444444444444444444444444444444444444444444444444"

const test_computation_thread_asset_name =
  #"00000005aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const test_non_empty_spend_inputs_hash: MidgardInputsHash =
  bounded_collection_v1.from_items(
    0,
    [#"d8799f581c00000000000000000000000000000000000000000000000000000000ff"],
  )

const test_own_out_ref =
  OutputReference {
    transaction_id: #"5555555555555555555555555555555555555555555555555555555555555555",
    output_index: 0,
  }

fn test_script_address(script_hash) -> Address {
  Address {
    payment_credential: address.Script(script_hash),
    stake_credential: None,
  }
}

fn test_step_datum(spend_inputs_hash: MidgardInputsHash) -> Datum {
  ct.StepDatum {
    fraud_prover: test_fraud_prover,
    data: Some(State { bad_tx_spend_inputs_hash: spend_inputs_hash }),
  }
}

fn test_continue_redeemer() -> SpendRedeemer {
  ct.Continue(
    Args { input_index: 0, output_index: 0, fraud_proof_mint_redeemer_index: 0 },
  )
}

fn test_finalize_tx(spend_inputs_hash: MidgardInputsHash) -> Transaction {
  let input_datum_data: Data = test_step_datum(spend_inputs_hash)
  let fraud_proof_datum_data: Data = TestFraudProofDatum {
    fraud_prover: test_fraud_prover,
  }
  let fraud_proof_mint_redeemer_data: Data = TestFraudProofMintRedeemer {
    computation_thread_token_asset_name: test_computation_thread_asset_name,
    computation_thread_mint_redeemer_index: 1,
  }
  Transaction {
    ..transaction.placeholder,
    inputs: [
      transaction.Input {
        output_reference: test_own_out_ref,
        output: transaction.Output {
          address: test_script_address(test_step_script_hash),
          value: assets.from_lovelace(2)
            |> assets.add(
                test_computation_thread_policy,
                test_computation_thread_asset_name,
                1,
              ),
          datum: transaction.InlineDatum(input_datum_data),
          reference_script: None,
        },
      },
    ],
    outputs: [
      transaction.Output {
        address: test_script_address(test_fraud_proof_policy),
        value: assets.from_lovelace(2)
          |> assets.add(
              test_fraud_proof_policy,
              test_computation_thread_asset_name,
              1,
            ),
        datum: transaction.InlineDatum(fraud_proof_datum_data),
        reference_script: None,
      },
    ],
    redeemers: [
      Pair(
        transaction.Mint(test_fraud_proof_policy),
        fraud_proof_mint_redeemer_data,
      ),
    ],
  }
}

test validator_accepts_the_empty_native_input_hash() {
  main.spend(
    test_fraud_proof_policy,
    test_script_address(test_fraud_proof_policy),
    test_computation_thread_policy,
    Some(test_step_datum(empty_spend_inputs_hash)),
    test_continue_redeemer(),
    test_own_out_ref,
    test_finalize_tx(empty_spend_inputs_hash),
  )
}

test validator_rejects_a_non_empty_native_input_hash() fail {
  main.spend(
    test_fraud_proof_policy,
    test_script_address(test_fraud_proof_policy),
    test_computation_thread_policy,
    Some(test_step_datum(test_non_empty_spend_inputs_hash)),
    test_continue_redeemer(),
    test_own_out_ref,
    test_finalize_tx(test_non_empty_spend_inputs_hash),
  )
}
