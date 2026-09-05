use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, FieldOpeningV1, opened_field_view,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_output_cbor, encode_midgard_tx_output,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardCredential, MidgardTransaction, MidgardTransactionBody,
  MidgardTxOutput, PubKeyCredential, ScriptCredential,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/protected_output_signer_missing/rule.{
  BoundOutputV1, ProtectedCredentialV1, outputs_field_index,
}
use midgard/fraud_proofs/protected_output_signer_missing/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/native_tx_field_access_v1.{field_item_at}
use midgard/rejection_reason_v1.{ProtectedOutputSignerMissing}

validator main(
  step_03_hash: ScriptHash,
  computation_thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args { input_index, output_index, opening }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _own, _name, _prover, prior, next_hash, next_data <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(State { bound, witness_set_hash }) = prior
        let view = opened_field_view(
          opening,
          BodyAnchor { tx_id: bound.subject.transaction_id },
          outputs_field_index,
          reference_inputs,
          certificate_policy,
        )
        let output = decode_midgard_tx_output_cbor(
          field_item_at(view, bound.output_index),
        )
        expect output.address.protected
        expect PubKeyCredential(payment_credential) = output.address.payment_credential
        let protected = ProtectedCredentialV1 {
          subject: bound.subject,
          transaction_id: bound.subject.transaction_id,
          witness_set_hash,
          output_index: bound.output_index,
          payment_credential,
        }
        let expected: Data = protected
        and {
          next_hash == step_03_hash,
          next_data == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}

// ### Selectors
//
// Step 02 opens field 2 of the bound transaction against the thread's
// `BodyAnchor`, decodes the item at the bound coordinate, and forwards the
// protected pub-key credential. The direction is carried through untouched:
// an accepted and a forced subject take the same door.

const credential_a = #"30303030303030303030303030303030303030303030303030303030"

const credential_b = #"31313131313131313131313131313131313131313131313131313131"

fn output_with(
  protected: Bool,
  payment_credential: MidgardCredential,
) -> MidgardTxOutput {
  MidgardTxOutput {
    ..fixture.sample_native_output(),
    address: MidgardAddress {
      protected,
      network_id: 0,
      payment_credential,
      stake_credential: None,
    },
  }
}

fn protected_key_output(credential: ByteArray) -> MidgardTxOutput {
  output_with(True, PubKeyCredential(credential))
}

fn block_with_outputs(
  outputs: List<MidgardTxOutput>,
) -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      body: MidgardTransactionBody { ..base.body, outputs },
    },
  )
}

fn accepted_subject(
  block: fixture.NativeBlockFixtureV1,
) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: block.tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

fn forced_subject(
  block: fixture.NativeBlockFixtureV1,
  output_index: Int,
) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: block.tx_id,
    source_key: #"81",
    rejection_reason: Some(ProtectedOutputSignerMissing { output_index }),
  }
}

fn bound_state(
  block: fixture.NativeBlockFixtureV1,
  subject: substrate.VerdictSubjectV1,
  output_index: Int,
) -> State {
  State {
    bound: BoundOutputV1 { subject, output_index },
    witness_set_hash: block.compact.witness_set_hash,
  }
}

fn expected_credential(state: State, payment_credential: ByteArray) -> Data {
  let value: Data = ProtectedCredentialV1 {
    subject: state.bound.subject,
    transaction_id: state.bound.subject.transaction_id,
    witness_set_hash: state.witness_set_hash,
    output_index: state.bound.output_index,
    payment_credential,
  }
  value
}

fn opening_of(
  block: fixture.NativeBlockFixtureV1,
  outputs: List<MidgardTxOutput>,
) -> FieldOpeningV1 {
  fixture.inline_body_opening(
    block.compact_cbor,
    list.map(outputs, encode_midgard_tx_output),
  )
}

fn run_step_02(state: State, opening: FieldOpeningV1, tx: Transaction) -> Bool {
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(Args { input_index: 0, output_index: 0, opening }),
    fixture.own_out_ref(),
    tx,
  )
}

fn handoff_tx(
  block: fixture.NativeBlockFixtureV1,
  output_state: Data,
) -> Transaction {
  Transaction {
    ..fixture.continue_mid_step_l1_tx_v1(
      fixture.ct_token_asset_name(block),
      output_state,
    ),
    reference_inputs: [],
  }
}

/// A protected pub-key output at the bound coordinate forwards its credential
/// with the subject and witness-set anchor untouched.
test protected_output_step_02_forwards_a_protected_key_credential() {
  let outputs = [protected_key_output(credential_a)]
  let block = block_with_outputs(outputs)
  let state = bound_state(block, accepted_subject(block), 0)
  run_step_02(
    state,
    opening_of(block, outputs),
    handoff_tx(block, expected_credential(state, credential_a)),
  )
}

/// The coordinate is positional: with two protected outputs, index 1 forwards
/// the second credential and nothing about the first.
test protected_output_step_02_selects_the_bound_coordinate() {
  let outputs = [
    protected_key_output(credential_a),
    protected_key_output(credential_b),
  ]
  let block = block_with_outputs(outputs)
  let state = bound_state(block, accepted_subject(block), 1)
  run_step_02(
    state,
    opening_of(block, outputs),
    handoff_tx(block, expected_credential(state, credential_b)),
  )
}

/// A forced wrongful-rejection subject takes the same door and is carried
/// verbatim into the credential state.
test protected_output_step_02_forwards_a_forced_subject() {
  let outputs = [protected_key_output(credential_a)]
  let block = block_with_outputs(outputs)
  let state = bound_state(block, forced_subject(block, 0), 0)
  run_step_02(
    state,
    opening_of(block, outputs),
    handoff_tx(block, expected_credential(state, credential_a)),
  )
}

/// Honest refusal: an unprotected output requires no signer, so neither
/// direction has a credential to scan for.
test protected_output_step_02_refuses_an_unprotected_output() fail {
  let outputs = [output_with(False, PubKeyCredential(credential_a))]
  let block = block_with_outputs(outputs)
  let state = bound_state(block, accepted_subject(block), 0)
  run_step_02(
    state,
    opening_of(block, outputs),
    handoff_tx(block, expected_credential(state, credential_a)),
  )
}

/// Honest refusal: a protected script-locked output is receive-script
/// territory, not a missing-signer subject.
test protected_output_step_02_refuses_a_script_credential() fail {
  let outputs = [output_with(True, ScriptCredential(credential_a))]
  let block = block_with_outputs(outputs)
  let state = bound_state(block, accepted_subject(block), 0)
  run_step_02(
    state,
    opening_of(block, outputs),
    handoff_tx(block, expected_credential(state, credential_a)),
  )
}

/// Out-of-range coordinate: the door refuses a read past the field's count.
test protected_output_step_02_refuses_an_out_of_range_coordinate() fail {
  let outputs = [protected_key_output(credential_a)]
  let block = block_with_outputs(outputs)
  let state = bound_state(block, accepted_subject(block), 1)
  run_step_02(
    state,
    opening_of(block, outputs),
    handoff_tx(block, expected_credential(state, credential_a)),
  )
}

/// Subject coordinate: thread state naming another transaction does not
/// anchor the compact bytes the opening carries.
test protected_output_step_02_refuses_a_substituted_subject() fail {
  let outputs = [protected_key_output(credential_a)]
  let block = block_with_outputs(outputs)
  let foreign = substrate.VerdictSubjectV1 {
    ..accepted_subject(block),
    transaction_id: #"9999999999999999999999999999999999999999999999999999999999999999",
  }
  let state = bound_state(block, foreign, 0)
  run_step_02(
    state,
    opening_of(block, outputs),
    handoff_tx(block, expected_credential(state, credential_a)),
  )
}

/// Compact transaction: another transaction's bytes under the anchored id.
test protected_output_step_02_refuses_substituted_compact_bytes() fail {
  let outputs = [protected_key_output(credential_a)]
  let block = block_with_outputs(outputs)
  let foreign = block_with_outputs([protected_key_output(credential_b)])
  let state = bound_state(block, accepted_subject(block), 0)
  run_step_02(
    state,
    opening_of(foreign, [protected_key_output(credential_b)]),
    handoff_tx(block, expected_credential(state, credential_b)),
  )
}

/// Field preimage: outputs the transaction did not commit fail the flat
/// field commitment at the door.
test protected_output_step_02_refuses_a_substituted_field_preimage() fail {
  let outputs = [protected_key_output(credential_a)]
  let block = block_with_outputs(outputs)
  let state = bound_state(block, accepted_subject(block), 0)
  run_step_02(
    state,
    opening_of(block, [protected_key_output(credential_b)]),
    handoff_tx(block, expected_credential(state, credential_b)),
  )
}

/// The forwarded credential is decoded on chain: a datum naming a credential
/// the output does not carry is refused.
test protected_output_step_02_refuses_a_substituted_credential() fail {
  let outputs = [protected_key_output(credential_a)]
  let block = block_with_outputs(outputs)
  let state = bound_state(block, accepted_subject(block), 0)
  run_step_02(
    state,
    opening_of(block, outputs),
    handoff_tx(block, expected_credential(state, credential_b)),
  )
}

/// The credential state may only continue at step 03.
test protected_output_step_02_refuses_a_wrong_successor() fail {
  let outputs = [protected_key_output(credential_a)]
  let block = block_with_outputs(outputs)
  let state = bound_state(block, accepted_subject(block), 0)
  run_step_02(
    state,
    opening_of(block, outputs),
    Transaction {
      ..fixture.continue_same_step_l1_tx_v1(
        fixture.ct_token_asset_name(block),
        expected_credential(state, credential_a),
      ),
      reference_inputs: [],
    },
  )
}
