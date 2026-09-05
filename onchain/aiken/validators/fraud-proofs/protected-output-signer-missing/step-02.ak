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
  BoundOutputV1, ProtectedCredentialV1, VerdictV1, direct_verdict_v1,
  outputs_field_index,
}
use midgard/fraud_proofs/protected_output_signer_missing/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/native_tx_field_access_v1.{field_item_at, field_item_count}
use midgard/rejection_reason_v1.{ProtectedOutputSignerMissing}

validator main(
  step_03_hash: ScriptHash,
  step_05_hash: ScriptHash,
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
      ct.Continue(Args {
        input_index,
        output_index,
        opening,
        coordinate_out_of_range,
      }) -> {
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
        if coordinate_out_of_range {
          // Canonical validation never evaluates a position at or past the
          // field's count, so a forced rejection citing one needs no signer.
          // `field_item_count` answers only with an authenticated count and
          // refuses the provisional count of a certified variable-width view.
          expect bound.output_index >= field_item_count(view)
          let expected: Data = direct_verdict_v1(bound)
          and {
            next_hash == step_05_hash,
            next_data == expected,
          }
        } else {
          let output = decode_midgard_tx_output_cbor(
            field_item_at(view, bound.output_index),
          )
          // `protected_output_authorization` consults the signer frontier
          // only for a protected pub-key output; every other output is
          // authorized with no signer, so a forced rejection citing it is
          // wrong without any witness.
          when
            (output.address.protected, output.address.payment_credential)
          is {
            (True, PubKeyCredential(payment_credential)) -> {
              let expected: Data = ProtectedCredentialV1 {
                subject: bound.subject,
                transaction_id: bound.subject.transaction_id,
                witness_set_hash,
                output_index: bound.output_index,
                payment_credential,
              }
              and {
                next_hash == step_03_hash,
                next_data == expected,
              }
            }
            _ -> {
              let expected: Data = direct_verdict_v1(bound)
              and {
                next_hash == step_05_hash,
                next_data == expected,
              }
            }
          }
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
// `BodyAnchor`, decodes the item at the bound coordinate, and classifies it.
// A protected pub-key output forwards its credential to the witness scan in
// both directions; an unprotected output, a protected script output, or a
// coordinate past the authenticated count closes at step 05 directly, and
// only for a wrongful forced rejection.

const credential_a = #"30303030303030303030303030303030303030303030303030303030"

const credential_b = #"31313131313131313131313131313131313131313131313131313131"

/// The parameter standing in for the scan successor; `next_step_script_hash`
/// is the successor the fixture hand-off transaction pays to.
const scan_successor = fixture.catalogue_script_hash

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

fn direct_verdict(subject: substrate.VerdictSubjectV1) -> Data {
  let value: Data = VerdictV1 {
    subject,
    signer_required: False,
    signer_present: False,
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

fn run_step_02_with(
  step_03_hash: ScriptHash,
  step_05_hash: ScriptHash,
  state: State,
  opening: FieldOpeningV1,
  coordinate_out_of_range: Bool,
  tx: Transaction,
) -> Bool {
  main.spend(
    step_03_hash,
    step_05_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      Args { input_index: 0, output_index: 0, opening, coordinate_out_of_range },
    ),
    fixture.own_out_ref(),
    tx,
  )
}

/// Runs the scan hand-off: the fixture successor is the step-03 parameter.
fn run_step_02(state: State, opening: FieldOpeningV1, tx: Transaction) -> Bool {
  run_step_02_with(
    fixture.next_step_script_hash,
    scan_successor,
    state,
    opening,
    False,
    tx,
  )
}

/// Runs the direct exit: the fixture successor is the step-05 parameter.
fn run_direct_exit(
  state: State,
  opening: FieldOpeningV1,
  coordinate_out_of_range: Bool,
  tx: Transaction,
) -> Bool {
  run_step_02_with(
    scan_successor,
    fixture.next_step_script_hash,
    state,
    opening,
    coordinate_out_of_range,
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

/// Honest refusal: an unprotected output requires no signer, so an accepted
/// subject has no credential to scan for and no fault to close on.
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
/// territory, not a missing-signer subject for an accepted block.
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

// ### Direct terminal route

/// An unprotected output needs no signer, so the forced rejection is
/// contradicted straight at the terminal.
test protected_output_step_02_closes_a_forced_unprotected_output_directly() {
  let outputs = [output_with(False, PubKeyCredential(credential_a))]
  let block = block_with_outputs(outputs)
  let subject = forced_subject(block, 0)
  run_direct_exit(
    bound_state(block, subject, 0),
    opening_of(block, outputs),
    False,
    handoff_tx(block, direct_verdict(subject)),
  )
}

/// A protected script output is authorized without a signer likewise.
test protected_output_step_02_closes_a_forced_script_output_directly() {
  let outputs = [output_with(True, ScriptCredential(credential_a))]
  let block = block_with_outputs(outputs)
  let subject = forced_subject(block, 0)
  run_direct_exit(
    bound_state(block, subject, 0),
    opening_of(block, outputs),
    False,
    handoff_tx(block, direct_verdict(subject)),
  )
}

/// A forced coordinate past the authenticated field-2 count is never
/// evaluated by canonical validation.
test protected_output_step_02_closes_a_forced_out_of_range_coordinate_directly() {
  let outputs = [protected_key_output(credential_a)]
  let block = block_with_outputs(outputs)
  let subject = forced_subject(block, 1)
  run_direct_exit(
    bound_state(block, subject, 1),
    opening_of(block, outputs),
    True,
    handoff_tx(block, direct_verdict(subject)),
  )
}

/// The out-of-range arm is decided from the authenticated count, not the
/// flag: an in-range coordinate under the flag is refused.
test protected_output_step_02_refuses_an_in_range_out_of_range_claim() fail {
  let outputs = [
    protected_key_output(credential_a),
    protected_key_output(credential_b),
  ]
  let block = block_with_outputs(outputs)
  let subject = forced_subject(block, 1)
  run_direct_exit(
    bound_state(block, subject, 1),
    opening_of(block, outputs),
    True,
    handoff_tx(block, direct_verdict(subject)),
  )
}

/// Honest refusal: an accepted subject has no fault at an unprotected output.
test protected_output_step_02_refuses_an_accepted_unprotected_direct_exit() fail {
  let outputs = [output_with(False, PubKeyCredential(credential_a))]
  let block = block_with_outputs(outputs)
  let subject = accepted_subject(block)
  run_direct_exit(
    bound_state(block, subject, 0),
    opening_of(block, outputs),
    False,
    handoff_tx(block, direct_verdict(subject)),
  )
}

/// Honest refusal: an accepted subject has no fault past the field's count.
test protected_output_step_02_refuses_an_accepted_out_of_range_direct_exit() fail {
  let outputs = [protected_key_output(credential_a)]
  let block = block_with_outputs(outputs)
  let subject = accepted_subject(block)
  run_direct_exit(
    bound_state(block, subject, 1),
    opening_of(block, outputs),
    True,
    handoff_tx(block, direct_verdict(subject)),
  )
}

/// Credential seam: a protected pub-key coordinate cannot skip the scan.
test protected_output_step_02_refuses_a_direct_verdict_for_a_protected_key() fail {
  let outputs = [protected_key_output(credential_a)]
  let block = block_with_outputs(outputs)
  let subject = forced_subject(block, 0)
  run_direct_exit(
    bound_state(block, subject, 0),
    opening_of(block, outputs),
    False,
    handoff_tx(block, direct_verdict(subject)),
  )
}

/// The direct verdict is exact: a thread claiming the signer was required
/// and absent at an unprotected output is refused.
test protected_output_step_02_refuses_a_direct_verdict_claiming_a_required_signer() fail {
  let outputs = [output_with(False, PubKeyCredential(credential_a))]
  let block = block_with_outputs(outputs)
  let subject = forced_subject(block, 0)
  let claimed: Data = VerdictV1 {
    subject,
    signer_required: True,
    signer_present: False,
  }
  run_direct_exit(
    bound_state(block, subject, 0),
    opening_of(block, outputs),
    False,
    handoff_tx(block, claimed),
  )
}

/// Subject coordinate: the direct verdict must carry the bound subject.
test protected_output_step_02_refuses_a_substituted_direct_verdict_subject() fail {
  let outputs = [output_with(True, ScriptCredential(credential_a))]
  let block = block_with_outputs(outputs)
  run_direct_exit(
    bound_state(block, forced_subject(block, 0), 0),
    opening_of(block, outputs),
    False,
    handoff_tx(block, direct_verdict(forced_subject(block, 1))),
  )
}

/// The direct verdict may only continue at step 05.
test protected_output_step_02_refuses_the_direct_exit_at_the_scan_successor() fail {
  let outputs = [output_with(True, ScriptCredential(credential_a))]
  let block = block_with_outputs(outputs)
  let subject = forced_subject(block, 0)
  run_step_02(
    bound_state(block, subject, 0),
    opening_of(block, outputs),
    handoff_tx(block, direct_verdict(subject)),
  )
}

/// The credential hand-off may only continue at step 03.
test protected_output_step_02_refuses_the_scan_hand_off_at_the_terminal_successor() fail {
  let outputs = [protected_key_output(credential_a)]
  let block = block_with_outputs(outputs)
  let state = bound_state(block, forced_subject(block, 0), 0)
  run_direct_exit(
    state,
    opening_of(block, outputs),
    False,
    handoff_tx(block, expected_credential(state, credential_a)),
  )
}
