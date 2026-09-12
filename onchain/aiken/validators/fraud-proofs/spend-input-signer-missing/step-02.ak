use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/forced_submission_fixture as forced_fixture
use midgard/fraud_proofs/common.{
  RedeemerCarriedMembership, cancel, continue, verify_membership_carried,
}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, FieldOpeningV1, opened_field_view, spend_inputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_tx_input, encode_midgard_tx_output,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardCredential, MidgardTxInput, MidgardTxOutput,
  PubKeyCredential, ScriptCredential,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/spend_input_signer_missing/rule.{
  AuthenticatedCredentialV1, BoundSpendInputV1, VerdictV1,
  authenticate_credential_v1, direct_verdict_v1,
}
use midgard/fraud_proofs/spend_input_signer_missing/step_02.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_descriptor_v1
use midgard/ledger_output_v1
use midgard/native_tx_machine_walk_v1.{spend_input_at, spend_input_count}
use midgard/rejection_reason_v1.{SpendInputSignerMissing}

validator main(
  step_03_hash: ScriptHash,
  step_05_hash: ScriptHash,
  thread_policy: PolicyId,
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
          thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        spend_inputs_opening,
        descriptor_cbor,
        membership,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, state, next_hash, next_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(bound) = state
        let view = opened_field_view(
          spend_inputs_opening,
          BodyAnchor {
            tx_id: bound.subject.transaction_id,
            source_kind: bound.subject.source_kind,
          },
          spend_inputs_field_index,
          reference_inputs,
          certificate_policy,
        )
        // Field 0 is fixed-stride, so its count is authenticated under every
        // carriage tier. Canonical validation never evaluates a position at
        // or past it, so a forced rejection citing one needs no signer.
        if bound.input_index >= spend_input_count(view) {
          let expected: Data = direct_verdict_v1(bound)
          and {
            next_hash == step_05_hash,
            next_state == expected,
          }
        } else {
          let selected = spend_input_at(view, bound.input_index)
          let out_ref = OutputReference {
            transaction_id: selected.tx_id,
            output_index: selected.output_index,
          }
          expect
            verify_membership_carried(
              membership,
              bound.prior_root,
              ledger_outref_key(out_ref),
              descriptor_cbor,
              reference_inputs,
              redeemers,
            )
          let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
          expect descriptor.output_index == out_ref.output_index
          expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
            descriptor.address,
          )
          when address.payment_credential is {
            PubKeyCredential(payment_credential) -> {
              let expected: Data = authenticate_credential_v1(
                bound,
                payment_credential,
              )
              and {
                next_hash == step_03_hash,
                next_state == expected,
              }
            }
            // `payment_credential_signer_authorization` authorizes a script
            // credential with no signer, so the forced rejection is wrong
            // without any witness.
            ScriptCredential(_) -> {
              let expected: Data = direct_verdict_v1(bound)
              and {
                next_hash == step_05_hash,
                next_state == expected,
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
// Step 02 opens field 0 of the bound transaction against the thread's
// `BodyAnchor`, resolves the bound spend input in the bound prior root, and
// classifies the resolved credential. A pub-key credential is handed to the
// witness scan at step 03 in both directions; a script credential or a
// coordinate past the authenticated count closes at step 05 directly, and
// only for a wrongful forced rejection.

const credential_a = #"30303030303030303030303030303030303030303030303030303030"

const credential_b = #"31313131313131313131313131313131313131313131313131313131"

/// The parameter standing in for the scan successor; `next_step_script_hash`
/// is the successor the fixture hand-off transaction pays to.
const scan_successor = fixture.catalogue_script_hash

fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn bound_input(block: fixture.NativeBlockFixtureV1) -> MidgardTxInput {
  expect [input] = block.tx.body.inputs
  input
}

/// The prior-ledger descriptor of the output the block's single spend input
/// references, locked by `payment_credential`, and the root committing it.
fn prior_material(
  block: fixture.NativeBlockFixtureV1,
  payment_credential: MidgardCredential,
) -> (ByteArray, ByteArray) {
  let input = bound_input(block)
  let output_cbor = encode_midgard_tx_output(
    MidgardTxOutput {
      ..fixture.sample_native_output(),
      address: MidgardAddress {
        protected: False,
        network_id: 0,
        payment_credential,
        stake_credential: None,
      },
    },
  )
  expect Some(descriptor) = ledger_output_descriptor_v1.build_v1(
    input.output_index,
    output_cbor,
  )
  let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  let key = ledger_outref_key(
    OutputReference {
      transaction_id: input.tx_id,
      output_index: input.output_index,
    },
  )
  let root = mpf.root(mpf.insert(mpf.empty, key, descriptor_cbor, []))
  (descriptor_cbor, root)
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
  input_index: Int,
) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: block.tx_id,
    source_key: #"81",
    rejection_reason: Some(SpendInputSignerMissing { input_index }),
  }
}

fn bound_state(
  block: fixture.NativeBlockFixtureV1,
  subject: substrate.VerdictSubjectV1,
  input_index: Int,
  prior_root: ByteArray,
) -> BoundSpendInputV1 {
  BoundSpendInputV1 {
    subject,
    input_index,
    prior_root,
    witness_set_hash: block.compact.witness_set_hash,
  }
}

fn expected_credential(
  state: BoundSpendInputV1,
  payment_credential: ByteArray,
) -> Data {
  let value: Data = AuthenticatedCredentialV1 {
    subject: state.subject,
    transaction_id: state.subject.transaction_id,
    witness_set_hash: state.witness_set_hash,
    payment_credential,
  }
  value
}

fn direct_verdict(subject: substrate.VerdictSubjectV1) -> Data {
  let value: Data = VerdictV1 {
    subject,
    signer_required: False,
    signer_missing: False,
  }
  value
}

fn opening_of(block: fixture.NativeBlockFixtureV1) -> FieldOpeningV1 {
  fixture.inline_body_opening(
    block.compact_cbor,
    block.tx.body.inputs |> list.map(encode_midgard_tx_input),
  )
}

fn args_of(
  block: fixture.NativeBlockFixtureV1,
  descriptor_cbor: ByteArray,
) -> Args {
  Args {
    input_index: 0,
    output_index: 0,
    spend_inputs_opening: opening_of(block),
    descriptor_cbor,
    membership: RedeemerCarriedMembership {
      membership_proof: [],
      membership_proof_script_redeemer_index: 0,
    },
  }
}

/// The hand-off transaction, carrying the delegated membership redeemer over
/// the prior root when the in-range arm needs it.
fn handoff_tx(
  block: fixture.NativeBlockFixtureV1,
  output_state: Data,
  prior_root: ByteArray,
  descriptor_cbor: ByteArray,
) -> Transaction {
  let input = bound_input(block)
  Transaction {
    ..fixture.continue_mid_step_l1_tx_v1(
      fixture.ct_token_asset_name(block),
      output_state,
    ),
    redeemers: [
      fixture.phas_withdraw_redeemer_entry(
        prior_root,
        ledger_outref_key(
          OutputReference {
            transaction_id: input.tx_id,
            output_index: input.output_index,
          },
        ),
        descriptor_cbor,
      ),
    ],
  }
}

fn run_step_02(
  step_03_hash: ScriptHash,
  step_05_hash: ScriptHash,
  state: BoundSpendInputV1,
  args: Args,
  tx: Transaction,
) -> Bool {
  let args = if state.subject.source_kind == 1 {
    Args {
      ..args,
      spend_inputs_opening: forced_fixture.from_native_opening(
        args.spend_inputs_opening,
      ),
    }
  } else {
    args
  }
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
    ct.Continue(args),
    fixture.own_out_ref(),
    tx,
  )
}

/// Runs the scan hand-off: the fixture successor is the step-03 parameter.
fn run_scan_handoff(
  state: BoundSpendInputV1,
  args: Args,
  tx: Transaction,
) -> Bool {
  run_step_02(fixture.next_step_script_hash, scan_successor, state, args, tx)
}

/// Runs the direct exit: the fixture successor is the step-05 parameter.
fn run_direct_exit(
  state: BoundSpendInputV1,
  args: Args,
  tx: Transaction,
) -> Bool {
  run_step_02(scan_successor, fixture.next_step_script_hash, state, args, tx)
}

/// A pub-key spend input under an accepted subject forwards its credential
/// to the witness scan.
test spend_input_step_02_forwards_a_pub_key_credential() {
  let block = challenged_block()
  let (descriptor, root) = prior_material(block, PubKeyCredential(credential_a))
  let state = bound_state(block, accepted_subject(block), 0, root)
  run_scan_handoff(
    state,
    args_of(block, descriptor),
    handoff_tx(
      block,
      expected_credential(state, credential_a),
      root,
      descriptor,
    ),
  )
}

/// A pub-key spend input under a forced subject takes the same scan door.
test spend_input_step_02_forwards_a_forced_pub_key_credential() {
  let block = challenged_block()
  let (descriptor, root) = prior_material(block, PubKeyCredential(credential_a))
  let state = bound_state(block, forced_subject(block, 0), 0, root)
  run_scan_handoff(
    state,
    args_of(block, descriptor),
    handoff_tx(
      block,
      expected_credential(state, credential_a),
      root,
      descriptor,
    ),
  )
}

/// A script-locked spend input needs no signer, so the forced rejection is
/// contradicted straight at the terminal.
test spend_input_step_02_closes_a_forced_script_locked_coordinate_directly() {
  let block = challenged_block()
  let (descriptor, root) = prior_material(block, ScriptCredential(credential_a))
  let subject = forced_subject(block, 0)
  let state = bound_state(block, subject, 0, root)
  run_direct_exit(
    state,
    args_of(block, descriptor),
    handoff_tx(block, direct_verdict(subject), root, descriptor),
  )
}

/// A forced coordinate past the authenticated field-0 count is never
/// evaluated by canonical validation; it closes without any membership.
test spend_input_step_02_closes_a_forced_out_of_range_coordinate_directly() {
  let block = challenged_block()
  let (_, root) = prior_material(block, PubKeyCredential(credential_a))
  let subject = forced_subject(block, 1)
  let state = bound_state(block, subject, 1, root)
  run_direct_exit(
    state,
    args_of(block, #""),
    Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(
        fixture.ct_token_asset_name(block),
        direct_verdict(subject),
      ),
      redeemers: [],
    },
  )
}

/// Honest refusal: an accepted subject has no fault at a script-locked input.
test spend_input_step_02_refuses_an_accepted_script_locked_coordinate() fail {
  let block = challenged_block()
  let (descriptor, root) = prior_material(block, ScriptCredential(credential_a))
  let subject = accepted_subject(block)
  let state = bound_state(block, subject, 0, root)
  run_direct_exit(
    state,
    args_of(block, descriptor),
    handoff_tx(block, direct_verdict(subject), root, descriptor),
  )
}

/// Honest refusal: an accepted subject has no fault past the field's count.
test spend_input_step_02_refuses_an_accepted_out_of_range_coordinate() fail {
  let block = challenged_block()
  let (_, root) = prior_material(block, PubKeyCredential(credential_a))
  let subject = accepted_subject(block)
  let state = bound_state(block, subject, 1, root)
  run_direct_exit(
    state,
    args_of(block, #""),
    Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(
        fixture.ct_token_asset_name(block),
        direct_verdict(subject),
      ),
      redeemers: [],
    },
  )
}

/// The direct verdict is exact: a thread claiming the signer was required
/// and missing at a script-locked input is refused.
test spend_input_step_02_refuses_a_direct_verdict_claiming_a_required_signer() fail {
  let block = challenged_block()
  let (descriptor, root) = prior_material(block, ScriptCredential(credential_a))
  let subject = forced_subject(block, 0)
  let state = bound_state(block, subject, 0, root)
  let claimed: Data = VerdictV1 {
    subject,
    signer_required: True,
    signer_missing: True,
  }
  run_direct_exit(
    state,
    args_of(block, descriptor),
    handoff_tx(block, claimed, root, descriptor),
  )
}

/// Credential seam: a pub-key coordinate cannot skip the witness scan.
test spend_input_step_02_refuses_a_direct_verdict_for_a_pub_key_coordinate() fail {
  let block = challenged_block()
  let (descriptor, root) = prior_material(block, PubKeyCredential(credential_a))
  let subject = forced_subject(block, 0)
  let state = bound_state(block, subject, 0, root)
  run_direct_exit(
    state,
    args_of(block, descriptor),
    handoff_tx(block, direct_verdict(subject), root, descriptor),
  )
}

/// Subject coordinate: the direct verdict must carry the bound subject.
test spend_input_step_02_refuses_a_substituted_direct_verdict_subject() fail {
  let block = challenged_block()
  let (descriptor, root) = prior_material(block, ScriptCredential(credential_a))
  let subject = forced_subject(block, 0)
  let state = bound_state(block, subject, 0, root)
  run_direct_exit(
    state,
    args_of(block, descriptor),
    handoff_tx(
      block,
      direct_verdict(forced_subject(block, 1)),
      root,
      descriptor,
    ),
  )
}

/// Descriptor seam: the forwarded credential is the resolved one, not a
/// credential the datum names.
test spend_input_step_02_refuses_a_substituted_descriptor_credential() fail {
  let block = challenged_block()
  let (descriptor, root) = prior_material(block, PubKeyCredential(credential_a))
  let state = bound_state(block, accepted_subject(block), 0, root)
  run_scan_handoff(
    state,
    args_of(block, descriptor),
    handoff_tx(
      block,
      expected_credential(state, credential_b),
      root,
      descriptor,
    ),
  )
}

/// Prior-root seam: a descriptor under a root the thread did not bind is
/// refused by the delegated membership.
test spend_input_step_02_refuses_a_substituted_prior_root() fail {
  let block = challenged_block()
  let (descriptor, root) = prior_material(block, ScriptCredential(credential_a))
  let (_, other_root) = prior_material(block, PubKeyCredential(credential_b))
  let subject = forced_subject(block, 0)
  let state = bound_state(block, subject, 0, other_root)
  run_direct_exit(
    state,
    args_of(block, descriptor),
    handoff_tx(block, direct_verdict(subject), root, descriptor),
  )
}

/// The direct verdict may only continue at step 05.
test spend_input_step_02_refuses_the_direct_exit_at_the_scan_successor() fail {
  let block = challenged_block()
  let (descriptor, root) = prior_material(block, ScriptCredential(credential_a))
  let subject = forced_subject(block, 0)
  let state = bound_state(block, subject, 0, root)
  run_scan_handoff(
    state,
    args_of(block, descriptor),
    handoff_tx(block, direct_verdict(subject), root, descriptor),
  )
}

/// The credential hand-off may only continue at step 03.
test spend_input_step_02_refuses_the_scan_hand_off_at_the_terminal_successor() fail {
  let block = challenged_block()
  let (descriptor, root) = prior_material(block, PubKeyCredential(credential_a))
  let state = bound_state(block, forced_subject(block, 0), 0, root)
  run_direct_exit(
    state,
    args_of(block, descriptor),
    handoff_tx(
      block,
      expected_credential(state, credential_a),
      root,
      descriptor,
    ),
  )
}
