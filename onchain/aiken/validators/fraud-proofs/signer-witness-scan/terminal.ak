use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/signer_witness_scan_v1/state.{VerdictV1, terminal_v1}
use midgard/fraud_proofs/signer_witness_scan_v1/terminal.{
  Args, Datum, SpendRedeemer,
}
use midgard/rejection_reason_v1.{
  ProtectedOutputSignerMissing, SpendInputSignerMissing,
}

validator main(
  fraud_proof_policy: PolicyId,
  fraud_proof_address: Address,
  thread_policy: PolicyId,
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
        fraud_proof_mint_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, state <- finalize(
          thread_policy,
          fraud_proof_policy,
          fraud_proof_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_ref,
          inputs,
          outputs,
          redeemers,
        )
        expect Some(verdict) = state
        terminal_v1(verdict)
      }
    }
  }

  else(_) {
    fail
  }
}

// ### Terminal selectors
//
// The terminal reads the polarity nothing upstream branched on: an accepted
// subject closes only when a required signer was not found, a forced
// wrongful-rejection subject only when the required signer was found or no
// signer was required. The carried reason is checked for canonical form here;
// which reason it is was fixed by the family's step 01.

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
) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: block.tx_id,
    source_key: #"81",
    rejection_reason: Some(ProtectedOutputSignerMissing { output_index: 0 }),
  }
}

fn run_terminal(block: fixture.NativeBlockFixtureV1, verdict: VerdictV1) -> Bool {
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(verdict),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(fixture.ct_token_asset_name(block)),
  )
}

/// Wrongful acceptance closes: the operator accepted a transaction whose
/// required signer the complete scan never found.
test terminal_mints_for_an_accepted_missing_signer() {
  let block = fixture.challenged_block_v1()
  run_terminal(
    block,
    VerdictV1 {
      subject: accepted_subject(block),
      signer_required: True,
      signer_present: False,
    },
  )
}

/// Honest accepted transaction: the signer was present, so nothing closes.
test terminal_refuses_an_accepted_present_signer() fail {
  let block = fixture.challenged_block_v1()
  run_terminal(
    block,
    VerdictV1 {
      subject: accepted_subject(block),
      signer_required: True,
      signer_present: True,
    },
  )
}

/// Wrongful forced rejection closes: the operator rejected for a missing
/// signer the scan found.
test terminal_mints_for_a_forced_present_signer() {
  let block = fixture.challenged_block_v1()
  run_terminal(
    block,
    VerdictV1 {
      subject: forced_subject(block),
      signer_required: True,
      signer_present: True,
    },
  )
}

/// The terminal is reason-agnostic: the other family's canonical forced
/// subject closes under the same rule.
test terminal_mints_for_a_forced_present_spend_input_signer() {
  let block = fixture.challenged_block_v1()
  run_terminal(
    block,
    VerdictV1 {
      subject: substrate.VerdictSubjectV1 {
        ..forced_subject(block),
        rejection_reason: Some(SpendInputSignerMissing { input_index: 0 }),
      },
      signer_required: True,
      signer_present: True,
    },
  )
}

/// Honest forced rejection: the signer really was missing.
test terminal_refuses_a_forced_missing_signer() fail {
  let block = fixture.challenged_block_v1()
  run_terminal(
    block,
    VerdictV1 {
      subject: forced_subject(block),
      signer_required: True,
      signer_present: False,
    },
  )
}

/// A forced subject that lost its reason is not canonical and cannot close.
test terminal_refuses_a_forced_subject_without_a_reason() fail {
  let block = fixture.challenged_block_v1()
  run_terminal(
    block,
    VerdictV1 {
      subject: substrate.VerdictSubjectV1 {
        ..forced_subject(block),
        rejection_reason: None,
      },
      signer_required: True,
      signer_present: True,
    },
  )
}

/// An accepted subject carrying a reason is likewise non-canonical.
test terminal_refuses_an_accepted_subject_with_a_reason() fail {
  let block = fixture.challenged_block_v1()
  run_terminal(
    block,
    VerdictV1 {
      subject: substrate.VerdictSubjectV1 {
        ..accepted_subject(block),
        rejection_reason: Some(ProtectedOutputSignerMissing { output_index: 0 }),
      },
      signer_required: True,
      signer_present: False,
    },
  )
}

/// Direct route: no signer required closes a wrongful forced rejection.
test terminal_mints_for_a_forced_unrequired_signer() {
  let block = fixture.challenged_block_v1()
  run_terminal(
    block,
    VerdictV1 {
      subject: forced_subject(block),
      signer_required: False,
      signer_present: False,
    },
  )
}

/// An accepted subject has no fault where no signer is required.
test terminal_refuses_an_accepted_unrequired_signer() fail {
  let block = fixture.challenged_block_v1()
  run_terminal(
    block,
    VerdictV1 {
      subject: accepted_subject(block),
      signer_required: False,
      signer_present: False,
    },
  )
}
