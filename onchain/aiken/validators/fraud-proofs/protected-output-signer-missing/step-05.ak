use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/protected_output_signer_missing/rule.{
  VerdictV1, terminal_v1,
}
use midgard/fraud_proofs/protected_output_signer_missing/step_05.{
  Args, Datum, SpendRedeemer,
}
use midgard/rejection_reason_v1.{ProtectedOutputSignerMissing}

validator main(
  fraud_proof_policy: PolicyId,
  fraud_proof_address: Address,
  computation_thread_policy: PolicyId,
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
        fraud_proof_mint_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx
        expect Some(step_datum) = datum
        let _own, _name, _prover, prior <- finalize(
          computation_thread_policy,
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
        expect Some(verdict) = prior
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
// Step 05 reads the polarity nothing upstream branched on: an accepted
// subject closes only when the scan found no valid signer, a forced
// wrongful-rejection subject only when it found one, and the exact carried
// reason is re-checked on the way through `terminal_contradiction_v1`.

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

fn run_step_05(block: fixture.NativeBlockFixtureV1, verdict: VerdictV1) -> Bool {
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

/// Wrongful acceptance closes: the operator accepted a protected output whose
/// signer the complete scan never found.
test protected_output_step_05_mints_for_an_accepted_missing_signer() {
  let block = fixture.challenged_block_v1()
  run_step_05(
    block,
    VerdictV1 {
      subject: accepted_subject(block),
      signer_required: True,
      signer_present: False,
    },
  )
}

/// Honest accepted block: the signer was present, so nothing closes.
test protected_output_step_05_refuses_an_accepted_present_signer() fail {
  let block = fixture.challenged_block_v1()
  run_step_05(
    block,
    VerdictV1 {
      subject: accepted_subject(block),
      signer_required: True,
      signer_present: True,
    },
  )
}

/// Wrongful forced rejection closes: the operator rejected for a missing
/// signer the complete scan found.
test protected_output_step_05_mints_for_a_forced_present_signer() {
  let block = fixture.challenged_block_v1()
  run_step_05(
    block,
    VerdictV1 {
      subject: forced_subject(block),
      signer_required: True,
      signer_present: True,
    },
  )
}

/// Honest forced rejection: the signer really was missing, so the operator's
/// verdict stands.
test protected_output_step_05_refuses_a_forced_missing_signer() fail {
  let block = fixture.challenged_block_v1()
  run_step_05(
    block,
    VerdictV1 {
      subject: forced_subject(block),
      signer_required: True,
      signer_present: False,
    },
  )
}

/// A forced subject that lost its reason on the way is not canonical and
/// cannot close either way.
test protected_output_step_05_refuses_a_forced_subject_without_a_reason() fail {
  let block = fixture.challenged_block_v1()
  run_step_05(
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

/// An accepted-root subject carrying a reason is likewise non-canonical.
test protected_output_step_05_refuses_an_accepted_subject_with_a_reason() fail {
  let block = fixture.challenged_block_v1()
  run_step_05(
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

// ### Direct terminal route
//
// Step 02 may hand the terminal a verdict that needed no scan: canonical
// validation required no signer at the bound coordinate. That verdict closes
// only a wrongful forced rejection.

/// Wrongful forced rejection closes on the direct verdict.
test protected_output_step_05_mints_for_a_forced_unrequired_signer() {
  let block = fixture.challenged_block_v1()
  run_step_05(
    block,
    VerdictV1 {
      subject: forced_subject(block),
      signer_required: False,
      signer_present: False,
    },
  )
}

/// An accepted subject has no fault where no signer is required, whatever the
/// carried scan answer says.
test protected_output_step_05_refuses_an_accepted_unrequired_signer() fail {
  let block = fixture.challenged_block_v1()
  run_step_05(
    block,
    VerdictV1 {
      subject: accepted_subject(block),
      signer_required: False,
      signer_present: False,
    },
  )
}

/// The direct verdict still needs a canonical forced subject.
test protected_output_step_05_refuses_an_unrequired_signer_without_a_reason() fail {
  let block = fixture.challenged_block_v1()
  run_step_05(
    block,
    VerdictV1 {
      subject: substrate.VerdictSubjectV1 {
        ..forced_subject(block),
        rejection_reason: None,
      },
      signer_required: False,
      signer_present: False,
    },
  )
}
