use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/script_integrity_hash_mismatch/rule
use midgard/rejection_reason_v1.{
  OutputNonCanonical, ScriptIntegrityHashMismatch,
}

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const h32 = #"1111111111111111111111111111111111111111111111111111111111111111"

const redeemer_hash =
  #"1111111111111111111111111111111111111111111111111111111111111111"

fn subject(direction: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction,
    source_kind: if direction == substrate.direction_wrongful_acceptance {
      0
    } else {
      1
    },
    transaction_id: tx_id,
    source_key: if direction == substrate.direction_wrongful_acceptance {
      #""
    } else {
      #"01"
    },
    rejection_reason: if direction == substrate.direction_wrongful_acceptance {
      None
    } else {
      Some(ScriptIntegrityHashMismatch)
    },
  }
}

fn authenticated(
  direction: Int,
  bitmap: Int,
  committed_hash: ByteArray,
) -> rule.AuthenticatedIntegrityV1 {
  rule.AuthenticatedIntegrityV1 {
    bound: rule.bind_integrity_v1(subject(direction), h32, 1, committed_hash),
    prior_ledger_root: h32,
    redeemer_witness_hash: redeemer_hash,
    selected_language_bitmap: bitmap,
    execution_count: 2,
  }
}

fn decision(
  direction: Int,
  bitmap: Int,
  committed_hash: ByteArray,
) -> rule.DecisionV1 {
  let initial = rule.initialize_language_fold_v1(
    authenticated(direction, bitmap, committed_hash),
  )
  rule.decide_integrity_v1(
    rule.fold_next_language_v1(rule.fold_next_language_v1(initial)),
  )
}

test folds_empty_language_view_vector() {
  decision(0, 0, h32).expected_hash == #"01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53"
}

test folds_plutus_v3_language_view_vector() {
  decision(0, 1, h32).expected_hash == #"b37aff1034628d20a78418d2c2cf20796ac08c83467b6f8558e6cea542de4c26"
}

test folds_midgard_v1_language_view_vector() {
  decision(0, 2, h32).expected_hash == #"74c606847a40bed5f147cd6cca208fa1b0db6a130f7d86d7c478fd5838d6d878"
}

test folds_dual_language_view_vector() {
  decision(0, 3, h32).expected_hash == #"ceab70e09654e1f35ddf1566b15861055566fda429e056107de070c58c8b3991"
}

test mismatch_convicts_wrongful_acceptance() {
  rule.terminal_contradiction_v1(decision(0, 3, h32))
}

test equality_contradicts_wrongful_rejection() {
  rule.terminal_contradiction_v1(
    decision(
      1,
      1,
      #"b37aff1034628d20a78418d2c2cf20796ac08c83467b6f8558e6cea542de4c26",
    ),
  )
}

test equality_refuses_wrongful_acceptance() fail {
  rule.terminal_contradiction_v1(
    decision(
      0,
      2,
      #"74c606847a40bed5f147cd6cca208fa1b0db6a130f7d86d7c478fd5838d6d878",
    ),
  )
}

test mismatch_refuses_wrongful_rejection() fail {
  rule.terminal_contradiction_v1(decision(1, 0, h32))
}

test wrong_reason_constructor_refuses() fail {
  let bad = substrate.VerdictSubjectV1 {
    ..subject(1),
    rejection_reason: Some(OutputNonCanonical { output_index: 0 }),
  }
  rule.bind_integrity_v1(bad, h32, 1, h32).validation_trace_count == 1
}

test bitmap_above_canonical_domain_refuses() fail {
  rule.initialize_language_fold_v1(authenticated(0, 4, h32)).cursor == 0
}

test premature_fold_finalization_refuses() fail {
  let initial = rule.initialize_language_fold_v1(authenticated(0, 3, h32))
  rule.decide_integrity_v1(rule.fold_next_language_v1(initial)).expected_hash == h32
}
