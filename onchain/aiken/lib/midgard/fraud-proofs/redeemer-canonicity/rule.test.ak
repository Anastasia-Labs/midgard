use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/redeemer_canonicity/rule
use midgard/rejection_reason_v1.{RedeemerMalformed, RedeemerMissing}

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const witness_set_hash =
  #"202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f"

fn bind(subject: substrate.VerdictSubjectV1, index: Int) {
  rule.bind_redeemer_v1(subject, witness_set_hash, index)
}

fn accepted() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

fn rejected(index: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"01",
    rejection_reason: Some(RedeemerMalformed { redeemer_index: index }),
  }
}

test redeemer_canonicity_binds_exact_forced_coordinate() {
  bind(rejected(7), 7).redeemer_index == 7
}

test redeemer_canonicity_refuses_coordinate_substitution() fail {
  bind(rejected(7), 8).redeemer_index == 8
}

test redeemer_canonicity_refuses_other_typed_reason() fail {
  bind(
    substrate.VerdictSubjectV1 {
      ..rejected(0),
      rejection_reason: Some(
        RedeemerMissing { purpose_kind: 0, purpose_index: 0 },
      ),
    },
    0,
  ).redeemer_index == 0
}

test redeemer_canonicity_refuses_negative_coordinate() fail {
  bind(accepted(), -1).redeemer_index == -1
}

test redeemer_canonicity_wrongful_acceptance_convicts_malformed() {
  rule.terminal_contradiction_v1(
    rule.TerminalStateV1 { bound: bind(accepted(), 0), canonical: False },
  )
}

test redeemer_canonicity_wrongful_rejection_convicts_canonical() {
  rule.terminal_contradiction_v1(
    rule.TerminalStateV1 { bound: bind(rejected(0), 0), canonical: True },
  )
}

test redeemer_canonicity_honest_acceptance_refuses_canonical() fail {
  rule.terminal_contradiction_v1(
    rule.TerminalStateV1 { bound: bind(accepted(), 0), canonical: True },
  )
}

test redeemer_canonicity_honest_rejection_refuses_malformed() fail {
  rule.terminal_contradiction_v1(
    rule.TerminalStateV1 { bound: bind(rejected(0), 0), canonical: False },
  )
}

test redeemer_canonicity_total_decoder_accepts_canonical_item() {
  rule.item_is_canonical_v1(#"8400004100820102")
}

test redeemer_canonicity_total_decoder_rejects_nonminimal_data() {
  !rule.item_is_canonical_v1(#"840000421800820102")
}

test redeemer_canonicity_total_decoder_rejects_malformed_envelope() {
  !rule.item_is_canonical_v1(#"8300004100820102")
}

test redeemer_canonicity_total_decoder_rejects_trailing_bytes() {
  !rule.item_is_canonical_v1(#"840000410082010200")
}

test redeemer_canonicity_total_decoder_rejects_unsupported_purpose() {
  !rule.item_is_canonical_v1(#"8402004100820102")
}

test redeemer_canonicity_bound_abi_vector_is_stable() {
  let bound = bind(accepted(), 0)
  rule.encode_bound_v1(bound) == #"d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff5820202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f00ff"
}

test redeemer_canonicity_terminal_abi_vector_is_stable() {
  let bound = bind(accepted(), 0)
  rule.encode_terminal_v1(rule.TerminalStateV1 { bound, canonical: True }) == #"d8799fd8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff5820202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f00ffd87a80ff"
}
