//// Semantic-arm selectors for the family's authenticated reducer: the exact
//// native proof-source verifier feeding `authenticated_length_evidence_v1`
//// over real committed structures, then the shared terminal polarity. Every
//// refusal below is the on-chain one a mutated lifecycle transaction meets.

use aiken/primitive/bytearray
use midgard/fraud_proofs/canonical_decodability/rule.{
  BodyFieldClaim, CommittedFieldClaimV1, WitnessFieldClaim,
} as claim_rule
use midgard/fraud_proofs/field_preimage_length_mismatch/rule.{
  LengthEvidenceV1, authenticated_length_evidence_v1, decisive_fault_holds_v1,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_field_preimage_lengths_v1,
  encode_native_tx_witness_set_compact, verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/preimages as native_preimages
use midgard/fraud_proofs/native_tx/types.{
  NativeTxFieldPreimageLengthsV1, NativeTxWitnessSetCompact,
  VerifiedTxSourceView,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/native_tx_field_access_v1 as field_access
use midgard/rejection_reason_v1.{FieldPreimageLengthMismatch}

const spend_inputs_field: Int = 0

const redeemers_field: Int = 8

fn block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn honest_lengths(
  b: fixture.NativeBlockFixtureV1,
) -> NativeTxFieldPreimageLengthsV1 {
  fixture.field_preimage_lengths_v1(b.tx)
}

/// The operator-declared vector with field 0 overstated by one byte: the
/// accepted-invalid fixture, and the leaf an honest forced rejection carries.
fn mismatched_lengths(
  b: fixture.NativeBlockFixtureV1,
) -> NativeTxFieldPreimageLengthsV1 {
  let lengths = honest_lengths(b)
  NativeTxFieldPreimageLengthsV1 {
    ..lengths,
    spend_inputs: lengths.spend_inputs + 1,
  }
}

fn lengths_cbor(lengths: NativeTxFieldPreimageLengthsV1) -> ByteArray {
  encode_native_tx_field_preimage_lengths_v1(lengths)
}

fn verified_source(
  b: fixture.NativeBlockFixtureV1,
  lengths: ByteArray,
) -> VerifiedTxSourceView {
  let Pair(verified, _) = verify_native_tx_proof_source_v1(
    b.tx_id,
    b.compact_cbor,
    encode_native_tx_witness_set_compact(b.witness_set_compact),
    lengths,
  )
  verified
}

fn spend_inputs_preimage(b: fixture.NativeBlockFixtureV1) -> ByteArray {
  native_preimages.encode_input_preimage(b.tx.body.inputs)
}

fn redeemers_preimage(b: fixture.NativeBlockFixtureV1) -> ByteArray {
  native_preimages.encode_redeemer_witness_preimage(
    b.tx.witness_set.redeemer_tx_wits,
  )
}

fn inline_body_claim(
  field_index: Int,
  preimage: ByteArray,
) -> CommittedFieldClaimV1 {
  BodyFieldClaim { field_index, carriage: field_access.Inline { preimage } }
}

fn inline_witness_claim(
  field_index: Int,
  witness_set: NativeTxWitnessSetCompact,
  preimage: ByteArray,
) -> CommittedFieldClaimV1 {
  WitnessFieldClaim {
    field_index,
    witness_set,
    carriage: field_access.Inline { preimage },
  }
}

fn evidence(
  b: fixture.NativeBlockFixtureV1,
  lengths: NativeTxFieldPreimageLengthsV1,
  claim: CommittedFieldClaimV1,
) -> LengthEvidenceV1 {
  let cbor = lengths_cbor(lengths)
  authenticated_length_evidence_v1(
    verified_source(b, cbor),
    cbor,
    claim,
    [],
    fixture.test_certificate_policy_id,
  )
}

fn accepted(b: fixture.NativeBlockFixtureV1) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: b.tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

fn rejected(
  b: fixture.NativeBlockFixtureV1,
  field_index: Int,
) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: b.tx_id,
    source_key: #"d8799f5820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f00ff",
    rejection_reason: Some(FieldPreimageLengthMismatch { field_index }),
  }
}

fn terminal(subject: substrate.VerdictSubjectV1, e: LengthEvidenceV1) -> Bool {
  substrate.terminal_contradiction_v1(
    subject,
    decisive_fault_holds_v1(e.field_index, e.declared_length, e.actual_length),
  )
}

/// The family's forced bind: the exact typed reason's coordinate must be the
/// authenticated field, the same `expect` the forced authenticator runs.
fn bind_forced_coordinate(
  subject: substrate.VerdictSubjectV1,
  e: LengthEvidenceV1,
) -> LengthEvidenceV1 {
  expect FieldPreimageLengthMismatch { field_index } = substrate.rejection_reason_of_v1(
    subject,
  )
  expect field_index == e.field_index
  e
}

// ## Both successful directions

test field_preimage_length_authenticated_acceptance_convicts_overstated_body_field() {
  let b = block()
  let e = evidence(
    b,
    mismatched_lengths(b),
    inline_body_claim(spend_inputs_field, spend_inputs_preimage(b)),
  )
  and {
    e.field_index == spend_inputs_field,
    e.declared_length == e.actual_length + 1,
    e.actual_length == bytearray.length(spend_inputs_preimage(b)),
    terminal(accepted(b), e),
  }
}

test field_preimage_length_authenticated_rejection_contradicts_exact_reason() {
  let b = block()
  let e = bind_forced_coordinate(
    rejected(b, spend_inputs_field),
    evidence(
      b,
      honest_lengths(b),
      inline_body_claim(spend_inputs_field, spend_inputs_preimage(b)),
    ),
  )
  and {
    e.declared_length == e.actual_length,
    terminal(rejected(b, spend_inputs_field), e),
  }
}

test field_preimage_length_authenticated_witness_set_field_reaches_terminal() {
  let b = block()
  let lengths = honest_lengths(b)
  let e = evidence(
    b,
    NativeTxFieldPreimageLengthsV1 {
      ..lengths,
      redeemers: lengths.redeemers + 7,
    },
    inline_witness_claim(
      redeemers_field,
      b.witness_set_compact,
      redeemers_preimage(b),
    ),
  )
  and {
    e.field_index == redeemers_field,
    e.declared_length == e.actual_length + 7,
    terminal(accepted(b), e),
  }
}

// ## Honest refusals: the reducer authenticates, the terminal polarity refuses

test field_preimage_length_authenticated_honest_acceptance_is_refused() {
  let b = block()
  let e = evidence(
    b,
    honest_lengths(b),
    inline_body_claim(spend_inputs_field, spend_inputs_preimage(b)),
  )
  !terminal(accepted(b), e)
}

test field_preimage_length_authenticated_honest_rejection_is_refused() {
  let b = block()
  let e = bind_forced_coordinate(
    rejected(b, spend_inputs_field),
    evidence(
      b,
      mismatched_lengths(b),
      inline_body_claim(spend_inputs_field, spend_inputs_preimage(b)),
    ),
  )
  !terminal(rejected(b, spend_inputs_field), e)
}

// ## Reason and coordinate mutation

test field_preimage_length_authenticated_rejects_mutated_reason_coordinate() fail {
  let b = block()
  let e = bind_forced_coordinate(
    rejected(b, 1),
    evidence(
      b,
      honest_lengths(b),
      inline_body_claim(spend_inputs_field, spend_inputs_preimage(b)),
    ),
  )
  e.field_index == spend_inputs_field
}

test field_preimage_length_authenticated_rejects_other_reason_constructor() fail {
  let b = block()
  let subject = substrate.VerdictSubjectV1 {
    ..rejected(b, spend_inputs_field),
    rejection_reason: Some(
      rejection_reason_v1.FieldItemWidthIllegal {
        field_index: spend_inputs_field,
        item_index: 0,
      },
    ),
  }
  let e = bind_forced_coordinate(
    subject,
    evidence(
      b,
      honest_lengths(b),
      inline_body_claim(spend_inputs_field, spend_inputs_preimage(b)),
    ),
  )
  e.field_index == spend_inputs_field
}

// ## Out-of-range coordinates

test field_preimage_length_authenticated_rejects_coordinate_nine() fail {
  let b = block()
  let e = evidence(
    b,
    honest_lengths(b),
    inline_witness_claim(9, b.witness_set_compact, redeemers_preimage(b)),
  )
  e.field_index == 9
}

test field_preimage_length_authenticated_rejects_negative_coordinate() fail {
  let b = block()
  let e = evidence(
    b,
    honest_lengths(b),
    inline_body_claim(-1, spend_inputs_preimage(b)),
  )
  e.field_index == -1
}

test field_preimage_length_authenticated_rejects_body_claim_on_witness_field() fail {
  let b = block()
  let e = evidence(
    b,
    honest_lengths(b),
    inline_body_claim(redeemers_field, redeemers_preimage(b)),
  )
  e.field_index == redeemers_field
}

// ## Substituted source

test field_preimage_length_authenticated_rejects_non_canonical_lengths() fail {
  let b = block()
  let padded = bytearray.concat(lengths_cbor(honest_lengths(b)), #"00")
  let e = authenticated_length_evidence_v1(
    verified_source(b, padded),
    padded,
    inline_body_claim(spend_inputs_field, spend_inputs_preimage(b)),
    [],
    fixture.test_certificate_policy_id,
  )
  e.field_index == spend_inputs_field
}

test field_preimage_length_authenticated_rejects_compact_of_another_transaction() fail {
  let b = block()
  let other = fixture.committed_field_preimage_block_fixture_v1(
    b,
    spend_inputs_field,
    bytearray.concat(spend_inputs_preimage(b), #"00"),
  )
  let cbor = lengths_cbor(honest_lengths(b))
  let Pair(verified, _) = verify_native_tx_proof_source_v1(
    b.tx_id,
    other.compact_cbor,
    encode_native_tx_witness_set_compact(b.witness_set_compact),
    cbor,
  )
  let e = authenticated_length_evidence_v1(
    verified,
    cbor,
    inline_body_claim(spend_inputs_field, spend_inputs_preimage(b)),
    [],
    fixture.test_certificate_policy_id,
  )
  e.field_index == spend_inputs_field
}

// ## Substituted bytes and descriptor

test field_preimage_length_authenticated_rejects_padded_preimage() fail {
  let b = block()
  // The prover "repairs" the mismatch by padding the bytes to the declared
  // length; the commitment refuses it.
  let e = evidence(
    b,
    mismatched_lengths(b),
    inline_body_claim(
      spend_inputs_field,
      bytearray.concat(spend_inputs_preimage(b), #"00"),
    ),
  )
  e.declared_length == e.actual_length
}

test field_preimage_length_authenticated_rejects_substituted_witness_set() fail {
  let b = block()
  // A witness set that does not re-derive to the anchored `witness_set_hash`
  // cannot open any witness-set field, even with that field's real bytes.
  let forged = NativeTxWitnessSetCompact {
    ..b.witness_set_compact,
    addr_tx_wits_hash: #"1111111111111111111111111111111111111111111111111111111111111111",
  }
  let e = evidence(
    b,
    honest_lengths(b),
    inline_witness_claim(redeemers_field, forged, redeemers_preimage(b)),
  )
  e.field_index == redeemers_field
}

// ## Maximum shape and the adjacent over-bound refusal, certified carriage

/// One item whose §5.1 envelope (`81` + `59 LLLL` + payload) fills the field
/// to exactly `total` bytes.
fn single_item_preimage(total: Int) -> ByteArray {
  field_access.encode_field_preimage(
    [fixture.repeated_bytes_v1(#"a5", total - 4)],
  )
}

fn certified_evidence(total: Int) -> LengthEvidenceV1 {
  let preimage = single_item_preimage(total)
  expect bytearray.length(preimage) == total
  let b = fixture.committed_field_preimage_block_fixture_v1(
    block(),
    spend_inputs_field,
    preimage,
  )
  let cbor = lengths_cbor(honest_lengths(block()))
  authenticated_length_evidence_v1(
    verified_source(b, cbor),
    cbor,
    BodyFieldClaim {
      field_index: spend_inputs_field,
      carriage: fixture.certified_carriage_v1(preimage),
    },
    fixture.certified_reference_inputs_v1(b.tx_id, spend_inputs_field, preimage),
    fixture.test_certificate_policy_id,
  )
}

test field_preimage_length_authenticated_maximum_certified_field_convicts() {
  let e = certified_evidence(field_access.max_transaction_aggregate_field_bytes)
  and {
    e.actual_length == field_access.max_transaction_aggregate_field_bytes,
    e.declared_length != e.actual_length,
    terminal(accepted(block()), e),
  }
}

test field_preimage_length_authenticated_refuses_adjacent_over_bound_field() fail {
  let e = certified_evidence(
    field_access.max_transaction_aggregate_field_bytes + 1,
  )
  e.actual_length > field_access.max_transaction_aggregate_field_bytes
}

test field_preimage_length_authenticated_rejects_certificate_of_another_field() fail {
  let preimage = single_item_preimage(
    field_access.max_transaction_aggregate_field_bytes,
  )
  let b = fixture.committed_field_preimage_block_fixture_v1(
    block(),
    spend_inputs_field,
    preimage,
  )
  let cbor = lengths_cbor(honest_lengths(block()))
  let e = authenticated_length_evidence_v1(
    verified_source(b, cbor),
    cbor,
    BodyFieldClaim {
      field_index: spend_inputs_field,
      carriage: fixture.certified_carriage_v1(preimage),
    },
    fixture.certified_reference_inputs_v1(b.tx_id, 2, preimage),
    fixture.test_certificate_policy_id,
  )
  e.field_index == spend_inputs_field
}

test field_preimage_length_authenticated_rejects_reordered_chunks() fail {
  let preimage = single_item_preimage(
    field_access.max_transaction_aggregate_field_bytes,
  )
  let b = fixture.committed_field_preimage_block_fixture_v1(
    block(),
    spend_inputs_field,
    preimage,
  )
  let cbor = lengths_cbor(honest_lengths(block()))
  let e = authenticated_length_evidence_v1(
    verified_source(b, cbor),
    cbor,
    BodyFieldClaim {
      field_index: spend_inputs_field,
      carriage: field_access.Certified {
        cert_ref_input_index: 0,
        chunk_ref_input_indices: [2, 1, 3],
      },
    },
    fixture.certified_reference_inputs_v1(b.tx_id, spend_inputs_field, preimage),
    fixture.test_certificate_policy_id,
  )
  e.field_index == spend_inputs_field
}
