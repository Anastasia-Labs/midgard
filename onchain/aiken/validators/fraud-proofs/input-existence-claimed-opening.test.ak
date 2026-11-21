//// P-N7-1: the cost of reopening a forced leaf's claimed input in the
//// input-existence select step (step-02 of noInput and noReferenceInput).
////
//// A rejected leaf's resolution coordinate names a field and an index. When
//// that field is the one the step already opened, the claimed input is a read
//// of the same view; when it is the other field, the step opens it too; when
//// the coordinate names no input, nothing is opened. Each case has a control
//// row that builds the same fixture and L1 transaction without entering the
//// validator, so measured minus control is the step, at one item per field
//// and at the admissible spend-input count in both fields.

use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/transaction.{OutputReference, Transaction}
use fraud_proofs/no_input/step_02 as no_input_step_02
use fraud_proofs/no_reference_input/step_02 as no_reference_input_step_02
use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{BodyFieldOpening}
use midgard/fraud_proofs/input_existence_fixture_v1 as carriage_fixture
use midgard/fraud_proofs/input_existence_v1/types.{
  SelectArgsV1, SelectedInputStateV1, SourceStateV1,
} as input_existence_types
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/compact.{
  decode_forced_tx_compact_v1, encode_forced_tx_compact_v1,
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/types.{
  ForcedTxCompact, MidgardTxInput, NativeTxBodyCompact, TxCompactView,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/native_tx_field_access_v1.{
  Inline, encode_field_preimage, field_commitment,
  maximum_cardano_spend_redeemer_count, spend_input_stride,
}
use midgard/rejection_reason_v1 as reason
use midgard/validation_machine_tests/fixtures_phase_a.{
  empty_v1_transaction, submitted_source,
}

const admissible_count: Int = maximum_cardano_spend_redeemer_count

/// `count` field-`field` inputs, distinct across the two fields.
fn inputs_of(field: Int, count: Int) -> List<ByteArray> {
  list.map(
    list.range(0, count - 1),
    fn(n) {
      #"825820"
        |> bytearray.concat(bytearray.from_int_big_endian(0x41 + field, 32))
        |> bytearray.concat(#"19")
        |> bytearray.concat(bytearray.from_int_big_endian(n, 2))
    },
  )
}

fn input_at(field: Int, n: Int) -> MidgardTxInput {
  MidgardTxInput {
    tx_id: bytearray.from_int_big_endian(0x41 + field, 32),
    output_index: n,
  }
}

type Case {
  field: Int,
  count: Int,
  r: reason.RejectionReasonV1,
  claimed_opening: Option<Int>,
  claimed_input: Option<MidgardTxInput>,
}

type Built {
  datum: Option<ct.StepDatum<SourceStateV1>>,
  redeemer: ct.StepRedeemer<SelectArgsV1>,
  tx: Transaction,
  preimage_bytes: Int,
}

fn build(case: Case) -> Built {
  let preimages = [
    encode_field_preimage(inputs_of(0, case.count)),
    encode_field_preimage(inputs_of(1, case.count)),
  ]
  expect Some(spend) = list.at(preimages, 0)
  expect Some(reference) = list.at(preimages, 1)
  let base = decode_forced_tx_compact_v1(
    submitted_source(empty_v1_transaction()).compact_cbor,
  )
  let compact = ForcedTxCompact {
    ..base,
    body: NativeTxBodyCompact {
      ..base.body,
      spend_inputs_hash: field_commitment(spend),
      reference_inputs_hash: field_commitment(reference),
    },
  }
  let compact_cbor = encode_forced_tx_compact_v1(compact)
  let view = TxCompactView {
    body: compact.body,
    witness_set_hash: compact.witness_set_hash,
    validity_code: None,
  }
  let order_id = OutputReference {
    transaction_id: #"7777777777777777777777777777777777777777777777777777777777777777",
    output_index: 0,
  }
  let subject = substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: native_tx_id_for_version(
      1,
      encode_native_tx_body_compact(compact.body),
    ),
    source_key: cbor.serialise(order_id),
    rejection_reason: Some(case.r),
  }
  let claim = tf.thread_claim_v1(
    ledger_state.ForcedTransactionEventKey { tx_order_id: order_id },
    ledger_state.ForcedTransaction,
    #"0000000000000000000000000000000000000000000000000000000000000000",
    None,
  )
  let opening = fn(field) {
    BodyFieldOpening {
      native_tx_compact_cbor: compact_cbor,
      carriage: Inline {
        preimage: if field == 0 {
          spend
        } else {
          reference
        },
      },
    }
  }
  let expected: Data = SelectedInputStateV1 {
    subject,
    header: claim.header,
    compact: view,
    source_kind: case.field,
    input_index: case.count - 1,
    selected_input: Some(input_at(case.field, case.count - 1)),
    claimed_input: case.claimed_input,
    step_index: claim.event_to_step_membership.value.step_index,
  }
  Built {
    datum: Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          SourceStateV1 { subject, header: claim.header, compact: view },
        ),
      },
    ),
    redeemer: ct.Continue(
      SelectArgsV1 {
        input_index: 0,
        output_index: 0,
        inputs_opening: opening(case.field),
        selected_input_index: case.count - 1,
        claimed_inputs_opening: when case.claimed_opening is {
          Some(field) -> Some(opening(field))
          None -> None
        },
        event_membership: carriage_fixture.direct_carried_membership_v1(
          claim.event_to_step_membership,
          0,
        ),
      },
    ),
    tx: Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(#"01", expected),
      redeemers: [
        fixture.phas_withdraw_redeemer_entry(
          claim.event_to_step_membership.phas_root,
          cbor.serialise(claim.event_to_step_membership.key),
          cbor.serialise(claim.event_to_step_membership.value),
        ),
      ],
    },
    preimage_bytes: bytearray.length(spend),
  }
}

fn control(case: Case) -> Bool {
  let built = build(case)
  and {
    built.preimage_bytes >= case.count * spend_input_stride,
    list.length(built.tx.inputs) > 0,
  }
}

fn measured(case: Case) -> Bool {
  let built = build(case)
  if case.field == 0 {
    no_input_step_02.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.test_certificate_policy_id,
      built.datum,
      built.redeemer,
      fixture.own_out_ref(),
      built.tx,
    )
  } else {
    no_reference_input_step_02.main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.test_certificate_policy_id,
      built.datum,
      built.redeemer,
      fixture.own_out_ref(),
      built.tx,
    )
  }
}

/// The observed field's last input is claimed again: one opening.
fn same_field(field: Int, count: Int) -> Case {
  Case {
    field,
    count,
    r: reason.InputSpentOutputNonCanonical {
      source_kind: field,
      input_index: count - 1,
    },
    claimed_opening: None,
    claimed_input: Some(input_at(field, count - 1)),
  }
}

/// The other field's last input is claimed: a second opening.
fn other_field(field: Int, count: Int) -> Case {
  Case {
    field,
    count,
    r: reason.InputNotFound { source_kind: 1 - field, input_index: count - 1 },
    claimed_opening: Some(1 - field),
    claimed_input: Some(input_at(1 - field, count - 1)),
  }
}

/// The claimed coordinate names no field: no claimed opening.
fn out_of_domain(field: Int, count: Int) -> Case {
  Case {
    field,
    count,
    r: reason.InputNotFound { source_kind: 2, input_index: 0 },
    claimed_opening: None,
    claimed_input: None,
  }
}

test n7_no_input_step_02_same_field_control_at_one() {
  control(same_field(0, 1))
}

test n7_no_input_step_02_same_field_at_one() {
  measured(same_field(0, 1))
}

test n7_no_input_step_02_other_field_control_at_one() {
  control(other_field(0, 1))
}

test n7_no_input_step_02_other_field_at_one() {
  measured(other_field(0, 1))
}

test n7_no_input_step_02_same_field_control_at_maximum() {
  control(same_field(0, admissible_count))
}

test n7_no_input_step_02_same_field_at_maximum() {
  measured(same_field(0, admissible_count))
}

test n7_no_input_step_02_other_field_control_at_maximum() {
  control(other_field(0, admissible_count))
}

test n7_no_input_step_02_other_field_at_maximum() {
  measured(other_field(0, admissible_count))
}

test n7_no_input_step_02_out_of_domain_control_at_maximum() {
  control(out_of_domain(0, admissible_count))
}

test n7_no_input_step_02_out_of_domain_at_maximum() {
  measured(out_of_domain(0, admissible_count))
}

test n7_no_reference_input_step_02_same_field_control_at_maximum() {
  control(same_field(1, admissible_count))
}

test n7_no_reference_input_step_02_same_field_at_maximum() {
  measured(same_field(1, admissible_count))
}

test n7_no_reference_input_step_02_other_field_control_at_maximum() {
  control(other_field(1, admissible_count))
}

test n7_no_reference_input_step_02_other_field_at_maximum() {
  measured(other_field(1, admissible_count))
}

test n7_no_input_step_02_other_field_without_opening_is_refused() fail {
  measured(Case { ..other_field(0, 1), claimed_opening: None })
}
