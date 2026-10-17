use cardano/assets.{PolicyId}
use cardano/transaction.{Input}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, FieldOpeningV1, opened_field_view,
}
use midgard/fraud_proofs/first_fault_v1
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/native_tx_field_access_v1.{FieldViewV1}
use midgard/native_tx_machine_walk_v1.{spend_input_at, spend_input_count}
use midgard/rejection_reason_v1.{InputNotFound}

pub fn selected_index_v1(
  subject: substrate.VerdictSubjectV1,
  field_index: Int,
  chosen_index: Int,
) -> Int {
  expect substrate.subject_is_canonical_v1(subject)
  expect field_index == 0 || field_index == 1
  expect
    chosen_index >= 0 || subject.rejection_reason == Some(
      InputNotFound { source_kind: field_index, input_index: chosen_index },
    )
  chosen_index
}

fn input_at_or_none(view: FieldViewV1, index: Int) -> Option<MidgardTxInput> {
  if index >= 0 && index < spend_input_count(view) {
    Some(spend_input_at(view, index))
  } else {
    None
  }
}

pub fn selected_input_v1(
  subject: substrate.VerdictSubjectV1,
  field_index: Int,
  view: FieldViewV1,
  index: Int,
) -> Option<MidgardTxInput> {
  let selected = input_at_or_none(view, index)
  expect
    selected != None || subject.rejection_reason == Some(
      InputNotFound { source_kind: field_index, input_index: index },
    )
  selected
}

/// Reopen the *claimed* original field/index independently of the observed
/// original field/index. If both name one field, reuse that authenticated view.
/// No extra opening is accepted when it would not be read.
pub fn claimed_input_v1(
  subject: substrate.VerdictSubjectV1,
  observed_field: Int,
  observed_view: FieldViewV1,
  claimed_opening: Option<FieldOpeningV1>,
  reference_inputs: List<Input>,
  certificate_policy: PolicyId,
) -> Option<MidgardTxInput> {
  let coordinate = when subject.rejection_reason is {
    None -> None
    Some(r) -> first_fault_v1.resolution_coordinate_of_reason_v1(r)
  }
  when coordinate is {
    None -> {
      expect claimed_opening == None
      None
    }
    Some(Pair(kind, index)) ->
      if kind < 0 || kind > 1 || index < 0 {
        expect claimed_opening == None
        None
      } else if kind == observed_field {
        expect claimed_opening == None
        input_at_or_none(observed_view, index)
      } else {
        expect Some(opening) = claimed_opening
        let view = opened_field_view(
          opening,
          BodyAnchor {
            tx_id: subject.transaction_id,
            source_kind: subject.source_kind,
          },
          kind,
          reference_inputs,
          certificate_policy,
        )
        input_at_or_none(view, index)
      }
  }
}
