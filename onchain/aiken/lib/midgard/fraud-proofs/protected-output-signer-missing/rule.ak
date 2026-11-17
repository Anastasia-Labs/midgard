//// Narrow rule for `ProtectedOutputSignerMissing`.

use aiken/cbor
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/signer_witness_scan_v1/state.{
  VerdictV1, direct_verdict,
}
use midgard/rejection_reason_v1.{ProtectedOutputSignerMissing}

pub type BoundOutputV1 {
  subject: substrate.VerdictSubjectV1,
  output_index: Int,
}

/// `subject` is the value `open_subject_v1` returned in this Step 01 run, so it
/// is canonical by construction (see `substrate.opened_rejection_reason_v1`).
///
/// `output_index` may be negative: such a coordinate names no output, and
/// step 02 sends it to `direct_verdict_v1`, which accepts it only for a
/// wrongful forced rejection.
pub fn bind_output_v1(
  subject: substrate.VerdictSubjectV1,
  output_index: Int,
) -> BoundOutputV1 {
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    Some(
      substrate.bind_opened_rejection_reason_v1(
        subject,
        ProtectedOutputSignerMissing { output_index },
      ),
    )
  } else {
    None
  }
  BoundOutputV1 { subject, output_index }
}

/// The verdict step 02 writes straight to the terminal when canonical
/// validation requires no signer at the bound coordinate. Only a wrongful
/// forced rejection can be contradicted this way: an accepted subject has no
/// fault to close on, so the direct exit is refused for it.
pub fn direct_verdict_v1(bound: BoundOutputV1) -> VerdictV1 {
  expect bound.subject.direction == substrate.direction_wrongful_rejection
  direct_verdict(bound.subject, False, False)
}

pub fn encode_bound_v1(value: BoundOutputV1) -> ByteArray {
  cbor.serialise(value)
}
