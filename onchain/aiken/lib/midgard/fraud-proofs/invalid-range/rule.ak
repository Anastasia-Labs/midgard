use aiken_design_patterns/validity_range_normalization.{
  Always, ClosedRange, FromNegInf, InvalidRange, NormalizedTimeRange, ToPosInf,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{
  ValidityIntervalExcludesBlockSlot, ValidityIntervalMalformed,
}

pub fn range_excludes_slot_v1(range: NormalizedTimeRange, slot: Int) -> Bool {
  when range is {
    ClosedRange { lower, upper } -> or {
        lower > slot,
        upper < slot,
      }
    FromNegInf { upper } -> upper < slot
    ToPosInf { lower } -> lower > slot
    Always -> False
    InvalidRange -> False
  }
}

pub fn terminal_contradiction_v1(
  subject: substrate.VerdictSubjectV1,
  range: NormalizedTimeRange,
  slot: Int,
) -> Bool {
  if subject.direction == substrate.direction_wrongful_acceptance {
    substrate.terminal_contradiction_v1(subject, or {
        range == InvalidRange,
        range_excludes_slot_v1(range, slot),
      })
  } else {
    let fault_holds = when substrate.rejection_reason_of_v1(subject) is {
      ValidityIntervalMalformed -> {
        expect _ = substrate.bind_exact_rejection_reason_v1(
          subject,
          ValidityIntervalMalformed,
        )
        range == InvalidRange
      }
      ValidityIntervalExcludesBlockSlot -> {
        expect _ = substrate.bind_exact_rejection_reason_v1(
          subject,
          ValidityIntervalExcludesBlockSlot,
        )
        range_excludes_slot_v1(range, slot)
      }
      _ -> fail @"invalidRange: typed rejection reason changed"
    }
    substrate.terminal_contradiction_v1(subject, fault_holds)
  }
}
