use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1} as dd_types
use midgard/fraud_proofs/execution_native_script_invalid/terminal.{convicts_v1}
use midgard/fraud_proofs/execution_native_script_invalid/types.{
  Evaluated, FactV1, FinalV1, NoExecution,
}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/rejection_reason_v1.{
  ExecutionNativeScriptFalse, ExecutionNativeScriptMalformed,
  PlutusExecutionFailed, RejectionReasonV1, ScriptIntegrityHashMismatch,
  UnusedRedeemer,
}

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

fn at(kind: Int, index: Int) -> PurposeCoordinateV1 {
  PurposeCoordinateV1 { kind, index }
}

fn final(
  direction: Int,
  source_kind: Int,
  reason: Option<RejectionReasonV1>,
  fact: FactV1,
) -> FinalV1 {
  FinalV1 {
    bound: BoundV1 {
      subject: VerdictSubjectV1 {
        version: 1,
        direction,
        source_kind,
        transaction_id: root,
        source_key: if source_kind == 1 {
          #"00"
        } else {
          #""
        },
        rejection_reason: reason,
      },
      compact_cbor: #"",
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: root,
    },
    fact,
  }
}

fn accepted_normal(fact: FactV1) -> Bool {
  convicts_v1(final(0, 0, None, fact))
}

fn accepted_forced(fact: FactV1) -> Bool {
  convicts_v1(final(0, 1, None, fact))
}

fn rejected(reason: RejectionReasonV1, fact: FactV1) -> Bool {
  convicts_v1(final(1, 1, Some(reason), fact))
}

fn recorded(kind: Int, index: Int) -> RejectionReasonV1 {
  ExecutionNativeScriptFalse { purpose_kind: kind, purpose_index: index }
}

// ## Wrongful acceptance (A-N, A-F): a false native script convicts

test accepted_normal_leaf_with_a_false_native_script_convicts() {
  and {
    accepted_normal(Evaluated { coordinate: at(0, 0), satisfied: False }),
    accepted_normal(Evaluated { coordinate: at(3, 4), satisfied: False }),
  }
}

test accepted_forced_leaf_with_a_false_native_script_convicts() {
  accepted_forced(Evaluated { coordinate: at(1, 2), satisfied: False })
}

test accepted_leaf_with_a_true_or_absent_native_script_does_not_convict() {
  and {
    !accepted_normal(Evaluated { coordinate: at(0, 0), satisfied: True }),
    !accepted_normal(NoExecution { coordinate: at(0, 0) }),
    !accepted_forced(Evaluated { coordinate: at(2, 0), satisfied: True }),
    !accepted_forced(NoExecution { coordinate: at(2, 0) }),
  }
}

// ## Wrongful rejection at the recorded coordinate (R-F)

test rejection_whose_native_script_holds_convicts() {
  rejected(recorded(0, 1), Evaluated { coordinate: at(0, 1), satisfied: True })
}

test rejection_without_a_native_execution_convicts() {
  and {
    rejected(recorded(0, 1), NoExecution { coordinate: at(0, 1) }),
    rejected(recorded(2, 9), NoExecution { coordinate: at(2, 9) }),
  }
}

test rejection_at_an_invalid_coordinate_convicts() {
  rejected(recorded(7, 0), NoExecution { coordinate: at(7, 0) })
}

test correct_rejection_does_not_convict() {
  !rejected(
    recorded(0, 1),
    Evaluated { coordinate: at(0, 1), satisfied: False },
  )
}

// ## Another coordinate or a later recorded reason

test false_native_script_before_the_recorded_coordinate_convicts() {
  and {
    rejected(
      recorded(0, 5),
      Evaluated { coordinate: at(0, 2), satisfied: False },
    ),
    rejected(
      recorded(1, 0),
      Evaluated { coordinate: at(0, 9), satisfied: False },
    ),
  }
}

test false_native_script_after_the_recorded_coordinate_does_not_convict() {
  and {
    !rejected(
      recorded(0, 5),
      Evaluated { coordinate: at(0, 6), satisfied: False },
    ),
    !rejected(
      recorded(0, 5),
      Evaluated { coordinate: at(1, 0), satisfied: False },
    ),
  }
}

test other_coordinate_facts_that_are_not_false_do_not_convict() {
  and {
    !rejected(
      recorded(0, 5),
      Evaluated { coordinate: at(0, 2), satisfied: True },
    ),
    !rejected(recorded(0, 5), NoExecution { coordinate: at(0, 2) }),
  }
}

test later_phase_reason_with_a_false_native_script_convicts() {
  and {
    rejected(
      ScriptIntegrityHashMismatch,
      Evaluated { coordinate: at(0, 0), satisfied: False },
    ),
    rejected(
      PlutusExecutionFailed { purpose_kind: 0, purpose_index: 0 },
      Evaluated { coordinate: at(3, 1), satisfied: False },
    ),
  }
}

test earlier_phase_reason_does_not_convict() {
  !rejected(
    UnusedRedeemer { redeemer_index: 0 },
    Evaluated { coordinate: at(0, 0), satisfied: False },
  )
}

test same_phase_reason_at_or_before_the_position_does_not_convict() {
  and {
    !rejected(
      ExecutionNativeScriptMalformed { purpose_kind: 0, purpose_index: 1 },
      Evaluated { coordinate: at(0, 1), satisfied: False },
    ),
    rejected(
      ExecutionNativeScriptMalformed { purpose_kind: 0, purpose_index: 1 },
      Evaluated { coordinate: at(0, 0), satisfied: False },
    ),
  }
}

test acceptance_carrying_a_reason_is_refused() fail {
  convicts_v1(
    final(
      0,
      1,
      Some(recorded(0, 0)),
      Evaluated { coordinate: at(0, 0), satisfied: True },
    ),
  )
}
