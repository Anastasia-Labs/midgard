use midgard/fraud_proofs/direct_script_derivation/bind_state.{EventSelectedV1}
use midgard/fraud_proofs/execution_source_script_decoding/terminal.{convicts_v1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/rejection_reason_v1.{
  ExecutionNativeScriptDepthLimit, ExecutionNativeScriptFalse,
  ExecutionNativeScriptMalformed, ExecutionNativeScriptNodeLimit,
  RejectionReasonV1, ScriptIntegrityHashMismatch, UnusedRedeemer,
}

fn selected(
  direction: Int,
  source_kind: Int,
  rejection_reason: Option<RejectionReasonV1>,
) -> EventSelectedV1 {
  let block = fixture.challenged_block_v1()
  EventSelectedV1 {
    subject: VerdictSubjectV1 {
      version: 1,
      direction,
      source_kind,
      transaction_id: block.tx_id,
      source_key: if source_kind == 1 {
        #"00"
      } else {
        #""
      },
      rejection_reason,
    },
    header: block.header,
    compact_cbor: block.compact_cbor,
    step_index: 0,
  }
}

fn rejected(reason: RejectionReasonV1) -> Bool {
  convicts_v1(selected(1, 1, Some(reason)))
}

// ## Wrongful rejection, forced source (R-F): every recorded arm convicts

test rejection_recording_malformed_convicts() {
  rejected(ExecutionNativeScriptMalformed { purpose_kind: 0, purpose_index: 0 })
}

test rejection_recording_node_limit_convicts() {
  rejected(ExecutionNativeScriptNodeLimit { purpose_kind: 1, purpose_index: 7 })
}

test rejection_recording_depth_limit_convicts() {
  rejected(
    ExecutionNativeScriptDepthLimit { purpose_kind: 3, purpose_index: 255 },
  )
}

test every_recorded_coordinate_convicts() {
  and {
    rejected(
      ExecutionNativeScriptMalformed { purpose_kind: 2, purpose_index: 0 },
    ),
    rejected(
      ExecutionNativeScriptNodeLimit { purpose_kind: 0, purpose_index: 1024 },
    ),
    rejected(
      ExecutionNativeScriptDepthLimit { purpose_kind: 1, purpose_index: 0 },
    ),
  }
}

// ## Leaves this family never convicts

test rejection_recording_a_false_native_script_does_not_convict() {
  !rejected(ExecutionNativeScriptFalse { purpose_kind: 0, purpose_index: 0 })
}

test rejection_recording_another_phase_does_not_convict() {
  and {
    !rejected(UnusedRedeemer { redeemer_index: 0 }),
    !rejected(ScriptIntegrityHashMismatch),
  }
}

test accepted_normal_leaf_does_not_convict() {
  !convicts_v1(selected(0, 0, None))
}

test accepted_forced_leaf_does_not_convict() {
  !convicts_v1(selected(0, 1, None))
}

// ## The subject must still be a canonical rejection

test acceptance_carrying_a_recorded_arm_is_refused() fail {
  convicts_v1(
    selected(
      0,
      1,
      Some(ExecutionNativeScriptMalformed { purpose_kind: 0, purpose_index: 0 }),
    ),
  )
}

test normal_source_carrying_a_recorded_arm_is_refused() fail {
  convicts_v1(
    selected(
      1,
      0,
      Some(ExecutionNativeScriptNodeLimit { purpose_kind: 0, purpose_index: 0 }),
    ),
  )
}
