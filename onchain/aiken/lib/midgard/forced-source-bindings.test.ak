use aiken/cbor
use aiken/merkle_patricia_forestry as mpf
use cardano/transaction.{OutputReference}
use midgard/fraud_proofs/native_tx/compact as codec
use midgard/ledger_state
use midgard/rejection_reason_v1 as reason
use midgard/transition_trace as roots
use midgard/user_events/tx_order_v1 as order
use midgard/validation_machine/machine_types
use midgard/validation_machine/shared
use midgard/validation_trace_v1 as vt

const compact: ByteArray =
  #"83018c582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c017201818582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582011111111111111111111111111111111111111111111111111111111111111115820222222222222222222222222222222222222222222222222222222222222222201582052408b440567f475c3eee21c9a8be91ed05a31e81e175c69fcaec5ccd187258c"

const witness: ByteArray =
  #"83582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"

const lengths: ByteArray = #"89010101010101010101"

const commitment: ByteArray =
  #"29f0c824abc1283bf70e2d1774261027d7de033e16b28dac14681c8ba9035c06"

const tx_id: ByteArray =
  #"c27090b9b20b3e6da5906b85969adad0a7c8a8f01a899925b16b93ad8be476f4"

fn source() -> ledger_state.ForcedTxProofSourceV1 {
  ledger_state.ForcedTxProofSourceV1 {
    compact_cbor: compact,
    witness_set_compact_cbor: witness,
    field_preimage_lengths_cbor: lengths,
  }
}

fn event() -> ledger_state.TxOrderEventV1 {
  ledger_state.TxOrderEventV1 {
    id: OutputReference { transaction_id: tx_id, output_index: 0 },
    tx: ledger_state.TxOrderPayloadV1 {
      tx_id,
      transaction_commitment: commitment,
      submitted_source: source(),
    },
  }
}

fn leaf(verdict: reason.OperatorVerdictV1) -> ledger_state.ForcedInclusionTxV1 {
  order.forced_inclusion_key_value(event(), verdict, fn(_, value) { value })
}

fn membership(
  value: ledger_state.ForcedInclusionTxV1,
) -> roots.RootMembershipProof<
  OutputReference,
  ledger_state.ForcedInclusionTxV1,
> {
  let key = event().id
  let phas_root = mpf.root(
    mpf.insert(mpf.empty, cbor.serialise(key), cbor.serialise(value), []),
  )
  roots.RootMembershipProof {
    domain: roots.ForcedTransactionsV1RootDomain,
    root: roots.commit_counted_root(
      roots.ForcedTransactionsV1RootDomain,
      phas_root,
      1,
    ),
    phas_root,
    count: 1,
    key,
    value,
    proof: [],
  }
}

fn authenticated(
  proof: roots.RootMembershipProof<
    OutputReference,
    ledger_state.ForcedInclusionTxV1,
  >,
  key: OutputReference,
  value: ledger_state.ForcedInclusionTxV1,
) -> Bool {
  roots.verify_root_membership_with_bytes(
    proof,
    roots.ForcedTransactionsV1RootDomain,
    proof.root,
    1,
    cbor.serialise(key),
    cbor.serialise(value),
  )
}

test forced_source_binds_both_verdicts() {
  let accepted = leaf(reason.ForcedTxValid)
  let rejected = leaf(reason.ForcedTxInvalid { reason: reason.EmptyInputs })
  and {
    accepted.submitted_source == rejected.submitted_source,
    accepted.tx_id == rejected.tx_id,
    accepted != rejected,
    authenticated(membership(accepted), event().id, accepted),
    authenticated(membership(rejected), event().id, rejected),
    codec.forced_tx_proof_commitment_v1(compact, witness, lengths) == commitment,
  }
}

test forced_source_rejects_foreign_order() {
  let value = leaf(reason.ForcedTxValid)
  let foreign_order = OutputReference { ..event().id, output_index: 1 }
  !authenticated(membership(value), foreign_order, value)
}

test forced_source_rejects_foreign_witnesses() {
  let value = leaf(reason.ForcedTxValid)
  let changed = ledger_state.ForcedInclusionTxV1 {
    ..value,
    submitted_source: ledger_state.ForcedTxProofSourceV1 {
      ..source(),
      witness_set_compact_cbor: #"80",
    },
  }
  !authenticated(membership(value), event().id, changed)
}

test forced_source_binds_reason_subject() {
  let value = leaf(
    reason.ForcedTxInvalid {
      reason: reason.PlutusExecutionFailed { execution_index: 0 },
    },
  )
  let changed = leaf(
    reason.ForcedTxInvalid {
      reason: reason.PlutusExecutionFailed { execution_index: 1 },
    },
  )
  and {
    authenticated(membership(value), event().id, value),
    !authenticated(membership(value), event().id, changed),
  }
}

test forced_source_kind_is_authenticated() fail {
  expect _view = codec.decode_tx_compact_view_v1(vt.Normal, compact)
}

test forced_source_rejection_preserves_claimed_delta() {
  let pre = vt.ValidationMachineStateV1 {
    machine_version: 1,
    event_key_hash: tx_id,
    transaction_id: tx_id,
    transaction_commitment: commitment,
    validation_context_hash: tx_id,
    source_kind: vt.Forced,
    prior_ledger_root: tx_id,
    phase: vt.CanonicalDecode,
    program_counter: 0,
    work_root: vt.hash_work_witness(vt.CanonicalDecode, 0, #"80"),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: vt.Pending,
    rejection_code_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
    ledger_delta_root: commitment,
  }
  let post = vt.ValidationMachineStateV1 {
    ..pre,
    phase: vt.Terminal,
    program_counter: 1,
    verdict: vt.Rejected,
    rejection_code_hash: vt.hash_rejection_code(reason.reject_min_fee),
    work_root: vt.hash_work_witness(
      vt.Terminal,
      1,
      shared.encode_terminal_rejection_witness(
        reason.reject_min_fee,
        pre.prior_ledger_root,
      ),
    ),
  }
  let witness = machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: #"80",
    claimed_successor: post,
  }
  and {
    shared.structural_transition_is_valid(pre, witness),
    shared.rejected_successor_is_exact(pre, post, reason.reject_min_fee),
    !shared.structural_transition_is_valid(
      pre,
      machine_types.ValidationOneStepWitnessV1 {
        ..witness,
        claimed_successor: vt.ValidationMachineStateV1 {
          ..post,
          ledger_delta_root: tx_id,
        },
      },
    ),
  }
}
