use midgard/computation_thread as ct
use midgard/fraud_proofs/canonical_decodability/rule.{CommittedFieldClaimV1}
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/ledger_state
use midgard/transition_trace

pub type State {
  BoundSource { subject: VerdictSubjectV1, source_cbor: ByteArray }
  PendingForced { direction: Int }
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  AuthenticateAccepted {
    input_index: Int,
    output_index: Int,
    claim: CommittedFieldClaimV1,
  }
  AuthenticateForced {
    input_index: Int,
    output_index: Int,
    header: ledger_state.HeaderV1,
    membership: transition_trace.RootMembershipProof<
      ledger_state.TxOrderId,
      ledger_state.ForcedInclusionTxV1,
    >,
    claim: CommittedFieldClaimV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
