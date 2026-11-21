//// The two withdraw-zero verifiers of the phase-11 output step. The step's
//// yield, `accepted_transaction_yields.l2_output_step`, is parameterised by
//// both script hashes; it binds each verifier's redeemer to the walked
//// output and to the other verifier's redeemer, and inserts the descriptor
//// the descriptor verifier checked. Each verifier is a pure function of its
//// own redeemer and fails unless the claim it carries holds.
////
//// `dispatcher` is not read: it makes each deployment's verifier credentials
//// its own, like every other yield of the dispatcher.

use aiken/crypto.{ScriptHash}
use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use midgard/data_summary_walk_v1.{Summarised} as datum_walk
use midgard/fraud_proofs/transition_trace/output_pipeline.{
  OutputDescriptorClaim, OutputSummaryClaim,
} as pipeline
use midgard/fraud_proofs/transition_trace/output_summaries
use midgard/ledger_output_value_v1 as value

/// Holds when `claim.facts` is the Data of the terminal scan facts of
/// `claim.bytes` and `claim.descriptor` is the encoded descriptor of the output
/// at `claim.index` with those bytes and `claim.summary`.
validator descriptor(_dispatcher: ScriptHash) {
  withdraw(
    claim: OutputDescriptorClaim,
    _credential: Credential,
    _tx: Transaction,
  ) {
    let (facts, value_start) = pipeline.scan_whole(claim.bytes)
    let facts_data: Data = facts
    let (s1, s2, s3) = claim.summary
    and {
      facts_data == claim.facts,
      claim.descriptor == pipeline.descriptor_cbor(
        claim.index,
        claim.bytes,
        facts,
        value_start,
        s1,
        s2,
        s3,
      ),
    }
  }

  else(_) {
    fail
  }
}

/// Holds when `claim.summary` is the Data of the three summaries of the output
/// with bytes `claim.bytes` and scan facts `claim.facts`: the value fold over
/// `claim.value_witnesses` completes, and an inline datum's walk completes in
/// this one transaction.
validator summary(_dispatcher: ScriptHash) {
  withdraw(claim: OutputSummaryClaim, _credential: Credential, _tx: Transaction) {
    let facts = claim.facts
    expect Some(value_summary) = value.finalize_v1(
      pipeline.value_batch(
        value.initial_control_v1(facts.asset_count),
        facts,
        claim.value_witnesses,
      ),
    )
    let datum_summary = if facts.datum_offset == -1 {
      None
    } else {
      expect Summarised(summary) = output_summaries.resume_datum(
        facts,
        claim.bytes,
        datum_walk.start(facts.datum_offset),
        claim.datum,
      )
      Some(summary)
    }
    let derived: Data = output_summaries.summary_of(
      facts,
      claim.bytes,
      value_summary,
      datum_summary,
    )
    derived == claim.summary
  }

  else(_) {
    fail
  }
}
