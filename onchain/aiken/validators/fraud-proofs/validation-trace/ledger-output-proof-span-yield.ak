//// The authenticated-output-span attestation yield of the shared LOP step
//// family (`V1VtLopSpanYield`). It performs the chunk merkle verification of
//// the output span the step dispatcher action claims, exactly once per step
//// transaction; the span-consuming stage yield of the same transaction takes
//// the span bytes by pure slicing of the same witness chunks and requires the
//// claim to be exactly the span its control demands, so the conjunction
//// re-states the monolithic `authenticated_output_span_for_output` call
//// clause for clause. With an empty span claim it requires the witness to
//// carry no chunk proofs; whether an empty claim is allowed at all is the
//// consuming stage yield's decision, never this validator's.

use aiken/crypto.{ScriptHash}
use cardano/transaction.{Transaction}
use midgard/ledger_output_proof_raw as raw
use midgard/ledger_output_proof_stages as stages
use midgard/ledger_output_proof_v1 as proof
use midgard/ledger_output_proof_yield as shared

validator main(dispatcher_script_hashes: List<ScriptHash>) {
  withdraw(_redeemer: Data, _credential, tx: Transaction) {
    let claim = shared.dispatch_attestation(
      dispatcher_script_hashes,
      tx.inputs,
      tx.redeemers,
    )
    let chunks = shared.span_chunks(claim.witness)
    when stages.span_claim_v1(claim.claimed_span) is {
      stages.SpanClaimNone -> chunks == proof.LedgerOutputProofNoWitness
      stages.SpanClaimSliced { start, length } ->
        when authenticated_span(claim.control, start, length, chunks) is {
          None -> False
          Some(_) -> True
        }
      stages.SpanClaimContent { start, length, bytes } ->
        authenticated_span(claim.control, start, length, chunks) == Some(bytes)
    }
  }

  else(_) {
    fail
  }
}

fn authenticated_span(
  control: raw.Frame,
  start: Int,
  length: Int,
  chunks: proof.LedgerOutputProofWitnessV1,
) -> Option<ByteArray> {
  proof.authenticated_output_span_for_output(
    raw.integer(control, 2),
    raw.integer(control, 3),
    raw.bytes(control, 4),
    start,
    length,
    chunks,
  )
}
