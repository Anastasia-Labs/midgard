//// The output step's binding of its two withdraw-zero verifiers: the
//// descriptor it inserts is the one the descriptor verifier checked, only
//// when both verifiers ran under their exact credentials and their claims
//// name the walked output and agree with each other.

use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/transaction.{Transaction, Withdraw}
use midgard/fraud_proofs/transition_trace/output_pipeline.{
  OutputDescriptorClaim, OutputSummaryClaim,
} as pipeline
use midgard/fraud_proofs/transition_trace/output_summaries.{WalkWitness}
use midgard/ledger_output_descriptor_v1
use midgard/script_context_v1

const descriptor_verifier: ScriptHash =
  #"d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1d1"

const summary_verifier: ScriptHash =
  #"d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2"

const other_verifier: ScriptHash =
  #"d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3"

/// A lovelace-only output, 39 bytes.
const output =
  #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a004c4b40a0"

const other_output =
  #"a200581d60bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb01821a004c4b40a0"

const index = 3

fn not_a_record() -> Data {
  let data: Data = #"00"
  data
}

fn descriptor_claim(
  claimed_index: Int,
  bytes: ByteArray,
  facts_of: ByteArray,
) -> Data {
  let (facts, _) = pipeline.scan_whole(facts_of)
  expect Some(summary) = script_context_v1.ledger_output_summaries_v1(bytes)
  expect Some(descriptor) = ledger_output_descriptor_v1.ledger_value_v1(
    claimed_index,
    bytes,
  )
  let facts_data: Data = facts
  let claim: Data = OutputDescriptorClaim {
    index: claimed_index,
    bytes,
    descriptor,
    facts: facts_data,
    summary,
  }
  claim
}

fn summary_claim(bytes: ByteArray, facts_of: ByteArray, summary_of: ByteArray) {
  let (facts, _) = pipeline.scan_whole(facts_of)
  expect Some(summary) = script_context_v1.ledger_output_summaries_v1(
    summary_of,
  )
  let summary_data: Data = summary
  let claim: Data = OutputSummaryClaim {
    bytes,
    facts,
    summary: summary_data,
    value_witnesses: [],
    datum: WalkWitness { restore: [], base: #"", steps: 0 },
  }
  claim
}

/// A transaction that withdraws zero from `withdrawn` and carries `claims`
/// under their withdraw purposes.
fn tx_with(
  withdrawn: List<ScriptHash>,
  claims: List<(ScriptHash, Data)>,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    withdrawals: list.map(withdrawn, fn(hash) { Pair(Script(hash), 0) }),
    redeemers: list.map(
      claims,
      fn(claim) {
        let (hash, redeemer) = claim
        Pair(Withdraw(Script(hash)), redeemer)
      },
    ),
  }
}

fn honest() -> List<(ScriptHash, Data)> {
  [
    (descriptor_verifier, descriptor_claim(index, output, output)),
    (summary_verifier, summary_claim(output, output, output)),
  ]
}

fn verified(tx: Transaction) -> ByteArray {
  pipeline.verified_descriptor(
    tx,
    descriptor_verifier,
    summary_verifier,
    index,
    output,
  )
}

test bound_claims_yield_the_checked_descriptor() {
  expect Some(expected) = ledger_output_descriptor_v1.ledger_value_v1(
    index,
    output,
  )
  verified(tx_with([descriptor_verifier, summary_verifier], honest())) == expected
}

test descriptor_verifier_withdrawal_is_required() fail {
  verified(tx_with([summary_verifier], honest())) != #""
}

test summary_verifier_withdrawal_is_required() fail {
  verified(tx_with([descriptor_verifier], honest())) != #""
}

test descriptor_claim_under_another_credential_is_refused() fail {
  verified(
    tx_with(
      [other_verifier, summary_verifier],
      [
        (other_verifier, descriptor_claim(index, output, output)),
        (summary_verifier, summary_claim(output, output, output)),
      ],
    ),
  ) != #""
}

test summary_claim_under_another_credential_is_refused() fail {
  verified(
    tx_with(
      [descriptor_verifier, other_verifier],
      [
        (descriptor_verifier, descriptor_claim(index, output, output)),
        (other_verifier, summary_claim(output, output, output)),
      ],
    ),
  ) != #""
}

test descriptor_claim_for_another_index_is_refused() fail {
  verified(
    tx_with(
      [descriptor_verifier, summary_verifier],
      [
        (descriptor_verifier, descriptor_claim(index + 1, output, output)),
        (summary_verifier, summary_claim(output, output, output)),
      ],
    ),
  ) != #""
}

test descriptor_claim_for_other_bytes_is_refused() fail {
  verified(
    tx_with(
      [descriptor_verifier, summary_verifier],
      [
        (
          descriptor_verifier,
          descriptor_claim(index, other_output, other_output),
        ),
        (
          summary_verifier,
          summary_claim(other_output, other_output, other_output),
        ),
      ],
    ),
  ) != #""
}

test summary_claim_for_other_bytes_is_refused() fail {
  verified(
    tx_with(
      [descriptor_verifier, summary_verifier],
      [
        (descriptor_verifier, descriptor_claim(index, output, output)),
        (summary_verifier, summary_claim(other_output, output, output)),
      ],
    ),
  ) != #""
}

test summary_claim_with_other_facts_is_refused() fail {
  verified(
    tx_with(
      [descriptor_verifier, summary_verifier],
      [
        (descriptor_verifier, descriptor_claim(index, output, output)),
        (summary_verifier, summary_claim(output, other_output, output)),
      ],
    ),
  ) != #""
}

test summary_claim_with_other_summaries_is_refused() fail {
  verified(
    tx_with(
      [descriptor_verifier, summary_verifier],
      [
        (descriptor_verifier, descriptor_claim(index, output, output)),
        (summary_verifier, summary_claim(output, output, other_output)),
      ],
    ),
  ) != #""
}

test a_claim_that_is_not_a_record_is_refused() fail {
  verified(
    tx_with(
      [descriptor_verifier, summary_verifier],
      [
        (descriptor_verifier, descriptor_claim(index, output, output)),
        (summary_verifier, not_a_record()),
      ],
    ),
  ) != #""
}
