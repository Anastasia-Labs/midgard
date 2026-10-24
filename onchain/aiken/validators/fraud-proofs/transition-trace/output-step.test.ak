//// The output-step verifiers on their own: each accepts exactly the claim it
//// checks, against the one-shot descriptor and summaries derived from the
//// output bytes, and refuses a claim that does not hold.

use aiken/builtin
use cardano/address.{Script}
use cardano/transaction
use fraud_proofs/transition_trace/output_step
use midgard/cek_data_v1
use midgard/fraud_proofs/transition_trace/output_pipeline.{
  OutputDescriptorClaim, OutputSummaryClaim,
} as pipeline
use midgard/fraud_proofs/transition_trace/output_summaries.{WalkWitness}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_descriptor_v1
use midgard/ledger_output_value_v1 as value
use midgard/script_context_v1

const dispatcher = #"cececececececececececececececececececececececececececece"

/// One policy holding asset names `aa` and `0102`, 79 bytes, no datum.
const asset_output =
  #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a007a1200a1581c11111111111111111111111111111111111111111111111111111111a24201020741aa05"

/// A lovelace-only output with the inline datum `121([])`.
const datum_output =
  #"a300581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a004c4b40a00243d87980"

fn policy() -> ByteArray {
  builtin.replicate_byte(28, 17)
}

/// The asset witnesses of `asset_output`'s value fold, in fold order.
fn asset_witnesses() -> List<value.LedgerOutputValueWitnessV1> {
  let narrow = ledger_output_commitment_v1.asset_leaf_hash(policy(), #"aa", 5)
  let wide = ledger_output_commitment_v1.asset_leaf_hash(policy(), #"0102", 7)
  [
    value.LedgerOutputValueAsset {
      asset_index: 1,
      policy_id: policy(),
      asset_name: #"aa",
      quantity: 5,
      siblings: [wide],
      previous: None,
    },
    value.LedgerOutputValueAsset {
      asset_index: 0,
      policy_id: policy(),
      asset_name: #"0102",
      quantity: 7,
      siblings: [narrow],
      previous: Some(
        value.LedgerOutputValueHeadV1 {
          asset_name: #"aa",
          quantity: 5,
          tail: cek_data_v1.empty_data_pair_summary_v1(),
        },
      ),
    },
    value.LedgerOutputValueNoWitness,
    value.LedgerOutputValueNoWitness,
  ]
}

fn walk(steps: Int) -> WalkWitness {
  WalkWitness { restore: [], base: #"", steps }
}

fn facts_data(bytes: ByteArray) -> Data {
  let (facts, _) = pipeline.scan_whole(bytes)
  let data: Data = facts
  data
}

fn summaries(
  bytes: ByteArray,
) -> (
  cek_data_v1.DataSummaryV1,
  cek_data_v1.DataSummaryV1,
  cek_data_v1.DataSummaryV1,
) {
  expect Some(summary) = script_context_v1.ledger_output_summaries_v1(bytes)
  summary
}

fn one_shot_descriptor(index: Int, bytes: ByteArray) -> ByteArray {
  expect Some(descriptor) = ledger_output_descriptor_v1.ledger_value_v1(
    index,
    bytes,
  )
  descriptor
}

fn descriptor_holds(claim: OutputDescriptorClaim) -> Bool {
  output_step.descriptor.withdraw(
    dispatcher,
    claim,
    Script(dispatcher),
    transaction.placeholder,
  )
}

fn summary_holds(claim: OutputSummaryClaim) -> Bool {
  output_step.summary.withdraw(
    dispatcher,
    claim,
    Script(dispatcher),
    transaction.placeholder,
  )
}

fn honest_descriptor(index: Int, bytes: ByteArray) -> OutputDescriptorClaim {
  OutputDescriptorClaim {
    index,
    bytes,
    descriptor: one_shot_descriptor(index, bytes),
    facts: facts_data(bytes),
    summary: summaries(bytes),
  }
}

fn honest_summary(
  bytes: ByteArray,
  value_witnesses: List<value.LedgerOutputValueWitnessV1>,
  steps: Int,
) -> OutputSummaryClaim {
  let (facts, _) = pipeline.scan_whole(bytes)
  let summary: Data = summaries(bytes)
  OutputSummaryClaim {
    bytes,
    facts,
    summary,
    value_witnesses,
    datum: walk(steps),
  }
}

// Descriptor verifier.

test descriptor_verifier_accepts_the_one_shot_descriptor() {
  descriptor_holds(honest_descriptor(3, asset_output))
}

test descriptor_verifier_accepts_an_output_with_a_datum() {
  descriptor_holds(honest_descriptor(0, datum_output))
}

test descriptor_verifier_refuses_the_descriptor_of_another_index() fail {
  descriptor_holds(
    OutputDescriptorClaim {
      ..honest_descriptor(3, asset_output),
      descriptor: one_shot_descriptor(4, asset_output),
    },
  )
}

test descriptor_verifier_refuses_the_descriptor_of_another_output() fail {
  descriptor_holds(
    OutputDescriptorClaim {
      ..honest_descriptor(0, asset_output),
      descriptor: one_shot_descriptor(0, datum_output),
    },
  )
}

test descriptor_verifier_refuses_other_scan_facts() fail {
  descriptor_holds(
    OutputDescriptorClaim {
      ..honest_descriptor(0, asset_output),
      facts: facts_data(datum_output),
    },
  )
}

test descriptor_verifier_refuses_other_summaries() fail {
  descriptor_holds(
    OutputDescriptorClaim {
      ..honest_descriptor(0, asset_output),
      summary: summaries(datum_output),
    },
  )
}

test descriptor_verifier_refuses_non_canonical_bytes() fail {
  descriptor_holds(
    OutputDescriptorClaim {
      ..honest_descriptor(0, asset_output),
      bytes: #"bf00581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa011a004c4b40ff",
    },
  )
}

// Summary verifier.

test summary_verifier_accepts_the_one_shot_summaries() {
  summary_holds(honest_summary(asset_output, asset_witnesses(), 0))
}

test summary_verifier_accepts_an_output_with_a_datum() {
  summary_holds(
    honest_summary(
      datum_output,
      [value.LedgerOutputValueNoWitness, value.LedgerOutputValueNoWitness],
      16,
    ),
  )
}

test summary_verifier_refuses_the_summaries_of_another_output() fail {
  let other: Data = summaries(datum_output)
  summary_holds(
    OutputSummaryClaim {
      ..honest_summary(asset_output, asset_witnesses(), 0),
      summary: other,
    },
  )
}

test summary_verifier_refuses_an_unfinished_value_fold() fail {
  summary_holds(
    honest_summary(
      asset_output,
      [
        value.LedgerOutputValueAsset {
          asset_index: 1,
          policy_id: policy(),
          asset_name: #"aa",
          quantity: 5,
          siblings: [
            ledger_output_commitment_v1.asset_leaf_hash(policy(), #"0102", 7),
          ],
          previous: None,
        },
      ],
      0,
    ),
  )
}

test summary_verifier_refuses_a_datum_walk_that_does_not_finish() fail {
  summary_holds(
    honest_summary(
      datum_output,
      [value.LedgerOutputValueNoWitness, value.LedgerOutputValueNoWitness],
      0,
    ),
  )
}

test summary_verifier_refuses_other_scan_facts() fail {
  let (facts, _) = pipeline.scan_whole(datum_output)
  summary_holds(
    OutputSummaryClaim {
      ..honest_summary(asset_output, asset_witnesses(), 0),
      facts,
    },
  )
}
