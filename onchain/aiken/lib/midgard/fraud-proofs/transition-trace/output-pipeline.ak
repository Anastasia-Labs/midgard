//// The per-output pipeline of the transition-trace replays: the canonical
//// scan, the value fold and the descriptor assembly over one output's
//// committed bytes. The staged yields run one bounded piece per transaction
//// and keep their progress in thread state; the accepted-transaction output
//// step runs the whole pipeline for one small output in one transaction.

use aiken/builtin
use aiken/collection/pairs
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/address.{Script}
use cardano/transaction.{Transaction}
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/common/utils
use midgard/fraud_proofs/native_tx/components
use midgard/fraud_proofs/transition_trace/output_summaries.{WalkWitness}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_descriptor_v1
use midgard/ledger_output_scan_v1 as scan
use midgard/ledger_output_value_v1 as value
use midgard/validation_merkle_v1

/// Advance the output scan by at most `remaining` stages, tracking the byte
/// span of the Value: `start` is the cursor after the required fields and
/// `end` the cursor where the optional fields begin.
pub fn scan_batch(
  control: scan.LedgerOutputScanControlV1,
  bytes: ByteArray,
  start: Int,
  end: Int,
  remaining: Int,
) -> (scan.LedgerOutputScanControlV1, Int, Int) {
  let length = bytearray.length(bytes)
  if remaining == 0 || scan.terminal_is_exact_v1(control, length) {
    (control, start, end)
  } else {
    let next = when scan.finish_v1(control, length) is {
      Some(finished) -> finished
      None -> {
        expect Some(next) = scan.step_v1(control, length, bytes, control.cursor)
        next
      }
    }
    scan_batch(
      next,
      bytes,
      if control.stage == scan.stage_required_fields {
        next.cursor
      } else {
        start
      },
      if control.stage <= scan.stage_asset && next.stage == scan.stage_optional_field {
        next.cursor
      } else {
        end
      },
      remaining - 1,
    )
  }
}

/// Scan a whole output in one transaction: run every stage to the terminal
/// stage, then check the terminal facts once. Returns the facts and the
/// cursor where the Value starts.
pub fn scan_whole(bytes: ByteArray) -> (scan.LedgerOutputScanControlV1, Int) {
  let length = bytearray.length(bytes)
  let (facts, value_start) = scan_to_terminal(
    scan.initial_control_v1(),
    bytes,
    length,
    0,
  )
  expect scan.terminal_is_exact_v1(facts, length)
  (facts, value_start)
}

fn scan_to_terminal(
  control: scan.LedgerOutputScanControlV1,
  bytes: ByteArray,
  length: Int,
  value_start: Int,
) -> (scan.LedgerOutputScanControlV1, Int) {
  if control.stage == scan.stage_terminal {
    (control, value_start)
  } else {
    let next = when scan.finish_v1(control, length) is {
      Some(finished) -> finished
      None -> {
        expect Some(next) = scan.step_v1(control, length, bytes, control.cursor)
        next
      }
    }
    scan_to_terminal(
      next,
      bytes,
      length,
      if control.stage == scan.stage_required_fields {
        next.cursor
      } else {
        value_start
      },
    )
  }
}

/// Fold the asset witnesses, in order, into the value control.
pub fn value_batch(
  control: value.LedgerOutputValueControlV1,
  facts: scan.LedgerOutputScanControlV1,
  witnesses: List<value.LedgerOutputValueWitnessV1>,
) -> value.LedgerOutputValueControlV1 {
  when witnesses is {
    [] -> control
    [witness, ..rest] -> {
      expect Some(next) = value.step_v1(
        control,
        facts.asset_count,
        facts.asset_peaks,
        facts.lovelace,
        witness,
      )
      value_batch(next, facts, rest)
    }
  }
}

/// The output with its Value emptied and its datum dropped; Value and datum
/// semantics are frozen separately by the value fold and the summaries.
pub fn metadata_skeleton(
  facts: scan.LedgerOutputScanControlV1,
  value_start: Int,
  bytes: ByteArray,
) -> ByteArray {
  let prefix = if facts.reference_script_item_offset == -1 {
    #"a2"
  } else {
    #"a3"
  }
    |> bytearray.concat(bytearray.drop(bytearray.take(bytes, value_start), 1))
    |> bytearray.concat(#"018200a0")
  if facts.reference_script_item_offset == -1 {
    prefix
  } else {
    prefix
      |> bytearray.concat(#"03")
      |> bytearray.concat(
          bytearray.drop(bytes, facts.reference_script_item_offset),
        )
  }
}

/// The encoded ledger descriptor of the output at `index` whose committed
/// bytes are `bytes`, from its terminal scan facts and its three summaries.
pub fn descriptor_cbor(
  index: Int,
  bytes: ByteArray,
  facts: scan.LedgerOutputScanControlV1,
  value_start: Int,
  s1: DataSummaryV1,
  s2: DataSummaryV1,
  s3: DataSummaryV1,
) -> ByteArray {
  let skeleton = metadata_skeleton(facts, value_start, bytes)
  expect Some(base) = ledger_output_descriptor_v1.assemble(
    index,
    components.decode_midgard_tx_output_cbor(skeleton),
    skeleton,
    s1,
    s2,
    s3,
  )
  let descriptor = ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    ..base,
    total_length: bytearray.length(bytes),
    item_commitment: ledger_output_commitment_v1.output_item_commitment(
      index,
      bytes,
    ),
    lovelace: facts.lovelace,
    asset_count: facts.asset_count,
    asset_frontier_commitment: validation_merkle_v1.frontier_commitment(
      facts.asset_count,
      facts.asset_peaks,
    ),
    cardano_value_size: facts.cardano_value_size,
  }
  expect ledger_output_commitment_v1.descriptor_is_well_formed(descriptor)
  ledger_output_commitment_v1.encode(descriptor)
}

/// The redeemer of the output-step descriptor verifier: `descriptor` is the
/// encoded ledger descriptor of the output at `index` whose committed bytes
/// are `bytes`, given its three summaries. `facts` is the Data of the bytes'
/// terminal scan facts, which the summary verifier's claim must repeat.
pub type OutputDescriptorClaim {
  index: Int,
  bytes: ByteArray,
  descriptor: ByteArray,
  facts: Data,
  summary: (DataSummaryV1, DataSummaryV1, DataSummaryV1),
}

/// The redeemer of the output-step summary verifier: `summary` is the Data of
/// the three summaries of the output whose committed bytes are `bytes` and
/// whose terminal scan facts are `facts`, under the value fold's asset
/// witnesses and the datum walk witness.
pub type OutputSummaryClaim {
  bytes: ByteArray,
  facts: scan.LedgerOutputScanControlV1,
  summary: Data,
  value_witnesses: List<value.LedgerOutputValueWitnessV1>,
  datum: WalkWitness,
}

/// The fields of the redeemer the withdraw-zero script `verifier` runs with in
/// `tx`. Its withdrawal must be present and its redeemer unique under that
/// exact purpose.
fn verifier_claim(tx: Transaction, verifier: ScriptHash) -> List<Data> {
  expect pairs.has_key(tx.withdrawals, Script(verifier))
  builtin.unconstr_fields(
    utils.get_unique_withdraw_redeemer(tx.redeemers, verifier),
  )
}

/// The descriptor of the output at `index` whose committed bytes are `item`,
/// as checked in `tx` by the two output-step verifiers. The descriptor
/// verifier's claim must name `index` and `item`; the summary verifier's
/// claim must repeat its bytes, scan facts and summaries, so the summaries
/// the descriptor was assembled from are the ones derived from `item`.
pub fn verified_descriptor(
  tx: Transaction,
  descriptor_verifier: ScriptHash,
  summary_verifier: ScriptHash,
  index: Int,
  item: ByteArray,
) -> ByteArray {
  expect [claimed_index,
        bytes,
        descriptor, facts, summary] = verifier_claim(tx, descriptor_verifier)
  expect [summary_bytes,
        summary_facts,
        summary_summary, _, _] = verifier_claim(tx, summary_verifier)
  expect and {
      builtin.un_i_data(claimed_index) == index,
      builtin.un_b_data(bytes) == item,
      summary_bytes == bytes,
      summary_facts == facts,
      summary_summary == summary,
    }
  builtin.un_b_data(descriptor)
}
