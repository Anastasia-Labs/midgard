//// The four descriptor facts of a terminal ledger-output-proof control,
//// factored so that each one decodes a single sub-control. Their conjunction
//// is `ledger_output_proof_v1.descriptor_is_exact_v1` on the same control and
//// descriptor: the finalize dispatchers require all four positions, so no
//// clause of the monolith is dropped. See
//// docs/fault-proofs/size-plans/validation-trace-script-sources-output-proof-finalize-semantic-v1.md
//// §4.2.
////
//// The two leaf summaries (value and datum) travel through the dispatcher
//// redeemer as prover-claimed values, each pinned by its own yield against
//// the terminal sub-control; the scan-facts yield only ever consumes pinned
//// leaves, so the composite summaries are rebuilt exactly.

use aiken/builtin
use aiken/primitive/bytearray
use midgard/blake2b_224_trace_v1 as blake
use midgard/bounded_item_v1 as bounded
use midgard/cek_data_traverse_v1 as traverse
use midgard/cek_data_v1 as summary
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardCredential, PubKeyCredential, ScriptCredential,
}
use midgard/ledger_output_commitment_v1 as commitment
use midgard/ledger_output_proof_raw as raw
use midgard/ledger_output_proof_v1 as proof
use midgard/ledger_output_scan_v1 as scan
use midgard/ledger_output_v1
use midgard/ledger_output_value_v1 as value
use midgard/native_script_scan_v1 as native
use midgard/validation_machine/witness.{decode_frontier_peaks}
use midgard/validation_merkle_v1 as merkle

/// Item indices of the twelve-item ledger-output-proof control.
const item_stage: Int = 1

const item_output_index: Int = 2

const item_total_length: Int = 3

const item_item_commitment: Int = 4

const item_output_scan: Int = 5

const item_value: Int = 6

const item_datum: Int = 7

const item_reference_script_count: Int = 8

const item_reference_script_peaks: Int = 9

const item_script_hash: Int = 10

const item_native_script: Int = 11

fn none_data() -> Data {
  builtin.constr_data(1, [])
}

fn inner_of(control: raw.Frame, index: Int) -> Data {
  expect Pair(0, [inner]) = builtin.un_constr_data(raw.item(control, index))
  inner
}

/// The output scan sub-control of a terminal control, with the terminal
/// clauses `control_is_well_formed` states for it.
pub fn terminal_scan(control: raw.Frame) -> scan.LedgerOutputScanControlV1 {
  expect raw.integer(control, item_stage) == proof.stage_terminal
  let total_length = raw.integer(control, item_total_length)
  let scan_control = scan.control_from_data_v1(
    raw.item(control, item_output_scan),
  )
  expect scan_control.cursor <= total_length
  expect scan.terminal_is_exact_v1(scan_control, total_length)
  scan_control
}

// ---------------------------------------------------------------------------
// Role 3: value summary
// ---------------------------------------------------------------------------

/// The terminal value sub-control folds to exactly the claimed leaf summary.
pub fn value_summary_is_pinned(
  control: raw.Frame,
  claimed: summary.DataSummaryV1,
) -> Bool {
  let scan_control = terminal_scan(control)
  let value_control = value.control_from_data_v1(inner_of(control, item_value))
  expect value_control.asset_remaining <= scan_control.asset_count
  when value.finalize_v1(value_control) is {
    None -> False
    Some(folded) -> folded == claimed
  }
}

// ---------------------------------------------------------------------------
// Role 2: datum summary
// ---------------------------------------------------------------------------

/// The terminal datum traversal folds to exactly the claimed leaf summary,
/// or the output carries no datum and the claim is absent. The datum span is
/// pinned to the scan's own datum offset and length.
pub fn datum_summary_is_pinned(
  control: raw.Frame,
  claimed: Option<summary.DataSummaryV1>,
) -> Bool {
  let scan_control = terminal_scan(control)
  if scan_control.datum_offset == -1 {
    and {
      raw.item(control, item_datum) == none_data(),
      claimed == None,
    }
  } else {
    let datum_control = traverse.control_from_data_v1(
      inner_of(control, item_datum),
    )
    expect datum_control.source_start == scan_control.datum_offset
    expect datum_control.source_length == scan_control.datum_length
    expect scan_control.datum_length > 0
    expect datum_control.stage == traverse.stage_terminal
    when traverse.finalize_v1(datum_control) is {
      None -> False
      Some(folded) -> claimed == Some(folded)
    }
  }
}

// ---------------------------------------------------------------------------
// Role 1: reference script language, digest, span and item commitment
// ---------------------------------------------------------------------------

/// `descriptor_reference_script_is_exact_v1` plus the terminal structural
/// clauses of `control_is_well_formed` that concern the reference-script
/// frontier, the script-hash trace and the native-script structure.
pub fn reference_script_is_pinned(
  control: raw.Frame,
  descriptor_cbor: ByteArray,
) -> Bool {
  let descriptor = commitment.decode(descriptor_cbor)
  let scan_control = terminal_scan(control)
  let total_length = raw.integer(control, item_total_length)
  let language = scan_control.reference_script_language
  let reference_count = raw.integer(control, item_reference_script_count)
  let reference_peaks = decode_frontier_peaks(
    raw.item(control, item_reference_script_peaks),
  )
  if language == -1 {
    and {
      descriptor.reference_script_language == -1,
      descriptor.reference_script_hash == #"",
      descriptor.reference_script_total_length == 0,
      descriptor.reference_script_item_commitment == #"",
      reference_count == 0,
      reference_peaks == [],
      raw.item(control, item_script_hash) == none_data(),
      raw.item(control, item_native_script) == none_data(),
    }
  } else {
    let item_length = total_length - scan_control.reference_script_item_offset
    let hash_control = blake.control_from_data_v1(
      inner_of(control, item_script_hash),
    )
    let native_is_pinned = if language == 0 {
      let native_control = native.structure_control_from_data_v1(
        inner_of(control, item_native_script),
      )
      and {
        native_control.start_offset == scan_control.reference_script_offset,
        native_control.end_offset == scan_control.reference_script_offset + scan_control.reference_script_length,
        native.structure_terminal_is_exact_v1(native_control),
      }
    } else {
      raw.item(control, item_native_script) == none_data()
    }
    when blake.digest_v1(hash_control) is {
      None -> False
      Some(digest) -> and {
          item_length > 0,
          reference_count == bounded.chunk_count(item_length),
          merkle.frontier_is_well_formed(reference_count, reference_peaks),
          hash_control.total_length == scan_control.reference_script_length + 1,
          native_is_pinned,
          descriptor.reference_script_language == language,
          descriptor.reference_script_hash == digest,
          descriptor.reference_script_total_length == item_length,
          descriptor.reference_script_item_commitment == bounded.commitment(
            proof.output_field_index,
            raw.integer(control, item_output_index),
            item_length,
            reference_peaks,
          ),
        }
    }
  }
}

// ---------------------------------------------------------------------------
// Role 0: scan facts and the three composite summaries
// ---------------------------------------------------------------------------

fn credential_data(credential: MidgardCredential) -> Data {
  when credential is {
    PubKeyCredential(hash) -> builtin.constr_data(0, [builtin.b_data(hash)])
    ScriptCredential(hash) -> builtin.constr_data(1, [builtin.b_data(hash)])
  }
}

fn stake_credential_data(credential: Option<MidgardCredential>) -> Data {
  when credential is {
    None -> builtin.constr_data(1, [])
    Some(inner) ->
      builtin.constr_data(0, [builtin.constr_data(0, [credential_data(inner)])])
  }
}

fn address_summary(
  address: MidgardAddress,
  midgard_encoding: Bool,
) -> summary.DataSummaryV1 {
  summary.semantic_data_summary_v1(
    builtin.constr_data(
      if midgard_encoding && address.protected {
        1
      } else {
        0
      },
      [
        credential_data(address.payment_credential),
        stake_credential_data(address.stake_credential),
      ],
    ),
  )
}

fn tx_out_summary(
  address: MidgardAddress,
  midgard_encoding: Bool,
  value_leaf: summary.DataSummaryV1,
  datum_field: summary.DataSummaryV1,
  reference_script_field: summary.DataSummaryV1,
) -> summary.DataSummaryV1 {
  summary.small_constr_data_summary_v1(
    0,
    summary.prepend_data_list_summary_v1(
      address_summary(address, midgard_encoding),
      summary.prepend_data_list_summary_v1(
        value_leaf,
        summary.prepend_data_list_summary_v1(
          datum_field,
          summary.prepend_data_list_summary_v1(
            reference_script_field,
            summary.empty_data_list_summary_v1(),
          ),
        ),
      ),
    ),
  )
}

/// Every descriptor field that reads the output scan, plus the three
/// composite summaries rebuilt from the scan and the two pinned leaves. The
/// reference-script leaf is taken from the descriptor, which role 1 pins to
/// the trace digest, so no composite consumes an unpinned value.
pub fn scan_facts_are_pinned(
  control: raw.Frame,
  descriptor_cbor: ByteArray,
  claimed_value_summary: summary.DataSummaryV1,
  claimed_datum_summary: Option<summary.DataSummaryV1>,
) -> Bool {
  let descriptor = commitment.decode(descriptor_cbor)
  let scan_control = terminal_scan(control)
  expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
    scan_control.address,
  )
  let empty_fields = summary.empty_data_list_summary_v1()
  let datum_field = when claimed_datum_summary is {
    None -> summary.small_constr_data_summary_v1(0, empty_fields)
    Some(datum) ->
      summary.small_constr_data_summary_v1(
        2,
        summary.prepend_data_list_summary_v1(datum, empty_fields),
      )
  }
  let spend_datum = when claimed_datum_summary is {
    None -> summary.small_constr_data_summary_v1(1, empty_fields)
    Some(datum) ->
      summary.small_constr_data_summary_v1(
        0,
        summary.prepend_data_list_summary_v1(datum, empty_fields),
      )
  }
  let reference_script_field = if scan_control.reference_script_language == -1 {
    summary.small_constr_data_summary_v1(1, empty_fields)
  } else {
    summary.semantic_data_summary_v1(
      builtin.constr_data(0, [builtin.b_data(descriptor.reference_script_hash)]),
    )
  }
  and {
    // The claimed datum mode is the scan's own datum presence, so a prover
    // cannot summarise a datum-bearing output as datum-free.
    claimed_datum_summary == None == ( scan_control.datum_offset == -1 ),
    descriptor.version == commitment.version,
    descriptor.output_index == raw.integer(control, item_output_index),
    descriptor.total_length == raw.integer(control, item_total_length),
    descriptor.item_commitment == raw.bytes(control, item_item_commitment),
    descriptor.address == scan_control.address,
    descriptor.lovelace == scan_control.lovelace,
    descriptor.asset_count == scan_control.asset_count,
    descriptor.asset_frontier_commitment == merkle.frontier_commitment(
      scan_control.asset_count,
      scan_control.asset_peaks,
    ),
    descriptor.cardano_value_size == scan_control.cardano_value_size,
    descriptor.cardano_tx_out == tx_out_summary(
      address,
      False,
      claimed_value_summary,
      datum_field,
      reference_script_field,
    ),
    descriptor.midgard_tx_out == tx_out_summary(
      address,
      True,
      claimed_value_summary,
      datum_field,
      reference_script_field,
    ),
    descriptor.cardano_spend_datum == spend_datum,
  }
}

/// The number of bytes of `item_commitment` the raw frame guarantees; kept
/// here so a reader can see that role 0 re-states the descriptor scalars the
/// carriers already pinned rather than trusting them.
pub const item_commitment_length: Int = 32

/// Guard against a silently shrinking commitment field.
pub fn item_commitment_is_sized(control: raw.Frame) -> Bool {
  bytearray.length(raw.bytes(control, item_item_commitment)) == item_commitment_length
}
