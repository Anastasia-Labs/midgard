use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/primitive/bytearray
use cardano/address.{Script}
use cardano/transaction.{
  InlineDatum, Input, Output, Redeemer, ScriptPurpose, Spend,
}
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/transition_trace/proof_carriage
use midgard/ledger_output_scan_v1 as scan

pub const l2_scan_role = "V1FpTtF4ScanYield"

pub const deposit_scan_role = "V1FpTtF5ScanYield"

pub const l2_value_role = "V1FpTtF4ValueYield"

pub const deposit_value_role = "V1FpTtF5ValueYield"

pub const l2_open_role = "V1FpTtF4L2OpenYield"

pub const l2_summaries_role = "V1FpTtF4L2SummariesYield"

pub const l2_assembly_role = "V1FpTtF4L2AssemblyYield"

pub const deposit_assembly_role = "V1FpTtF5AssemblyYield"

pub const l2_replay_role = "V1FpTtF4L2ReplayYield"

pub const l2_delete_role = "V1FpTtF4L2DeleteYield"

pub const l2_outputs_role = "V1FpTtF4L2OutputsYield"

pub const claim_structure_role = "V1FpTtF4ClaimStructYield"

pub const claim_source_role = "V1FpTtF4ClaimSourceYield"

pub const claim_endpoints_role = "V1FpTtF4ClaimEndsYield"

pub const deposit_projection_role = "V1FpTtF5ProjectionYield"

pub const deposit_replay_role = "V1FpTtF5ReplayYield"

pub const deposit_summaries_role = "V1FpTtF5SummariesYield"

pub type Datum =
  ct.StepDatum<State>

pub type State {
  kind: Int,
  phase: Int,
  proof_commitment: proof_carriage.Commitment,
  /// The disputed L2 transaction, anchored once by phase 0 from the committed
  /// source leaf. Every field walk re-opens against it.
  tx_id: ByteArray,
  /// `field_walk_checkpoint_hash` of the spend-inputs (field 0) walk.
  inputs_walk: ByteArray,
  /// The outputs (field 2) position: a grammar checkpoint hash during phase
  /// 10, then the certified field walk's checkpoint hash.
  outputs_walk: ByteArray,
  /// The certified number of outputs the replay inserts.
  output_count: Int,
  /// `blake2b_256` of the output being replayed.
  output_hash: ByteArray,
  output_index: Int,
  current_root: ByteArray,
  summaries: OutputSummaries,
  scan_cbor: ByteArray,
  value_cbor: ByteArray,
  value_start: Int,
  value_end: Int,
  value_summary: Option<DataSummaryV1>,
  descriptor_cbor: ByteArray,
  deposit_index: Int,
  deposit_source_cbor: ByteArray,
  deposit_asset_count: Int,
  /// The `data_summary_walk_v1.Walk` checkpoint of the output datum; `Some`
  /// only while phase 8 walks a datum. Held as `Data` so that only phase 8
  /// decodes it.
  datum_walk: Option<Data>,
}

pub fn initial(kind: Int, commitment: proof_carriage.Commitment) -> State {
  State {
    kind,
    phase: if kind != 2 {
      6
    } else {
      0
    },
    proof_commitment: commitment,
    tx_id: #"",
    inputs_walk: #"",
    outputs_walk: #"",
    output_count: 0,
    output_hash: #"",
    output_index: 0,
    current_root: #"",
    summaries: OutputSummaries { summaries: [] },
    scan_cbor: #"",
    value_cbor: #"",
    value_start: 0,
    value_end: 0,
    value_summary: None,
    descriptor_cbor: #"",
    deposit_index: 0,
    deposit_source_cbor: #"",
    deposit_asset_count: 0,
    datum_walk: None,
  }
}

pub type Args {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  yield_ref_input_indices: List<Int>,
  proof_ref_indices: List<Int>,
  output_ref_indices: List<Int>,
  deposit_event_ref_index: Int,
  deposit_external_ref_index: Option<Int>,
  /// Decode the authenticated opening in the deposit yield only. Requiring its
  /// full payload decoder in every shared dispatcher duplicates script bytes.
  deposit_opening: Option<Data>,
  /// Only event-dependent terminal validators decode this witness.
  completed_fraud_witness: Option<Data>,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

/// One resumed field-walk step: the redeemer half of `resume_opened_field_walk`
/// and its grammar counterpart. The committed half is thread state.
pub type WalkStep {
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
}

pub type OutputSummaries {
  summaries: List<(DataSummaryV1, DataSummaryV1, DataSummaryV1)>,
}

/// The route authenticated the full proof shape. Read only the fields needed
/// by this yield, after binding to exactly one dispatcher input and Continue.
pub fn dispatch(
  dispatcher: ScriptHash,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> (Datum, Args) {
  expect
      [
        Input {
          output_reference,
          output: Output { datum: InlineDatum(raw), .. },
        },
      ]
    = list.filter(
    inputs,
    fn(input) { input.output.address.payment_credential == Script(dispatcher) },
  )
  expect [Pair(_, redeemer)] = list.filter(
    redeemers,
    fn(pair) { pair.1st == Spend(output_reference) },
  )
  expect ct.Continue(args): SpendRedeemer = redeemer
  expect datum: Datum = raw
  (datum, args)
}

pub fn dispatched(
  dispatcher: ScriptHash,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  references: List<Input>,
) -> (Data, Data, Args) {
  let (datum, args) = dispatch(dispatcher, inputs, redeemers)
  expect Some(state) = datum.data
  let data = proof_carriage.read(
    state.proof_commitment,
    args.proof_ref_indices,
    references,
  )
  let (header, fault) = proof_carriage.fields(data)
  (header, fault, args)
}

pub fn advance(
  datum: Datum,
  args: Args,
  outputs: List<Output>,
  next: State,
) -> Bool {
  let output = list.expect_at(outputs, args.output_index)
  expect InlineDatum(actual) = output.datum
  let expected: Data = ct.StepDatum {
    fraud_prover: datum.fraud_prover,
    data: Some(next),
  }
  actual == expected
}

pub fn one_step_witness(fault: Data, tag: Int) -> List<Data> {
  expect builtin.unconstr_index(fault) == 4
  expect [witness] = builtin.unconstr_fields(fault)
  expect builtin.unconstr_index(witness) == tag
  builtin.unconstr_fields(witness)
}

/// Remove only the already-scanned Value, retaining its map key and every
/// other committed output byte. Empty Value keeps decoding independent of
/// asset cardinality; its semantic summary is authenticated separately.
pub fn output_skeleton(state: State, bytes: ByteArray) -> ByteArray {
  bytearray.take(bytes, state.value_start)
    |> bytearray.concat(#"018200a0")
    |> bytearray.concat(bytearray.drop(bytes, state.value_end))
}

pub fn output_bytes(
  state: State,
  args: Args,
  references: List<Input>,
) -> ByteArray {
  let bytes = proof_carriage.chunks(args.output_ref_indices, references)
  expect blake2b_256(bytes) == state.output_hash
  bytes
}

/// Value and datum semantics have already been frozen independently.
pub fn output_metadata_skeleton(state: State, bytes: ByteArray) -> ByteArray {
  let facts = scan.decode_control_v1(state.scan_cbor)
  let prefix = if facts.reference_script_item_offset == -1 {
    #"a2"
  } else {
    #"a3"
  }
    |> bytearray.concat(
        bytearray.drop(bytearray.take(bytes, state.value_start), 1),
      )
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
