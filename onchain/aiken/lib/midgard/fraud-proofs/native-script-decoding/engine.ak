//// ## Native-script decoding-fault thread engine (#635, #633)
////
//// Library core of the standalone single-party computation-thread family
//// that proves an operator verdict wrong about the *decodability of a
//// resolved reference script* — the `ResolvedReferenceScriptMalformed` /
//// `ResolvedReferenceScriptNodeLimit` / `ResolvedReferenceScriptDepthLimit`
//// corner of the rejection catalogue — in either direction:
////
//// - direction A (wrongful acceptance): the operator accepted a transaction
////   whose resolved reference script the frozen scan primitives refuse;
//// - direction B (wrongful rejection): the operator force-rejected a
////   transaction with one of the three scan arms although the accused
////   resolved reference script scans to the exact canonical terminal.
////
//// Everything semantic is delegated to the FROZEN primitives in
//// `midgard/native_script_scan_v1`: this module only folds them under a
//// prover-chosen step budget over a chunk window that is authenticated once
//// per L1 transaction against the bound descriptor's
//// `reference_script_item_commitment`. The custody chain is: chunk window →
//// `reference_script_item_commitment` → descriptor → ledger-trie membership
//// under the transition step's `pre_utxos_root` → transition trace → header
//// → thread NFT.
////
//// Owner rulings incorporated:
//// - 2026-08-25 "store the pair": the thread state carries
////   `outpoint_source_kind` (0 = spend field 0, 1 = reference field 1) and
////   `outpoint_cursor` (item ordinal within that field) — for direction B
////   copied verbatim from the trie-authenticated forced leaf's rejection
////   reason, for direction A chosen by the prover — so the bind step opens
////   exactly the accused field at the accused ordinal and no spend-count
////   authority is ever needed.
//// - The design's `tx_order_id: Int` cannot hold the forced leaf's key
////   (`TxOrderId = OutputReference`); the state stores the serialised leaf
////   key bytes instead (`#""` for normal-source threads).

use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/ledger_output_commitment_v1.{LedgerOutputCommitmentV1}
use midgard/ledger_state
use midgard/native_script_scan_v1.{
  NativeScriptFrameV1, NativeScriptStructureAdvanced,
  NativeScriptStructureControlV1, NativeScriptStructureDepthLimit,
  NativeScriptStructureInvalid, NativeScriptStructureNodeLimit,
}
use midgard/rejection_reason_v1.{ForcedTxInvalid, ForcedTxValid}
use midgard/transition_trace

// ## Constants

/// Direction A: the operator accepted a transaction whose resolved reference
/// script the frozen scan refuses.
pub const direction_wrongful_acceptance: Int = 0

/// Direction B: the operator force-rejected a transaction with a scan arm
/// although the accused script scans to the exact canonical terminal.
pub const direction_wrongful_rejection: Int = 1

/// The thread's verdict subject is a leaf of the block's counted
/// `transactions_root` (an acceptance verdict by definition).
pub const source_kind_normal: Int = 0

/// The thread's verdict subject is a leaf of the block's counted
/// `forced_transactions_root` (carries an explicit operator verdict).
pub const source_kind_forced: Int = 1

/// The accused outpoint lives in the subject transaction's field 0.
pub const outpoint_source_spend: Int = 0

/// The accused outpoint lives in the subject transaction's field 1.
pub const outpoint_source_reference: Int = 1

/// The scan refused with `NativeScriptStructureInvalid` (malformed). Also the
/// direction-B contradiction marker: the accused script reached the exact
/// canonical terminal, contradicting every scan arm.
pub const refusal_class_malformed: Int = 0

/// The scan refused with `NativeScriptStructureNodeLimit`.
pub const refusal_class_node_limit: Int = 1

/// The scan refused with `NativeScriptStructureDepthLimit`.
pub const refusal_class_depth_limit: Int = 2

/// Sentinel for a class not (yet) established.
pub const class_pending: Int = -1

/// Sentinel for the descriptor fields before the bind step froze them. Kept
/// outside the descriptor language domain {-1, 0, 3, 128}.
pub const language_unbound: Int = -2

/// The widest canonical native-script token is a signature node
/// (`82 00 581c` + 28 key bytes = 32 bytes); one spare byte keeps the margin
/// strictly conservative.
pub const max_token_byte_width: Int = 33

/// Domain separator for `hash_machine_control_v1`.
pub const machine_control_domain: ByteArray =
  "midgard/fraud-proofs/native-script-decoding/control-v1"

// ## Thread state

/// Step-02's input state (step-01's output): the bound verdict subject.
/// `verified_tx_id` is the id-verified bad transaction for normal-source
/// threads and the `#""` sentinel for forced-source threads until step-02
/// opens the forced leaf.
pub type BindStateV1 {
  direction: Int,
  source_kind: Int,
  verified_tx_id: ByteArray,
}

/// The constant-size thread state from step-02's output onward (§4 of the
/// design, as amended by the 2026-08-25 pair ruling and the `tx_order_id`
/// type repair — see the module header).
pub type ScanThreadStateV1 {
  direction: Int,
  source_kind: Int,
  verified_tx_id: ByteArray,
  /// Forced threads: the serialised trie key of the forced leaf
  /// (`cbor.serialise` of its `TxOrderId`); `#""` for normal threads.
  tx_order_id: ByteArray,
  /// Direction B: 0/1/2 mirroring the leaf's scan arm; `class_pending` for
  /// direction A.
  scan_reason_class: Int,
  /// The transition step's `pre_utxos_root`: the ledger the machine resolved
  /// the subject transaction against.
  prior_ledger_root: ByteArray,
  /// The ruled pair: which field holds the accused outpoint, and the item
  /// ordinal within that field.
  outpoint_source_kind: Int,
  outpoint_cursor: Int,
  /// blake2b-256 of the canonical CBOR of the accused outpoint — the exact
  /// ledger-trie key bytes; `#""` until bind.
  outpoint_key_hash: ByteArray,
  /// Frozen verbatim from the bound descriptor; sentinels until bind.
  reference_script_language: Int,
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  /// `hash_machine_control_v1` of the current scan control; `#""` until the
  /// scan machine is initialised (and forever, on paths that close at bind).
  machine_state_hash: ByteArray,
  /// `class_pending` until the verdict step; then the proven refusal class
  /// (direction A) or `refusal_class_malformed` as the contradiction marker
  /// (direction B).
  refusal_class: Int,
}

/// The canonical byte encoding of the thread state (§3.3): definite-length
/// CBOR array of the fields in declaration order, integers in canonical NIB
/// form, byte strings definite.
pub fn encode_scan_thread_state_v1(state: ScanThreadStateV1) -> ByteArray {
  encode_definite_array_header(15)
    |> bytearray.concat(cbor.serialise(state.direction))
    |> bytearray.concat(cbor.serialise(state.source_kind))
    |> bytearray.concat(encode_definite_bytes(state.verified_tx_id))
    |> bytearray.concat(encode_definite_bytes(state.tx_order_id))
    |> bytearray.concat(cbor.serialise(state.scan_reason_class))
    |> bytearray.concat(encode_definite_bytes(state.prior_ledger_root))
    |> bytearray.concat(cbor.serialise(state.outpoint_source_kind))
    |> bytearray.concat(cbor.serialise(state.outpoint_cursor))
    |> bytearray.concat(encode_definite_bytes(state.outpoint_key_hash))
    |> bytearray.concat(cbor.serialise(state.reference_script_language))
    |> bytearray.concat(cbor.serialise(state.output_index))
    |> bytearray.concat(cbor.serialise(state.total_length))
    |> bytearray.concat(encode_definite_bytes(state.item_commitment))
    |> bytearray.concat(encode_definite_bytes(state.machine_state_hash))
    |> bytearray.concat(cbor.serialise(state.refusal_class))
}

/// The state step-02 emits: verdict subject and accusation bound, descriptor
/// and machine fields still at their sentinels.
pub fn pre_bind_scan_state_v1(
  direction: Int,
  source_kind: Int,
  verified_tx_id: ByteArray,
  tx_order_id: ByteArray,
  scan_reason_class: Int,
  prior_ledger_root: ByteArray,
  outpoint_source_kind: Int,
  outpoint_cursor: Int,
) -> ScanThreadStateV1 {
  ScanThreadStateV1 {
    direction,
    source_kind,
    verified_tx_id,
    tx_order_id,
    scan_reason_class,
    prior_ledger_root,
    outpoint_source_kind,
    outpoint_cursor,
    outpoint_key_hash: #"",
    reference_script_language: language_unbound,
    output_index: -1,
    total_length: -1,
    item_commitment: #"",
    machine_state_hash: #"",
    refusal_class: class_pending,
  }
}

/// The bind step's freeze: the accused outpoint's trie-key hash plus the
/// bound descriptor's reference-script anchor fields, copied verbatim. The
/// scan is anchored to the reference-script ITEM commitment
/// (`reference_script_item_commitment` / `reference_script_total_length`), in
/// item-relative coordinates — the same anchor
/// `ledger_output_commitment_v1.verify_reference_script_chunk` authenticates
/// against.
pub fn bound_scan_state_v1(
  state: ScanThreadStateV1,
  descriptor: LedgerOutputCommitmentV1,
  outpoint_key_bytes: ByteArray,
) -> ScanThreadStateV1 {
  ScanThreadStateV1 {
    ..state,
    outpoint_key_hash: blake2b_256(outpoint_key_bytes),
    reference_script_language: descriptor.reference_script_language,
    output_index: descriptor.output_index,
    total_length: descriptor.reference_script_total_length,
    item_commitment: descriptor.reference_script_item_commitment,
  }
}

// ## Machine-state commitment

/// Constant-size commitment to a scan control: blake2b-256 over the domain
/// separator and the control's canonical encoding.
pub fn hash_machine_control_v1(control_cbor: ByteArray) -> ByteArray {
  machine_control_domain
    |> bytearray.concat(control_cbor)
    |> blake2b_256
}

// ## Chunk-window authentication (§5 option (a))

/// A contiguous, authenticated view into the reference-script item:
/// `bytes[0]` sits at absolute item offset `start_offset`.
pub type ScanWindowV1 {
  bytes: ByteArray,
  start_offset: Int,
}

fn authenticated_reference_script_chunk(
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  chunk_index: Int,
  proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  and {
    proof.field_index == ledger_output_commitment_v1.output_field_index,
    proof.item_index == output_index,
    proof.total_length == total_length,
    proof.chunk_index == chunk_index,
    bounded_item_v1.verify_chunk(item_commitment, proof),
  }
}

/// Authenticates the scan window for one L1 transaction: the chunk holding
/// the cursor plus — mandatorily, whenever the item has one — the adjacent
/// following chunk, so no token that starts inside the first chunk can be cut
/// off by the window edge. Mirrors
/// `ledger_output_commitment_v1.verify_reference_script_chunk`, parameterised
/// on the state's frozen anchor fields because the descriptor itself is gone
/// after the bind step.
pub fn authenticated_scan_window_v1(
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  cursor: Int,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> ScanWindowV1 {
  expect cursor >= 0 && cursor < total_length
  let chunk_index = cursor / bounded_item_v1.chunk_bytes
  expect
    authenticated_reference_script_chunk(
      output_index,
      total_length,
      item_commitment,
      chunk_index,
      chunk_proof,
    )
  if chunk_index + 1 < bounded_item_v1.chunk_count(total_length) {
    expect Some(next_proof) = next_chunk_proof
    expect
      authenticated_reference_script_chunk(
        output_index,
        total_length,
        item_commitment,
        chunk_index + 1,
        next_proof,
      )
    ScanWindowV1 {
      bytes: bytearray.concat(chunk_proof.chunk, next_proof.chunk),
      start_offset: chunk_index * bounded_item_v1.chunk_bytes,
    }
  } else {
    expect None = next_chunk_proof
    ScanWindowV1 {
      bytes: chunk_proof.chunk,
      start_offset: chunk_index * bounded_item_v1.chunk_bytes,
    }
  }
}

// ## Machine bind

pub type MachineBindResultV1 {
  /// The versioned wrapper is undecodable, or decodes to a tag-0 script with
  /// an empty payload — the frozen machine's `InvalidReferenceScript`
  /// precedent. A provable class-0 refusal for direction A.
  MachineBindMalformedV1
  /// The wrapper decodes to a non-native language; the frozen machine never
  /// scans it, so neither direction proceeds through this family.
  MachineBindNonNativeV1 { language_tag: Int }
  /// The wrapper decodes to a non-empty tag-0 payload: the initial scan
  /// control over the payload region.
  MachineBoundV1 { control: NativeScriptStructureControlV1 }
}

/// Parses the versioned script wrapper from the authenticated first chunk and
/// initialises the frozen scan machine over the payload region. The wrapper
/// head is at most 12 bytes, so it never straddles the first chunk boundary.
pub fn bind_machine_v1(
  first_chunk: ByteArray,
  total_length: Int,
) -> MachineBindResultV1 {
  when
    native_script_scan_v1.versioned_script_header_v1(first_chunk, total_length)
  is {
    None -> MachineBindMalformedV1
    Some(header) ->
      if header.language_tag != 0 {
        MachineBindNonNativeV1 { language_tag: header.language_tag }
      } else if header.payload_length == 0 {
        MachineBindMalformedV1
      } else {
        MachineBoundV1 {
          control: native_script_scan_v1.initial_structure_control_v1(
            header.payload_offset,
            header.payload_length,
          ),
        }
      }
  }
}

// ## Budgeted scan fold

pub type ScanOutcomeV1 {
  /// The fold stopped without a refusal: budget exhausted, terminal reached,
  /// frames exhausted, or the safe-read margin stopped a token read.
  ScanAdvancedV1 { control: NativeScriptStructureControlV1 }
  /// A primitive step refused; the class survives to the verdict step.
  ScanRefusedV1 { refusal_class: Int }
}

/// A token step may only run when the window provably contains every byte the
/// widest token could need: either the window reaches the end of the item, or
/// at least `max_token_byte_width` bytes follow the cursor. Otherwise a
/// window truncated below a token would surface as a false
/// `NativeScriptStructureInvalid`. With the mandatory adjacent chunk the
/// margin never stops an honest validator-flow window; it exists so a refusal
/// returned by this fold is never a truncation artifact.
fn safe_token_read(
  control: NativeScriptStructureControlV1,
  window_start: Int,
  window_end: Int,
) -> Bool {
  and {
    control.cursor >= window_start,
    control.cursor < window_end,
    or {
      window_end >= control.end_offset,
      window_end - control.cursor >= max_token_byte_width,
    },
  }
}

/// Thin fold over the frozen primitives: up to `max_steps` primitive steps
/// (token, frame, finalize), stopping early at the terminal, on budget
/// exhaustion, when the frame witnesses run out, or when the safe-read margin
/// blocks a token read. A frame witness that does not hash to the control's
/// `stack_root` aborts (witness error, never a verdict). A refusal is
/// returned as-is; consuming it is the verdict step's business.
pub fn budgeted_scan_v1(
  control: NativeScriptStructureControlV1,
  window: Option<ScanWindowV1>,
  frames: List<NativeScriptFrameV1>,
  max_steps: Int,
) -> ScanOutcomeV1 {
  if max_steps <= 0 {
    ScanAdvancedV1 { control }
  } else if control.stage == native_script_scan_v1.structure_stage_terminal {
    ScanAdvancedV1 { control }
  } else {
    let stepped = if control.stage == native_script_scan_v1.structure_stage_token {
      when window is {
        None -> None
        Some(scan_window) -> {
          let window_end = scan_window.start_offset + bytearray.length(
            scan_window.bytes,
          )
          if safe_token_read(control, scan_window.start_offset, window_end) {
            expect Some(result) = native_script_scan_v1.structure_token_step_v1(
              control,
              scan_window.bytes,
              control.cursor - scan_window.start_offset,
            )
            Some(Pair(result, frames))
          } else {
            None
          }
        }
      }
    } else if control.stage == native_script_scan_v1.structure_stage_frame {
      when frames is {
        [] -> None
        [frame, ..remaining_frames] -> {
          expect Some(result) = native_script_scan_v1.structure_frame_step_v1(
            control,
            frame,
          )
          Some(Pair(result, remaining_frames))
        }
      }
    } else {
      expect Some(result) = native_script_scan_v1.finalize_structure_v1(control)
      Some(Pair(result, frames))
    }
    when stepped is {
      None -> ScanAdvancedV1 { control }
      Some(Pair(result, remaining_frames)) ->
        when result is {
          NativeScriptStructureAdvanced { control: next_control } ->
            budgeted_scan_v1(
              next_control,
              window,
              remaining_frames,
              max_steps - 1,
            )
          NativeScriptStructureInvalid ->
            ScanRefusedV1 { refusal_class: refusal_class_malformed }
          NativeScriptStructureNodeLimit ->
            ScanRefusedV1 { refusal_class: refusal_class_node_limit }
          NativeScriptStructureDepthLimit ->
            ScanRefusedV1 { refusal_class: refusal_class_depth_limit }
        }
    }
  }
}

// ## Step-02 committed-claim openings
//
// Verbatim mirrors of the frozen validation-claim openings, so this family
// authenticates the pre-state ledger root and the forced leaf against exactly
// the roots the interactive machine would.

fn phase_for_event_key(
  event_key: ledger_state.EventKey,
) -> ledger_state.TransitionPhase {
  when event_key is {
    ledger_state.WithdrawalEventKey { .. } -> ledger_state.Withdrawal
    ledger_state.ForcedTransactionEventKey { .. } ->
      ledger_state.ForcedTransaction
    ledger_state.L2TransactionEventKey { .. } -> ledger_state.L2Transaction
    ledger_state.DepositEventKey { .. } -> ledger_state.Deposit
  }
}

/// Opens the event's transition step and event-to-step leaf under the
/// header's counted roots, cross-checks them against each other and the event
/// key (the `committed_claim_structure_is_valid` cross-check block), and
/// returns the transition step's `pre_utxos_root` — the ledger the subject
/// transaction was resolved against.
pub fn verify_committed_pre_state_v1(
  header: ledger_state.HeaderV1,
  event_key: ledger_state.EventKey,
  event_to_step_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  transition_step_membership: transition_trace.RootMembershipProof<
    Int,
    ledger_state.TransitionStep,
  >,
) -> ByteArray {
  let event_to_step = event_to_step_membership.value
  let transition_step = transition_step_membership.value
  expect and {
      header.protocol_version == ledger_state.protocol_version_v1,
      transition_trace.verify_root_membership_with_bytes(
        transition_step_membership,
        transition_trace.TransitionTraceRootDomain,
        header.transition_trace_root,
        header.transition_step_count,
        cbor.serialise(transition_step_membership.key),
        cbor.serialise(transition_step_membership.value),
      ),
      transition_step_membership.key == transition_step.step_index,
      transition_step_membership.key >= 0,
      transition_step_membership.key < transition_step_membership.count,
      transition_step.schema_version == ledger_state.transition_step_schema_version_v1,
      transition_trace.verify_root_membership_with_bytes(
        event_to_step_membership,
        transition_trace.EventToStepRootDomain,
        header.event_to_step_root,
        header.total_event_count,
        cbor.serialise(event_to_step_membership.key),
        cbor.serialise(event_to_step_membership.value),
      ),
      transition_step_membership.key == event_to_step.step_index,
      event_to_step_membership.key == event_key,
      transition_step.event_key == event_key,
      transition_step.phase == phase_for_event_key(event_key),
      transition_step.phase == event_to_step.phase,
    }
  transition_step.pre_utxos_root
}

/// Opens the forced leaf under the header's counted
/// `forced_transactions_root` and enforces the §2.4.3(e) leaf-internal
/// authoritativeness predicate: the leaf's verdict must agree with the
/// embedded validity scalar of the leaf's own id-verified compact bytes.
/// The caller builds the event key from `membership.key`, which is what binds
/// the leaf to the transition step.
pub fn verify_forced_leaf_v1(
  header: ledger_state.HeaderV1,
  membership: transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
) -> ledger_state.ForcedInclusionTxV1 {
  let leaf = membership.value
  let Pair(verified, _witness_set) = verify_native_tx_proof_source_v1(
    leaf.tx_id,
    leaf.source.compact_cbor,
    leaf.source.witness_set_compact_cbor,
    leaf.source.field_preimage_lengths_cbor,
  )
  expect and {
      transition_trace.verify_root_membership_with_bytes(
        membership,
        transition_trace.ForcedTransactionsV1RootDomain,
        header.forced_transactions_root,
        header.forced_transaction_count,
        cbor.serialise(membership.key),
        cbor.serialise(membership.value),
      ),
      verified.version == 1,
      when leaf.verdict is {
        ForcedTxValid -> verified.tx_compact.validity_code == 0
        ForcedTxInvalid { .. } -> verified.tx_compact.validity_code == 1
      },
    }
  leaf
}

// ## Direction-B accusation extraction

/// The accusation a direction-B thread must contradict: the scan class and
/// the ruled outpoint pair, copied verbatim from the forced leaf's rejection
/// reason. Aborts on any reason outside this family's three arms.
pub type ScanAccusationV1 {
  scan_reason_class: Int,
  outpoint_source_kind: Int,
  outpoint_cursor: Int,
}

pub fn scan_accusation_of_v1(
  reason: rejection_reason_v1.RejectionReasonV1,
) -> ScanAccusationV1 {
  when reason is {
    rejection_reason_v1.ResolvedReferenceScriptMalformed {
      source_kind,
      input_index,
    } ->
      ScanAccusationV1 {
        scan_reason_class: refusal_class_malformed,
        outpoint_source_kind: source_kind,
        outpoint_cursor: input_index,
      }
    rejection_reason_v1.ResolvedReferenceScriptNodeLimit {
      source_kind,
      input_index,
    } ->
      ScanAccusationV1 {
        scan_reason_class: refusal_class_node_limit,
        outpoint_source_kind: source_kind,
        outpoint_cursor: input_index,
      }
    rejection_reason_v1.ResolvedReferenceScriptDepthLimit {
      source_kind,
      input_index,
    } ->
      ScanAccusationV1 {
        scan_reason_class: refusal_class_depth_limit,
        outpoint_source_kind: source_kind,
        outpoint_cursor: input_index,
      }
    _ -> fail @"rejection reason outside the decoding-fault family"
  }
}
