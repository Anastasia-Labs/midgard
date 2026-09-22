//// Cross-language wire pins for the Q35 `network-id` forced door.
////
//// Every constant below is an **absolute** hex literal shared verbatim with
//// `demo/midgard-sdk/tests/network-id.test.ts`. Nothing here compares one
//// Aiken derivation against another: if either language's encoding of the
//// forced handoff datum, the forced-step redeemer, the resumable scan's state
//// and actions, or the forced-flavoured terminal state moves, the literal
//// stops matching on both sides.

use aiken/cbor
use cardano/transaction.{OutputReference}
use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1
use midgard/fraud_proofs/network_id/forced_scan
use midgard/fraud_proofs/network_id/forced_step.{Args, Datum, SpendRedeemer}
use midgard/fraud_proofs/network_id/step_01.{ForcedNetworkIdMismatch}
use midgard/fraud_proofs/network_id/step_02 as step_02
use midgard/ledger_state
use midgard/native_tx_field_access_v1.{Inline}
use midgard/rejection_reason_v1.{ForcedTxInvalid, NetworkIdMismatch}
use midgard/transition_trace

const fraud_prover = #"11111111111111111111111111111111111111111111111111111111"

const operator_vkey =
  #"99999999999999999999999999999999999999999999999999999999"

const bad_tx_id =
  #"2222222222222222222222222222222222222222222222222222222222222222"

const forced_transactions_root =
  #"3333333333333333333333333333333333333333333333333333333333333333"

const forced_transactions_phas_root =
  #"4444444444444444444444444444444444444444444444444444444444444444"

const prev_utxos_root =
  #"5555555555555555555555555555555555555555555555555555555555555555"

const utxos_root =
  #"6666666666666666666666666666666666666666666666666666666666666666"

const order_key_transaction_id =
  #"7777777777777777777777777777777777777777777777777777777777777777"

const zero_root =
  #"0000000000000000000000000000000000000000000000000000000000000000"

/// Canonical CBOR of the forced order key — the exact bytes the forced step
/// freezes into `forced_source_key`.
const forced_source_key =
  #"d8799f5820777777777777777777777777777777777777777777777777777777777777777700ff"

const forced_step_datum_cbor =
  #"d8799f581c11111111111111111111111111111111111111111111111111111111d8799fd87c80ffff"

const empty_forced_step_datum_cbor =
  #"d8799f581c11111111111111111111111111111111111111111111111111111111d87a80ff"

const forced_step_02_state_cbor =
  #"d8799f5820222222222222222222222222222222222222222222222222222222222222222218ff00d87c80d87a80d8799f5827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffffff"

const forced_step_02_datum_cbor =
  #"d8799f581c11111111111111111111111111111111111111111111111111111111d8799fd8799f5820222222222222222222222222222222222222222222222222222222222222222218ff00d87c80d87a80d8799f5827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffffffffff"

const forced_step_redeemer_cbor =
  #"d87a9fd8799f0000d8799f582055555555555555555555555555555555555555555555555555555555555555555820666666666666666666666666666666666666666666666666666666666666666658200000000000000000000000000000000000000000000000000000000000000000582033333333333333333333333333333333333333333333333333333333333333335820000000000000000000000000000000000000000000000000000000000000000058200000000000000000000000000000000000000000000000000000000000000000582000000000000000000000000000000000000000000000000000000000000000005820000000000000000000000000000000000000000000000000000000000000000058200000000000000000000000000000000000000000000000000000000000000000000100000101000a1400000000581c99999999999999999999999999999999999999999999999999999999581c9999999999999999999999999999999999999999999999999999999901ffd8799fd87a80582033333333333333333333333333333333333333333333333333333333333333335820444444444444444444444444444444444444444444444444444444444444444401d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffd8799f58202222222222222222222222222222222222222222222222222222222222222222d8799f418041814182ffd87a9fd87e80ffff80ff01ffff"

fn order_key() -> ledger_state.TxOrderId {
  OutputReference { transaction_id: order_key_transaction_id, output_index: 0 }
}

fn pinned_header() -> ledger_state.HeaderV1 {
  ledger_state.HeaderV1 {
    prev_utxos_root,
    utxos_root,
    withdrawals_root: zero_root,
    forced_transactions_root,
    transactions_root: zero_root,
    deposits_root: zero_root,
    transition_trace_root: zero_root,
    event_to_step_root: zero_root,
    validation_traces_root: zero_root,
    withdrawal_count: 0,
    forced_transaction_count: 1,
    l2_transaction_count: 0,
    deposit_count: 0,
    total_event_count: 1,
    transition_step_count: 1,
    validation_trace_count: 0,
    start_time: 10,
    end_time: 20,
    block_slot: 0,
    expected_network_id: 0,
    min_fee_a: 0,
    min_fee_b: 0,
    prev_header_hash: operator_vkey,
    operator_vkey,
    protocol_version: 1,
  }
}

fn pinned_membership() -> transition_trace.RootMembershipProof<
  ledger_state.TxOrderId,
  ledger_state.ForcedInclusionTxV1,
> {
  transition_trace.RootMembershipProof {
    domain: transition_trace.ForcedTransactionsV1RootDomain,
    root: forced_transactions_root,
    phas_root: forced_transactions_phas_root,
    count: 1,
    key: order_key(),
    value: ledger_state.ForcedInclusionTxV1 {
      tx_id: bad_tx_id,
      submitted_source: ledger_state.ForcedTxProofSourceV1 {
        compact_cbor: #"80",
        witness_set_compact_cbor: #"81",
        field_preimage_lengths_cbor: #"82",
      },
      verdict: ForcedTxInvalid { reason: NetworkIdMismatch },
    },
    proof: [],
  }
}

fn pinned_step_02_state() -> step_02.State {
  step_02.State {
    bad_tx_id,
    committed_tx_network_id: 255,
    expected_network_id: 0,
    fault: ForcedNetworkIdMismatch,
    post_utxo: None,
    forced_source_key: Some(forced_source_key),
  }
}

/// The handoff datum step 01 writes and the forced door re-authenticates.
/// `ForcedNetworkIdMismatch` is `NetworkIdFaultV1` constructor 3.
test q35_forced_step_datum_wire_is_pinned() {
  let datum: Datum = ct.StepDatum {
    fraud_prover,
    data: Some(ForcedNetworkIdMismatch),
  }
  cbor.serialise(datum) == forced_step_datum_cbor
}

/// The same datum with an empty state — the shape the forced door refuses.
test q35_forced_step_empty_datum_wire_is_pinned() {
  let datum: Datum = ct.StepDatum { fraud_prover, data: None }
  cbor.serialise(datum) == empty_forced_step_datum_cbor
}

test q35_forced_step_redeemer_wire_is_pinned() {
  let redeemer: SpendRedeemer = ct.Continue(
    Args {
      input_index: 0,
      output_index: 0,
      header: pinned_header(),
      membership: pinned_membership(),
      direction: 1,
    },
  )
  cbor.serialise(redeemer) == forced_step_redeemer_cbor
}

/// The terminal state in its forced flavour: `forced_source_key` populated,
/// `post_utxo` empty, and `committed_tx_network_id` carrying Cardano's absent
/// value 255.
test q35_forced_step_02_state_wire_is_pinned() {
  cbor.serialise(pinned_step_02_state()) == forced_step_02_state_cbor
}

test q35_forced_step_02_datum_wire_is_pinned() {
  let datum: step_02.Datum = ct.StepDatum {
    fraud_prover,
    data: Some(pinned_step_02_state()),
  }
  cbor.serialise(datum) == forced_step_02_datum_cbor
}

/// A `forced_scan.BoundV1`: Constr 0 over
/// `[bad_tx_id: bytes32, committed_tx_network_id: int,
///   expected_network_id: int, forced_source_key: bytes]`.
/// `18ff` is 255, Cardano's absent body network field.
const forced_scan_bound_cbor =
  #"d8799f5820222222222222222222222222222222222222222222222222222222222222222218ff005827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffff"

/// `forced_scan.State` Constr 0 `Ready { bound }` — what the forced door
/// writes. One field: the bound above.
const forced_scan_ready_state_cbor =
  #"d8799fd8799f5820222222222222222222222222222222222222222222222222222222222222222218ff005827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffffff"

/// `forced_scan.State` Constr 1 (`d87a`) `Grammar { bound, checkpoint_hash }`.
/// `checkpoint_hash` is a 32-byte `field_grammar_checkpoint_hash`.
const forced_scan_grammar_state_cbor =
  #"d87a9fd8799f5820222222222222222222222222222222222222222222222222222222222222222218ff005827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffff58208888888888888888888888888888888888888888888888888888888888888888ff"

/// `forced_scan.State` Constr 2 (`d87b`) `Scanning { bound, checkpoint_hash }`.
/// `checkpoint_hash` is a 32-byte `field_walk_checkpoint_hash`.
const forced_scan_scanning_state_cbor =
  #"d87b9fd8799f5820222222222222222222222222222222222222222222222222222222222222222218ff005827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffff58208888888888888888888888888888888888888888888888888888888888888888ff"

/// `ct.StepDatum<State>`: Constr 0 over `[fraud_prover: bytes28,
/// data: Option<State>]`, here `Some(Scanning { .. })`.
const forced_scan_datum_cbor =
  #"d8799f581c11111111111111111111111111111111111111111111111111111111d8799fd87b9fd8799f5820222222222222222222222222222222222222222222222222222222222222222218ff005827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ffff58208888888888888888888888888888888888888888888888888888888888888888ffffff"

/// The same datum with an empty state — the shape every scan action refuses.
const empty_forced_scan_datum_cbor =
  #"d8799f581c11111111111111111111111111111111111111111111111111111111d87a80ff"

/// The opening every action carries, pinned at its smallest tier-1 shape:
/// `FieldOpeningV1` Constr 0 `BodyFieldOpening { native_tx_compact_cbor: #"80",
/// carriage: FieldCarriageV1 Constr 0 `Inline { preimage: #"81" } }`.
const forced_scan_opening_cbor = #"d8799f4180d8799f4181ffff"

/// `ActionV1` Constr 0 `Open { input_index, output_index, opening }`.
const forced_scan_open_action_cbor = #"d8799f0000d8799f4180d8799f4181ffffff"

/// `ActionV1` Constr 1 (`d87a`) `StartGrammar { input_index, output_index,
/// opening, item_budget }`; `1880` is 128, the `grammar_batch` cap.
const forced_scan_start_grammar_action_cbor =
  #"d87a9f0000d8799f4180d8799f4181ffff1880ff"

/// `ActionV1` Constr 2 (`d87b`) `ResumeGrammar { input_index, output_index,
/// opening, checkpoint_bytes, item_budget }`. `checkpoint_bytes` is the
/// opaque 87-byte `FieldGrammarCheckpointV1` encoding in production; the pin
/// uses a two-byte marker so the field order, not a derivation, is what the
/// vector fixes.
const forced_scan_resume_grammar_action_cbor =
  #"d87b9f0000d8799f4180d8799f4181ffff4299991880ff"

/// `ActionV1` Constr 3 (`d87c`) `FinishGrammar { input_index, output_index,
/// opening, checkpoint_bytes }`.
const forced_scan_finish_grammar_action_cbor =
  #"d87c9f0000d8799f4180d8799f4181ffff429999ff"

/// `ActionV1` Constr 4 (`d87d`) `Advance { input_index, output_index, opening,
/// checkpoint_bytes, item_budget }`; `checkpoint_bytes` is the opaque 53-byte
/// `FieldWalkCheckpointV1` encoding in production, and `1840` is 64, the
/// `scan_batch` cap.
const forced_scan_advance_action_cbor =
  #"d87d9f0000d8799f4180d8799f4181ffff4299991840ff"

/// `ct.StepRedeemer<ActionV1>` Constr 1 (`d87a`) `Continue(Advance { .. })`.
const forced_scan_redeemer_cbor =
  #"d87a9fd87d9f0000d8799f4180d8799f4181ffff4299991840ffff"

/// `ct.StepRedeemer<ActionV1>` Constr 0 `Cancel { input_index,
/// computation_thread_mint_redeemer_index }`.
const forced_scan_cancel_redeemer_cbor = #"d8799f0000ff"

const scan_checkpoint_hash =
  #"8888888888888888888888888888888888888888888888888888888888888888"

const scan_checkpoint_bytes = #"9999"

fn pinned_scan_bound() -> forced_scan.BoundV1 {
  forced_scan.BoundV1 {
    bad_tx_id,
    committed_tx_network_id: 255,
    expected_network_id: 0,
    forced_source_key,
  }
}

fn pinned_scan_opening() -> field_opening_v1.FieldOpeningV1 {
  field_opening_v1.BodyFieldOpening {
    native_tx_compact_cbor: #"80",
    carriage: Inline { preimage: #"81" },
  }
}

test q35_forced_scan_bound_wire_is_pinned() {
  cbor.serialise(pinned_scan_bound()) == forced_scan_bound_cbor
}

/// The three thread states, in constructor order — the same order the SDK's
/// discriminant must use.
test q35_forced_scan_state_wire_is_pinned() {
  let ready: forced_scan.State = forced_scan.Ready {
    bound: pinned_scan_bound(),
  }
  let grammar: forced_scan.State = forced_scan.Grammar {
    bound: pinned_scan_bound(),
    checkpoint_hash: scan_checkpoint_hash,
  }
  let scanning: forced_scan.State = forced_scan.Scanning {
    bound: pinned_scan_bound(),
    checkpoint_hash: scan_checkpoint_hash,
  }
  and {
    cbor.serialise(ready) == forced_scan_ready_state_cbor,
    cbor.serialise(grammar) == forced_scan_grammar_state_cbor,
    cbor.serialise(scanning) == forced_scan_scanning_state_cbor,
  }
}

test q35_forced_scan_datum_wire_is_pinned() {
  let datum: forced_scan.Datum = ct.StepDatum {
    fraud_prover,
    data: Some(
      forced_scan.Scanning {
        bound: pinned_scan_bound(),
        checkpoint_hash: scan_checkpoint_hash,
      },
    ),
  }
  let empty: forced_scan.Datum = ct.StepDatum { fraud_prover, data: None }
  and {
    cbor.serialise(datum) == forced_scan_datum_cbor,
    cbor.serialise(empty) == empty_forced_scan_datum_cbor,
  }
}

/// Every action in constructor order, each carrying the same pinned opening so
/// the vectors differ only where the action shapes differ.
test q35_forced_scan_action_wire_is_pinned() {
  let open_action: forced_scan.ActionV1 = forced_scan.Open {
    input_index: 0,
    output_index: 0,
    opening: pinned_scan_opening(),
  }
  let start_action: forced_scan.ActionV1 = forced_scan.StartGrammar {
    input_index: 0,
    output_index: 0,
    opening: pinned_scan_opening(),
    item_budget: forced_scan.grammar_batch,
  }
  let resume_action: forced_scan.ActionV1 = forced_scan.ResumeGrammar {
    input_index: 0,
    output_index: 0,
    opening: pinned_scan_opening(),
    checkpoint_bytes: scan_checkpoint_bytes,
    item_budget: forced_scan.grammar_batch,
  }
  let finish_action: forced_scan.ActionV1 = forced_scan.FinishGrammar {
    input_index: 0,
    output_index: 0,
    opening: pinned_scan_opening(),
    checkpoint_bytes: scan_checkpoint_bytes,
  }
  let advance_action: forced_scan.ActionV1 = forced_scan.Advance {
    input_index: 0,
    output_index: 0,
    opening: pinned_scan_opening(),
    checkpoint_bytes: scan_checkpoint_bytes,
    item_budget: forced_scan.scan_batch,
  }
  and {
    cbor.serialise(pinned_scan_opening()) == forced_scan_opening_cbor,
    cbor.serialise(open_action) == forced_scan_open_action_cbor,
    cbor.serialise(start_action) == forced_scan_start_grammar_action_cbor,
    cbor.serialise(resume_action) == forced_scan_resume_grammar_action_cbor,
    cbor.serialise(finish_action) == forced_scan_finish_grammar_action_cbor,
    cbor.serialise(advance_action) == forced_scan_advance_action_cbor,
  }
}

test q35_forced_scan_redeemer_wire_is_pinned() {
  let continue_redeemer: forced_scan.SpendRedeemer = ct.Continue(
    forced_scan.Advance {
      input_index: 0,
      output_index: 0,
      opening: pinned_scan_opening(),
      checkpoint_bytes: scan_checkpoint_bytes,
      item_budget: forced_scan.scan_batch,
    },
  )
  let cancel_redeemer: forced_scan.SpendRedeemer = ct.Cancel {
    input_index: 0,
    computation_thread_mint_redeemer_index: 0,
  }
  and {
    cbor.serialise(continue_redeemer) == forced_scan_redeemer_cbor,
    cbor.serialise(cancel_redeemer) == forced_scan_cancel_redeemer_cbor,
  }
}

/// The batch caps are wire too: the SDK never proposes an `item_budget` above
/// them, so a drift here is a drift in every scan transaction it builds.
test q35_forced_scan_batch_caps_are_pinned() {
  and {
    forced_scan.scan_batch == 64,
    forced_scan.grammar_batch == 128,
  }
}
