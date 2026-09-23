//// Authenticate a current history-list gap/filler or immutable Order facts.
//// Captured commitments survive pointer continuations; nonce liveness is not evidence.

use aiken/crypto.{ScriptHash}
use cardano/address
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input, NoDatum, Output, Transaction}
use midgard/common/types.{PosixTime}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/event_history/order_facts
use midgard/event_history/proof_evidence
use midgard/event_history/proof_fixture
use midgard/fraud_proofs/fabricated_withdrawal/step_01
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/user_events
use midgard/user_events/withdrawal

// ## Step-02 thread types

/// What step-01 hands forward: the authenticated challenged-header facts plus the
/// committed withdrawal leaf, with the leaf's value reduced to its commitment.
pub type State {
  state_queue_policy: PolicyId,
  challenged_header_hash: ledger_state.HeaderHash,
  header_start_time: PosixTime,
  header_end_time: PosixTime,
  committed_withdrawal_id: ledger_state.WithdrawalId,
  committed_withdrawal_content_hash: step_01.WithdrawalContentHash,
}

/// Both paths authenticate the deployment through its hub oracle.
pub type WithdrawalEvidenceV1 {
  AbsentWithdrawalIdentity {
    hub_ref_input_index: Int,
    history_ref_input_index: Int,
  }
  PresentWithdrawalEvent {
    hub_ref_input_index: Int,
    event_ref_input_index: Int,
    external_ref_input_index: Option<Int>,
  }
}

pub type WithdrawalEvidenceVerdictV1 {
  WithdrawalIdentityAbsent
  WithdrawalEventObserved { commitment: order_facts.Commitment }
}

pub type Args {
  input_index: Int,
  output_index: Int,
  /// The prover's chosen L1 witness about the committed withdrawal identity.
  evidence: WithdrawalEvidenceV1,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

// ## Evidence authentication

/// Authenticates the prover's L1 witness and returns the verdict step-03 argues
/// from. Every branch either produces an authenticated verdict or fails; there is
/// no arm that forwards an unauthenticated claim.
pub fn verify_withdrawal_evidence_v1(
  hub_oracle: ScriptHash,
  state: State,
  evidence: WithdrawalEvidenceV1,
  tx: Transaction,
  params: proof_evidence.Parameters,
) -> WithdrawalEvidenceVerdictV1 {
  let hub_index = when evidence is {
    AbsentWithdrawalIdentity { hub_ref_input_index, .. } -> hub_ref_input_index
    PresentWithdrawalEvent { hub_ref_input_index, .. } -> hub_ref_input_index
  }
  let authority = hub.get_datum(tx.reference_inputs, hub_oracle, hub_index)
  expect authority.state_queue == state.state_queue_policy
  when evidence is {
    AbsentWithdrawalIdentity { history_ref_input_index, .. } -> {
      expect
        proof_evidence.absence(
          tx,
          authority.withdrawal,
          authority.withdrawal_addr,
          state.committed_withdrawal_id,
          state.header_end_time,
          history_ref_input_index,
        )
      WithdrawalIdentityAbsent
    }
    PresentWithdrawalEvent {
      event_ref_input_index,
      external_ref_input_index,
      ..
    } ->
      WithdrawalEventObserved {
        commitment: proof_evidence.presence(
          tx,
          authority.withdrawal,
          authority.withdrawal_addr,
          order_facts.Withdrawal,
          state.committed_withdrawal_id,
          state.header_end_time,
          params,
          event_ref_input_index,
          external_ref_input_index,
        ),
      }
  }
}

// ## Family test fixtures
//
// Test-fixture support for this family's step selectors, unreachable from any
// on-chain validator entry point. It extends
// `midgard/fraud_proofs/native_binding_fixture_v1` rather than editing it,
// because Q39 and Q40 must keep disjoint family paths.

/// Inclusion time of the fixture's authentic withdrawal event, inside the fixture
/// header's `(start_time, end_time]` window (10, 20].
pub const authentic_inclusion_time_v1: PosixTime = 15

/// The withdrawal event NFT policy the fixture hub oracle registers.
///
/// Read out of the fixture hub datum rather than restated, so it cannot drift
/// from `native_binding_fixture_v1`'s `hub_reference_input`.
///
/// Known fixture limitation: that shared fixture hub datum registers the *same*
/// placeholder script hash for `deposit`, `withdrawal` and every other event
/// policy, so no selector built on it can distinguish "reads `hub_datum.deposit`"
/// from "reads `hub_datum.withdrawal`". The step-02 selectors therefore prove the
/// policy is hub-*registered* (by substituting an unregistered one), and the
/// choice of hub field is held by `verify_withdrawal_evidence_v1` reading
/// `hub_datum.withdrawal` directly. Making the two distinguishable means giving
/// the shared fixture distinct per-event policies, which is a change to a
/// parent-owned module and is reported rather than made here.
pub fn fixture_withdrawal_event_policy_v1() -> PolicyId {
  let hub_datum = hub.get_datum(
    [fixture.hub_reference_input()],
    fixture.hub_oracle_script_hash,
    0,
  )
  hub_datum.withdrawal
}

/// The step-02 thread state step-01 really produces for `block` — the exact
/// handoff its positive selector pins, so the family's steps are exercised
/// against continuous state rather than restated constants.
pub fn fixture_state_v1(block: step_01.FabricatedWithdrawalBlockV1) -> State {
  State {
    state_queue_policy: proof_fixture.state_queue_policy(),
    challenged_header_hash: block.block.header_hash,
    header_start_time: block.block.header.start_time,
    header_end_time: block.block.header.end_time,
    committed_withdrawal_id: block.committed_withdrawal_id,
    committed_withdrawal_content_hash: step_01.committed_withdrawal_content_hash_v1(
      step_01.committed_withdrawal_membership_v1(block),
    ),
  }
}

/// The authentic L1 withdrawal event datum for `id`/`info`.
///
/// The `refund_address`/`refund_datum` pair is part of what the withdrawer
/// commits at authentication time, so it is part of the datum this family
/// commits to — a header that got the order right but the refund path wrong is
/// still a content mismatch.
pub fn authentic_withdrawal_datum_v1(
  id: ledger_state.WithdrawalId,
  info: ledger_state.WithdrawalInfo,
  inclusion_time: PosixTime,
) -> withdrawal.Datum {
  user_events.OptimisticDatum {
    event: ledger_state.WithdrawalEvent { id, info },
    inclusion_time,
    witness: #"57575757575757575757575757575757575757575757575757575757",
    refund_address: address.from_verification_key(
      #"2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b",
    ),
    refund_datum: NoDatum,
  }
}

/// The verdict step-02 produces for `event_datum`: the commitment is the exact
/// hash of the datum bytes the authentic event UTxO carries.
pub fn history_payload(event_datum: withdrawal.Datum) -> order_facts.Payload {
  order_facts.WithdrawalPayload {
    event: event_datum.event,
    refund_address: event_datum.refund_address,
    refund_datum: event_datum.refund_datum,
  }
}

pub fn observed_verdict_v1(
  event_datum: withdrawal.Datum,
) -> WithdrawalEvidenceVerdictV1 {
  let payload = history_payload(event_datum)
  WithdrawalEventObserved {
    commitment: order_facts.commitment(
      fixture_withdrawal_event_policy_v1(),
      order_facts.Withdrawal,
      proof_fixture.facts(payload, event_datum.inclusion_time),
      payload,
      proof_fixture.original_assets(),
    ),
  }
}

/// A reference input standing in for the authentic withdrawal event UTxO: one NFT
/// of `event_nft_policy` under `event_nft_asset_name`, plus the inline event
/// datum.
pub fn withdrawal_event_reference_input_v1(
  event_nft_policy: PolicyId,
  event_nft_asset_name: AssetName,
  event_datum: withdrawal.Datum,
) -> Input {
  proof_fixture.order_reference(
    event_nft_policy,
    event_nft_asset_name,
    history_payload(event_datum),
    event_datum.inclusion_time,
  )
}

/// A reference input carrying a well-formed withdrawal event datum with **no**
/// event NFT at all — the shape any user can build, and therefore no evidence.
pub fn untrusted_withdrawal_datum_reference_input_v1(
  event_datum: withdrawal.Datum,
) -> Input {
  let event_datum_data: Data = event_datum
  Input {
    output_reference: fixture.own_out_ref(),
    output: Output {
      address: address.from_verification_key(
        #"3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f3f",
      ),
      value: assets.from_lovelace(3_000_000),
      datum: InlineDatum(event_datum_data),
      reference_script: None,
    },
  }
}
