//// ## `fabricated-deposit` step-02 — authenticated L1 deposit evidence
//// (`GOAL_SPEC.md` §9.1 / Q39)
////
//// Step-01 established *what the challenged header commits*. Step-02
//// establishes *what L1 actually holds* about that committed identity, and it
//// is the only step that reads the L1 chain. Everything it hands forward is
//// authenticated material, so the later steps never touch a prover-supplied
//// claim about L1 again.
////
//// ### The two admissible witnesses
////
//// - `AbsentDepositIdentity` — the prover exhibits the **still-unspent L1
////   output at exactly the committed `DepositId`**. `DepositId` is the output
////   reference a deposit authentication transaction must have *spent*
////   (`midgard/user_events`'s `authenticate_new_event`, rule 4, requires the
////   event id to equal the spent nonce output reference, and rule 1 mints the
////   event NFT under `out_ref_to_nonce` of that same reference). An output is
////   spent at most once, so a live output at that reference is a positive proof
////   that no deposit event with that identity was ever authenticated. Verdict:
////   `DepositIdentityAbsent`.
////
//// - `PresentDepositEvent` — the prover exhibits the authentic deposit event
////   itself: a reference input carrying exactly one NFT of the hub-registered
////   deposit policy under the asset name `out_ref_to_nonce(committed
////   DepositId)`, whose inline `DepositDatum` names that same id. Verdict:
////   `DepositEventObserved`, carrying a 32-byte commitment to the event datum
////   and the event's inclusion time.
////
//// ### Why the verdict carries a datum hash and not the datum
////
//// The event datum embeds `DepositInfo.l2_datum`, an `Option<Data>` of
//// unbounded size chosen by the depositor. Threading it through the dispute
//// would make every later step's L1 footprint attacker-chosen; threading its
//// hash keeps each handoff fixed-size and lets step-03 re-open the preimage
//// from its own redeemer, so the family keeps working off retained
//// authenticated material after the event NFT is burned at settlement.
////
//// ### Shared with `fabricated-withdrawal` (Q40)
////
//// `verify_deposit_evidence_v1` is the piece Q40 mirrors: the same
//// unspent-nonce and hub-registered-event-NFT arguments hold for withdrawals
//// against `hub_datum.withdrawal`. It lives in this family's own lib module
//// because `GOAL_PROGRESS.md`'s triage requires Q39 and Q40 to keep disjoint
//// family paths.

use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input, Output}
use midgard/common/types.{H32, PosixTime}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/fabricated_deposit/step_01
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/user_events
use midgard/user_events/deposit

/// Blake2b-256 hash of `cbor.serialise` of a deposit event's `DepositDatum`.
pub type DepositDatumHash =
  H32<deposit.DepositDatum>

// ## Step-02 thread types

/// What step-01 hands forward: the authenticated challenged-header facts plus
/// the committed deposit leaf, with the leaf's value reduced to its commitment.
pub type State {
  challenged_header_hash: ledger_state.HeaderHash,
  header_start_time: PosixTime,
  header_end_time: PosixTime,
  committed_deposit_id: ledger_state.DepositId,
  committed_deposit_info_hash: step_01.DepositInfoHash,
}

/// The prover's chosen L1 witness about the committed deposit identity.
pub type DepositEvidenceV1 {
  /// The still-unspent L1 output at the committed `DepositId`.
  AbsentDepositIdentity { unspent_ref_input_index: Int }
  /// The authentic deposit event NFT and its datum.
  PresentDepositEvent { hub_ref_input_index: Int, event_ref_input_index: Int }
}

/// What L1 says, once authenticated. Only this reaches step-03.
pub type DepositEvidenceVerdictV1 {
  DepositIdentityAbsent
  DepositEventObserved {
    event_datum_hash: DepositDatumHash,
    event_inclusion_time: PosixTime,
  }
}

pub type Args {
  input_index: Int,
  output_index: Int,
  /// The prover's chosen L1 witness about the committed deposit identity.
  evidence: DepositEvidenceV1,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

// ## Evidence authentication

/// Authenticates the prover's L1 witness and returns the verdict step-03 argues
/// from. Every branch either produces an authenticated verdict or fails; there
/// is no arm that forwards an unauthenticated claim.
pub fn verify_deposit_evidence_v1(
  hub_oracle: ScriptHash,
  committed_deposit_id: ledger_state.DepositId,
  evidence: DepositEvidenceV1,
  reference_inputs: List<Input>,
) -> DepositEvidenceVerdictV1 {
  when evidence is {
    AbsentDepositIdentity { unspent_ref_input_index } -> {
      // The witness must be the live output at *exactly* the committed
      // identity. Any other still-unspent output proves nothing: on a
      // valid block the committed `DepositId` is spent (that spend is what
      // authenticated the event), so a prover attacking a valid block has no
      // choice but to substitute some other live output here, and this equality
      // is what refuses it. Nothing about the referenced output's content
      // matters — its continued existence is the whole argument.
      expect Some(Input { output_reference, .. }) = list.at(
        reference_inputs,
        unspent_ref_input_index,
      )
      expect output_reference == committed_deposit_id
      DepositIdentityAbsent
    }
    PresentDepositEvent { hub_ref_input_index, event_ref_input_index } -> {
      // The deposit event policy is read from the authentic hub oracle, never
      // from the redeemer, and the NFT's asset name is derived from the
      // committed identity rather than accepted from the prover — so the only
      // reference input that can satisfy this is the authentic event UTxO for
      // this very `DepositId`.
      let hub_datum = hub.get_datum(
        reference_inputs,
        hub_oracle,
        hub_ref_input_index,
      )
      let event_datum_data = utils.get_authentic_input_datum_with_nft_at(
        reference_inputs,
        hub_datum.deposit,
        user_events.out_ref_to_nonce(committed_deposit_id),
        event_ref_input_index,
      )
      expect event_datum: deposit.DepositDatum = event_datum_data
      // Belt and braces against a datum that disagrees with the NFT it sits
      // under: `authenticate_new_event` already ties the two together, and this
      // re-derives it from the committed identity rather than trusting it.
      expect event_datum.event.id == committed_deposit_id
      DepositEventObserved {
        event_datum_hash: utils.serialise_and_hash_32(event_datum_data),
        event_inclusion_time: event_datum.inclusion_time,
      }
    }
  }
}

// ## Family test fixtures
//
// Test-fixture support for this family's step selectors, unreachable from any
// on-chain validator entry point. It extends
// `midgard/fraud_proofs/native_binding_fixture_v1` rather than editing it,
// because Q39 and Q40 must keep disjoint family paths.

/// Inclusion time of the fixture's authentic deposit event, inside the
/// fixture header's `(start_time, end_time]` window (10, 20].
pub const authentic_inclusion_time_v1: PosixTime = 15

/// The deposit event NFT policy the fixture hub oracle registers.
///
/// Read out of the fixture hub datum rather than restated, so it cannot drift
/// from `native_binding_fixture_v1`'s `hub_reference_input`.
pub fn fixture_deposit_event_policy_v1() -> PolicyId {
  let hub_datum = hub.get_datum(
    [fixture.hub_reference_input()],
    fixture.hub_oracle_script_hash,
    0,
  )
  hub_datum.deposit
}

/// The step-02 thread state step-01 really produces for `block` — the exact
/// handoff its positive selector pins, so the family's steps are exercised
/// against continuous state rather than restated constants.
pub fn fixture_state_v1(block: step_01.FabricatedDepositBlockV1) -> State {
  State {
    challenged_header_hash: block.block.header_hash,
    header_start_time: block.block.header.start_time,
    header_end_time: block.block.header.end_time,
    committed_deposit_id: block.committed_deposit_id,
    committed_deposit_info_hash: step_01.committed_deposit_info_hash_v1(
      step_01.committed_deposit_membership_v1(block),
    ),
  }
}

/// The authentic L1 deposit event datum for `id`/`info`.
pub fn authentic_deposit_datum_v1(
  id: ledger_state.DepositId,
  info: ledger_state.DepositInfo,
  inclusion_time: PosixTime,
) -> deposit.DepositDatum {
  deposit.DepositDatum {
    event: ledger_state.DepositEvent { id, info },
    inclusion_time,
    witness: #"57575757575757575757575757575757575757575757575757575757",
  }
}

/// The verdict step-02 produces for `event_datum`: the commitment is the exact
/// hash of the datum bytes the authentic event UTxO carries.
pub fn observed_verdict_v1(
  event_datum: deposit.DepositDatum,
) -> DepositEvidenceVerdictV1 {
  DepositEventObserved {
    event_datum_hash: utils.serialise_and_hash_32(event_datum),
    event_inclusion_time: event_datum.inclusion_time,
  }
}

/// A reference input standing in for the authentic deposit event UTxO: one NFT
/// of `event_nft_policy` under `event_nft_asset_name`, plus the inline
/// `DepositDatum`.
pub fn deposit_event_reference_input_v1(
  event_nft_policy: PolicyId,
  event_nft_asset_name: AssetName,
  event_datum: deposit.DepositDatum,
) -> Input {
  let event_datum_data: Data = event_datum
  Input {
    output_reference: fixture.own_out_ref(),
    output: Output {
      address: address.from_script(event_nft_policy),
      value: assets.from_lovelace(3_000_000)
        |> assets.add(event_nft_policy, event_nft_asset_name, 1),
      datum: InlineDatum(event_datum_data),
      reference_script: None,
    },
  }
}

/// A reference input carrying a well-formed `DepositDatum` with **no** event
/// NFT at all — the shape any user can build, and therefore no evidence.
pub fn untrusted_deposit_datum_reference_input_v1(
  event_datum: deposit.DepositDatum,
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
