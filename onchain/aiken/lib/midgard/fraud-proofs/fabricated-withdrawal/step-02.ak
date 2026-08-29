//// ## `fabricated-withdrawal` step-02 — authenticated L1 withdrawal evidence
//// (`GOAL_SPEC.md` §9.1 / Q40)
////
//// Step-01 established *what the challenged header commits*. Step-02 establishes
//// *what L1 actually holds* about that committed identity, and it is the only
//// step in this family that reads the L1 chain. Everything it hands forward is
//// authenticated material, so the later steps never touch a prover-supplied
//// claim about L1 again.
////
//// ### The two admissible witnesses
////
//// - `AbsentWithdrawalIdentity` — the prover exhibits the **still-unspent L1
////   output at exactly the committed `WithdrawalId`**. `WithdrawalId` is the
////   output reference a withdrawal authentication transaction must have *spent*
////   (`midgard/user_events`'s `authenticate_new_event`, rule 4, requires the
////   event id to equal the spent nonce output reference, and rule 1 mints the
////   event NFT under `out_ref_to_nonce` of that same reference). An output is
////   spent at most once, so a live output at that reference is a positive proof
////   that no withdrawal event with that identity was ever authenticated. Verdict:
////   `WithdrawalIdentityAbsent`.
////
//// - `PresentWithdrawalEvent` — the prover exhibits the authentic withdrawal
////   event itself: a reference input carrying exactly one NFT of the
////   hub-registered *withdrawal* policy under the asset name
////   `out_ref_to_nonce(committed WithdrawalId)`, whose inline `withdrawal.Datum`
////   names that same id. Verdict: `WithdrawalEventObserved`, carrying a 32-byte
////   commitment to the event datum and the event's inclusion time.
////
//// ### Why the policy comes from the hub datum
////
//// `hub_datum.withdrawal` is read out of the authentic hub-oracle UTxO
//// (`hub.get_datum` authenticates it against the `hub_oracle` script hash the
//// validator is parameterised by), and the asset name is *derived* from the
//// committed identity rather than accepted from the redeemer. A prover therefore
//// cannot nominate a policy it controls, and cannot point at a genuine event NFT
//// of some other withdrawal: both halves of the token are pinned by material the
//// prover does not choose. Note the withdrawal policy is a different hub field
//// from the deposit one — a withdrawal fraud proof authenticated against
//// `hub_datum.deposit` would be reading the wrong event family entirely.
////
//// ### Why the verdict carries a datum hash and not the datum
////
//// A withdrawal event datum is `user_events.OptimisticDatum<WithdrawalEvent>`,
//// which embeds the whole `WithdrawalInfo` (multi-asset `l2_value`, an unbounded
//// `l1_datum`) *and* a `refund_address`/`refund_datum` pair, all chosen by the
//// withdrawer. Threading it through the dispute would make every later step's L1
//// footprint attacker-chosen; threading its hash keeps each handoff fixed-size.
//// It also keeps the family alive past settlement: the event NFT is burned when
//// the withdrawal is settled or refunded, so by the time a fraud proof runs the
//// UTxO exhibited here may be gone — step-03 re-opens the preimage from its own
//// redeemer and checks it against this commitment, which is retained
//// authenticated material rather than a live-chain lookup.
////
//// ### Mirror of `fabricated-deposit` (Q39)
////
//// The unspent-nonce and hub-registered-event-NFT arguments are the same ones
//// `midgard/fraud_proofs/fabricated_deposit/step_02` makes for deposits. This
//// module mirrors rather than imports them because the two families read different
//// hub fields and different datum shapes (a withdrawal event datum has five
//// fields, a `DepositDatum` three).

use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, Input, NoDatum, Output}
use midgard/common/types.{H32, PosixTime}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/fabricated_withdrawal/step_01
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/user_events
use midgard/user_events/withdrawal

/// Blake2b-256 hash of `cbor.serialise` of a withdrawal event's datum.
pub type WithdrawalDatumHash =
  H32<withdrawal.Datum>

// ## Step-02 thread types

/// What step-01 hands forward: the authenticated challenged-header facts plus the
/// committed withdrawal leaf, with the leaf's value reduced to its commitment.
pub type State {
  challenged_header_hash: ledger_state.HeaderHash,
  header_start_time: PosixTime,
  header_end_time: PosixTime,
  committed_withdrawal_id: ledger_state.WithdrawalId,
  committed_withdrawal_info_hash: step_01.WithdrawalInfoHash,
}

/// The prover's chosen L1 witness about the committed withdrawal identity.
pub type WithdrawalEvidenceV1 {
  /// The still-unspent L1 output at the committed `WithdrawalId`.
  AbsentWithdrawalIdentity { unspent_ref_input_index: Int }
  /// The authentic withdrawal event NFT and its datum.
  PresentWithdrawalEvent {
    hub_ref_input_index: Int,
    event_ref_input_index: Int,
  }
}

/// What L1 says, once authenticated. Only this reaches step-03.
pub type WithdrawalEvidenceVerdictV1 {
  WithdrawalIdentityAbsent
  WithdrawalEventObserved {
    event_datum_hash: WithdrawalDatumHash,
    event_inclusion_time: PosixTime,
  }
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
  committed_withdrawal_id: ledger_state.WithdrawalId,
  evidence: WithdrawalEvidenceV1,
  reference_inputs: List<Input>,
) -> WithdrawalEvidenceVerdictV1 {
  when evidence is {
    AbsentWithdrawalIdentity { unspent_ref_input_index } -> {
      // The witness must be the live output at *exactly* the committed identity.
      // Any other still-unspent output proves nothing: on a valid block the
      // committed `WithdrawalId` is spent (that spend is what authenticated the
      // event), so a prover attacking a valid block has no choice but to
      // substitute some other live output here, and this equality is what
      // refuses it. Nothing about the referenced output's content matters — its
      // continued existence is the whole argument.
      expect Some(Input { output_reference, .. }) = list.at(
        reference_inputs,
        unspent_ref_input_index,
      )
      expect output_reference == committed_withdrawal_id
      WithdrawalIdentityAbsent
    }
    PresentWithdrawalEvent { hub_ref_input_index, event_ref_input_index } -> {
      // The withdrawal event policy is read from the authentic hub oracle, never
      // from the redeemer, and the NFT's asset name is derived from the committed
      // identity rather than accepted from the prover — so the only reference
      // input that can satisfy this is the authentic event UTxO for this very
      // `WithdrawalId`.
      let hub_datum = hub.get_datum(
        reference_inputs,
        hub_oracle,
        hub_ref_input_index,
      )
      let event_datum_data = utils.get_authentic_input_datum_with_nft_at(
        reference_inputs,
        hub_datum.withdrawal,
        user_events.out_ref_to_nonce(committed_withdrawal_id),
        event_ref_input_index,
      )
      expect event_datum: withdrawal.Datum = event_datum_data
      // Belt and braces against a datum that disagrees with the NFT it sits
      // under: `authenticate_new_event` already ties the two together, and this
      // re-derives it from the committed identity rather than trusting it.
      expect event_datum.event.id == committed_withdrawal_id
      WithdrawalEventObserved {
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
    challenged_header_hash: block.block.header_hash,
    header_start_time: block.block.header.start_time,
    header_end_time: block.block.header.end_time,
    committed_withdrawal_id: block.committed_withdrawal_id,
    committed_withdrawal_info_hash: step_01.committed_withdrawal_info_hash_v1(
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
pub fn observed_verdict_v1(
  event_datum: withdrawal.Datum,
) -> WithdrawalEvidenceVerdictV1 {
  WithdrawalEventObserved {
    event_datum_hash: utils.serialise_and_hash_32(event_datum),
    event_inclusion_time: event_datum.inclusion_time,
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
