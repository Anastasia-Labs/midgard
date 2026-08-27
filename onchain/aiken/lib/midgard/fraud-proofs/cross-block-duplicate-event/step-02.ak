//// ## `cross-block-duplicate-event` step-02 — confirmed-history duplicate
////
//// Step-01 authenticated the challenged live header and opened its committed
//// event leaf. This final step opens the same event identity under a different,
//// confirmed header's root. The historical commitment is the settlement NFT
//// and datum created by `settlement.mint.Spawn`: its asset name is the merged
//// header hash and its datum retains the merged event roots. No live event NFT
//// is read, so settlement may already have burned the deposit/withdrawal NFT.

use aiken/cbor
use aiken/primitive/bytearray
use cardano/address
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/cross_block_duplicate_event/step_01 as step_01
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/settlement
use midgard/transition_trace

pub type State {
  challenged_header_hash: ledger_state.HeaderHash,
  settlement_policy_id: PolicyId,
  event_kind: step_01.DuplicateEventKindV1,
  event_key: OutputReference,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  settlement_ref_input_index: Int,
  /// Membership of the same event under the confirmed settlement root.
  settled_event: step_01.CommittedDuplicateEventProofV1,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

/// Authenticates the settlement node and proves a same-domain, same-key event
/// membership under its counted root. Returns the confirmed header hash from
/// the settlement NFT so the caller can retain/audit both header identities.
pub fn verify_confirmed_duplicate_v1(
  state: State,
  settlement_ref_input_index: Int,
  settled_event: step_01.CommittedDuplicateEventProofV1,
  reference_inputs: List<Input>,
) -> ledger_state.HeaderHash {
  utils.get_authentic_input_datum_and_asset_name_with_policy_at(
    reference_inputs,
    state.settlement_policy_id,
    settlement_ref_input_index,
    fn(settled_header_hash, settlement_datum_data) {
      // `settlement.mint.Spawn` names the NFT with a HeaderHash. Enforce the
      // width here so a token under the settlement policy cannot smuggle an
      // arbitrary identifier into the proof record.
      expect bytearray.length(settled_header_hash) == 28
      // A block cannot prove itself duplicated. In the real state machine the
      // state-queue node is consumed before its settlement NFT is spawned, but
      // this explicit inequality makes the two-header requirement local.
      expect settled_header_hash != state.challenged_header_hash

      expect settled: settlement.Datum = settlement_datum_data
      when (state.event_kind, settled_event) is {
        (
          step_01.DuplicateDepositV1,
          step_01.CommittedDuplicateDepositV1 { membership },
        ) -> {
          expect membership.key == state.event_key
          // The settlement datum does not repeat the count. The supplied count
          // is still bound: it is hashed into `membership.root`, which must be
          // exactly the settlement's counted root.
          expect
            transition_trace.verify_root_membership_with_bytes(
              membership,
              transition_trace.DepositsRootDomain,
              settled.deposits_root,
              membership.count,
              cbor.serialise(membership.key),
              cbor.serialise(membership.value),
            )
        }
        (
          step_01.DuplicateWithdrawalV1,
          step_01.CommittedDuplicateWithdrawalV1 { membership },
        ) -> {
          expect membership.key == state.event_key
          expect
            transition_trace.verify_root_membership_with_bytes(
              membership,
              transition_trace.WithdrawalsRootDomain,
              settled.withdrawals_root,
              membership.count,
              cbor.serialise(membership.key),
              cbor.serialise(membership.value),
            )
        }
        _ -> fail @"cross-block-duplicate-event: root domains differ"
      }
      settled_header_hash
    },
  )
}

// ## Family test fixtures

/// The settlement policy registered by the native binding fixture's authentic
/// hub oracle.
pub fn fixture_settlement_policy_v1() -> PolicyId {
  let hub_datum = hub.get_datum(
    [fixture.hub_reference_input()],
    fixture.hub_oracle_script_hash,
    0,
  )
  hub_datum.settlement
}

/// A different header identity over the same committed roots, standing in for
/// the earlier header whose merge spawned the settlement node.
pub fn historical_header_hash_v1(
  block: step_01.DuplicateEventBlockV1,
) -> ledger_state.HeaderHash {
  let historical_header = ledger_state.HeaderV1 {
    ..block.block.header,
    start_time: 0,
    end_time: 10,
  }
  let historical_header_data: Data = historical_header
  utils.serialise_and_hash_28(historical_header_data)
}

/// Authentic settlement reference input retaining `block`'s event roots.
pub fn settlement_reference_input_v1(
  block: step_01.DuplicateEventBlockV1,
  settled_header_hash: ledger_state.HeaderHash,
  settlement_policy: PolicyId,
) -> Input {
  let datum: Data = settlement.Datum {
    deposits_root: block.block.header.deposits_root,
    withdrawals_root: block.block.header.withdrawals_root,
    forced_transactions_root: block.block.header.forced_transactions_root,
    transactions_root: block.block.header.transactions_root,
    resolution_claim: None,
  }
  Input {
    output_reference: OutputReference {
      transaction_id: #"7373737373737373737373737373737373737373737373737373737373737373",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(settlement_policy),
      value: assets.from_lovelace(3_000_000)
        |> assets.add(settlement_policy, settled_header_hash, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}
