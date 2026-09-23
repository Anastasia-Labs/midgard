//// Reopen only immutable facts authenticated by the list capture. Pointer
//// successors, current UTxO references and operator archives are not authority.

use cardano/assets.{PolicyId}
use midgard/common/types.{PosixTime}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/event_history/horizon
use midgard/event_history/order_facts
use midgard/fraud_proofs/fabricated_deposit/step_01
use midgard/fraud_proofs/fabricated_deposit/step_02.{
  DepositEventObserved, DepositEvidenceVerdictV1, DepositIdentityAbsent,
}
use midgard/fraud_proofs/fabricated_deposit/step_04
use midgard/ledger_state

pub type State {
  state_queue_policy: PolicyId,
  challenged_header_hash: ledger_state.HeaderHash,
  header_start_time: PosixTime,
  header_end_time: PosixTime,
  committed_deposit_id: ledger_state.DepositId,
  committed_deposit_info_hash: step_01.DepositInfoHash,
  verdict: DepositEvidenceVerdictV1,
}

pub type AuthenticContentOpeningV1 {
  NoAuthenticContent
  RetainedEventData { payload: order_facts.Payload, original_assets: Data }
}

pub type Args {
  input_index: Int,
  output_index: Int,
  authentic_content: AuthenticContentOpeningV1,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

pub fn fixture_state_v1(
  input_state: step_02.State,
  verdict: DepositEvidenceVerdictV1,
) -> State {
  State {
    state_queue_policy: input_state.state_queue_policy,
    challenged_header_hash: input_state.challenged_header_hash,
    header_start_time: input_state.header_start_time,
    header_end_time: input_state.header_end_time,
    committed_deposit_id: input_state.committed_deposit_id,
    committed_deposit_info_hash: input_state.committed_deposit_info_hash,
    verdict,
  }
}

pub fn fixture_next_state_v1(
  state: State,
  fault: step_04.FabricatedDepositFaultV1,
) -> step_04.State {
  step_04.State {
    state_queue_policy: state.state_queue_policy,
    challenged_header_hash: state.challenged_header_hash,
    header_start_time: state.header_start_time,
    header_end_time: state.header_end_time,
    committed_deposit_id: state.committed_deposit_id,
    fault,
  }
}

pub fn open_authentic_deposit_content_v1(
  state: State,
  authentic_content: AuthenticContentOpeningV1,
) -> step_04.FabricatedDepositFaultV1 {
  when (state.verdict, authentic_content) is {
    (DepositIdentityAbsent, NoAuthenticContent) ->
      step_04.NonexistentDepositIdentity
    (
      DepositEventObserved { commitment },
      RetainedEventData { payload, original_assets },
    ) -> {
      expect commitment.kind == order_facts.Deposit
      expect commitment.event_id == state.committed_deposit_id
      expect order_facts.opens(commitment, payload, original_assets)
      let event_inclusion_time = commitment.inclusion_time
      if !horizon.eligible(
        event_inclusion_time,
        state.header_start_time,
        state.header_end_time,
      ) {
        step_04.IneligibleDepositEvent { event_inclusion_time }
      } else {
        expect order_facts.DepositPayload { event } = payload
        let authentic_deposit_info_hash = utils.serialise_and_hash_32(
          event.info,
        )
        expect authentic_deposit_info_hash != state.committed_deposit_info_hash
        step_04.MismatchedDepositContent {
          committed_deposit_info_hash: state.committed_deposit_info_hash,
          authentic_deposit_info_hash,
          event_inclusion_time,
        }
      }
    }
    (DepositIdentityAbsent, RetainedEventData { .. }) ->
      fail @"absence admits no content opening"
    (DepositEventObserved { .. }, NoAuthenticContent) ->
      fail @"observed Order requires authenticated content"
  }
}
