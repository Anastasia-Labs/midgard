use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/state_queue_yield.{require_authenticated_zero_yield}
use midgard/validation_asset_fold.{asset_fold_role}
use midgard/validation_machine/machine_types.{
  AssetFoldClaim, ValidationOneStepWitnessV1, ValueInputAssetWitness,
}
use midgard/validation_machine/value_and_mint.{
  verify_value_and_mint_replay_asset_dispatch,
}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_trace_v1.{ValueAndMint}

pub type ActionV1 {
  VerifyReplayAsset {
    claim: AssetFoldClaim,
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    source_kind: Int,
    key: ByteArray,
    next_schedule_hash: ByteArray,
    yield_to_ref_input_index: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  award_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  reference_script_auth_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(VerifyReplayAsset {
        claim,
        input_index,
        output_index,
        transition,
        source_kind,
        key,
        next_schedule_hash,
        yield_to_ref_input_index,
      }) -> {
        let yield_hash = require_authenticated_zero_yield(
          tx.reference_inputs,
          tx.withdrawals,
          tx.redeemers,
          reference_script_auth_policy_id,
          asset_fold_role,
          yield_to_ref_input_index,
        )
        expect Some(d) = claim.descriptor
        let auxiliary = ValueInputAssetWitness {
          source_kind,
          key,
          next_schedule_hash,
          policy_id: claim.policy_id,
          asset_name: claim.asset_name,
          quantity: claim.quantity,
          mutation: claim.mutation,
          descriptor_cbor: d.descriptor_cbor,
          asset_index: d.asset_index,
          asset_peaks: d.asset_peaks,
          asset_siblings: d.asset_siblings,
        }
        let auxiliary_data: Data = auxiliary
        continue_winning(
          ValueAndMint,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          yield_hash != #"" && verify_value_and_mint_replay_asset_dispatch(
            datum |> semantic_pre_state,
            transition,
            source_kind,
            key,
            next_schedule_hash,
            claim,
          ),
          own_out_ref,
          tx,
        )
      }
    }
  }

  else(_) {
    fail
  }
}

fn semantic_pre_state(datum: Option<Datum>) {
  expect Some(ct.StepDatum { data: Some(state), .. }) = datum
  state.resolution.pre_state
}
