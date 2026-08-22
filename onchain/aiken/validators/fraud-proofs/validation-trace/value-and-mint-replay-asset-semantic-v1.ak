use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/validation_machine_v1.{
  ValidationOneStepWitnessV1, ValueAssetMutationWitnessV1,
  ValueInputAssetWitness, verify_value_and_mint_replay_asset_semantics_v1,
}
use midgard/validation_merkle_v1.{FrontierPeak}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_trace_v1.{ValueAndMint}

pub type ActionV1 {
  VerifyReplayAsset {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    source_kind: Int,
    key: ByteArray,
    next_schedule_hash: ByteArray,
    descriptor_cbor: ByteArray,
    asset_index: Int,
    policy_id: ByteArray,
    asset_name: ByteArray,
    quantity: Int,
    asset_peaks: List<FrontierPeak>,
    asset_siblings: List<ByteArray>,
    mutation: ValueAssetMutationWitnessV1,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  award_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
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
        input_index,
        output_index,
        transition,
        source_kind,
        key,
        next_schedule_hash,
        descriptor_cbor,
        asset_index,
        policy_id,
        asset_name,
        quantity,
        asset_peaks,
        asset_siblings,
        mutation,
      }) -> {
        let auxiliary = ValueInputAssetWitness {
          source_kind,
          key,
          next_schedule_hash,
          descriptor_cbor,
          asset_index,
          policy_id,
          asset_name,
          quantity,
          asset_peaks,
          asset_siblings,
          mutation,
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
          verify_value_and_mint_replay_asset_semantics_v1(
            datum |> semantic_pre_state,
            transition,
            source_kind,
            key,
            next_schedule_hash,
            descriptor_cbor,
            asset_index,
            policy_id,
            asset_name,
            quantity,
            asset_peaks,
            asset_siblings,
            mutation,
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
