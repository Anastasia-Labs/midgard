use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/validation_machine/machine_types.{
  ValidationOneStepWitnessV1, ValueAssetMutationWitnessV1,
  ValueOutputAssetWitness,
}
use midgard/validation_machine/value_and_mint.{
  verify_value_and_mint_output_asset_semantics_v1,
}
use midgard/validation_merkle_v1.{FrontierPeak}
use midgard/validation_semantic_v1.{Datum, continue_winning}
use midgard/validation_trace_v1.{ValueAndMint}

pub type ActionV1 {
  VerifyOutputAsset {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    ledger_output_index: Int,
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
      ct.Continue(VerifyOutputAsset {
        input_index,
        output_index,
        transition,
        ledger_output_index,
        descriptor_cbor,
        asset_index,
        policy_id,
        asset_name,
        quantity,
        asset_peaks,
        asset_siblings,
        mutation,
      }) -> {
        let auxiliary = ValueOutputAssetWitness {
          output_index: ledger_output_index,
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
          verify_value_and_mint_output_asset_semantics_v1(
            datum |> semantic_pre_state,
            transition,
            ledger_output_index,
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
