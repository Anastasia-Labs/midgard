use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/bounded_collection_v1
use midgard/canonical_decode_item_staging_v1 as staging
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/validation_machine_v1.{
  TransactionFieldItemWitness, ValidationAuxiliaryWitnessV1,
  ValidationOneStepWitnessV1, ValidationProofItemDatumV1,
}
use midgard/validation_resolution_v1.{hash_one_step_evidence}
use midgard/validation_semantic_v1.{Datum}
use midgard/validation_trace_v1.{ValidationMachineStateV1}

pub type ActionV1 {
  Verify {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    collection_proof: bounded_collection_v1.ItemProofV1,
    item_cbor: ByteArray,
  }
  VerifyReference {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    reference_input_index: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  source_binder_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  proof_item_script_hash: ScriptHash,
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
      ct.Continue(Verify {
        input_index,
        output_index,
        transition,
        collection_proof,
        item_cbor,
      }) ->
        bind_item_source(
          source_binder_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          TransactionFieldItemWitness { collection_proof, item_cbor },
          own_out_ref,
          tx,
        )
      ct.Continue(VerifyReference {
        input_index,
        output_index,
        transition,
        reference_input_index,
      }) -> {
        let pre = datum |> semantic_pre_state
        let auxiliary = proof_item_from_reference(
          proof_item_script_hash,
          pre,
          reference_input_index,
          tx.reference_inputs,
        )
        expect TransactionFieldItemWitness { collection_proof, item_cbor } = auxiliary
        bind_item_source(
          source_binder_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          TransactionFieldItemWitness { collection_proof, item_cbor },
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

fn bind_item_source(
  source_binder_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  datum: Option<Datum>,
  input_index: Int,
  output_index: Int,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, .. } = tx
  expect Some(step_datum) = datum
  let transition_data: Data = transition
  let auxiliary_data: Data = auxiliary
  continue(
    computation_thread_policy_id,
    step_datum,
    input_index,
    output_index,
    own_out_ref,
    inputs,
    outputs,
    fn(
      _input_script_hash,
      _computation_thread_token_asset_name,
      _fraud_prover,
      input_state,
      output_script_hash,
      output_state,
    ) {
      expect Some(base) = input_state
      let expected_output_state: Data = staging.authenticate(base, transition)
      and {
        hash_one_step_evidence(transition_data, auxiliary_data) == base.evidence_hash,
        output_script_hash == source_binder_script_hash,
        output_state == expected_output_state,
      }
    },
  )
}

fn proof_item_from_reference(
  proof_item_script_hash: ScriptHash,
  pre: ValidationMachineStateV1,
  reference_input_index: Int,
  reference_inputs: List<Input>,
) -> ValidationAuxiliaryWitnessV1 {
  expect
      Some(Input {
        output: Output {
          address,
          datum: InlineDatum(item_data),
          reference_script: None,
          ..
        },
        ..
      })
    = reference_inputs |> list.at(reference_input_index)
  expect Script(actual_script_hash) = address.payment_credential
  expect actual_script_hash == proof_item_script_hash
  expect proof_item: ValidationProofItemDatumV1 = item_data
  expect proof_item.version == 1
  expect proof_item.transaction_id == pre.transaction_id
  expect proof_item.transaction_commitment == pre.transaction_commitment
  TransactionFieldItemWitness {
    collection_proof: proof_item.collection_proof,
    item_cbor: proof_item.item_cbor,
  }
}

fn semantic_pre_state(datum: Option<Datum>) {
  expect Some(ct.StepDatum { data: Some(state), .. }) = datum
  state.resolution.pre_state
}
