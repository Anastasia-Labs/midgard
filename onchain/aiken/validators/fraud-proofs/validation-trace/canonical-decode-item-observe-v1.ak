use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/canonical_decode_item_staging_v1 as staging
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/native_tx_field_access_v1.{FieldCarriageV1, Inline}
use midgard/validation_machine_field_door_v1.{MachineFieldDoorV1}
use midgard/validation_machine_v1.{
  TransactionFieldItemWitness, ValidationAuxiliaryWitnessV1,
  ValidationProofItemDatumV1, observe_canonical_decode_item_v1,
}
use midgard/validation_trace_v1.{ValidationMachineStateV1}

pub type Datum =
  ct.StepDatum<staging.PreparedCanonicalDecodeItemV1>

pub type ActionV1 {
  Observe { input_index: Int, output_index: Int, carriage: FieldCarriageV1 }
  ObserveReference {
    input_index: Int,
    output_index: Int,
    reference_input_index: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  proof_verifier_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  proof_item_script_hash: ScriptHash,
  field_preimage_certificate_policy_id: PolicyId,
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
      ct.Continue(Observe { input_index, output_index, carriage }) ->
        observe_item(
          proof_verifier_script_hash,
          computation_thread_policy_id,
          field_preimage_certificate_policy_id,
          datum,
          input_index,
          output_index,
          TransactionFieldItemWitness { carriage },
          own_out_ref,
          tx,
        )
      ct.Continue(ObserveReference {
        input_index,
        output_index,
        reference_input_index,
      }) -> {
        let pre = datum |> semantic_pre_state
        let auxiliary = proof_item_from_reference(
          proof_item_script_hash,
          pre,
          reference_input_index,
          tx.reference_inputs,
        )
        expect TransactionFieldItemWitness { carriage } = auxiliary
        observe_item(
          proof_verifier_script_hash,
          computation_thread_policy_id,
          field_preimage_certificate_policy_id,
          datum,
          input_index,
          output_index,
          TransactionFieldItemWitness { carriage },
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

fn observe_item(
  proof_verifier_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
  datum: Option<Datum>,
  input_index: Int,
  output_index: Int,
  auxiliary: ValidationAuxiliaryWitnessV1,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  let Transaction { inputs, outputs, .. } = tx
  expect Some(step_datum) = datum
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
      expect Some(prepared) = input_state
      expect TransactionFieldItemWitness { carriage } = auxiliary
      // The one place the §8.8 door runs for the complete-item path — and,
      // since Option B (#620) narrowed the prepare-time commitment to the
      // transition alone, the sole content gate on this path: the retired
      // evidence-hash equality here re-checked a commitment that froze the
      // delivery wrapper three transactions early, while the door binds the
      // carriage's bytes to the challenged block's own field commitment
      // whichever tier delivers them. Everything downstream of this stage
      // consumes the numbers it recorded; see `CanonicalDecodeItemObservationV1`
      // on why a later stage cannot re-check them from a datum alone.
      let expected_output_state: Data = staging.observe(
        prepared,
        observe_canonical_decode_item_v1(
          prepared.authenticated.base.resolution.pre_state,
          prepared.authenticated.transition,
          MachineFieldDoorV1 {
            reference_inputs: tx.reference_inputs,
            certificate_policy_id: field_preimage_certificate_policy_id,
          },
          carriage,
        ),
      )
      and {
        staging.prepared_is_well_formed(prepared),
        output_script_hash == proof_verifier_script_hash,
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
  // §8.1 `Inline` is constructed here, from bytes read off the publication's
  // own datum: the consuming step names a reference input, not a tier, so no
  // prover can name a carriage the door was not going to hash itself.
  TransactionFieldItemWitness {
    carriage: Inline { preimage: proof_item.field_preimage },
  }
}

fn semantic_pre_state(datum: Option<Datum>) {
  expect Some(ct.StepDatum { data: Some(state), .. }) = datum
  state.authenticated.base.resolution.pre_state
}
