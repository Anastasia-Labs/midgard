use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel}
use midgard/native_tx_field_access_v1.{FieldCarriageV1}
use midgard/validation_machine/machine_types.{
  TransactionRedeemerItemBeginWitness, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/script_sources_early_stages.{
  verify_script_sources_stage_one_begin_raw_semantics,
}
use midgard/validation_machine_field_door_v1.{MachineFieldDoorV1}
use midgard/validation_semantic_v1.{Datum, continue_winning_at}
use midgard/validation_trace_v1.{ScriptSources}

pub type ActionV1 {
  VerifyRedeemer {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    carriage: FieldCarriageV1,
    /// The field grammar certificate's reference input, beside the evidence
    /// and not in it: the certificate is a function of the committed field.
    cert_ref_input_index: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

validator main(
  award_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
  field_grammar_certificate_policy_id: PolicyId,
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
      ct.Continue(VerifyRedeemer {
        input_index,
        output_index,
        transition,
        carriage,
        cert_ref_input_index,
      }) -> {
        let auxiliary_data: Data = TransactionRedeemerItemBeginWitness {
          carriage,
        }
        let door = MachineFieldDoorV1 {
          reference_inputs: tx.reference_inputs,
          certificate_policy_id: field_preimage_certificate_policy_id,
        }
        continue_winning_at(
          ScriptSources,
          award_script_hash,
          computation_thread_policy_id,
          datum,
          input_index,
          output_index,
          transition,
          auxiliary_data,
          fn(pre) {
            verify_script_sources_stage_one_begin_raw_semantics(
              pre,
              transition,
              carriage,
              door,
              field_grammar_certificate_policy_id,
              cert_ref_input_index,
            )
          },
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
