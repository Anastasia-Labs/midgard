use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/bounded_item_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/resolved_output_non_canonical/rule.{CanonicalVerdictV1}
use midgard/fraud_proofs/resolved_output_non_canonical/step_03.{
  ReconstructionV1,
}
use midgard/fraud_proofs/resolved_output_non_canonical/step_04.{
  Advance, Args, Datum, FinalizeCanonical, SpendRedeemer,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_scan_v1

fn valid_chunk(
  descriptor: ledger_output_commitment_v1.LedgerOutputCommitmentV1,
  proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  and {
    proof.field_index == ledger_output_commitment_v1.output_field_index,
    proof.item_index == descriptor.output_index,
    proof.total_length == descriptor.total_length,
    ledger_output_commitment_v1.verify_chunk(descriptor, proof),
  }
}

validator main(step_05_hash: ScriptHash, thread_policy: PolicyId) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args { input_index, output_index, action }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _, _, state, next_hash, next_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(ReconstructionV1 { subject, descriptor_cbor, control }) = state
        let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
        when action is {
          Advance { chunk_proof, next_chunk_proof } -> {
            expect valid_chunk(descriptor, chunk_proof)
            let expected_chunk_index = control.cursor / bounded_item_v1.chunk_bytes
            expect chunk_proof.chunk_index == expected_chunk_index
            let has_next = chunk_proof.chunk_index + 1 < bounded_item_v1.chunk_count(
              descriptor.total_length,
            )
            expect
              if has_next {
                next_chunk_proof != None
              } else {
                next_chunk_proof == None
              }
            let window = when next_chunk_proof is {
              None -> chunk_proof.chunk
              Some(next_proof) -> {
                expect and {
                    next_proof.chunk_index == chunk_proof.chunk_index + 1,
                    valid_chunk(descriptor, next_proof),
                  }
                bytearray.concat(chunk_proof.chunk, next_proof.chunk)
              }
            }
            let advanced = ledger_output_scan_v1.step_v1(
              control,
              descriptor.total_length,
              window,
              control.cursor - chunk_proof.chunk_index * bounded_item_v1.chunk_bytes,
            )
            when advanced is {
              Some(next_control) -> {
                let output: Data = ReconstructionV1 {
                  subject,
                  descriptor_cbor,
                  control: next_control,
                }
                and {
                  next_hash == own_hash,
                  next_state == output,
                }
              }
              None -> {
                let output: Data = CanonicalVerdictV1 {
                  subject,
                  output_is_non_canonical: True,
                }
                and {
                  next_hash == step_05_hash,
                  next_state == output,
                }
              }
            }
          }
          FinalizeCanonical -> {
            expect Some(terminal) = ledger_output_scan_v1.finish_v1(
              control,
              descriptor.total_length,
            )
            expect
              ledger_output_scan_v1.terminal_is_exact_v1(
                terminal,
                descriptor.total_length,
              )
            let output: Data = CanonicalVerdictV1 {
              subject,
              output_is_non_canonical: False,
            }
            and {
              next_hash == step_05_hash,
              next_state == output,
            }
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
