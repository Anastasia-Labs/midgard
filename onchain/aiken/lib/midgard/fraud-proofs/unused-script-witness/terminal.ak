//// Terminal decision for `unusedScriptWitness` (arm 33, phase 8 audit,
//// coordinate the field-6 item index). A field-6 witness is used when it is
//// the selected source for its script hash and some purpose of the
//// transaction executes that hash. It is unused when another source is
//// selected for its hash (a resolved-output reference script, or an earlier
//// witness with the same hash), or when no purpose reaches its hash.

use aiken/cbor
use aiken/merkle_patricia_forestry.{Proof}
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/field_opening_v1.{script_witnesses_field_index}
use midgard/fraud_proofs/first_fault_v1.{UnusedAuditFault, UnusedWitnessAudit}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/unused_script_witness/walk.{WalkStateV1}
use midgard/mpf_proof_v1 as mpf
use midgard/rejection_reason_v1.{UnusedScriptWitness}

/// The selected source for the witness's script hash and its source-map
/// proof.
pub type SelectionV1 {
  source: SourceV1,
  proof: Proof,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  selection: Option<SelectionV1>,
}

pub type Datum =
  ct.StepDatum<WalkStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

/// Whether the witness at `script_index` is unused. A field that ends before
/// the index has no such witness, so nothing is unused there. The scan role
/// hands the walk over only once it has read the witness or exhausted the
/// field (`walk.is_finished_v1`), so neither is checked again here.
pub fn unused_v1(state: WalkStateV1, selection: Option<SelectionV1>) -> Bool {
  when state.witness_hash is {
    None -> False
    Some(hash) -> {
      expect Some(SelectionV1 { source, proof }) = selection
      // Every field-6 witness entered the source map, and each value is stored
      // under its own script hash (`source_index.admit_v1`), so membership at
      // `hash` names the source selected for this witness's hash.
      expect
        mpf.has(
          state.result.source_index.root,
          hash,
          cbor.serialise(source),
          proof,
        )
      if and {
        source.field_index == script_witnesses_field_index,
        source.item_index == state.script_index,
      } {
        // The witness is the selected source: it is used exactly when some
        // purpose reaches its hash, which the walk folded against the probe.
        expect state.result.facts.probe == Some(hash)
        !state.result.facts.probe_reached
      } else {
        True
      }
    }
  }
}

/// A wrongful acceptance convicts when the witness is unused. A wrongful
/// rejection convicts when it records this witness and the witness is used,
/// or when the witness is unused and the recorded reason ranks after this
/// phase-8 audit position.
pub fn convicts_v1(state: WalkStateV1, selection: Option<SelectionV1>) -> Bool {
  let bound = state.result.bound
  let subject = bound.subject
  let unused = unused_v1(state, selection)
  substrate.terminal_contradiction_v1(
    subject,
    when subject.rejection_reason is {
      None -> unused
      Some(reason) ->
        if reason == UnusedScriptWitness { script_index: state.script_index } {
          unused
        } else {
          !unused || first_fault_v1.leaf_admits_later_fault_v1(
            subject,
            bound.network_order_context,
            UnusedAuditFault(
              UnusedWitnessAudit { script_index: state.script_index },
            ),
          )
        }
    },
  )
}
