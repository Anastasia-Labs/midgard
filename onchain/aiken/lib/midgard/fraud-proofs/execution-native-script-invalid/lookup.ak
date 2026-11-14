//// The source of the purpose the shared route selected. An ordinal past the
//// last purpose of its kind, an absent source, or a source of another
//// language has no native execution. A native source starts a paged
//// evaluation over its committed item, under the transaction's validity
//// interval, once the signer frontier is built. The redeemer actions are the
//// shared adjudication's (`claim_adjudicate.ActionV1`); `LookupScan` has no
//// use here.

use aiken/merkle_patricia_forestry.{Proof}
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/claim_adjudicate.{ActionV1}
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as workers
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/execution_native_script_invalid/types.{
  EvalV1, FinalV1, NoExecution,
}
use midgard/fraud_proofs/field_opening_v1.{source_kind_of_anchor}
use midgard/fraud_proofs/native_tx/compact.{decode_tx_compact_view_v1}

/// `script_proof_v1.language_tag(NativeCardanoScript)`.
pub const native_language_tag: Int = 0

pub type Datum =
  ct.StepDatum<SelectionV1>

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>

pub type LookedUpV1 {
  NativeSource { state: EvalV1 }
  NoNativeSource { state: FinalV1 }
}

/// The shared route found no purpose at the selected ordinal.
pub fn resolve_v1(selection: SelectionV1) -> FinalV1 {
  expect None = selection.purpose
  FinalV1 {
    bound: selection.bound,
    fact: NoExecution { coordinate: selection.coordinate },
  }
}

pub fn lookup_v1(
  selection: SelectionV1,
  source: Option<SourceV1>,
  proof: Proof,
) -> LookedUpV1 {
  let authenticated = workers.lookup_v1(selection, source, proof)
  let bound = selection.bound
  let none = NoNativeSource {
    state: FinalV1 {
      bound,
      fact: NoExecution { coordinate: selection.coordinate },
    },
  }
  when authenticated.source is {
    None -> none
    Some(present) ->
      if present.language_tag == native_language_tag {
        let body = decode_tx_compact_view_v1(
          source_kind_of_anchor(bound.subject.source_kind),
          bound.compact_cbor,
        ).body
        NativeSource {
          state: EvalV1 {
            bound,
            coordinate: selection.coordinate,
            item_commitment: present.item_commitment,
            validity_interval_start: body.validity_interval_start,
            validity_interval_end: body.validity_interval_end,
            signer_count: 0,
            signer_peaks: [],
            total_length: 0,
            cursor: 0,
            stack_root: #"",
            stack_depth: 0,
            pending: None,
          },
        }
      } else {
        none
      }
  }
}
