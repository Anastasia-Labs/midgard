# `resolvedOutputNonCanonical` V1 maximum-shape and size plan

- Frozen category ID: `00000026`.
- Typed rejection reason: `InputSpentOutputNonCanonical { source_kind,
input_index }`.
- Subject: the output referenced by one field-0 normal input or field-1
  reference input of an accepted or forced native transaction, in the ledger
  committed by that block's `prev_utxos_root`.
- Logical topology: five family-owned computation-thread steps after generic
  `Init`.

## Applied validators, state, and parameter order

1. `fraud-proofs/resolved-output-non-canonical/step-01`
   `(step_02_hash, computation_thread_policy, hub_oracle)`: authenticates the
   accepted/forced transaction, binds `(source_kind, input_index)` and the
   authenticated header's prior UTxO root, and (for wrongful rejection) binds
   the exact typed reason. It forwards `BoundInputV1`.
2. `fraud-proofs/resolved-output-non-canonical/step-02`
   `(step_03_hash, computation_thread_policy, field_preimage_certificate_policy)`:
   opens the exact input item from
   field 0 or 1, decodes only its canonical out-ref shape, and forwards
   `AuthenticatedOutRefV1`. A caller cannot substitute an out-ref.
3. `fraud-proofs/resolved-output-non-canonical/step-03`
   `(step_04_hash, computation_thread_policy)`: proves membership of exactly
   that out-ref key and compact descriptor in the bound prior UTxO root,
   decodes the descriptor canonically, and binds its exact output item
   commitment. It forwards `ReconstructionV1` with an initial family-local
   `ledger_output_scan_v1` control.
4. `fraud-proofs/resolved-output-non-canonical/step-04`
   `(step_05_hash, computation_thread_policy)`: a resumable
   self-loop. Each transition authenticates the required bounded output chunk
   against the descriptor's item commitment and advances the shared canonical
   ledger-output structural scan once. It forwards either another
   `ReconstructionV1` or terminal `CanonicalVerdictV1`; invalid structure is a
   terminal non-canonical verdict, while a terminal structural scan over the
   descriptor's exact committed item is a canonical verdict.
5. `fraud-proofs/resolved-output-non-canonical/step-05`
   `(fraud_proof_policy, fraud_proof_address, computation_thread_policy)`:
   recomputes polarity from the authenticated
   verdict and finalizes only an accepted transaction that spent a
   non-canonical resolved output, or a forced transaction rejected for the
   exact bound reason whose resolved output is canonical.

Every applied validator has the common computation-thread cancel branch.
Step 04 is the sole self-loop. No module imports or delegates to the distinct
`transactionOutputNonCanonical`/own-output family.

## Maximum dynamic evidence

The maximum admitted output preimage is 16,384 bytes, split according to the
existing `bounded_item_v1` chunk domain. The largest prior-ledger membership
opening is the protocol MPF maximum-depth proof and may use the existing raw
redeemer or published-proof-chunk carriage. The largest canonical path is a
four-field output containing the maximum admitted multi-asset value, maximum
canonical datum traversal, and a maximum admitted native reference script;
the engine's value, datum, reference-script commitment/hash, and native-script
structural stages are checkpointed in constant-bounded state and never place
the complete output in the thread datum.

The decisive mutation frontier includes: wrong input coordinate; normal versus
reference source substitution; mutated transaction ID or output index; prior
root substitution; membership value substitution; descriptor index or item
commitment substitution; output chunk reorder/mutation; non-canonical map,
integer, datum, value, and reference-script encodings; premature terminal;
canonical output under accepted polarity; and non-canonical output under the
forced wrongful-rejection polarity.

## Imported semantic engines and reachability boundary

The family imports the shared, already-production `ledger_output_scan_v1` only
from step 04; it deliberately does not pull broader resolved value, datum, or
reference-script descriptor interpretation into this typed structural reason.
Step 01 can reach only verdict-subject and native transaction
authentication; step 02 only the native field-opening door and out-ref codec;
step 03 only prior-ledger MPF membership, descriptor decoding, and output item
commitment initialization; step 05 only the family terminal-polarity rule.
This directional split makes a proof independent of all own-output adapters.

## Reproducible fit gate

The family fit ledger records the fresh testnet blueprint digest and locally
evaluated signed transaction bytes, memory, CPU, and hard-limit margins for all
five applied reference-script publications; a work-feasible maximum-depth
prior-ledger MPF membership; maximum Certified field-carriage publication and
certificate; every step including all step-04 self-loop stages; cancellation;
terminal mint; and state-queue target plus descendant removal. Acceptance uses
ordinary Van Rossem limits: signed bytes `<= 16,384`, memory `<= 16,500,000`,
CPU `<= 10,000,000,000`; reference publication additionally targets
`<= 15,872` bytes. Local UPLC evaluation remains enabled.

## Wrongful-rejection direction: state/transition sketch

The forced direction (`ForcedTxInvalid { InputSpentOutputNonCanonical
{ source_kind, input_index } }` on a forced leaf whose resolved output is
canonical) rides the same five physical scripts, the same parameter order,
and the same canonical wire shapes as the accepted direction. No physical
split is required: every forced-only branch is a data path through code the
accepted direction already compiles (step 01's `ForcedSource` arm, step 04's
`FinalizeCanonical` arm, step 05's rejection polarity), and the measured
publications stay inside the 15,872-byte reserve.

| Physical step | Forced-direction transition                                                                                                                                                                                                                                                                                                                                                                           |
| ------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| step 01       | `ForcedSource { header, membership, direction: 1 }`: `bind_forced_subject_to_thread_v1` binds the header to the thread token, opens the leaf under the counted forced-transactions root, re-verifies the adjudicated proof source and tx id, and copies the leaf's typed reason into the subject; `bind_input_v1` then requires that reason to be exactly `InputSpentOutputNonCanonical { source_kind, input_index }` for the claimed coordinate and binds `header.prev_utxos_root`. Forwards `BoundInputV1`. |
| step 02       | Unchanged: opens field 0 (`source_kind = 0`) or field 1 (`source_kind = 1`) of the forced transaction's committed compact bytes (raw, published or certified carriage) and decodes the exact out-ref at `input_index`. Forwards `AuthenticatedOutRefV1`.                                                                                                                                                 |
| step 03       | Unchanged: proves the out-ref key and descriptor in `prev_utxos_root` and starts the scan control. Forwards `ReconstructionV1`.                                                                                                                                                                                                                                                                       |
| step 04       | `Advance` self-loops through `advance_reconstruction_v1` exactly as the accepted direction does. The direction closes through `FinalizeCanonical`, admitted only at the control the engine's zero-byte closing edge accepts (`finish_v1`, optional fields complete, `cursor == total_length`) whose terminal is exact (`terminal_is_exact_v1`). A control that can finish is never advanced (see below). Forwards `CanonicalVerdictV1 { output_is_non_canonical: False }`. |
| step 05       | `terminal_contradiction_v1`: a rejection subject closes only on `False`; an accepted subject only on `True`.                                                                                                                                                                                                                                                                                          |

The finishing rule is the one on-chain change this direction needed. The
shared engine's `step_v1` refuses a window whose offset has reached the
window's end, and at `cursor == total_length` that refusal is exactly what
`Advance` would hand it; step 04 previously read every `None` as the
structural non-canonical verdict, so a prover could close a canonical
resolved output as non-canonical under an accepted subject.
`advance_reconstruction_v1` now refuses to advance a control that
`finish_v1` can already close, which makes `None` mean a structural fault at
the cursor and nothing else. The off-chain driver was aligned with the same
rule: it no longer persists a pseudo-terminal control and issues
`FinalizeCanonical` at the finishable control.

Maximum dynamic evidence for the forced direction is the largest canonical
resolved output the ledger admits: a 16,384-byte four-entry output (address,
multi-asset value, datum payload, reference script) walked to its exact end,
under both source kinds. Its transaction's input field takes the same raw,
published or Certified carriage as the accepted direction. The forced
mutation frontier adds: a leaf committed under another typed reason or
coordinate; a subject whose reason was rewritten to match a lying coordinate;
a forced root, header or leaf substitution at the forced-leaf seam; premature
`FinalizeCanonical`; `Advance` at the finishable control; and `Advance` with
substituted chunk bytes or a checkpoint the thread never committed.

The family fit ledger records, beside the accepted rows, every forced
lifecycle transaction at that maximum shape: forced step 01, the field
opening, the prior-ledger membership, every step-04 stage including the
`FinalizeCanonical` closing transaction, the permanent mint, cancellation and
removal.
