# `missingRedeemer` V1 maximum-shape and transition plan

- Frozen category ID: `0000002e`.
- Typed rejection reason: `RedeemerMissing { purpose_kind, purpose_index }`.
- Subject: one authenticated Plutus-matched purpose in the complete canonical
  purpose frontier of an accepted or forced native transaction, and that
  transaction's complete field-8 redeemer collection.
- Logical topology: five family-owned computation-thread steps after generic
  `Init`, implemented by seven physical validators so trace verification and
  frontier membership each remain independently publishable.

## Applied validators and reverse parameter order

1. `fraud-proofs/missing-redeemer/step-01`
   `(step_02_hash, computation_thread_policy, hub_oracle)` binds the accepted
   or forced transaction, exact purpose coordinate, and exact typed reason.
2. `fraud-proofs/missing-redeemer/step-02`
   `(step_02a_hash, computation_thread_policy)` authenticates the exact event,
   descriptor verdict/reason, and header validation-trace membership.
3. `fraud-proofs/missing-redeemer/step-02a`
   `(step_02b_hash, computation_thread_policy)` authenticates the machine
   state, trace state, native proof-source commitment, and exact stage-10
   `ScriptSourcesControlV1` work root.
4. `fraud-proofs/missing-redeemer/step-02b`
   `(step_03_hash, computation_thread_policy)` authenticates the selected
   purpose leaf and its exact matched Plutus source descriptor.
5. `fraud-proofs/missing-redeemer/step-03`
   `(step_04_hash, computation_thread_policy, field_preimage_certificate_policy)`
   opens the complete committed field-8 collection and initializes its
   digest-bound pointer walk.
6. `fraud-proofs/missing-redeemer/step-04`
   `(step_05_hash, computation_thread_policy, field_preimage_certificate_policy)`
   resumes fixed-size batches, total-decodes every pointer, and self-loops
   until a match is found or the complete field is exhausted.
7. `fraud-proofs/missing-redeemer/step-05`
   `(fraud_proof_policy, fraud_proof_address, computation_thread_policy)`
   checks accepted/forced polarity, burns the thread token, and permanently
   mints the proof token.

Each physical validator keeps the common cancellation arm. Step 04 is the only
self-loop. The state carries only authenticated commitments, counts, cursor,
and the monotone `found` bit; it never carries caller-selected verdicts.

## Semantic engine and maximum evidence

The family-local engine uses the consensus purpose order spend=0, mint=1,
observe=2, receive=3 and redeemer tags 0, 1, 3, 6. Step 02 follows the exact
header validation-root/count, descriptor, event/source-kind, verdict/rejection,
machine-state, native-source, work-root, and trace-proof chain into the
canonical stage-10 control. It verifies the selected purpose membership and
the matched source descriptor membership, and admits only Plutus language 3
or 128. It does not accept a caller-authored purpose root. A fabricated
frontier therefore makes the producer-committed trace invalid; an honest
canonical trace cannot convict an honest transaction. Steps 03/04 authenticate
field 8 through published raw or certified carriage and scan every item.
A terminal absence is reachable only at `cursor == item_count`.
Alternate-purpose or alternate-pointer substitution, skipped/reordered items,
checkpoint regression, a premature absence terminal, malformed pointers, and
an omitted suffix all fail.

The stage-10 state the family binds is the producer's earliest selection state
for the exact purpose: every such state carries the same purpose and
matched-source frontiers, and the family's own committed field-8 scan, not
the producer's auxiliary witness, decides presence. An honest missing-redeemer
rejection commits exactly that state as its terminal; a wrongful
`RedeemerMissing` claim commits it before its own scan continues.

Maximum evidence is the exact 32,768-byte certified field frontier and the
largest purpose frontier admitted by the native-transaction aggregate bounds.
The fit lifecycle publishes every applied validator in an ordinary signed
reference-script transaction, runs accepted absence and forced presence in all
four purpose kinds with both inline and reference-script sources, resumes the
maximum scan, cancels each nonterminal state, mints the permanent proof, and
performs descendant-aware leased removal.

## Reachability and fit gate

Step 01 imports only native-transaction/source binding. Step 02 imports the
validation trace verifier plus the canonical purpose/source frontier helpers.
Steps 03/04 import only field opening, redeemer pointer decoding, and the
bounded scan engine. Step 05 imports only the terminal contradiction and
generic finalizer.

The reproducible ledger is generated from the fresh `testnet` blueprint with
local UPLC evaluation enabled. Signed bytes must be `<= 16,384`, memory
`<= 16,500,000`, CPU `<= 10,000,000,000`, and reference publication targets
`<= 15,872` bytes. Every recorded margin must be positive.

The retained replay consumes only public `validation_traces` plus retained
`ScriptPurposeScanWitness`, `ScriptSourceScanWitness`, and the stage-10
selection states. It reconstructs the exact 31-field work witness, joins the
purpose and selected-source membership proofs, and rejects duplicate or
ambiguous coordinates. The production runner owns its fsynced directory
journal, concrete Lucid actuator, and the authenticated field-carriage
prerequisite port that publishes (and, above the raw bound, certifies) field 8
before the first field-consuming action. Its path is `Init -> 01 -> 02 -> 02a
-> 02b -> 03 -> 04* -> 05 -> permanent proof -> leased removal`; cancellation
burns the computation thread from every nonterminal physical validator.

## Evidence

Fresh pinned `aiken v1.1.23+5adf783` testnet blueprint SHA-256 at the family
fit gate:
`ae5d600efa7ac46b2e58286d125d6b94521e010493a4c0d93c4b2e97faf435ef`. The
machine-readable ledger is `missing-redeemer-v1-fit-ledger.json`
(42 entries, digest
`45d86d9ff9bb2f57b9841ff1431297b63fca981ba0520c2e2697452d9bc51b21`),
reproduced by `demo/midgard-fault-proofs/tests/missing-redeemer-fit-ledger.test.ts`
from the rows `tests/missing-redeemer-lifecycle.test.ts` prints under
`MIDGARD_PRINT_FIT=1`.

Fully applied signed reference-script publication sizes in physical order are
`15,129`, `7,616`, `12,192`, `5,291`, `10,601`, `9,755`, and `2,185` bytes,
leaving respective 15,872-byte reserve margins `743`, `8,256`, `3,680`,
`10,581`, `5,271`, `6,117`, and `13,687` bytes. (The `Preprod`-addressed
publication-fit suite measures the same scripts at `15,157`, `7,644`,
`12,220`, `5,319`, `10,629`, `9,783`, and `2,213` bytes.)

The maximum supported shape is the exact 32,768-byte field 8 carrying 17
redeemer items under tier-3 certified carriage: two full 15,148-byte chunk
publications at `15,872` signed bytes each (the shared chunker's publishable
frontier, 512 bytes below the hard limit), a 2,800-byte remainder chunk, and a
`1,318`-byte certificate mint. Its thread runs `Init` (`1,641` bytes), step 01
(`2,062`), step 02 (`1,174`), step 02a (`2,776`), step 02b (`914`), the
grammar start/resume/finish triple (`1,426`/`1,518`/`1,522`), the widest
16-item pointer batch (`1,486` bytes, `5,019,938` memory,
`2,182,994,737` CPU), the resumed final batch (`1,378`), the permanent mint
(`916`), and the leased target and descendant removals (`2,429` and `1,544`).
The wrongful forced rejection of a mint purpose with a reference-script source
runs the raw-carriage direct opening (`1,364`) and one exact pointer match
(`1,312`); every cancellation, the certified grammar state and the mid-walk
checkpoint included, is `611` bytes.

Minimum margins across the ledger: `512` signed bytes (a full carriage chunk),
`11,480,062` memory units and `7,817,005,263` CPU units (the widest pointer
batch), and `743` bytes of publication reserve (step 01).
