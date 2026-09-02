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
field 8 through direct, published, or certified carriage and scan every item.
A terminal absence is reachable only at `cursor == item_count`.
Alternate-purpose or alternate-pointer substitution, skipped/reordered items,
checkpoint regression, a premature absence terminal, malformed pointers, and
an omitted suffix all fail.

Maximum evidence is the exact 32,768-byte certified field frontier and the
largest purpose frontier admitted by the native-transaction aggregate bounds.
The fit lifecycle publishes every applied validator in an ordinary signed
reference-script transaction, runs accepted absence and forced presence in all
four purpose kinds, resumes the maximum scan, cancels each nonterminal state,
mints the permanent proof, and performs descendant-aware leased removal.

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
`ScriptPurposeScanWitness`, `ScriptSourceScanWitness`, and the terminal stage-10
`NoAuxiliaryWitness`. It reconstructs the exact 31-field work witness, joins the
purpose and selected-source membership proofs, and rejects duplicate or
ambiguous coordinates. The production runner owns its fsynced directory
journal and concrete Lucid actuator. Its path is `Init -> 01 -> 02 -> 02a ->
02b -> 03 -> 04* -> 05 -> permanent proof -> leased removal`; cancellation
burns the computation thread from every nonterminal physical validator.

Fresh testnet blueprint SHA-256:
`845116acc86a8884f5d25558a594df62fd947e3fb7bd28ccfc307e38815accb9`.
Raw applied script bytes in declaration order are `14,774`, `7,296`, `11,872`,
`4,970`, `10,229`, `9,401`, and `1,820`. Ordinary signed reference-script
publication transaction bytes are `15,157`, `7,644`, `12,220`, `5,319`,
`10,612`, `9,783`, and `2,213`, leaving respective reserve margins `715`,
`8,228`, `3,652`, `10,553`, `5,260`, `6,089`, and `13,659` bytes.
