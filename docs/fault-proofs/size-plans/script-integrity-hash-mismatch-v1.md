# `scriptIntegrityHashMismatch` V1 maximum-shape and transition plan

- Frozen category ID: `00000033`.
- Typed rejection reason: transaction-global `ScriptIntegrityHashMismatch`.
- Subject: the exact script-integrity hash, redeemer-witness hash, and selected
  Plutus-language bitmap carried by the canonical `ScriptIntegrity` stage-3
  validation-machine frontier for one accepted or forced native transaction.
- Logical topology: five family-owned computation-thread steps after generic
  `Init`. Authentication may be split into two physical scripts if the first
  fresh signed-publication fit measurement requires it; the canonical wire
  state and sole successor remain unchanged.

## Applied validators and reverse parameter order

1. `fraud-proofs/script-integrity-hash-mismatch/step-01`
   `(step_02_hash, computation_thread_policy, hub_oracle)` binds the accepted
   or forced transaction, exact transaction-global reason, and committed body
   hash.
2. `fraud-proofs/script-integrity-hash-mismatch/step-02`
   `(step_03_hash, computation_thread_policy)` authenticates the exact event,
   validation-trace descriptor/state membership, and canonical
   `ScriptIntegrity` stage-3 work witness. It extracts rather than accepts as
   caller authority the body integrity hash, redeemer-witness hash, execution
   count/frontier, and selected-language bitmap.
3. `fraud-proofs/script-integrity-hash-mismatch/step-03`
   `(step_04_hash, computation_thread_policy)` initializes the canonical
   language-view fold at language bit zero and binds the authoritative Cardano
   V1 language-view domain.
4. `fraud-proofs/script-integrity-hash-mismatch/step-04`
   `(step_05_hash, computation_thread_policy)` folds one selected-language bit
   per transaction, self-looping until both supported bits are consumed. Each
   successor fixes cursor, rebuilt bitmap, and selected-language count.
5. `fraud-proofs/script-integrity-hash-mismatch/step-05`
   `(fraud_proof_policy, fraud_proof_address, computation_thread_policy)`
   derives the expected hash with `midgard/script_language_views_v1`, enforces
   accepted-mismatch or forced-equality polarity, burns the computation-thread
   token, and permanently mints the proof token.

Every physical validator retains the standard cancellation arm. Step 04 is
the only self-loop. No caller verdict, expected hash, language bitmap, or
language-view bytes are authoritative inputs.

## Imported semantic engine and maximum dynamic evidence

Step 01 imports only the common native-transaction/reason binding substrate.
Step 02 imports the validation-trace membership verifier, exact
`ScriptIntegrity` stage-3 work-witness codec, and native control
well-formedness checks. Step 03 imports only the language-fold initializer.
Step 04 imports only the bounded two-bit canonical-language-set fold. Step 05
imports `midgard/script_language_views_v1.expected_script_integrity_hash` and
the generic terminal contradiction/finalizer. Unrelated subject adapters and
the monolithic validation resolver are unreachable from every applied script.

Maximum dynamic evidence is one maximum-depth transition-trace membership,
one maximum-depth validation-trace proof, the bounded native control, and at
most two fixed language-bit folds. The retained replay accepts only public
`validation_traces` plus the exact retained stage-3 `ScriptIntegrity`
`NoAuxiliaryWitness`; it reconstructs and hashes the canonical work witness,
rejects duplicate/ambiguous coordinates, and never substitutes a locally
invented language set.

The authoritative vectors are the Cardano language-view encodings already
frozen by `midgard/script_language_views_v1`: bit 0 selects CBOR key `2`
(PlutusV3), bit 1 selects CBOR key `128` (MidgardV1), and bitmaps `0..3` must
match the Aiken/TypeScript cross-language expected hashes. Empty, singleton,
and dual-language sets are tested in both accepted and forced directions.

## Reachability and fit gate

The production lifecycle is callback-free and concrete:
`Init -> 01 -> 02 -> 03 -> 04* -> 05 -> permanent proof -> leased removal`.
The runner owns an fsynced directory journal, resumes after every physical
checkpoint, cancels from every nonterminal validator, and refuses permanent
or descendant removal before the terminal proof is durably observed.

The fit test rebuilds the fresh `testnet` blueprint under the repository Aiken
compiler lock, publishes every fully applied reference script in an ordinary
signed transaction with local UPLC evaluation enabled, and exercises maximum
authentication evidence plus all four bitmaps. Signed bytes must be
`<= 16,384`, memory `<= 16,500,000`, CPU `<= 10,000,000,000`, and target
reference publication bytes `<= 15,872`; every ledger margin must be positive.

Fresh testnet blueprint SHA-256:
`ff06bdfca25e8b5f2ec71ff32c599d89c4bf737a93f4e6826992f5af8762e78f`.
Fully applied script bytes are `14,692`, `11,817`, `1,603`, `5,401`, and
`1,957`; ordinary signed reference-publication transactions are `14,968`,
`12,093`, `1,879`, `5,677`, and `2,271` bytes. The tightest publication
reserve margin is therefore `904` bytes. The largest exact semantic vector
uses `221,775` memory units and `79,009,581` CPU units.

The concrete Lucid lifecycle gate signs and submits all five cancellation
paths, re-acquires every successor by its out-ref (the restart boundary), runs
the two physical step-04 folds, permanently mints the proof at step 05, and
performs canonical registered-family removal under a mutation lease. Its
largest row is leased removal at `2,060` signed bytes, `3,022,261` memory, and
`1,030,518,609` CPU; the tightest lifecycle transaction margin is `13,841`
bytes at step 02. Exact per-transaction rows are retained in the adjacent fit
ledger JSON.
