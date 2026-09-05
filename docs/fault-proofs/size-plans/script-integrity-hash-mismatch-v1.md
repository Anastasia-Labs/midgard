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

The 2026-09-05 consolidated baseline blueprint is
`db03f84b0157ac51bd26b69dc2d548bb7186847697db607c5c8699479bfb9094`.
The executable lifecycle now covers all four language bitmaps in both
accepted and forced directions, with a positive contradiction and an honest
claim refusal for each combination (16 cases). Honest claims are submitted
through the raw finalizer as well as the guarded SDK, so refusal is checked
by the real terminal validator. Every positive path cancels all five steps,
reopens each successor by out-ref, mints the permanent proof and removes the
registered block under a mutation lease.

Both dual-language positive cases carry a synthetic, library-verified
64-branch transaction-source MPF, a 64-branch descriptor MPF and a 32-level validation-state proof, the maximum
consensus trace depth (`maxValidationMachineStepCount = 0xffffffff`). The
challenged header commits the reconstructed root; synthetic neighbors avoid
an infeasible hash-prefix search while exercising the actual verifier. The
same cases refuse altered transaction identity, event identity, work root,
trace sibling, and descriptor membership key before proceeding successfully.

The adjacent ledger records 241 signed publication and lifecycle transactions.
Maximum publication size is 14,968 bytes (904 bytes of publication reserve).
Maximum lifecycle size is 12,280 bytes (4,104 bytes of L1 headroom), using
4,162,186 memory units and 1,416,781,205 CPU units. The ledger verifier binds
all rows to the blueprint and reconstructs every margin and digest.
Regenerate it with `MIDGARD_WRITE_FIT_LEDGER=1` while running the complete
`script-integrity-hash-mismatch-lifecycle.test.ts` file against the final
blueprint, then run `script-integrity-hash-mismatch-fit-ledger.test.ts`.

The empty-language accepted case also caught a missing integrity comparison
in the TypeScript plain-transaction optimization. That path now checks the
same canonical empty-language hash as the full validator and Aiken machine;
`phase-b.test.ts` includes a direct regression.

The accepted source's 64-branch proof would make the direct transaction
19,752 bytes because both the spending and rewarding redeemers carry it.
The existing published-chunk contract arm now has an SDK path. The installed
actuator tries direct completion first, captures a chunk publication only for
an exact 16,384-byte capacity failure, and resolves the exact published chunks
on restart. Publication goes through the same preflight, journal intent and
submission path as family transitions. Each intent is identified by its signed
transaction hash, and confirmation is matched to that hash, including the two
successive language-fold transactions. Non-capacity failures never publish.
