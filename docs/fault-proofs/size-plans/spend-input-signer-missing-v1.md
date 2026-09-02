# `spendInputSignerMissing` V1 maximum-shape and transition plan

- Frozen category ID: `00000027`.
- Typed rejection reason: `SpendInputSignerMissing { input_index }`.
- Subject: one field-0 spend input of an accepted or forced native transaction,
  the pub-key payment credential of the output it references in the header's
  committed `prev_utxos_root`, and the same transaction's authenticated field-7
  address-witness collection.
- Logical topology: five family-owned computation-thread steps after generic
  `Init`.

## Applied validators, state, and parameter order

1. `fraud-proofs/spend-input-signer-missing/step-01`
   `(step_02_hash, computation_thread_policy, hub_oracle)`: authenticates the
   accepted or forced transaction, binds the exact field-0 `input_index`, the
   authenticated header's prior-UTxO root, and, for wrongful rejection, exactly
   `SpendInputSignerMissing { input_index }`. It forwards `BoundSpendInputV1`.
2. `fraud-proofs/spend-input-signer-missing/step-02`
   `(step_03_hash, computation_thread_policy, field_preimage_certificate_policy)`:
   opens exactly field 0, selects and canonically decodes the bound input item,
   proves membership of that exact out-ref and descriptor in the bound prior
   root, checks the descriptor index, canonically decodes its address, and
   accepts only a pub-key payment credential. It forwards the authenticated
   28-byte credential together with the verdict subject and the transaction's
   witness-set anchor.
3. `fraud-proofs/spend-input-signer-missing/step-03`
   `(step_04_hash, computation_thread_policy, field_preimage_certificate_policy)`:
   opens the transaction's authenticated field-7 address-witness collection,
   checks its complete carriage/certificate commitment, and initializes the
   canonical field-walk checkpoint. No caller-supplied signer frontier enters
   thread state.
4. `fraud-proofs/spend-input-signer-missing/step-04`
   `(step_05_hash, computation_thread_policy, field_preimage_certificate_policy)`:
   resumes the digest-bound field walk in fixed batches of 16. Every item is
   canonically decoded and its Ed25519 signature is verified over the bound
   native transaction ID before its verification-key hash may enter the
   frontier. An invalid signature is never evidence that its key signed. A
   valid matching key terminates with `signer_missing = False`; exhausting all
   318 positions without one terminates with `signer_missing = True`.
   Otherwise the validator self-loops with only the next checkpoint digest.
5. `fraud-proofs/spend-input-signer-missing/step-05`
   `(fraud_proof_policy, fraud_proof_address, computation_thread_policy)`:
   recomputes the accepted/forced polarity from the authenticated terminal
   verdict, burns the computation-thread token, and mints the permanent proof.

Every applied validator retains the common cancellation branch. Step 04 is the
only self-loop. The state wire carries no output bytes, witness preimage, public
key, signature, membership proof, or caller-selected reason.

## Maximum evidence and mutation frontier

The fixed field-7 address-witness stride is 103 bytes. The exact maximum field
therefore contains
`floor((32,768 - 3) / 103) = 318` witnesses; 319 is refused by the aggregate
field bound. The maximum run initializes once and performs twenty step-04
transactions (nineteen 16-item batches and one 14-item suffix). Each step
re-opens field 7 through direct, published, or certified carriage and resumes
only from the checkpoint bytes whose digest the preceding transaction stored.

The prior-ledger credential proof exercises the maximum supported MPF depth and
both raw-redeemer and published-proof-chunk carriage. Field 0 and field 7 each
exercise the complete carriage ladder, including the exact 32,768-byte
certified frontier and 32,769-byte refusal.

Required negatives are: wrong input coordinate; normal/reference source
substitution; transaction ID, prior root, out-ref, descriptor, output index, or
payment credential substitution; script-locked output; wrong reason
constructor or reason coordinate; forged witness-set hash; reordered/mutated
field-7 chunks; malformed witness; correct key with an invalid signature;
valid signature from the wrong key; checkpoint substitution; skipped batch;
premature terminal; missing signer under forced wrongful-rejection polarity;
and present valid signer under accepted polarity. The two honest terminals are
accepted-plus-missing and forced-exact-reason-plus-present.

## Reachability boundary

Step 01 reaches only verdict-subject/native-transaction authentication. Step 02
reaches only the field-0 door, canonical out-ref codec, prior-root MPF
membership, descriptor/address decoder, and pub-key credential extraction.
Steps 03 and 04 reach only the authenticated field-7 door, canonical
address-witness decoder, checkpoint walk, key hashing, and Ed25519 verification.
Step 05 reaches only exact terminal polarity and generic finalization. No
applied validator imports observer, redeemer, native-script, CEK, mint/value,
or output-reconstruction engines.

## Reproducible fit gate

Build with pinned `aiken v1.1.23+5adf783` for `testnet` without changing the
repository blueprint. Publish every applied script in an ordinary complete
signed reference-script transaction and execute a Lucid Evolution lifecycle
against that fresh blueprint: accepted and forced init-to-terminal journeys;
maximum-depth membership; direct and certified field carriage; all twenty
maximum-frontier scan transactions; cancellation from every nonterminal step;
terminal mint; state-queue target; and descendant-aware removal. Local UPLC
evaluation remains enabled.

The deterministic JSON fit ledger records compiler and blueprint digests plus
signed bytes, memory, CPU, and remaining margins for every measured row.
Acceptance requires signed bytes `<= 16,384`, memory `<= 16,500,000`, and CPU
`<= 10,000,000,000`; reference publication additionally targets
`<= 15,872` bytes. Every margin must be positive and the ledger test must
reproduce the artifact from the lifecycle measurements.
