# `executionNativeScriptInvalid` V1 size and transition plan

- Frozen category ID: `00000032`.
- Typed reason: `ExecutionNativeScriptFalse { execution_index }`.
- Logical topology: six family steps after generic `Init`.

1. `step-01` authenticates accepted/forced provenance and binds the exact typed
   reason plus execution coordinate. It carries only `BoundExecutionV1`.
2. `step-02` authenticates the retained NativeScripts trace state and exact
   purpose/source/execution leaves. It carries the selected native source and
   the compact transaction bytes committed by the trace control.
3. `step-03` checks canonical supplied bytes against the exact authenticated
   bounded-item coordinate: field 6/source index for inline witnesses or field
   2/output index decoded from the source key for resolved reference sources.
   It derives signer/interval inputs from the authenticated compact transaction.
4. `step-04` either evaluates the bounded direct frontier or initializes the
   canonical signer frontier in a 16-item batch.
5. `step-05` resumes the field-7 signer walk from its domain-separated
   checkpoint and closes only at the authenticated end of the field.
6. `step-06` runs the deterministic native pushdown in at most 16-node batches.
   It mints only for false on wrongful acceptance or true on wrongful rejection.

The forced-direction applied chain imports only the shared execution-source authenticator,
field-opening primitives, signer-frontier proofs, and native pushdown engine.
No CEK, observer, redeemer, value/mint, or output-reconstruction adapter is
reachable. Maximum tested evidence is 318 canonical address witnesses and a
32-node native script; the adjacent 17-item/node batch is refused.

The testnet blueprint build with Aiken `v1.1.23+5adf783` succeeds. The 13
signed reference publications measure 15,575, 15,805, 5,317, 14,344, 10,375,
10,380, 3,082, 14,605, 10,389, 9,532, 12,048, 11,513, and 14,181 bytes.
Step 02 retains the narrowest 67-byte publication reserve.
The real Lucid wrongful-rejection lifecycle submits Init, all six applied
scripts, permanent mint, and leased canonical removal. It also cancels and
restarts from every physical state. Separate full terminal rows cover an inline
witness and a resolved-reference source authenticated from the prior ledger.
The largest signed lifecycle transaction
is 4,866 bytes, 5,118,774 memory, and 1,764,846,073 CPU.

The wrongful-acceptance lifecycle derives the complete
consensus purpose order (script spends, mint policies, observers, protected
script receives) and both inline and resolved-reference sources directly from
the canonical transaction and resolved prior-ledger material. It never depends
on an unavailable rejection trace. Real Lucid accepted-false spend, mint,
observer, and receive fixtures submit every applicable physical door; inline
and resolved-reference source fixtures both reach permanent mint and leased
canonical removal. Cancellation/restart is exercised at each of accepted
physical scripts 7 through 13. Its largest signed transaction is 6,106 bytes,
7,874,351 memory, and 2,857,543,900 CPU.

The manifest-bound production runner reconstructs the single decision from
authenticated L1 plus public retained DA, resumes from the observed 13-script
cursor, locally evaluates and journals every exact signed body before submit,
authenticates forced step 2 from retained validation witnesses, and performs
canonical leased state-queue removal without caller evidence or actuation
callbacks.

Accepted-direction logical step 2 is split into the following narrow physical
doors; all carry `accepted_reconstruction.StateV1` and its domain-separated
checkpoint until the final door emits the existing step-3 source state:

1. `accepted-reconstruction-init` starts from step-1-carried compact CBOR and
   `prev_utxos_root`.
2. `accepted-spend-prefix` opens field 0 and verifies one resolved output
   membership/descriptor per transaction, counting only script credentials.
3. `accepted-mint-prefix` walks mint policy IDs.
4. `accepted-observer-prefix` walks required observer hashes.
5. `accepted-receive-prefix` performs canonical unique protected-script receive
   selection over authenticated output passes.
6. `accepted-inline-source` opens field 6 and scans canonical inline
   source hashes before any reference source is admissible.
7. `accepted-reference-source` opens sorted field-2 out-refs and verifies
   their prior-ledger descriptor/reference-script facts.

The semantic fold, TypeScript twin, seven applied doors, all publications, and
both measured terminal directions are included in the fit ledger.
