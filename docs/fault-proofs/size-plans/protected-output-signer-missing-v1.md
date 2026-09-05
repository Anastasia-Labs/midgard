# `protectedOutputSignerMissing` V1 size and transition plan

- Category: `protectedOutputSignerMissing`
- Frozen category ID: `0000002b`
- Typed reason: `ProtectedOutputSignerMissing { output_index }`
- Subject: one field-2 transaction output, independent of spend-input
  authorization.

## Physical chain

| Step | Applied validator | Imported semantic engine | Carried state |
| --- | --- | --- | --- |
| 01 | `fraud_proofs/protected_output_signer_missing/step_01.main.spend` | common accepted/forced native transaction binding | exact verdict subject, transaction id, witness-set hash, output coordinate |
| 02 | `.../step_02.main.spend` | authenticated field-2 opening and ledger-output decoder | exact protected pub-key payment credential and transaction anchor |
| 03 | `.../step_03.main.spend` | authenticated field-7 opening and Ed25519 verification | initialized valid-signer frontier identity, item count, cursor and accumulator |
| 04 | `.../step_04.main.spend` | bounded valid-address-witness scan | resumable domain-separated checkpoint over source, cursor, count and signer frontier |
| 05 | `.../step_05.main.spend` | terminal verdict contradiction and common proof mint | signer-present verdict bound to the original subject |

Every applied validator imports only the protected-output adapter and shared
field/opening, signature, checkpoint, computation-thread and terminal engines.
No spend-input signer adapter or unrelated subject rule enters an applied
script. All five steps expose the common cancel arm and one exact successor;
step 04 alone self-loops.

## Forced-direction state and transition sketch

Both directions share one state wire and one physical chain. The direction
lives in the shared `VerdictSubjectV1` that step 01 writes and every later
step carries verbatim; nothing downstream branches on it until the terminal
polarity at step 05.

| Step | Wrongful acceptance (`direction = 0`) | Wrongful forced rejection (`direction = 1`) |
| --- | --- | --- |
| 01 | `AcceptedSource { inclusion }`: counted `transactions_root` membership through the shared native carriage; `bind_accepted_subject_v1` requires `validity_code == 0`. | `ForcedSource { header, membership, direction }`: `bind_forced_subject_to_thread_v1` binds the header to the thread NFT, proves counted `forced_transactions_root` membership of the leaf, re-verifies the leaf's proof source, requires `ForcedTxInvalid { reason }` with `validity_code == 1`, and `bind_output_v1` then binds the redeemer's `output_index` to exactly `ProtectedOutputSignerMissing { output_index }` by canonical reason serialisation. |
| 01 → 02 | `step_02.State { bound: BoundOutputV1 { subject, output_index }, witness_set_hash }`: identical shape and successor in both directions; the witness-set anchor is the one the counted root (accepted) or the forced leaf source (forced) committed. | same |
| 02 | Opens field 2 against `BodyAnchor { tx_id }`, decodes the bound output, requires `address.protected` and a pub-key payment credential, forwards `ProtectedCredentialV1`. | same |
| 03 | Opens field 7 against `WitnessAnchor { tx_id, witness_set_hash }`, derives the item-zero walk position from the authenticated view (fixed 103-byte stride, so the count is authenticated under every carriage tier), forwards `WitnessScanV1 { protected, checkpoint_hash, signer_present: False }`. | same |
| 04 | Resumes only from the committed checkpoint digest; folds at most 32 items, admitting a key to the frontier only when its Ed25519 signature verifies over the bound transaction id and hashes to the protected credential; self-loops until the walk reaches the field's exact end, then writes `VerdictV1 { subject, signer_present }`. | same: the forced direction proves presence by the same complete scan, never by an early exit. |
| 05 | `terminal_contradiction_v1(subject, !signer_present)`: closes on `signer_present = False`. | Closes on `signer_present = True` for the exact carried reason; refuses otherwise. |

Reason and coordinate mutations fail at step 01 (constructor or
`output_index` differs from the leaf's reason), source substitutions fail at
the counted-root membership or the leaf-source re-verification, and every
later seam (compact bytes, witness-set anchor, field preimage, certificate,
checkpoint) is authenticated identically for both directions. No new physical
script, parameter, or wire shape is introduced; the accepted path is
unchanged.

### Reachability boundary of the forced direction

The forced direction proves the reason false by exhibiting a valid witness
for the protected pub-key credential the reason names. It does not reach the
three shapes where the reason is false for a different cause: an
`output_index` past the field-2 count, an unprotected output, or a protected
output with a script payment credential. Step 02 refuses all three (out-of-
range read, `address.protected`, `PubKeyCredential`), so a forced rejection
citing such a coordinate cannot be corrected by this family today. Closing
that gap needs step 02 to reach step 05 directly, which adds `step_05_hash`
to step 02's applied parameters (a deployed-parameter-order change) and a
`signer_required` scalar to `VerdictV1`; that decision is escalated rather
than taken here.

## Maximum dynamic evidence

- A canonical native transaction at the protocol transaction-size frontier.
- The maximum legal field-2 output preimage, carried through Raw UTxO or a
  certified sequence when it cannot fit directly.
- The maximum address-witness field admitted by the native transaction bound.
  Only witnesses whose Ed25519 signature verifies over the authenticated
  transaction id enter the signer frontier; invalid signatures are scanned but
  never contribute a credential.
- Step 04 processes a fixed batch and commits the next cursor, total item
  count, source identity, accumulated valid-signer frontier and next script.
- Both accepted-invalid and exact forced-rejection subjects bind output index;
  any other reason or coordinate is rejected before credential authentication.

## Measured fit and lifecycle evidence

The testnet blueprint (`sha256
61ec67157434a1904ddac0a355337a1656d1ef62448744fa2856d0a1aa1602cb`) was
built with `aiken v1.1.23+5adf783`. Complete signed reference-script
publication measured 14,827, 9,239, 7,488, 9,119 and 2,214 bytes for steps
01–05. The tightest reserved publication margin is 1,045 bytes (step 01), and
the tightest hard-ledger margin is 1,557 bytes.

The applied Lucid lifecycle used the maximum 318 address witnesses and an
actual three-transaction Certified field-7 publication (15,872, 15,872 and
2,789 signed bytes), followed by its 1,317-byte certificate transaction. It
executed Init, cancel/re-init, accepted source binding, field-2 credential
opening, field-7 opening, ten exact-predecessor step-04 scans across fresh
isolated evaluators, terminal contradiction, proof mint, and leased fraudulent
block removal. Fresh evaluator processes preserve the emulator's exact
authenticated UTxO state and predecessor transactions while avoiding the
known cumulative WASM UPLC arena ceiling.

All lifecycle rows fit beneath 16,384 signed bytes, 16,500,000 memory units and
10,000,000,000 CPU units. The worst applied script margins were 8,040,990
memory units and 5,820,076,868 CPU units; the smallest lifecycle byte margin
was 14,023 bytes. The machine-readable measured rows are in
`protected-output-signer-missing-v1-fit-ledger.json`; its canonical ledger
digest is `7b418d3ca2bda2e656cc5c258c8f4930823ad48d31ab1bfcbdaa57367b69d72b`.

Focused semantics cover both successful directions, honest accepted and
forced refusal, invalid signatures excluded from the frontier, wrong
credential, reason/output-coordinate/source/item substitution, malformed or
replayed checkpoint, wrong successor, adjacent-over-bound refusal, restart
identity reconciliation, and maximum-carriage execution.
