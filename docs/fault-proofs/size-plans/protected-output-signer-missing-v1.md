# `protectedOutputSignerMissing` V1 size and transition plan

- Category: `protectedOutputSignerMissing`
- Frozen category ID: `0000002b`
- Typed reason: `ProtectedOutputSignerMissing { output_index }`
- Subject: one field-2 transaction output, independent of spend-input
  authorization.

## Physical chain

| Step | Applied validator                                                                                      | Imported semantic engine                                | Carried state                                                                                                                                      |
| ---- | ------------------------------------------------------------------------------------------------------ | ------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| 01   | `fraud_proofs/protected_output_signer_missing/step_01.main.spend`                                      | common accepted/forced native transaction binding       | exact verdict subject, transaction id, witness-set hash, output coordinate                                                                         |
| 02   | `.../step_02.main.spend` `(step_03_hash, step_05_hash, computation_thread_policy, certificate_policy)` | authenticated field-2 opening and ledger-output decoder | exact protected pub-key payment credential and transaction anchor to step 03, or the direct `signer_required = False` verdict to step 05           |
| 03   | `.../step_03.main.spend`                                                                               | authenticated field-7 opening and Ed25519 verification  | initialized valid-signer frontier identity, item count, cursor and accumulator                                                                     |
| 04   | `.../step_04.main.spend`                                                                               | bounded valid-address-witness scan                      | resumable domain-separated checkpoint over source, cursor, count and signer frontier                                                               |
| 05   | `.../step_05.main.spend`                                                                               | terminal verdict contradiction and common proof mint    | `VerdictV1 { subject, signer_required, signer_present }` bound to the original subject; the decisive fault is `signer_required && !signer_present` |

Every applied validator imports only the protected-output adapter and shared
field/opening, signature, checkpoint, computation-thread and terminal engines.
No spend-input signer adapter or unrelated subject rule enters an applied
script. All five steps expose the common cancel arm; steps 01, 03 and 04 have
one exact successor, step 02 has two (step 03 for the witness scan, step 05
for the direct verdict), and step 04 alone self-loops.

## Forced-direction state and transition sketch

Both directions share one state wire and one physical chain. The direction
lives in the shared `VerdictSubjectV1` that step 01 writes and every later
step carries verbatim; nothing downstream branches on it until the terminal
polarity at step 05.

| Step                                                                                     | Wrongful acceptance (`direction = 0`)                                                                                                                                                                                                                                                                                                                          | Wrongful forced rejection (`direction = 1`)                                                                                                                                                                                                                                                                                                                                                                                                                  |
| ---------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| 01                                                                                       | `AcceptedSource { inclusion }`: counted `transactions_root` membership through the shared native carriage; `bind_accepted_subject_v1` requires `validity_code == 0`.                                                                                                                                                                                           | `ForcedSource { header, membership, direction }`: `bind_forced_subject_to_thread_v1` binds the header to the thread NFT, proves counted `forced_transactions_root` membership of the leaf, re-verifies the leaf's proof source, requires `ForcedTxInvalid { reason }` with `validity_code == 1`, and `bind_output_v1` then binds the redeemer's `output_index` to exactly `ProtectedOutputSignerMissing { output_index }` by canonical reason serialisation. |
| 01 → 02                                                                                  | `step_02.State { bound: BoundOutputV1 { subject, output_index }, witness_set_hash }`: identical shape and successor in both directions; the witness-set anchor is the one the counted root (accepted) or the forced leaf source (forced) committed.                                                                                                            | same                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| 02, protected pub-key output                                                             | Opens field 2 against `BodyAnchor { tx_id }`, decodes the bound output, forwards `ProtectedCredentialV1` to `step_03_hash`.                                                                                                                                                                                                                                    | same, unchanged                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| 02, unprotected output or protected script output                                        | refused: an accepted subject never takes the direct exit.                                                                                                                                                                                                                                                                                                      | Decodes the bound output the same way, sees a coordinate canonical validation authorizes without a signer, and writes `VerdictV1 { subject, signer_required: False, signer_present: False }` to `step_05_hash`.                                                                                                                                                                                                                                              |
| 02, `output_index >= field_item_count(view)` (redeemer `coordinate_out_of_range = True`) | refused                                                                                                                                                                                                                                                                                                                                                        | Requires the view's authenticated count (`field_item_count`, which answers under tiers 1–2 and refuses a provisional §5.1 count) and writes the same direct verdict to `step_05_hash`.                                                                                                                                                                                                                                                                       |
| 03                                                                                       | Opens field 7 against `WitnessAnchor { tx_id, witness_set_hash }`, derives the item-zero walk position from the authenticated view (fixed 103-byte stride, so the count is authenticated under every carriage tier), forwards `WitnessScanV1 { protected, checkpoint_hash, signer_present: False }`.                                                           | same                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| 04                                                                                       | Resumes only from the committed checkpoint digest; folds at most 32 items, admitting a key to the frontier only when its Ed25519 signature verifies over the bound transaction id and hashes to the protected credential; self-loops until the walk reaches the field's exact end, then writes `VerdictV1 { subject, signer_required: True, signer_present }`. | same: the scan route proves presence by the same complete scan, never by an early exit.                                                                                                                                                                                                                                                                                                                                                                      |
| 05                                                                                       | `terminal_contradiction_v1(subject, signer_required && !signer_present)`: closes on `signer_present = False` from the scan; a direct verdict never closes an accepted subject.                                                                                                                                                                                 | Closes for the exact carried reason on the direct verdict, or on `signer_present = True` from the scan; a scan that found no signer is refused.                                                                                                                                                                                                                                                                                                              |

Reason and coordinate mutations fail at step 01 (constructor or
`output_index` differs from the leaf's reason), source substitutions fail at
the counted-root membership or the leaf-source re-verification, and every
later seam (compact bytes, witness-set anchor, field preimage, certificate,
checkpoint) is authenticated identically for both directions. The only new
wire shape is the `signer_required` scalar in `VerdictV1`; the only new
parameter is `step_05_hash` on step 02; the accepted path is unchanged in
behaviour.

### Direct terminal route and its canonical ordering

Canonical validation reaches this rejection in the script-sources middle
stage (`protected_output_authorization`): for the output at the cursor, an
unprotected address is `InputSignerAuthorized` with no signer, a protected
address hands its payment credential to
`payment_credential_signer_authorization`, where a `ScriptCredential` is
again authorized with no signer and only a `PubKeyCredential` consults the
signer frontier. A forced `ProtectedOutputSignerMissing { output_index }` is
therefore canonical only for a protected pub-key output at a position the
cursor visits. The two credential shapes are contradicted by the direct
route, on chain, from the same decoded output the scan route reads, with no
redeemer hint: the validator classifies the credential itself. The
out-of-range shape is not another family's reason — the machine's cursor
never evaluates a position at or past the field-2 count, so no reason is
canonical there — and it is contradicted here from the authenticated count of
the opened view. Field 2 is variable-width, so that count is authenticated
only where the whole preimage is present and hash-checked (tiers 1–2);
`field_item_count` refuses the self-asserted count of a certified variable-
width view by design, and the redeemer names the arm (`coordinate_out_of_range`)
because the count cannot be consulted on the scan arm without aborting tier-3
openings. A forced out-of-range coordinate over an outputs field that only
fits certified carriage therefore remains unreachable; that boundary is
recorded in the reachability paragraph below and in the handoff.

### Reachability boundary of the forced direction

The forced direction reaches a wrongful rejection whose coordinate is a
signed protected pub-key output (witness scan), an unprotected output, a
protected script-locked output, or a position past the field-2 count of a
tier-1/2 outputs field (direct route). It does not reach an out-of-range
coordinate over a certified (tier-3) outputs field.

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
7fa798050bf90e9dba1232f456879e8a56e29c48a2c3748067832d69a1f403fa`) was
built with `aiken v1.1.23+5adf783`. Complete signed reference-script
publication measured 14,827, 9,665, 7,488, 9,140 and 2,239 bytes for steps
01–05 (step 02 grew by 426 bytes for the direct exit and its second
successor parameter). The tightest reserved publication margin is 1,045
bytes (step 01), and the tightest hard-ledger margin is 1,557 bytes.

The accepted-direction Lucid lifecycle used the maximum 318 address witnesses
and an actual three-transaction Certified field-7 publication (15,872, 15,872
and 2,789 signed bytes), followed by its 1,317-byte certificate transaction.
It executed Init, cancel/re-init, accepted source binding, field-2 credential
opening, field-7 opening, the certificate-slot and checkpoint substitutions
refused at the scan door, ten exact-predecessor step-04 scans across fresh
isolated evaluators, terminal contradiction, proof mint, and leased fraudulent
block removal. Fresh evaluator processes preserve the emulator's exact
authenticated UTxO state and predecessor transactions while avoiding the
known cumulative WASM UPLC arena ceiling.

The forced direction was measured at its own maximum: the forced door's cost
is the header, the counted forced-root membership and the leaf's proof
source, none of which grows with the witness field, so the 1,757-byte forced
step-01 row (1,058,186 memory, 443,761,399 CPU) is the ledger's forced
maximum, and the scan and terminal rows are shared with the accepted
direction. The forced lifecycle proves an exact
`ProtectedOutputSignerMissing { 0 }` rejection wrong by finding the valid
witness behind four unverifiable decoys, and refuses the substituted reason
coordinate, the wrong direction, a re-verdicted leaf and a re-counted header
at the door. The honest suites walk both polarities to the terminal and are
refused at step 05, mutate every authentication seam (transaction membership,
forced leaf, compact bytes, witness-set anchor, field preimage, certificate
slot, checkpoint), cancel from every physical step, and drive a committed
319-witness transaction through step 02 before its 32,860-byte field is
refused on chain at the certificate mint (the production planner refuses it
earlier, before any transaction).

The direct terminal route was measured on its own forced blocks: a forced
`ProtectedOutputSignerMissing { 0 }` rejection over an unprotected output
closed at step 05 straight from step 02 in a 1,090-byte transaction (590,721
memory, 183,238,621 CPU), and the same rejection over a protected
script-locked output in 1,090 bytes (593,654 memory, 184,299,840 CPU); each
was followed by the ordinary 916-byte proof mint. Both blocks first refused
the credential seam on chain: the same forced claim presented at the scan
door, and an in-range coordinate presented on the out-of-range arm. A third
block, a protected pub-key output whose only witness does not verify,
refused the direct exit for a coordinate that needs a signer; its honest
forced rejection still walks the field and is refused at the terminal.

All lifecycle rows fit beneath 16,384 signed bytes, 16,500,000 memory units and
10,000,000,000 CPU units. The worst applied script margins were 8,043,858
memory units and 5,820,969,115 CPU units (scan resume 05); the smallest
lifecycle byte margin was 14,023 bytes (removal), and the carriage chunks sit
exactly on the 15,872-byte publication target. The machine-readable measured
rows are in `protected-output-signer-missing-v1-fit-ledger.json`; its
canonical ledger digest is
`195c9e0d2e99677aeca730421b1120c8fee63b14a5b0f3981471b53618479048`.

Focused Aiken selectors (82 across `rule` and the five step validators) cover
both successful directions, honest accepted and forced refusal, invalid
signatures excluded from the frontier, wrong credential, reason/output-
coordinate/source/item substitution, malformed or replayed checkpoint, wrong
successor at every step, the adjacent 319-witness over-bound refusal at the
field door, and the direct terminal route: unprotected, script-locked and
out-of-range forced coordinates closing at step 05, an in-range coordinate
refused on the out-of-range arm, an accepted subject refused on every direct
arm, a direct verdict refused for a protected key or with a claimed required
signer, a substituted direct-verdict subject, and each exit sent to the
other's successor.
