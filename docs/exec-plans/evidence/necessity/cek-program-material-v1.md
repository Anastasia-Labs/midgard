# §3.2 necessity artifact — C28 CEK program material

## Binding and source identities

- Family/item: `cek-program-material`; one complete canonical CEK material
  graph, its canonical program envelope, and independently authenticated
  material entries.
- Source revision at measurement: worktree over
  `536b190a6246b0faba53bd43a0c3d3f319e215a6` carrying the complete C28 batch.
  The C28 source of truth is `demo/midgard-validation/src/cek-program.ts` and
  `onchain/aiken/lib/midgard/{cek-proof-v1,cek-data-v1,validation-resolver-v1,validation-machine-v1}.ak`
  plus `onchain/aiken/validators/fraud-proofs/validation-trace/cek-v1.ak`.
- Protected generated `onchain/aiken/plutus.json` SHA-256
  `f5ae651e34cf3e1175d928634c002580c4f2af4659a229952007c458945b866b`
  (provenance only; C28 does not regenerate the protected blueprint). The
  current-tree disposable blueprint compiled from the changed Aiken sources
  with pinned `aiken v1.1.22+39d6b04` has SHA-256
  `6b6422eeb128663495d97272c965faecf77438b5d1e369d742e9f1de46688f20`; its
  `fraud_proofs/validation_trace/cek_v1.main.spend` is 156,006 compiled bytes
  with exactly 4 parameters (fraud-proof address, computation-thread policy,
  award hash, immutable CEK program-material identity), verified by the
  gated current-tree selector in
  `demo/midgard-sdk/tests/validation-resolver-applied-hashes.test.ts` (2/2
  with `MIDGARD_REAL_BLUEPRINT_PATH` set to the disposable blueprint).
- Applied direct-resolver identities on the measurement deployment
  (`hub_oracle=11…11`, `catalogue=22…22`):
  current-tree applied `cek_v1` is 156,161 bytes with hash
  `827fe0ad74a14400c89c08e4e8c0655d5fc1da85e32d8c21f9bd51f2`; the protected
  checked-in blueprint's applied `cek_v1` is 141,959 bytes with hash
  `92c53d4757c14275600484193355f09917437e05e731ba25b935d549`. Any change to
  either hash invalidates this artifact (GOAL_SPEC.md §3.2).
- The canonical envelope remains a complete item, at most 50 CBOR bytes.
  Its maximum claims are 1,597,819 nodes and 67,108,418 material bytes. The
  maximum Aiken envelope/program-hash vector is
  `e4fe7f19ef343b55fef5a5c4a80383dd4bbe4bc7009db0a3214bfec086584697`
  for UPLC `1.1.0`, term root `aa…aa`, and those maximum claims.

## Ordered complete-material attempts

`deriveMidgardCekProgramMaterialCarriagePlanV1` derives the complete graph's
actual sidecar byte length from the canonical material map and tries these
representations in this exact order. It does not select incremental
publication until all three complete-graph representations reject.

| Order | Representation | Source-derived fit rule | Executable measurement when it fits | Maximum 67,108,418-byte graph |
| --- | --- | --- | --- | --- |
| 1 | Direct proof material | Complete envelope + complete encoded sidecar `<= 8,769` bytes | 15,872-byte direct proof transaction; 853,925 lovelace measured publication fee | Does not fit |
| 2 | Authenticated input inline datum | Complete envelope + sidecar `<= 15,624` datum bytes | 15,872-byte complete-item publication transaction; 853,925 lovelace fee | Does not fit |
| 3 | Authenticated reference-input inline datum | Same complete datum fit as order 2 | 8,275-byte reference proof transaction, one reference input | Does not fit |
| 4 | Per-node authenticated publication/receipt | Reached only when 1–3 fail and every entry is independently publishable | 4,268-byte maximum datum; 4,369-byte unsigned publication; 3,398,228 memory and 1,209,745,039 CPU receipt bounds | Necessity-justified fallback |

The first three maximum cases are byte-impossible before execution: the
67,108,418-byte material lower bound exceeds the 16,384-byte target L1
transaction envelope. No fee or exunit result is invented for an
unconstructible complete-graph transaction. The order-4 figures are the
repository's executable per-publication/receipt measurements in
`MIDGARD_V1_ENVELOPE_MEASUREMENTS`, pinned by
`demo/midgard-sdk/tests/tx-order-v1.test.ts`; they are not one-shot graph
measurements.

The production submission path in
`demo/midgard-fault-proofs/src/validation-dispute/submit.ts`
(`submitValidationDisputeDirectResolution`) enforces the same order
executably: it constructs and locally evaluates the direct route first,
falls back to the caller-confirmed exact single-publication reference, then
the caller-confirmed exact root-ordered minimum multi-output reconstruction,
and only enters incremental traversal with a parsed, envelope-bound
`CekProgramMaterialNecessityReceiptSetV1`. Every rejected local attempt is
retained in the result's `rejectedLocalRouteAttempts`.

## Direct-resolver reference-script carriage (measured)

The CEK direct resolver (direct resolver 0, phase 11 `Cek`) can never travel
inside the 16,384-byte L1 proof envelope: its applied body alone is 156,161
bytes. C28 therefore registers and consumes it as an authenticated
reference-script deployment role:

- Role `V1 validation-trace CEK direct resolver` → token
  `V1ValidationTraceCekResolver0` in `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES`
  (`demo/midgard-sdk/src/reference-scripts.ts`).
- Deployment entry `validationTraceDisputeCekDirectResolver`; submission
  resolves and verifies the UTxO (exact script hash, exactly one role token)
  and consumes it via `readFrom`, never attaching the resolver body
  (`requireValidationCekDirectResolverReferenceScriptUtxo`).
- Measured authenticated publication receipts (real signed emulator
  transactions through the production publication program,
  `completeReferenceScriptPublicationTxProgram`): current-tree blueprint
  156,676 signed bytes (L1 margin −140,292); protected blueprint 142,474
  signed bytes (L1 margin −126,090). Both are deployment-time-only
  transactions hosted under a raised 262,144-byte emulator `maxTxSize`; they
  exceed the mainnet 16,384-byte `maxTxSize`, so the resolver itself remains
  unpublishable on the live target network until the tracked oversized-
  validator decomposition program (GOAL_PROGRESS P1 gate: 42 spend handlers
  over 16,384 bytes, topped by `cek_v1`) closes. The consuming finalization
  transaction is the artifact-relevant proof transaction and stays inside
  the envelope by reference-input carriage.
- Missing-registration, wrong-reference (no script, wrong validator), and
  wrong-role publications reject before any transaction is constructed
  (`demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute.test.ts`,
  `tests/validation-dispute-submit.test.ts`).

## Preserved fitting paths and integrity

The plan retains every fitting representation: the complete 50-byte envelope
is never chunked, and it reports direct/input/reference acceptance for every
complete graph that actually fits. Each independently retained material entry
must remain within the 4,268-byte publication datum measurement; no source
constant cap has been reduced. The 9,215-byte direct-constant rule remains a
per-constant proof envelope rule, not a graph-size compatibility shortcut.

The production selection path derives the canonical envelope hash from
the decoded envelope and carries it in both `CekContextControlV1` and the
nine-field CEK work witness while the selected program executes. A modified
term root, version, node count, material-byte claim, trailing/noncanonical
encoding, material preimage, unreachable entry, or substituted envelope hash
cannot produce the same selected-context identity.

Defect found and closed during C28 finalization: the first 9-field witness
layout ended with the possibly-empty `program_envelope_hash`, and
`aiken/cbor.deserialise` (stdlib v3.1.0) rejects any stream whose final item
is a zero-length bytestring at an exhausted cursor (byte-level probes:
`89…40` fails, `89…4161` and mid-stream `40` pass). Every pre-selection CEK
witness was therefore unverifiable on-chain. Both encoders now place the
hash before the two integer limits so the witness always ends with an
integer; the guarded Aiken selectors and the cross-language TS vectors pin
the corrected order.

## Receipt status and invalidation

Receipts recorded here are real executable measurements: signed emulator
transactions through the production publication/submission code against the
applied validators of both the protected and the current-tree disposable
blueprint, plus the pinned `MIDGARD_V1_ENVELOPE_MEASUREMENTS` publication and
evaluator receipt bounds. Two receipt classes remain unproducible in this
environment and are still owed before CG5: live target-network
fee/exunit/confirmation receipts for the material routes, and a complete
end-to-end CEK finalization proof transaction (no harness yet drives a
Cek-phase thread to a direct resolver; the finalization path is exercised at
the construction/verification boundary instead).

Invalidate this artifact on any change to the canonical envelope, sidecar,
content graph, script language tag, credential, CEK work-witness tuple,
target `maxTxSize`, datum/reference-input carriage, applied or generated
`cek_v1`/`cek_program_material_v1` hash, reference-script role registration,
or measured transaction profile. The C28 ABI change also invalidates
C29/C30 resolver vectors and the C32 maximum-terminal agreement matrix until
they consume the 25-field context and 9-field CEK witness identities.
