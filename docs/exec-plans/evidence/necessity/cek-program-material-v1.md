# §3.2 necessity artifact — C28 CEK program material

## Binding and source identities

- Family/item: `cek-program-material`; one complete canonical CEK material
  graph, its canonical program envelope, and independently authenticated
  material entries.
- Source revision at measurement: worktree over
  `84aa1ce3931ed67d241ca2ffd9e93671bc45d4c5` carrying the complete C28 batch
  plus the issue #521 duplicate-type-name renames
  (`cek_machine_v1.ValueWitnessV1` -> `MachineValueWitnessV1`,
  `midgard/user_events/deposit.Datum` -> `DepositDatum`). The renames change
  no constructor tag, field order, or CBOR encoding; they remove the stock
  `aiken v1.1.22` generated-decoder collision, so `cek_v1` and
  `scheduler.spend` now compile to identical bytes under the released and the
  patched compiler. The blueprint SHA-256, the `cek_v1` byte counts, and the
  applied `cek_v1` hash below were re-measured after those renames and
  supersede the earlier values taken over
  `536b190a6246b0faba53bd43a0c3d3f319e215a6`.
  The C28 source of truth is `demo/midgard-validation/src/cek-program.ts` and
  `onchain/aiken/lib/midgard/{cek-proof-v1,cek-data-v1,validation-resolver-v1,validation-machine-v1}.ak`
  plus `onchain/aiken/validators/fraud-proofs/validation-trace/cek-v1.ak`.
- Protected generated `onchain/aiken/plutus.json` SHA-256
  `f5ae651e34cf3e1175d928634c002580c4f2af4659a229952007c458945b866b`
  (provenance only; C28 did not regenerate the protected blueprint). That
  digest names the 380-validator measurement epoch and is NOT a current-tree
  claim. The C28-epoch disposable blueprint compiled from the then-changed
  Aiken sources with pinned `aiken v1.1.22+39d6b04` was SHA-256
  `b1c79edca9b305f4000a3116d73ba998687ea95aa5d1a9091de544218449937a`.
- Re-measured 2026-08-06 (issue #546): C28's sources are merged, so the
  protected/disposable split has collapsed to one blueprint. A fresh stock
  `aiken build --env testnet` of the current tree (`aiken v1.1.22+39d6b04`)
  produces `onchain/aiken/plutus.json` SHA-256
  `605c8b8dca1f01e2cde5219138a1f81e69214f9a182c10b73c20341187ddc2dc`
  (391 validators, including the chunked-MPF and harvest additions); that
  digest supersedes both `f5ae651e…` and `b1c79edc…` as the current-tree
  pin. Measured unchanged in it:
  `fraud_proofs/validation_trace/cek_v1.main.spend` is 156,312 compiled bytes
  with exactly 4 parameters (fraud-proof address, computation-thread policy,
  award hash, immutable CEK program-material identity). The 4-parameter
  identity is gated by
  `demo/midgard-sdk/tests/validation-resolver-applied-hashes.test.ts` with
  `MIDGARD_REAL_BLUEPRINT_PATH` set to that blueprint; its gated case
  `applies immutable CEK material identity as the exact fourth
  direct-resolver parameter` PASSES 1/1, but the file's other, ungated case
  now FAILS on unrelated `script_source_resolvers` fixture drift in
  `onchain/aiken/lib/midgard/validation-resolver-v1.test.ak` (that group's
  applied hashes moved with post-C28 script-sources work; none of this
  artifact's bound identities are in it). The earlier "2/2" reading of this
  file is therefore no longer reproducible and is corrected to 1/1 on the
  CEK-relevant case.
- Applied direct-resolver identities on the measurement deployment
  (`hub_oracle=11…11`, `catalogue=22…22`):
  current-tree applied `cek_v1` is 156,467 bytes with hash
  `f5d6395c562f2c0e1dc76582e2b1f2ba3e287345ab4abcd8cceb6666` — both
  re-measured unchanged on 2026-08-06 under blueprint `605c8b8d…` by the
  producing emulator selector named below. The 141,959-byte applied `cek_v1`
  with hash `92c53d4757c14275600484193355f09917437e05e731ba25b935d549` is an
  epoch-bound measurement of the superseded protected blueprint
  `f5ae651e…`; that blueprint is no longer producible from this tree (there
  is now a single generated blueprint), so the pin is retained as historical
  provenance and is not re-derivable. Any change to the current-tree hash
  invalidates this artifact (GOAL_SPEC.md §3.2).
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
| 4 | Per-node authenticated publication + single-transaction reconstruction (`MinimumMultiOutputCekMaterial`) | Reached only when 1–3 fail and every entry is independently publishable | 4,268-byte maximum datum; 4,369-byte unsigned publication; 3,398,228 memory and 1,209,745,039 CPU receipt bounds | Necessity-justified fallback, bounded by the reconstruction transaction (see below) |
| 5 | Incremental multi-transaction traversal (`IncrementalCekMaterial`) | Reached only when 1–4 fail | **No measurement. The route is CLOSED on L1** — see "Route 5 is closed" | Necessary in the limit, NOT deployed |

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
the caller-confirmed exact root-ordered minimum multi-output reconstruction.
Every rejected local attempt is retained in the result's
`rejectedLocalRouteAttempts`. It no longer submits route 5; after parsing the
envelope-bound `CekProgramMaterialNecessityReceiptSetV1` it refuses with an
explicit not-verifiable-on-L1 error rather than constructing a finalization
that the resolver rejects.

## Route 5 is closed: the incremental traversal has no L1 verification

This section is the measured §3.2 record for the incremental route. Its
honest conclusion is that the route is **necessary in the limit but not
soundly implementable within C28**, so it fails closed on L1.

**What routes 1–4 prove and route 5 did not.** Each of `DirectCekMaterial`,
`SinglePublicationCekMaterial`, and `MinimumMultiOutputCekMaterial` reaches
`cek_proof_v1.verify_complete_program_material_entries_v1`, which walks the
whole content-addressed DAG from `envelope.term_root`, checks every entry
`root == hash(preimage)`, and requires the traversal to reproduce
`envelope.node_count` and `envelope.material_byte_length` exactly with no
unreachable entry. `IncrementalCekMaterial` reached none of that. Its
predicate was

    program_envelope_hash == cek_envelope_hash_v1(selected_envelope)

and `cek_envelope_hash_v1` is a pure function of `selected_envelope` alone
(`inspect_program_envelope_v1` then `hash_program_envelope_v1` over the
decoded fields). Both sides therefore came from the disputer's own submitted
evidence: the check was the tautology `f(x) == chosen` satisfied by setting
`chosen := f(x)`. The branch read neither `reference_inputs` nor
`cek_program_material_script_hash`, so a CEK finalization could mint its
fraud proof — slashing the operator and taking the award — with **zero**
program material published on L1, while `verify_cek_one_step_v1` and
`challenger_wins_with_valid_successor` were satisfied honestly. The §3.2
gate and the material publication were enforced off-chain only, which an
adversarial submitter simply does not run.

**Why the specified design cannot be lifted on-chain.**
`validateRouteTransactionGrammarV1` in
`demo/midgard-validation/src/validation-dispute-evidence.ts` requires the
incremental role order `publication+ , proofConsumption ,
proofContinuation+` — at least one continuation *after* the consumption. The
consumption is the resolver finalization that mints the fraud proof. Material
verified after the dispute has already resolved secures nothing, so the
design is unsound by construction rather than merely unchecked.

**What a sound route 5 requires, and why it is not present.** An
authenticated cross-transaction traversal accumulator: a step chain whose
datum carries the partial material frontier, the reachable-node count, and
the material byte length, advanced by continuation transactions over
published entries, which the finalization requires to have already COMPLETED
against the selected envelope. Measured absence in this tree: program
material publications are permissionless, self-authenticating, and
unspendable (`onchain/aiken/validators/user-events/cek-program-material-v1.ak`
is `spend -> False`) with **no minting policy and no aggregate completion
commitment**; `onchain/aiken/lib/midgard/cek-blob-frontier-v1.ak` is an MMR
over the chunks of one blob leaf, not over the material DAG, and is consumed
only by `cek-source-blob-v1.ak`; no `NecessityReceipt` symbol and no
necessity verification exists anywhere under `onchain/aiken`.

**Necessity of route 5, honestly stated.** Route 5 IS necessary in the limit.
Route 4's limit is not publication — publication is permissionless and
unbounded, so any graph up to the 67,108,418-byte maximum is always
publishable — it is the single reconstruction transaction, which must carry
one reference input per entry inside 16,384 bytes and walk the whole DAG
inside the 13,200,000-memory / 8,000,000,000-CPU reserve, with
`find_program_material_entry_v1` scanning the entry list per task. So route 4
covers only small graphs and the 1,597,819-node maximum is far outside it.
This artifact therefore does **not** claim route 5 is unnecessary.

**Consequence and classification.** With route 5 closed, every accepted CEK
material route verifies the complete graph on L1, and oversized programs
cannot be CEK-disputed at all. That is a **liveness** limit on oversized
programs, not a soundness or data-availability hole: publication remains
permissionless so the material is always available, only the bounded
multi-transaction verification is missing. Per the repository tradeoff order
(correctness, safety, liveness, performance, convenience) a route that
verifies nothing is strictly worse than a route that rejects, so the closure
is the correct interim state. It sits alongside the already-recorded
oversized-validator limit below: `cek_v1` itself is 156,467 applied bytes and
is likewise not live-network deployable yet.

**Owed to the follow-up lease** (not C28): the authenticated
cross-transaction material-traversal accumulator, the measured frontier of
route 4 (the exact node count and material byte length at which the single
reconstruction transaction crosses 16,384 bytes and the execution reserve),
and the route-5 receipts that measurement makes meaningful. The ABI variant
`IncrementalCekMaterial`, the
`CekProgramMaterialNecessityReceiptSetV1` parser, and its CBOR vectors are
retained unchanged as that lease's seam.

**Executable pins for the closure**
(`onchain/aiken/lib/midgard/validation-resolver-v1.test.ak`, module selector
`aiken check -m 'validation_resolver_v1.{..}'`, 18/18):

- `cek_incremental_route_rejects_zero_published_material` — atomic, single
  assertion: the exact exploit (self-consistent hash, empty reference inputs).
- `cek_incremental_route_rejects_complete_published_material` — atomic.
- `cek_incremental_route_rejects_substituted_published_material` — atomic.
- `cek_complete_multi_output_route_accepts_the_same_material` — atomic
  positive control over the same envelope and the same reference input, so the
  three rejections above are attributable to the route selector alone.
- `cek_incremental_route_fails_closed_with_self_consistent_hash`,
  `cek_incremental_route_fails_closed_with_partial_material`,
  `cek_complete_item_carriage_survives_the_incremental_closure` — grouped
  cases with paired positive controls, including a two-node graph whose
  complete reference set is accepted through route 4 and whose partial set is
  rejected by route 4.

Differential attribution: with the pre-fix resolver and the identical test
file, the three atomic negatives and the three grouped tests FAIL and the
atomic positive control plus all eleven unrelated module tests PASS; with the
fix all 18 PASS. Each atomic negative is a single expression, so its
pre-fix failure is that assertion and nothing else.

## Direct-resolver reference-script carriage (measured)

The CEK direct resolver (direct resolver 0, phase 11 `Cek`) can never travel
inside the 16,384-byte L1 proof envelope: its applied body alone is 156,467
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
  156,982 signed bytes (L1 margin −140,598), re-measured 2026-08-06 (#546)
  under blueprint `605c8b8d…` by the producing selector `publishes and
  verifies the authenticated generated-blueprint CEK direct-resolver
  reference script` in
  `demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute.test.ts`
  (`MIDGARD_PRINT_PROOF_FIT=1`, suite 4/4, reproduced identically on two
  runs). The applied resolver body inside it is unchanged at 156,467 bytes /
  `f5d6395c…`, so the +306-byte move is in the publication framing, not the
  script. Superseded epoch value: 156,676 signed bytes (L1 margin −140,292)
  under blueprint `b1c79edc…`. The protected-blueprint receipt of 142,474
  signed bytes (L1 margin −126,090) is epoch-bound to `f5ae651e…` and has no
  producing surface in this tree any more (the suite now publishes exactly
  one blueprint's resolver), so it is retained as provenance and is not
  re-derivable. All are deployment-time-only
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

Complete-item carriage is preserved by the route-5 closure and pinned by
`cek_complete_item_carriage_survives_the_incremental_closure`: for one
selected envelope, the direct, single-publication, and minimum-multi-output
routes all accept, and only `IncrementalCekMaterial` and `NoCekMaterial`
reject. No complete-material route changed and no cap was lowered.

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
