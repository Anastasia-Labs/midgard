# §3.2 Necessity artifact — ledger-output preimage incremental proof

## Binding

- Family / item: `ledger-output-incremental`
  (`LedgerOutputProofBeginWitness` / `LedgerOutputProofStepWitness` with
  `LedgerOutputProofChunks`, `LedgerOutputProofDatum`,
  `LedgerOutputProofValue`, `LedgerOutputProofNativeFrame` /
  `LedgerOutputProofFinalizeWitness`) / one complete ledger output preimage;
  maximum shape 16,384 bytes (`maxLedgerOutputPreimageBytes`).
- Applied validator hashes measured (re-measured 2026-08-03):
  `983051b4a0c3fe90057a599e77ed44c5ab694014036d49c86373a143`
  (`canonical_decode_item_semantic_v1` applied on the measurement
  deployment), `22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab`
  (`proof_item_v1`, unchanged); blueprint sha256
  `277b6457197870a9df069ce5c492c166e8d0b4b32fb616294ae12404ecb070b6`.
  Any change invalidates this artifact (GOAL_SPEC.md §3.2). Superseded pin
  (2026-07-29): applied `925662085ac87eb3cd63221b5184f59fde2c8b46d8db93052e80fc96`,
  blueprint `6d23a25f8cb96f62f3e3aeeecb4e1506e8002ac712ae9bcb8873e42b4136ff1a`.
- Re-verification 2026-08-04 (C21-AUDIT, issue #484): the generated
  `onchain/aiken/plutus.json` of that epoch was SHA-256
  `f5ae651e34cf3e1175d928634c002580c4f2af4659a229952007c458945b866b`
  (380 validators, `aiken v1.1.22+39d6b04`), which superseded the whole-file
  blueprint pin above. Both validators this artifact binds were byte-identical
  in that blueprint:
  `fraud_proofs/validation_trace/canonical_decode_item_semantic_v1.main.spend`
  unapplied `62501cfe7cf63485a493c902060cd422acdd88757c319345eadb8819` and
  `fraud_proofs/validation_trace/proof_item_v1.main.else`
  `22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab`. That pass
  compared only the two bound script hashes and *inferred* the applied hash
  `983051b4a0c3fe90057a599e77ed44c5ab694014036d49c86373a143` from them, so it
  left a fresh applied re-measurement owed. The `f5ae651e…` digest is retained
  here as measurement-epoch provenance only.
- Re-verification 2026-08-06 (issue #546) — applied re-measurement, not
  inference. A fresh stock `aiken build --env testnet` of the current tree
  (`aiken v1.1.22+39d6b04`) produces `onchain/aiken/plutus.json` SHA-256
  `605c8b8dca1f01e2cde5219138a1f81e69214f9a182c10b73c20341187ddc2dc`
  (391 validators, including the chunked-MPF and harvest additions); that is
  the current whole-file pin and it supersedes both `277b6457…` and
  `f5ae651e…`. Measured against it, unchanged: the two bound scripts are
  byte-identical again (`62501cfe…` unapplied, `22c9a103…` for
  `proof_item_v1`), and the applied hash
  `983051b4a0c3fe90057a599e77ed44c5ab694014036d49c86373a143` was re-derived by
  the production builder `buildValidationTraceDisputeFaultProofContracts` on
  the measurement deployment (`hub_oracle=11…11`, `catalogue=22…22`, semantic
  resolver 1 of 76). Producing run: `pins the applied §3.2 necessity
  identities on the measurement deployment` in
  `demo/midgard-validation/tests/complete-item-proof-fit-emulator-v1.test.ts`,
  which now gates both identities instead of arguing them. Every hash pinned
  above is therefore current under `605c8b8d…` and the measurement tables
  below stay bound; the C21-AUDIT "fresh applied re-measurement owed before
  CG5" residual is discharged for these two identities.
- **Re-pinned 2026-08-15 (issue #580, Phase-7 confirm-and-publish) — the
  post-cascade identities. This bullet supersedes every pin above it; the
  earlier ones are retained as provenance on this file's own superseded-pin
  discipline.** The §3.2 hash binding fired exactly as designed across #579's
  single blueprint regeneration and #609's applied-hash cascade, and the
  movement was measured rather than inferred.

  Current blueprint: `onchain/aiken/plutus.json` SHA-256
  `91861fac2d0bcafade6d8e1b4872e669cf9e7b52f5ed75d3b9729e4e02d6dd6b`
  (md5 `b20c9a14a8fe445cdddbe5305b3857c1`, **398 validators / 702
  definitions**), built by the pinned fork **`aiken v1.1.23+2a78108`** (binary
  md5 `b3acfdf348235798cb6b921d0f87750a`) under the declared construction
  **`aiken build --env testnet`**. The env flag is load-bearing and was not
  recorded before: a default-env build of the same tree differs in
  `scheduler.spend` alone (`shift_duration` is the only env-divergent constant
  any validator compiles) and does **not** reproduce this digest. Stock
  `v1.1.22` is retired from every role (#579 owner ruling A), so every
  `v1.1.22` figure above is a void stock value rather than a second opinion.

  | bound identity | superseded | current |
  | --- | --- | --- |
  | blueprint SHA-256 | `605c8b8d…` (391 validators, stock v1.1.22) | `91861fac…` (398 validators, fork v1.1.23+2a78108) |
  | `canonical_decode_item_semantic_v1` unapplied | `62501cfe…` | `a8d8f3ac58bce62636725d394cb5953767f927518d6db6de70144b67` |
  | `canonical_decode_item_semantic_v1` applied (`hub_oracle=11…11`, `catalogue=22…22`) | `983051b4…` | `0a42b4c76739fa1a1a391c8a766fdadf58c2692b983e3c455cb5fdb6` |
  | `proof_item_v1` | `22c9a103…` | `22c9a103…` — **unmoved**, still unparameterized |

  Derived by producers on the deployment this file declares, not read off a
  diff. `demo/midgard-core/scripts/measure-validation-proof-item-envelope.mjs`
  applies `["11"×28, "22"×28, proof_item_script_hash]` to the blueprint's own
  `canonical_decode_item_semantic_v1.main.spend` and reports the applied hash;
  that three-argument application is exactly what #609's blueprint-arity guard
  now requires, because the validator declares **3** parameters and did *not*
  gain #592's `field_preimage_certificate_policy_id`. The gate named in the
  bullet above — `pins the applied §3.2 necessity identities on the measurement
  deployment` in
  `demo/midgard-validation/tests/complete-item-proof-fit-emulator-v1.test.ts` —
  re-derives both identities at runtime rather than hardcoding them, and is
  **green against this blueprint** (suite 5/5, 2026-08-15), so it gates the new
  values as it gated the old. Full record, commands and the
  provisional-versus-confirmed ledger:
  `docs/exec-plans/evidence/canonical-v1-p7-remeasurement-v1.json`.
- **Re-pinned 2026-08-16 (issue #606, the E2 certificate repair carrying the
  #608 empty-sentinel rider) — the post-#606 identities. This bullet supersedes
  every pin above it, the #580 bullet included; the earlier ones are retained as
  provenance on this file's own superseded-pin discipline.** #606 moved the
  blueprint again, so the digest pinned above stopped being a current-tree claim
  the moment that batch landed: the §8.6 repair rewrote
  `lib/midgard/native-tx-field-access-v1.ak` and
  `validators/field-preimage-certificate.ak` (constant asset name, `field_hash`
  welded into the mint-verified datum, the new door equality), and the rider
  rewrote `validators/pexcludes.ak` and re-pointed
  `env.plutarch_pexcludes_validator_hash` in both env files
  (`a9ec251d…` → `03adaadf…`) with its four embedding step validators behind
  it.

  Current blueprint: `onchain/aiken/plutus.json` SHA-256
  `f49cae224f24cfab577f1ed10b5340384b75e541851eb7b77b507a79cb7d5e00`
  (md5 `5e38d7c6ccb7987d0aca710307dcaea7`, **398 validators / 702
  definitions**), built by the same pinned fork **`aiken v1.1.23+2a78108`**
  (binary md5 `b3acfdf348235798cb6b921d0f87750a`) under the same declared
  construction **`aiken build --env testnet`** — the env flag is load-bearing
  exactly as the bullet above records — and verified byte-reproducible by a
  fresh build of this tree.

  | bound identity | superseded (#580) | current (#606) |
  | --- | --- | --- |
  | blueprint SHA-256 | `91861fac…` (md5 `b20c9a14…`, 398 validators) | `f49cae22…` (md5 `5e38d7c6…`, 398 validators / 702 definitions) |
  | `canonical_decode_item_semantic_v1` unapplied | `a8d8f3ac…` | `a8d8f3ac58bce62636725d394cb5953767f927518d6db6de70144b67` — **unmoved** |
  | `canonical_decode_item_semantic_v1` applied (`hub_oracle=11…11`, `catalogue=22…22`) | `0a42b4c7…` | `0a42b4c76739fa1a1a391c8a766fdadf58c2692b983e3c455cb5fdb6` — **unmoved** |
  | `proof_item_v1` | `22c9a103…` | `22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab` — **unmoved**, still unparameterized |

  **The blueprint digest moved; this artifact's own bound identities did not.**
  That is the measured outcome, not an assumption: neither of #606's two
  repairs reaches `canonical_decode_item_semantic_v1` or `proof_item_v1`, and
  the resolver still declares **3** parameters — it did not gain
  `field_preimage_certificate_policy_id`, so the three-argument application
  #609's arity guard requires is unchanged in shape *and* in value. Re-derived
  by the same producer the bullet above names,
  `demo/midgard-core/scripts/measure-validation-proof-item-envelope.mjs`, run
  against this tree: it reports `proofItemScriptHash`
  `22c9a103…` and `semanticScriptHash` `0a42b4c7…`, reproducing both pins
  exactly. The §3.2 hash binding therefore did **not** fire for this artifact
  this pass, and every measurement table below stays bound. Only the two
  identities #606 did move — the §8.6 `FieldPreimageCertificateV1` mint policy
  (`c3682abd…` → `f030476f9cddff41d15bdaa7951a9726252c6867901992e2a5f8427e`)
  and, through it and the rewritten field-access door, `cek_v1` — are re-pinned,
  on `cek-program-material-v1.md`.
- Parameter snapshot digests: consensus profile digest
  `181730d304796b764c8f657b0ae788b87c6aba9f4491dbfa9ce24d99932911b7`;
  capability floor per
  `docs/midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`.
- Fixture: exact-size canonical output items generated in
  `demo/midgard-validation/tests/complete-item-proof-fit-v1.test.ts`
  (deterministically regenerable).

## Measurements — flat `FieldCarriageV1` scheme (current; §3.2 order — stop at the first tier that fits)

**The flat reversion (#552/#565, complete) replaced the counted
`TransactionFieldChunkWitness` field-carriage mechanism below with the §8
three-tier ladder** (`docs/spec/midgard-tx.md` §8) for the *byte* question
(how one complete ledger-output item's bytes, as a whole `outputs`-field
item, reach the machine). It did **not** touch the *structural* question
this artifact actually adjudicates: how one output's already-authenticated
preimage is walked incrementally — chunk by chunk, datum action by datum
action, native-script frame by native-script frame, Value asset by Value
asset — once the machine has that preimage's commitment in hand. That walk
is still driven by `ValidationAuxiliaryWitnessV1.LedgerOutputProofBeginWitness
{ output_index, total_length, item_commitment, siblings }` /
`.LedgerOutputProofStepWitness { witness: ledger_output_proof_v1.LedgerOutputProofWitnessV1 }`
/ `.LedgerOutputProofFinalizeWitness { descriptor_cbor, signer_proof }`
(`onchain/aiken/lib/midgard/validation-machine-v1.ak:316-327`), dispatched by
stage-5's `script_sources_stage_five`
(`validation-machine-v1.ak:9429-9489`: begin routes to
`script_sources_output_proof_begin`, step to `script_sources_output_proof_step`,
finalize to `script_sources_output_proof_finalize`, gated on
`ledger_output_proof_v1.terminal_is_exact_v1`), and `LedgerOutputProofWitnessV1`'s
own four live variants — `LedgerOutputProofChunks { chunk_proof,
next_chunk_proof }`, `LedgerOutputProofValue { policy_id, asset_name,
quantity, siblings }`, `LedgerOutputProofDatum`, `LedgerOutputProofNativeFrame`,
and `LedgerOutputProofAdvanced { control }`
(`onchain/aiken/lib/midgard/ledger-output-proof-v1.ak:52-76`) — still open
their chunks against `bounded_item_v1.verify_chunk(control.item_commitment,
proof)` (`ledger-output-proof-v1.ak:486`) and still walk in strides of
`bounded_item_v1.chunk_bytes` (`:498-554,839-875`), unchanged since the
counted era, with `chunk_bytes: Int = 4095`
(`onchain/aiken/lib/midgard/bounded-item-v1.ak:11`). The `item_commitment`
and `total_length` this family's begin step receives are handed off from
stage 4's proof-only tag-29 fold (unchanged — see the "Preserved
complete-item path" discussion below, carried forward verbatim), not from
canonical decode's flat door directly; the flat reversion is one hop removed
from this family, reached only through the complete-item byte question
representations 1–2 below share with every other §3.2 family.

All figures below were reproduced this pass against the blueprint this
file's own Binding section already pins post-#606
(`onchain/aiken/plutus.json` sha256
`f49cae224f24cfab577f1ed10b5340384b75e541851eb7b77b507a79cb7d5e00`, md5
`5e38d7c6ccb7987d0aca710307dcaea7`, 398 validators / 702 definitions, fork
`aiken v1.1.23+2a78108`, `--env testnet` — confirmed by a fresh
`sha256sum`/`md5sum` of `onchain/aiken/plutus.json` this pass; no Binding
refresh needed, the last bullet above was already current) by running six
suites with `MIDGARD_PRINT_PROOF_FIT=1`, one file at a time
(`pnpm --config.verifyDepsBeforeRun=false --dir midgard-validation exec
vitest run tests/<file> --pool=forks --no-file-parallelism --bail=0` from
`demo/`): `complete-item-proof-fit-v1.test.ts` (5/5),
`complete-item-proof-fit-emulator-v1.test.ts` (6/6),
`field-preimage-carriage-fit-emulator-v1.test.ts` (16/16),
`complete-item-carriage-tiers-emulator-v1.test.ts` (5/5),
`complete-item-carriage-policy-v1.test.ts` (6/6), and
`complete-item-equivalence-v1.test.ts` (2/2) — all green. The Aiken-level
unit test this family's own "Preserved complete-item path" cites,
`script_sources_rejects_a_forged_output_item_commitment` /
`_length`, is an `.ak` test, not a vitest suite; per this pass's
measurement rules (no `aiken build`/`aiken check` in `onchain/aiken`) it was
not re-run — its continued presence in the source tree and in the compiled
398-validator blueprint above is cited as structural evidence only, not as
a re-taken measurement.

**Movement notes.** The byte-fit half of this artifact's argument is the
shared flat-carriage table, cited rather than re-derived:
`docs/exec-plans/evidence/necessity/transaction-field-chunk-v1.md`'s own
"Measurements — flat `FieldCarriageV1` scheme" section, re-run this pass via
the same suites (that file is the origin of the citation above; nothing
here is a second, independent derivation of it). Carried from there:
tier-1 nominal cap 14,336 (item ≤ 14,332), **falsified at the
signed-transaction layer above a 14,004-byte item (re-derived 2026-08-23; the
retired figure was a 13,357-byte item / 13,361-byte preimage on the
pre-Option-B authenticate route)
(#611, `docs/spec/midgard-tx.md` §8.3, escalated to owner authority, not
re-priced here)**; tier-2 `K` = 15,148 reliable / 15,644 exact, unmoved by
#606; tier-3 combined lower bound 16,613 bytes (not the P7-pinned 16,579 —
#606 welded `field_hash` into the certificate datum,
`docs/spec/midgard-tx.md` §8.10). `maxLedgerOutputPreimageBytes` itself is
unmoved at 16,384 (confirmed live this pass in
`complete-item-proof-fit-v1.test.ts`'s printed receipt:
`"maxLedgerOutputPreimageBytes": 16384`); it is the tier-1/tier-2/tier-3
*route* to that 16,384-byte ceiling that changed, not the ceiling. The
begin/step/finalize per-step receipt ceilings this family's own
"Re-measurement 2026-08-03" section pins (3,398,228 mem, 1,209,745,039 cpu)
are downstream of the incremental walk, not the field door, and are not
re-flagged as new movement here. None of the above is a new finding; it is
the same movement `transaction-field-chunk-v1.md` already records, cited
here because representations 1–2 below inherit it directly.

### Exact limiting constraint — flat scheme

Two constraints stack, and only one of them moved. **Byte fit** (how one
complete ledger-output item's bytes reach the machine as a field item) is
now the flat ladder cited above: tier 1 to the signed inline frontier at 14,004
item bytes, tier 2 to `K` = 15,148, tier 3 to the 32,768-byte aggregate cap
— none of which reach `maxLedgerOutputPreimageBytes` (16,384) in one
carriage step, since 16,384 exceeds even tier 3's per-item practical
frontier once framed as a complete field item. **Incremental-walk fit**
(how the output's datum/Value/native-script sub-structures are verified
once its preimage commitment is available) is unmoved: a 16,384-byte
complete output's `Verify` redeemer still cannot be framed in one
publication transaction at all (the original 16,900-byte measurement below
is a structural fact about redeemer framing overhead, independent of which
tier delivered the item), so the begin/step/finalize traversal — one
bounded chunk, one datum action, one native-script frame, or one Value
asset per step — remains the only route to a 16,384-byte output's complete
authentication.

### Why no simpler authenticated representation closes the gap — flat scheme

Tier 1 and tier 2 now hand the machine a *complete* item preimage in one
step below their respective frontiers — tier 2 "hashes the whole preimage
against the committed field hash (measured free at ≤ 32 KB)" per
`docs/spec/midgard-tx.md` §8.2 — which removes the old byte-revelation
argument for items at or below `K`. It does **not** remove the
incremental-walk argument for the ledger-output's own maximum, 16,384 bytes,
which sits above every tier's practical single-step frontier: a flat
multi-output publication would still need the datum, Value, and
native-script sub-structures verified against the output commitment, which
is exactly what the begin/step/finalize route does while consuming the
same `bounded_item_v1` chunks — a flat variant would duplicate the chunk
machinery without removing any step, now for the identical reason the
counted era gave, just with the field-level revelation question answered
separately.

### Preserved complete-item path — flat scheme

Ledger outputs that fit tier 1 (signed inline frontier 14,004 bytes; the 14,336-byte cap itself stages by reference) or tier 2
(`K` = 15,148) reach the machine as a complete preimage in one carriage step
through the canonical-decode complete-item route — the same
`TransactionFieldItemWitness` guard `complete-item-carriage-policy-v1.test.ts`
pins (6/6 this pass), now gated by the flat door rather than a per-item wire
commitment. Both representations still bind the same bounded-item
commitment; equivalence and
omission/duplication/reorder/substitution/trailing rejection are still
exercised by `demo/midgard-validation/tests/complete-item-equivalence-v1.test.ts`
(2/2 this pass) and the deployed-route rejection tests in
`demo/midgard-validation/tests/complete-item-proof-fit-emulator-v1.test.ts`
(6/6 this pass). The stage-4/stage-5 hand-off — the proof-only tag-29
witness that makes stage-4 evidence O(1) in output size, described in the
"Residual gap: CLOSED" paragraph below — is untouched by the flat
reversion; it operates on the authenticated
`(field_index, item_index, item_length, item_commitment)` tuple regardless
of which §8 tier delivered the underlying bytes, and is retained verbatim
below rather than re-argued.

### Necessity conclusion (re-derived for the flat scheme)

Re-derived same-direction: **YES**. A maximum, 16,384-byte ledger output
still equals the whole L1 envelope by itself and still cannot be framed
into one publication or proof transaction regardless of which §8 tier would
otherwise carry it, so the begin/step/finalize incremental traversal —
one bounded chunk, datum action, native frame, or Value asset per step —
remains required. The flat reversion answered the byte-carriage question
this artifact used to share with `transaction-field-chunk-v1.md`'s family
for items at or below tier 3, and left the incremental-walk question for
the ledger-output's own 16,384-byte maximum, which this artifact owns,
untouched. Nothing in this pass's measurements (all vitest suites cited
above green against the current blueprint) weakens or strengthens that
conclusion past what the counted-era analysis below already established
for its own scheme.

## Measurements — SUPERSEDED (counted-era `TransactionFieldChunkWitness` scheme; retained per GOAL_SPEC §3 invariant 14)

**Everything from this heading through the end of "Preserved complete-item
path" below prices the *counted* `TransactionFieldChunkWitness` mechanism's
byte-carriage half only.** It is retained verbatim as historical record —
superseded-not-deleted, GOAL_SPEC §3 invariant 14 — not because it is current
guidance. The flat-scheme section above is the current analysis for the
byte-fit representations (1–2); the incremental-walk representation (4) and
its conclusion carry forward unchanged, restated above rather than
re-argued. Nothing below this notice was re-measured this pass.

### Measurements (§3.2 order — stop at the first representation that fits)

| Representation | Tx bytes / maxTxSize | Mem / limit·0.8 | CPU / limit·0.8 | Fee | Fits §3.3? |
| --- | --- | --- | --- | --- | --- |
| 1. Complete output direct in proof tx | 16,384-byte output: `Verify` redeemer alone ≥ 16,900 bytes framed — exceeds 16,384 before framing; measured direct frontier for any complete item is 13,282 bytes (16,384/16,384, margin 0) | 205,594 / 13,200,000 | 500,275,649 / 8,000,000,000 | 974,576 | NO above 13,282 bytes |
| 2. Complete output as inline-datum publication + reference consumption | 16,384-byte output publication measures 18,290/16,384 — over by 1,906; publication fits only through 14,396 bytes (15,256/16,384) | consuming tx 264,106 / 13,200,000 | 552,114,352 / 8,000,000,000 | pub 826,821 | NO above 14,396 bytes |
| 3. Minimum multi-output publication + complete logical reconstruction | not deployed; the incremental route below consumes the same ≤4,095-byte bounded chunks and additionally interleaves Value, datum-traversal, and native-frame sub-proofs that a flat reconstruction would still need | — | — | — | superseded by 4 |
| 4. Incremental begin/step/finalize traversal (chunks + datum actions + native frames + Value proofs) | each step ≤ one bounded chunk reveal (≤4,675-byte publication, pinned) | within pinned per-step receipts (3,398,228 max observed field-chunk receipt) / 13,200,000 | 1,209,745,039 / 8,000,000,000 | per pinned receipts | YES |

### Exact limiting constraint — SUPERSEDED (counted era)

`maxTxSize = 16,384` on the complete serialized publication or proof
transaction: a maximum 16,384-byte ledger output equals the whole L1
envelope by itself, and its measured complete signed publication overshoots
by 1,906 bytes. The consensus profile therefore retains ledger outputs
"authenticated incrementally" (docs/consensus-profile-v1.md §10), and the
resolve-inputs membership route must be able to traverse output preimages,
inline datums, embedded native scripts, and Values chunk by chunk.

### Why no simpler authenticated representation closes the gap — SUPERSEDED (counted era)

The complete output cannot enter one transaction above 14,396 bytes
(measured, not inferred from item length). A multi-output flat publication
still requires the datum, Value, and native-script sub-structures to be
verified against the output commitment, which is exactly what the
begin/step/finalize route does while consuming the same bounded chunks; a
flat variant would duplicate the chunk machinery without removing any step.

### Preserved complete-item path — SUPERSEDED (counted era)

Every output at or below `maxSinglePublicationCompleteItemBytes` (14,396)
keeps its complete-item carriage: the canonical-decode producer emits
`TransactionFieldItemWitness` for it (guard pinned by
`complete-item-carriage-policy-v1.test.ts`). Both representations bind the
same bounded-item commitment; equivalence and
omission/duplication/reorder/substitution/trailing rejection are exercised
by `demo/midgard-validation/tests/complete-item-equivalence-v1.test.ts` and
the deployed-route rejection tests in
`demo/midgard-validation/tests/complete-item-proof-fit-emulator-v1.test.ts`.

Residual gap: CLOSED (2026-08-01, owner decision C21-STAGE4 Option A —
see `docs/exec-plans/evidence/c21-stage4-analysis.md` and the GOAL_PROGRESS
Decisions entry). Historical record with corrections: this paragraph
previously recorded the gap as "(14,774, 16,384]", which understated it on
two axes. First, 14,774 was a single-output best case — the collection
proof's frontier/siblings grow ~73 bytes per Merkle level of the outputs
collection, so the frontier fell as output count rose. Second, the deployed
carriage was the binding bound, not the producer envelope: stage 4 routes
through semantic resolver 0, whose prepare and resolution transactions
carried the auxiliary inline only, bounding the practical gap near
(8,769, 16,384]. The gap was a SOUNDNESS break (one-step resolution is
challenger-only and a `ReadyForOneStep` timeout awards nobody, so an
unbuildable argument finalized the challenged block). Neither closure this
paragraph proposed was adopted: instead the stage-4 fold stopped revealing
item bytes entirely — it now folds the authenticated
`(field_index, item_index, item_length, item_commitment)` tuple through
`bounded_collection_v1.verify_item` via the proof-only tag-29 witness,
making stage-4 evidence O(1) in output size with no bounded fallback and no
new carriage. Byte authentication remains where it already occurred:
canonical decode (chunk and complete carriage) and this artifact's stage-5
`LedgerOutputProof` traversal. Evidence:
`script_sources_rejects_a_forged_output_item_commitment` /
`_length` (forged-tuple rejection via `verify_item` alone) and the
"builds output-size-independent stage-4 one-step evidence up to the
16,384-byte ledger maximum" case in `complete-item-proof-fit-v1.test.ts`.

## Re-measurement 2026-08-03 (task C21-AUDIT)

Basis, blueprint provenance, and the shared by-reference byte series are
recorded once in `transaction-field-chunk-v1.md` §"Re-measurement
2026-08-03"; that section's overlay-build caveat applies to the digests
pinned above.

Re-verified unchanged for this family: `maxLedgerOutputPreimageBytes` 16,384,
`maxSinglePublicationCompleteItemBytes` 14,396,
`maxTransactionFieldChunkBytes` 4,095,
`maxFieldPublicationUnsignedTransactionBytes` 4,675, the per-step receipt
ceilings 3,398,228 / 1,209,745,039, and the consensus profile digest.

Re-measured on the fresh basis: a 16,384-byte output's complete signed
publication measures 17,922/16,384 — over by 1,538 rather than the recorded
1,906, a uniform 368-byte difference also seen at 32,768 bytes and most
likely a collection-proof-shape difference (see the basis-mismatch note in
`transaction-field-chunk-v1.md`). Direction and conclusion unchanged.

Independent consistency check on the stage-4 closure paragraph above: its
"practical gap near (8,769, 16,384]" now coincides exactly with
`MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxExactDirectCompleteItemBytes` = 8,769 in
`demo/midgard-core/src/consensus-profile-v1.ts`, whose reliable counterpart
8,273 is the deployed direct-carriage selector bound. The two records agree.

Conclusion still supported: YES. A maximum ledger output still equals the
whole L1 envelope by itself and still overshoots its own publication
transaction, so incremental authentication remains required.

Carried forward unverified: 16,900, 13,282, 18,290, 15,256, 205,594,
500,275,649, 974,576, 264,106, 552,114,352, 826,821 — see the
"not re-measurable" list in `transaction-field-chunk-v1.md`.
