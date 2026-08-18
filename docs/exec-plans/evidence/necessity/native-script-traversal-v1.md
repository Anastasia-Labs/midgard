# §3.2 Necessity artifact — native-script token and frame traversal

## Binding

- Family / item: `native-script-traversal` (`NativeScriptTokenWitness` with
  `chunk_proof`/`next_chunk_proof`, `NativeScriptFrameWitness`) / one
  complete Cardano native script from the script-witness field or an output;
  maximum shape bounded by the 32,768-byte script-witness aggregate field
  and the 16,384-node/depth guardrails (transaction bytes effective).
- Applied validator hashes measured (re-measured 2026-08-03): shared
  complete-item route
  `983051b4a0c3fe90057a599e77ed44c5ab694014036d49c86373a143` /
  `22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab` (the second
  unchanged); blueprint sha256
  `277b6457197870a9df069ce5c492c166e8d0b4b32fb616294ae12404ecb070b6`.
  Any change invalidates this artifact (GOAL_SPEC.md §3.2). Superseded pin
  (2026-07-29): `925662085ac87eb3cd63221b5184f59fde2c8b46d8db93052e80fc96` /
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
- Parameter snapshot digests: profile digest
  `181730d304796b764c8f657b0ae788b87c6aba9f4491dbfa9ce24d99932911b7`;
  capability floor per
  `docs/midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`.
- Fixture: shared exact-size item generators in
  `demo/midgard-validation/tests/complete-item-proof-fit-v1.test.ts`;
  native evaluation semantics covered by the phase-A Aiken suites.

## Measurements — flat `FieldCarriageV1` scheme (current; §3.2 order — stop at the first tier that fits)

**The flat reversion (#552/#565, complete) replaced the counted
`TransactionFieldChunkWitness` field-carriage mechanism below with the §8
three-tier ladder** (`docs/spec/midgard-tx.md` §8) for the *byte* question
(how one native-script item's bytes reach the machine). It did **not**
touch the *structural* question this artifact actually adjudicates: how one
native script's timelock/signature/threshold node tree is scanned and its
signer-set membership checked once the machine has the script's bytes in
hand. That scan is still driven by
`ValidationAuxiliaryWitnessV1.NativeScriptTokenWitness { chunk_proof,
next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>, signer_proof }` and
`.NativeScriptFrameWitness { frame }`
(`onchain/aiken/lib/midgard/validation-machine-v1.ak:131-137`), dispatched
across all eight native-script stages by
`verify_phase_a_native_scripts_scan`
(`validation-machine-v1.ak:4231-4340`; token-head, signature-payload,
all-or-any/at-least container-frame, and timelock-payload scans each still
take `chunk_proof`/`next_chunk_proof` and open them the same way), and still
opened via `bounded_item_v1`'s own `chunk_bytes: Int = 4095` constant
(`onchain/aiken/lib/midgard/bounded-item-v1.ak:11`) — unchanged since the
counted era. The item-begin step (one script, entered once) reaches the
flat door instead, via the same `TransactionFieldChunkWitness { field_index,
item_index, carriage }` constructor `verify_phase_a_native_item_scan`
pattern-matches at `validation-machine-v1.ak:4247-4256` — the same
flat-carriage constructor `transaction-field-chunk-v1.md` re-derives.

All figures below were reproduced this pass against the blueprint this
file's own Binding section already pins post-#606
(`onchain/aiken/plutus.json` sha256
`f49cae224f24cfab577f1ed10b5340384b75e541851eb7b77b507a79cb7d5e00`, md5
`5e38d7c6ccb7987d0aca710307dcaea7`, 398 validators / 702 definitions, fork
`aiken v1.1.23+2a78108`, `--env testnet` — confirmed by a fresh
`sha256sum`/`md5sum` of `onchain/aiken/plutus.json` this pass; no Binding
refresh needed, the pin above was already current) by running five suites
with `MIDGARD_PRINT_PROOF_FIT=1`, one file at a time
(`pnpm --config.verifyDepsBeforeRun=false --dir midgard-validation exec
vitest run tests/<file> --pool=forks --no-file-parallelism --bail=0` from
`demo/`): `complete-item-proof-fit-v1.test.ts` (5/5),
`complete-item-proof-fit-emulator-v1.test.ts` (6/6),
`field-preimage-carriage-fit-emulator-v1.test.ts` (16/16),
`complete-item-carriage-tiers-emulator-v1.test.ts` (5/5), and
`complete-item-equivalence-v1.test.ts` (2/2) — all green. This family's own
"phase-A Aiken suites" (the twelve
`phase-a-native-scripts-*-payload-semantic-v1.ak` validators cited in this
file's Binding section) are Aiken-level unit tests, not part of the vitest
suites this pass runs, and were not re-run here — building/checking the
Aiken tree is outside this pass's measurement rules; their continued
presence in the 398-validator blueprint above is structural evidence they
are still compiled and deployed, not a re-taken measurement.

**Movement notes.** The byte-fit half of this artifact's argument is now the
shared flat-carriage table, not a family-specific measurement, and is cited
rather than re-derived: `docs/exec-plans/evidence/necessity/transaction-field-chunk-v1.md`'s
own "Measurements — flat `FieldCarriageV1` scheme" section, re-run this pass
via the same suites (that file additionally cites
`complete-item-carriage-policy-v1.test.ts`, unchanged here). Carried from
there rather than re-taken: tier-1 nominal cap 14,336 (item ≤ 14,332),
**falsified at the signed-transaction layer above a 13,357-byte item /
13,361-byte preimage (#611, `docs/spec/midgard-tx.md` §8.3, escalated to
owner authority, not re-priced here)**; tier-2 `K` = 15,148 reliable / 15,644
exact, unmoved by #606; tier-3 combined lower bound 16,613 bytes (not the
P7-pinned 16,579 — #606 welded `field_hash` into the certificate datum,
`docs/spec/midgard-tx.md` §8.10). The recursion guardrails that make the
frame representation necessary in the first place —
`maxNativeScriptNodeCount` and `maxNativeScriptDepth`, both 16,384
(`MIDGARD_NATIVE_SCRIPT_MAX_NODE_COUNT_V1` /
`MIDGARD_NATIVE_SCRIPT_MAX_DEPTH_V1`,
`demo/midgard-core/src/consensus-profile-v1.ts:246-247`) — are structural
guardrails independent of carriage tier and did not move. None of the above
is re-flagged as a new finding; it is the same movement
`transaction-field-chunk-v1.md` already records, cited here because this
artifact's byte-fit representations 1–2 below inherit it directly.

### Exact limiting constraint — flat scheme

Two constraints stack, and only one of them moved. **Byte fit** (how one
native-script item's bytes reach the machine) is now the flat ladder cited
above: tier 1 to the signed frontier at 13,357 item bytes, tier 2 to `K` =
15,148, tier 3 to the 32,768-byte aggregate cap. **Execution fit** (how the
script's node tree is scanned and its signer sets checked once the bytes
are available) is unmoved: native evaluation is inherently recursive, and
the node/depth guardrails (16,384 each) still admit scripts whose subtree
one dispute step must never re-parse in full. `NativeScriptFrameWitness`
still bounds the on-chain stack so one step advances one node family, and
`NativeScriptTokenWitness` still reveals at most one 4,095-byte
`bounded_item_v1` chunk per step — the same shape the counted era used, now
gated internally rather than against a field-level wire commitment.

### Why no simpler authenticated representation closes the gap — flat scheme

Tier 1 and tier 2 now hand the machine the *complete* script-item preimage
in one step below their respective frontiers — tier 2 "hashes the whole
preimage against the committed field hash (measured free at ≤ 32 KB)" per
`docs/spec/midgard-tx.md` §8.2 — which removes the old byte-revelation
argument below `K`. It does **not** remove the recursive-evaluation
argument: a complete-script one-shot verifier would still have to
re-traverse up to 16,384 nodes in a single transition regardless of how
those bytes arrived. The frame representation remains the minimum stateful
addition that pins each step to one node family, and its chunks stay bound
to the same `bounded_item_v1` commitment the complete representations use —
now decoupled from (and cheaper to reach into, since whole-preimage reads
are free through tier 1/2) the byte-carriage question that
`transaction-field-chunk-v1.md` answers separately.

### Preserved complete-item path — flat scheme

Native scripts that fit tier 1 (signed frontier 13,357 bytes) or tier 2
(`K` = 15,148) reach the machine as a complete preimage in one carriage
step through the canonical-decode complete-item route, and single-chunk
tokens (`chunk_proof` with chunk 0 of a ≤4,095-byte item) still carry the
complete script bytes in one reveal exactly as the counted era's route did —
only the door supplying the item's bytes changed. Chunk proofs and the
complete-item read bind the identical bounded-item commitment:
`demo/midgard-validation/tests/complete-item-equivalence-v1.test.ts` (2/2
this pass) proves commitment equality and
omission/duplication/reorder/substitution/trailing rejection for both
representations.

### Necessity conclusion (re-derived for the flat scheme)

Re-derived same-direction: **YES**. A native script whose node/depth exceeds
what one CEK transition can safely re-parse still requires the staged
`NativeScriptTokenWitness`/`NativeScriptFrameWitness` walk, one node family
per step, regardless of which §8 tier delivered the script's bytes — the
flat reversion answered the byte-carriage question this artifact used to
share with `transaction-field-chunk-v1.md`'s family, and left the recursive-
evaluation question, which is this artifact's own, untouched. Nothing in
this pass's measurements (all vitest suites cited above green against the
current blueprint) weakens or strengthens that conclusion past what the
counted-era analysis below already established for its own scheme.

## Measurements — SUPERSEDED (counted-era `TransactionFieldChunkWitness` scheme; retained per GOAL_SPEC §3 invariant 14)

**Everything from this heading through the end of "Preserved complete-item
path" below prices the *counted* `TransactionFieldChunkWitness` mechanism's
byte-carriage half only.** It is retained verbatim as historical record —
superseded-not-deleted, GOAL_SPEC §3 invariant 14 — not because it is current
guidance. The flat-scheme section above is the current analysis for the
byte-fit representations (1–2); the execution-fit representation (4) and its
conclusion carry forward unchanged, restated above rather than re-argued.
Nothing below this notice was re-measured this pass.

### Measurements (§3.2 order — stop at the first representation that fits)

A native script is carried as one bounded field item, so its byte fit is the
transaction-field measurement applied to this family's maxima.

| Representation | Tx bytes / maxTxSize | Mem / limit·0.8 | CPU / limit·0.8 | Fee | Fits §3.3? |
| --- | --- | --- | --- | --- | --- |
| 1. Complete script direct in proof tx | fits through the measured 13,282-byte frontier (16,384/16,384, margin 0); a script-witness item may legally reach the 32,768-byte aggregate bound — 32,768-byte publication framing measures 35,186/16,384 | 205,594 / 13,200,000 | 500,275,649 / 8,000,000,000 | 974,576 | NO above 13,282 bytes |
| 2. Complete script as inline-datum publication + reference | fits through 14,396 bytes (15,256/16,384); 16,384 bytes → 18,290 (over by 1,906); 32,768 bytes → 35,186 (over by 18,802) | 264,106 / 13,200,000 | 552,114,352 / 8,000,000,000 | pub 826,821 | NO above 14,396 bytes |
| 3. Minimum multi-output publication + complete reconstruction | not deployed; the token/frame walk below consumes the same bounded chunks while simultaneously evaluating timelock/signature/threshold semantics that flat reconstruction would re-traverse | — | — | — | superseded by 4 |
| 4. Token scan (`chunk_proof`/`next_chunk_proof`) plus stateful frames | each step reveals at most one ≤4,095-byte chunk (≤4,675-byte publication, pinned) | within pinned per-step receipts / 13,200,000 | within pinned receipts / 8,000,000,000 | per pinned receipts | YES |

### Exact limiting constraint — SUPERSEDED (counted era)

`maxTxSize = 16,384` on the complete serialized transaction: a native
script item above 14,396 bytes cannot enter one publication transaction
(measured 18,290 bytes at 16,384 item bytes, 35,186 at 32,768), yet the
canonical guardrails admit native scripts whose encoding is bounded only by
the aggregate field. Additionally, native evaluation is inherently
recursive: frames bound the on-chain stack so one dispute step never
re-parses an unbounded subtree, keeping every step inside the reserved
execution ceilings.

### Why no simpler authenticated representation closes the gap — SUPERSEDED (counted era)

Above the publication maximum the bytes cannot fit one transaction at all
(measured, representation 1 and 2). For evaluation, a complete-script
one-shot verifier would re-traverse up to 16,384 nodes in a single step;
the frame representation is the minimum stateful addition that pins each
step to one node family while chunks stay bound to the same item
commitment used by the complete representations.

### Preserved complete-item path — SUPERSEDED (counted era)

Native scripts whose items fit 14,396 bytes retain complete-item carriage
through the canonical-decode complete-item route (direct at or below the
measured 13,282-byte frontier, publication + reference at or below 14,396),
and single-chunk tokens (`chunk_proof` with chunk 0 of a ≤4,095-byte item)
carry the complete script bytes in one reveal. Chunk proofs and the
complete item bind the identical bounded-item commitment
(`demo/midgard-validation/tests/complete-item-equivalence-v1.test.ts`
proves commitment equality and omission/duplication/reorder/substitution/
trailing rejection for both representations).

## Re-measurement 2026-08-03 (task C21-AUDIT)

Basis, blueprint provenance, and the shared by-reference byte series are
recorded once in `transaction-field-chunk-v1.md` §"Re-measurement
2026-08-03"; that section's overlay-build caveat applies to the digests
pinned above.

Re-verified unchanged for this family: `maxTransactionAggregateFieldBytes`
32,768, `maxNativeScriptNodeCount` 16,384, `maxNativeScriptDepth` 16,384,
`maxTransactionFieldChunkBytes` 4,095,
`maxSinglePublicationCompleteItemBytes` 14,396,
`maxFieldPublicationUnsignedTransactionBytes` 4,675, and the consensus
profile digest. Both guardrails the frame representation is sized against —
16,384 nodes and 16,384 depth — are byte-identical to the 2026-07-29 pin.

Re-measured on the fresh basis: 16,384 item bytes → 17,922/16,384 (over by
1,538, recorded 1,906); 32,768 item bytes → 34,818/16,384 (over by 18,434,
recorded 18,802). Both differ from the recorded values by a uniform 368
bytes (see the basis-mismatch note in `transaction-field-chunk-v1.md`);
direction and magnitude class are unchanged.

Conclusion still supported: YES. Byte-impossibility above the publication
frontier is re-confirmed, and the recursion argument that forces frames
depends only on the node/depth guardrails, which did not move.

Carried forward unverified: 13,282, 15,256, 18,290, 35,186, 205,594,
500,275,649, 974,576, 264,106, 552,114,352, 826,821 — see the
"not re-measurable" list in `transaction-field-chunk-v1.md`.
