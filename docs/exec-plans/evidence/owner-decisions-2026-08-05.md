# Owner-delegated decisions — 2026-08-05

Provenance: on 2026-08-05 the repository owner (Philip DiSarro) delegated
three pending owner decisions to an assistant research agent with the
instruction to "do the required research and make the best decision". This
file is the durable record of that memo, verbatim below its own title. The
decisions bind as owner decisions under that delegation; the ledger entry
of the same date records their summary and this file is the citable
authority for the #545 lane, the C26 promotion batch, and the catalogue
registration batch.

# Owner decisions memo — 2026-08-05

Delegated owner decisions for three pending items on
`colll78/canonical-v1-watcher-l1-source-checkpoint`. All ledger reads are from
committed content (`git show HEAD:GOAL_PROGRESS.md`, 5,761 lines); the
uncommitted 246-row "(live-verified on preprod)" edit and the in-flight #543
working-tree changes were ignored per the standing exclusion rule.

---

## Decision 1 — #541: the four OPEN Q1x output-5 cells

**DECISION:** The four output-5 cells (Q10/Q11/Q12/Q14) stay OPEN now and are
pre-authorized to flip to LOCAL_PASS — without a further owner round-trip —
in exactly the change where BOTH re-decision conditions below clear;
#541 stays open until `node demo/scripts/verify-canonical-v1-proof-family-q1x.mjs`
exits 0 with all four output-5 cells at LOCAL_PASS on a tree whose §4.4
journey and dual-compiler guard are green, and closes then.

### The exact decision rule (pre-authorized)

A cell may be re-decided OPEN → LOCAL_PASS if and only if all of:

1. **Shipped-path condition.** The family's real prepare/submit route (the
   one `bin.ts` builds, not a parallel test-only route) carries its MPF
   membership/non-membership openings via chunked carriage
   (`midgard/mpf_chunked_proof_v1` + the challenge thread), and the
   exhaustible single-transaction route is removed or demoted so no shipped
   step transaction's byte cost grows with proof depth. Evidence: emulator
   lifecycle through the real pipeline at adversarial depth >= 22 (strictly
   above the old byte ceiling 21–23), and at the MPF structural maximum 64
   (4 chunks) where cheap to construct synthetically.
2. **Re-measured bound.** `adversarialDepthBound` is re-derived under the new
   route with `envelopeExhaustibleByReferenceAdversary: false` from
   measurement, not assertion: each chunk-publication transaction fits the
   16,384-byte envelope (7 chunks/tx measured, worst chunk datum 2,230 B
   against bound 2,304 — ledger entry "#541 remediation implemented",
   2026-08-05), total chunks bounded at 4 by the MPF path maximum 64, and
   the finalize transaction's marginal byte cost per proof level is zero.
3. **Q1X-F6 exercised — YES, this is required before any cell closes.**
   The spend-input preimage cardinality axis must be measured for Q10/Q11
   (expected binding surface: the spend-inputs witness publication
   transaction and step execution units, per the artifact's
   `adversarialAxes[1].note`), and Q12/Q14's structural absence of the axis
   must be kept as the artifact's recorded executable exclusion. Rationale
   below.
4. The Q1x verifier's output-5 section is rewritten in the same change from
   unconditional OPEN asserts to LOCAL_PASS requirements bound to the
   measured facts (this is forced anyway — see verifier mechanics), and the
   re-decision is ledgered.

If the F6 measurement reveals a new envelope exhaustion inside the 2^128
reference adversary, the pre-authorization is void: cells stay OPEN, the
finding is recorded (Q1X-F7), and #541 (or a successor) stays open.

### Verifier mechanics that make this rule self-enforcing

`demo/scripts/verify-canonical-v1-proof-family-q1x.mjs`:

- Lines 686–694 assert every output-5 cell is `"OPEN"` **unconditionally** —
  cells cannot flip without editing this section.
- Lines 683–685: `output5MayClose = !envelopeExhaustibleByReferenceAdversary
&& unexercisedAxes.length === 0`. Line 695–699 asserts
  `output5MayClose === false` — the tripwire fires only when BOTH conditions
  clear, i.e. at exactly the re-decision moment.

Consequence for sequencing: if #545 lands without F6, the artifact can
honestly record `envelopeExhaustibleByReferenceAdversary: false` and the
verifier still passes with cells OPEN (one axis still unexercised). No
tripwire, no inconsistency. When F6 lands, the tripwire fires and forces the
re-decision in that same change. This is well-designed; do not weaken the
global unexercised-axes rule into a per-family rule — the conservative global
rule costs only that Q12/Q14 wait for the F6 measurement, and splitting it
would mean extra verifier surgery for a few days' scheduling gain.

### Why Q1X-F6 must be exercised first

- The cells' own title is "Maximum/adversarial proof-fit fixture"
  (GOAL_SPEC.md §9.1 output 5, line 880). Publishing LOCAL_PASS for Q10/Q11
  with a known-unmeasured adversarial axis repeats the exact defect class
  the artifact records as its own lesson (Q1X-F1: "a passage claim an
  adversary can falsify").
- The verifier already enforces it (global rule above); honoring it needs no
  code motion. The measurement itself is bounded work: the axis is capped by
  the protocol's per-transaction input limit and is a fixture + measurement,
  not a protocol change.
- Q12/Q14 genuinely lack the axis (`residualFindings` Q1X-F6: invalid-range
  operates on the compact transaction; zero-input spends nothing by
  construction) — that exclusion is already recorded and rides into the
  re-decision unchanged.

**F6 ownership:** offer it to the #545 lane (same machinery — published
material reached via reference inputs is exactly what the lane is wiring);
if the lane declines the scope, the parent files a dedicated issue
immediately, because after #545 it becomes the sole gate on #541 closure.

### `challenged_root_domain` — REQUIRED for #545, not deferred

`ProofChallengeDatum` (onchain/aiken/lib/midgard/mpf-chunked-proof-v1.ak,
type at ~line 101) carries `expected_root`/`expected_leaf_count` but not the
domain; the domain lives only in the `InitChallenge` mint redeemer
(challenge.ak:92–153) where `challenge_matches_header` authenticates the
pair against `header_counted_commitment(header, domain)`.

Honest analysis: this is **not a live soundness hole** — identical
(root, count) across domains implies identical tree commitment, so any
consumer family that re-binds `expected_root`/`expected_leaf_count` against
its own domain's header commitment gets soundness even without the field.
But it IS a misuse-prone consumer contract: each of the four #545 consumer
integrations (and every future family) must independently reproduce the
domain re-derivation, and a consumer that trusts the datum without
re-binding is silently wrong. One datum field turns four re-derivation
invariants into one equality check.

The decisive argument is deployment-identity timing, not soundness: the
challenge validator's hash enters `DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES`
and reference-script publication in the Decision-3 registration batch
(it is registered nowhere today — verified by grep over midgard-core/
midgard-sdk/midgard-node sources). Adding the field after that batch means a
second identity move of the exact kind Decision 3 exists to avoid. Adding it
now is a small change inside files the #545 lane already owns, before any
identity consumer pins the hash.

**Requirement passed to the #545 lane:** add `challenged_root_domain` to
`ProofChallengeDatum`, write it at init from the authenticated redeemer
value, and have each family's consumer assert datum domain equality; keep
the existing substituted-domain guard
(`mpf_chunked_challenge_init_rejects_a_substituted_root_domain`,
challenge.ak:497) and add its datum-side twin.

### Ledger stance now

No status motion. At the next parent checkpoint, ledger this decision rule
verbatim (the pre-authorization, the F6 requirement, the domain-field
requirement) so the #545 lane and the re-decision change have a durable
authority to cite. #541 issue disposition: stays open; closes on the
measurable condition stated in the DECISION line (Q1x verifier exit 0 with
4 LOCAL_PASS output-5 cells on a §4.4-green tree).

### Risks of being wrong

- If chunked carriage has an unfound flaw, pre-authorizing LOCAL_PASS could
  close cells on a still-exhaustible path — mitigated because the rule
  demands re-measured `envelopeExhaustibleByReferenceAdversary: false` and
  a >= depth-22 lifecycle, both runner-asserted, and the 45-test chunked
  suite pins the arithmetic (chunk bounds, index range, substitution
  guards).
- Requiring F6 could stall #541 closure if the measurement is harder than
  expected — accepted: an unmeasured adversarial axis is precisely what
  this program refuses to publish over.

---

## Decision 2 — C26 PARTIAL → PASS

**DECISION:** Promote C26 to PASS, executed as one parent-owned promotion
batch after the #543 CML 6.2.0-2 checkpoint commit lands, gated on a green
re-run of C26's two focused suites on the post-#543 tree; abort (stay
PARTIAL) only if a C26 witness reds under 6.2.0-2.

### Why promotion is correct

Every residual the authoritative first-queue row named is closed by
measurement:

- First-queue row (committed GOAL_PROGRESS.md line 373) named four
  residuals: genuine field-8 unary redeemer maximum; canonical maximum
  signed-byte/digest identity through retained reconstruction; malformed
  focused controls; (implicitly) maximum emulator admission. The 2026-08-04
  narrowing (issue #484, `140f0a83`) closed all but the first; wave 5
  (`e4335bbd`) closed the last one: accepted depth 3,995 at 16,381 signed
  bytes, adjacent 3,996 at 16,385, raw builder pinned byte-identical to the
  production CML path at depth 1, two mirroring Aiken tests, selector 4/4
  under both compilers.
- Evidence verified present on the tree: `buildRawSignedCardanoUnaryRedeemersCandidateV1`
  at demo/midgard-validation/tests/helpers/unary-depth-candidate-v1.ts:305;
  `maximum_unary_redeemer_depth_is_bound_by_signed_cardano_capacity` at
  onchain/aiken/lib/midgard/fraud-proofs/c26-unary-depth-v1.test.ak:150; the
  manifest row's `expectedNonzeroCounts` records the closure and ends
  "Promotion out of PARTIAL belongs to the authoritative [first queue]" —
  the exact call delegated here.
- Root cause A is fixed at source: CML 6.2.0-2 (upstream PR #6, 16 MiB
  stack); the #543 bump is landing today and is treated as landed per the
  delegation.
- Dependency check: C26's `blockedBecause` — "no remaining non-PASS
  dependency; C20-2 and C20-8 are both PASS" — verified against the
  capability artifact (both PASS).

### Direction correction (ledger claim that did not survive verification)

The wave-1 note "(C26 PARTIAL held by C30–C33/CG2)" (committed line 5222)
reads backwards against the manifest graph: C26 appears in the `blockedOn`
lists OF C30, C31, C32, C33 and CG2 (canonical-v1-goal-task-manifest-v1.json
rows), and nothing in C30–C33/CG2 is a precondition of C26. C26 is the
upstream hold; promoting it is what RELEASES them. No condition from
C30–C33/CG2 blocks this promotion.

What C30–C33/CG2 currently require (so the promotion is not oversold):
C30/C31 (strict normal/forced retained-DA reconstruction) remain PARTIAL and
blocked on C29 (PARTIAL) after C26 passes; C32 additionally on C30/C31; CG2
requires all 22 rows PASS (verify-canonical-v1-capability-reconciliation.mjs
:410–432 pins `pass: 17, partial: 5, gate: "OPEN"`). **C26 PASS moves the
matrix to 18/4 and does not close CG2.**

### Measurable promotion conditions (the batch's gate)

1. Post-#543 tree: `pnpm --dir demo/midgard-validation exec vitest run
tests/plutus-data-unary-depth-boundary-v1.test.ts` — 6/6, 0 failures.
2. Post-#543 tree: the 4-test guarded Aiken selector
   (`midgard/fraud_proofs/c26_unary_depth_v1.test`) — 4/4 under stock
   v1.1.22 and the fork (identical).
3. The capability gate re-runs C26's witnesses green with the updated pins
   (it derives PASS from runner reports — #529/#538 discipline — so an
   unverified promotion cannot land).

Rationale for the post-#543 gate: C26's `invalidationTriggers` include "CML
… changes"; the 6.2.0-2 bump fires it. The fix is stack-size, not
serialization, so byte-identity pins are expected to hold — but expected is
not measured, hence the gate. Cost: minutes.

### Exact edit set for the promotion batch (parent lane)

- GOAL_PROGRESS.md first-queue C26 row: PARTIAL → PASS with the measured
  closure facts and the post-#543 re-run transcript.
- canonical-v1-capability-reconciliation-v1.json: `p2Tasks.C26` → "PASS",
  `p2Summary` → `{pass: 18, partial: 4}`.
- verify-canonical-v1-capability-reconciliation.mjs:418–419: literal pin
  `pass: 17, partial: 5` → `18, 4`.
- canonical-v1-goal-task-manifest-v1.json: C26 row prose; remove C26 from
  `blockedOn` of C30/C31/C32/C33/CG2 and refresh their `blockedBecause`;
  correct F05's "exactly 2: C26, F41" claim to "exactly 1: F41" (the #525
  gate reconciles this claim — it must move in the same commit).
- Stale-claim repairs found during this review (fold into the same batch):
  - CG2 row `expectedNonzeroCounts` still claims the verifier asserts
    "exactly 16 PASS, exactly 6 PARTIAL" — the verifier pins 17/5 today and
    will pin 18/4; the claim is stale twice over.
  - C33 is published PASS while its manifest `blockedBecause` still reads
    "C33 remains dependency-blocked … C23, C24, C25, C26" — stale prose
    (C23–C25 are PASS; C33 itself is PASS). Refresh or delete.

### Risks of being wrong

- If 6.2.0-2 subtly changes CML behavior on deep Data, the promotion gate
  catches it (that is what the gate is for); the failure mode is a red
  witness, not a wrong PASS.
- Promoting C26 changes ready-derivation for dependents (the #529 lesson:
  decorated statuses govern dependents); the capability/quality gates
  reconcile this mechanically in CI.

---

## Decision 3 — PR-harvest catalogue registration (deployment-identity change)

**DECISION:** Batch it — execute the deployment-identity change exactly
once, immediately after #545 lands, combining (i) registration of the three
harvested families (Q18 `noReferenceInput`, Q31 `referenceInputNoIndex`,
Q15 `invalidSignature`) as appended catalogue categories, (ii) registration
of the chunked-proof challenge validator in the deployment manifest and
reference-script records, and (iii) recomputation of both hand-pinned
catalogue fixtures against the post-#545 blueprint. Escape hatch: if #545
has not landed by 2026-08-12, execute the three-category registration alone
against the current blueprint and accept a second, smaller identity move
later.

### Why batching wins (and why "now" loses)

The catalogue MPF's per-category leaf value is the category's first-step
spending script hash (demo/midgard-node/src/transactions/initialization.ts:
96–105, `encodeFraudProofCatalogueValue` over `spendingScriptHash`). #545
rewires the step-01 logic of doubleSpend/nonExistentInput/invalidRange/
zeroInput (= Q10/Q11/Q12/Q14), so their step-01 hashes move, so the
catalogue root and every membership proof move — meaning **both hand-pinned
fixtures must be recomputed after #545 regardless of what we do for
Q18/Q31/Q15**. Registering the three categories today would recompute the
root/proofs and move watcher/node identity pins twice within days. The cost
of batching is only that the three families' submit-step builder rows stay
blocked until #545 lands (their prepare suites and SDK modules are already
landed and green per the harvest entry); #545 is in progress today, so the
expected delay is days, bounded by the escape hatch.

Ordering relative to #545: strictly after. The #545 lane does not need the
new categories (its four families are already registered), and its emulator
work uses runtime-computed catalogue fixtures
(demo/midgard-fault-proofs/tests/support/submit-init-emulator-shared.ts
builds the trie at runtime), so nothing in #545 waits on this batch.

### Execution contract — the registration batch must contain

Production (append-only in every ordered list; category IDs are positional —
catalogue.ts:23–25 warns inserting shifts every later ID):

1. demo/midgard-sdk/src/fraud-proof/catalogue.ts:26–35 —
   `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` += `noReferenceInput`,
   `referenceInputNoIndex`, `invalidSignature` (IDs 8, 9, 10).
2. demo/midgard-sdk/src/common.ts:214–230 — `FraudProofs` type += the three
   `SpendingValidator` members.
3. demo/midgard-sdk/src/fraud-proof/contracts.ts — assembly arms for the
   three families' step-01 (the titles already exist post-harvest:
   `NO_REFERENCE_INPUT_FAULT_PROOF_TITLES` :48,
   `REFERENCE_INPUT_NO_IDX_FAULT_PROOF_TITLES` :62,
   `INVALID_SIGNATURE_FAULT_PROOF_TITLES` :74).
4. demo/midgard-core/src/deployment-manifest-identity-v1.ts —
   `DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES` += `fraudProofNoReferenceInput`,
   `fraudProofReferenceInputNoIndex`, `fraudProofInvalidSignature`, plus the
   chunked challenge validator's contract names (spend/mint), and
   `DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` (:83–94)
   += the three categories.
5. demo/midgard-node/src/deployment-manifest-v1.ts:609 area — category →
   contract-name map += 3.
6. demo/midgard-node/src/services/midgard-contracts.ts:710 area —
   `spendingValidatorFromManifest` arms += 3, plus challenge-validator
   resolution.
7. demo/midgard-node/src/services/always-succeeds.ts:257 area — `mkFP` arms
   += 3.
8. demo/midgard-node/src/commands/contract-deployment-info.ts:511 area —
   enumeration += 3.
9. demo/midgard-node/src/transactions/initialization.ts — no edit expected
   (`fraudProofsToIndexedValidators` :76–84 derives from the order
   constant); verify only.
10. demo/midgard-watcher/src/deployment-identity.ts:40 area — the watcher's
    production category → contract map += 3. **This file is a production
    surface the ledger's blocker enumeration did not name — see flags.**

Fixtures (the "two hand-pinned catalogue fixtures", identified exactly):

11. demo/midgard-watcher/tests/canonical-fraud-proof-catalogue.ts — BOTH
    pinned catalogues recomputed to 11 categories: `FIXED_SCRIPT_CATALOGUE`
    (root `774e736e…`, :20–74) and `POSITIONAL_SCRIPT_CATALOGUE` (root
    `6af775fd…`, :75–129), new roots + 11 membership proofs each, against
    the post-#545 blueprint's step-01 hashes.
12. demo/midgard-core/tests/deployment-manifest-identity-v1.test.ts:27 —
    pinned root `774e736e…` + its 8 proofs recomputed to 11.

Explicitly OUT of this batch: demo/midgard-fault-proofs/src/runtime.ts
(`SupportedFaultProofCategoryName`, `FRAUD_PROOF_DEPLOYMENT_ENTRY_BY_CATEGORY`,
`categoryLabel` arms, :410–533) — those arms belong to the three family
rows' own leases once the registered names exist; the registration batch
unblocks them, it does not do their work.

Verification the batch owes (per §4.4 and the harvest entry's lessons):

- Rebuild the SDK dist ("consumers import the built dist" — harvest entry).
- Typecheck/lint across sdk/core/node/watcher/fault-proofs; watcher focused
  suites (aggregate currently 599 — any pin move must be coherent, wave-4
  discipline); node deployment-manifest/fraud-proof-catalogue/contract-info
  suites; core deployment-manifest-identity suite.
- §4.4 journey selector 1/1 on a fresh isolated database.
- No Aiken change is expected in this batch (the three families' validators
  are already in the 391-validator blueprint), so the dual-compiler guard is
  not owed BY this batch — it is owed by #545's push, which precedes it.
- Blueprint-dependent pin currency spot-check rides here per the harvest
  entry's explicit deferral ("a currency spot-check rides with the next
  blueprint-adjacent lane"): the #542 CompletePublished tuple and the Q13
  `evidenceOutputs` blueprint SHA / four applied step hashes flagged in
  wave 6.

### Risks of being wrong

- Batching delays Q18/Q31/Q15 submit-builder work by the #545 gap; bounded
  by the 2026-08-12 escape hatch.
- If #545 unexpectedly does NOT move step-01 hashes (e.g. pure off-chain
  wiring), the batching argument weakens to "avoid two watcher/node pin
  churns", still positive but smaller; the escape hatch covers the schedule
  either way.
- A missed registration surface would fail closed (TS exhaustiveness over
  the category union type plus the fixture verifier's per-category
  requirement — deployment-manifest-identity-v1.test.ts proves missing
  proofs throw), so the failure mode is a red gate, not silent drift.

---

## Flags: ledger claims that did not survive verification

1. **Direction of the C26 hold.** "(C26 PARTIAL held by C30–C33/CG2)"
   (committed GOAL_PROGRESS.md line 5222) is backwards against the manifest
   graph: C26 sits in the `blockedOn` of C30/C31/C32/C33/CG2; nothing holds
   C26. (Decision 2 relies on the graph, not the prose.)
2. **CG2 row stale verifier claim.** The manifest CG2 row's
   `expectedNonzeroCounts` says the reconciliation verifier asserts "exactly
   16 PASS, exactly 6 PARTIAL"; the verifier pins 17/5
   (verify-canonical-v1-capability-reconciliation.mjs:418–419) and the
   artifact says 17/5. Stale claim; fix in the C26 promotion batch.
3. **C33 stale blocked prose.** C33 is published PASS while its manifest
   `blockedBecause` still says it "remains dependency-blocked … C23, C24,
   C25, C26" (all of which except C26 are PASS). Refresh in the same batch.
4. **Registration surface under-enumeration.** The harvest entry's shared-
   blocker text names "SDK catalogue.ts order + FraudProofs, midgard-core
   DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES + V1 category order, node
   services, and the two hand-pinned catalogue fixtures" — verified, but it
   omits demo/midgard-watcher/src/deployment-identity.ts (a production
   category map, :40) and, unless "node services" is read broadly,
   demo/midgard-node/src/deployment-manifest-v1.ts and
   src/commands/contract-deployment-info.ts. The execution contract above
   enumerates all of them.
5. **Verified sound (no flag, recorded for completeness):** the Q1X-F5
   arithmetic is internally consistent — per-family ceilings reproduce from
   the measured margins (e.g. Q12: 4,618 B margin at depth 5, /276 → +16 →
   ceiling 21; log2 work 84 = 4x21), and the two hand-pinned fixture files
   are exactly the two the ledger claimed.

## Next actions and owners

| Action                                                                                            | Owner                                                          | When                                                            |
| ------------------------------------------------------------------------------------------------- | -------------------------------------------------------------- | --------------------------------------------------------------- |
| Ledger the Decision-1 rule (pre-authorized re-decision, F6 requirement, domain-field requirement) | Parent lane                                                    | Next checkpoint                                                 |
| Add `challenged_root_domain` to `ProofChallengeDatum` + consumer equality asserts                 | #545 agent (requirement relayed)                               | Within #545                                                     |
| Exercise Q1X-F6 (Q10/Q11 measurement)                                                             | #545 agent if accepted, else new issue filed by parent         | Before cell re-decision                                         |
| Re-decide the four output-5 cells + rewrite verifier output-5 section                             | The lane landing the last clearing condition, citing this memo | When conditions 1–3 all hold                                    |
| Close #541                                                                                        | Parent                                                         | Q1x verifier exit 0 with 4 LOCAL_PASS cells, §4.4-green tree    |
| C26 promotion batch (edit set above, gated on post-#543 re-runs)                                  | Parent lane                                                    | After #543 checkpoint commit                                    |
| Catalogue registration batch (execution contract above)                                           | Parent lane                                                    | Immediately after #545 lands; alone by 2026-08-12 if #545 slips |
| Q18/Q31/Q15 submit-builder wiring incl. runtime.ts arms                                           | The three family rows' own leases                              | After registration batch                                        |
