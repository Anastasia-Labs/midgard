# C21-STAGE4-GAP — analysis and design (read-only, no builds run)

Analysed at HEAD `c89041f6` + working tree. Nothing in the repo was modified.

## 0. Verdict in one paragraph

The gap is **CONFIRMED**, it is a **SOUNDNESS** break (a dishonest operator
cannot be challenged), and the briefed interval **(14,774, 16,384] is too
narrow**. Three bounds stack, and the briefed one is the *loosest* of the
three. The tightest one is a deployed-carriage bound at roughly **8.3–8.8 KB**,
because the scriptSources stage-4 fold routes to a semantic resolver that has
**no reference/publication carriage at all** — the complete output item must
be embedded inline in *two* separate L1 proof transactions. The briefed
14,774 is a producer-side envelope guard on an abstract evidence CBOR, and it
is additionally a *single-output best case* that shrinks as a transaction's
output count grows.

---

## 1. Where 14,774 comes from — derived, not quoted

### 1.1 The code path that imposes it

`demo/midgard-validation/src/validation-machine-data.ts:1462-1475`

```ts
const evidenceCbor = Buffer.from(
  Data.to(record([transitionData, auxiliaryData]) as never), "hex");
const maximum = MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes;
if (transitionCbor.length >= maximum ||
    auxiliaryCbor.length >= maximum ||
    evidenceCbor.length >= maximum) {
  throw new Error(
    `validation transition ${stateIndex.toString()} exceeds the strict L1 preimage envelope`);
}
```

- `minSupportedL1MaxTxBytes` = `MAX_L1_FAULT_PROOF_TX_BYTES` = `16 * 1024`
  (`demo/midgard-core/src/consensus-profile.ts:48`, `:191`). The comparison
  is `>=`, so the admissible maximum is **16,383** bytes.
- The binding term is `evidenceCbor` (it strictly contains the other two).

So the answer to "is the bound on the fold, the envelope, the witness
encoding, or the successor commitment?" is: **the witness encoding, measured
against the envelope.** It is *not* the successor commitment — the successor
only stores `hash_work_witness(...)` of the control
(`onchain/aiken/lib/midgard/validation-machine-v1.ak:8854-8859`, and the
`script_sources_control_successor_is_exact` call at `:8998-9014`), which is
32 bytes regardless of output size. And it is not the fold's *logic* — the
fold is O(1) per item. It is the **unconditional complete-item reveal** the
fold demands.

### 1.2 The shape of the evidence

`validationOneStepWitnessDataV1` (`validation-machine-data.ts:1107-1114`):

```
transitionData = Constr(0, [ bytes(witness.cbor), validationMachineStateDataV1(successor) ])
```

`validationAuxiliaryWitnessDataV1`, case `transactionFieldItem`
(`validation-machine-data.ts:1356-1360`):

```
auxiliaryData = Constr(30, [ collectionProofData(proof), bytes(item.itemCbor) ])
```

`evidenceCbor = Data.to(Constr(0, [transitionData, auxiliaryData]))`.

`bytes(item.itemCbor)` is the complete serialized output. Everything else in
the evidence is independent of the output's byte length.

### 1.3 The exact size function of the item term

PlutusData byte strings longer than 64 bytes serialize as an indefinite-length
chunked byte string with definite 64-byte chunks. The repository's own encoder
states the rule at `demo/midgard-core/src/plutus-data-cbor.ts:445-483`
(`<= 64` definite; otherwise `0x5f` + `chunkOffset += 64` chunks + `0xff`),
and the profile records that the machine deliberately uses this form
(`demo/midgard-core/src/consensus-profile.ts:29-31`).

For an item of `N` bytes, with `q = floor(N/64)` and `r = N mod 64`:

```
E(N) = 2                       # 0x5f ... 0xff
     + 66 * q                  # each full chunk: 0x58 0x40 + 64 bytes
     + pad(r),   pad(0) = 0,  pad(r) = r + 1 for 1 <= r < 24,  pad(r) = r + 2 for 24 <= r < 64
```

Evaluate at the pinned frontier and one byte above it:

```
N = 14774 -> q = 230 (14,720 bytes), r = 54 -> E = 2 + 15,180 + 56 = 15,238
N = 14775 -> q = 230,                r = 55 -> E = 2 + 15,180 + 57 = 15,239
```

### 1.4 Solving for the constant, and hence for 14,774

Write `evidence(N) = C + E(N)` where `C` is everything except the item term.
The pinned test (`demo/midgard-validation/tests/complete-item-proof-fit.test.ts:311-331`)
bisects and establishes `evidence(14774) <= 16383 < evidence(14775)`.
Because `E(14775) - E(14774) = 1` exactly, this pins `C` to a single value:

```
C + 15,238 <= 16,383  and  C + 15,239 >= 16,384   =>   C = 1,145
```

**So 14,774 is the unique solution of `1,145 + E(N) <= 16,383`, i.e.
`E(N) <= 15,238`.** The 1,145-byte constant is the fold's fixed evidence
overhead:

| component | contents | approx bytes |
| --- | --- | --- |
| outer `Constr(0,[t,a])` | `d879 9f … ff` | 4 |
| `transitionData` framing | `d879 9f … ff` | 4 |
| `bytes(witness.cbor)` | scriptSources stage-4 control (≈30 fields: `compact_cbor`, `witness_set_compact_cbor`, `field_preimage_lengths_cbor`, `context_cbor`, source/redeemer/purpose/output frontiers, replay accumulator — see `validation-machine-v1.ak:8820-8852`) | ≈ 840 |
| `validationMachineStateDataV1` | 6 × 32-byte hashes + phase/pc/verdict (`validation-machine-data.ts:1064-1105`) | ≈ 205 |
| `Constr(30,…)` framing | tag 1303 (`d9 0517`) + `9f`/`ff` | 5 |
| `collectionProofData` | version, field_index, item_count, item_index, item_length, 32-byte commitment, **frontier peaks**, **siblings** (`validation-machine-data.ts:128-140`) | ≈ 88 |
| | **total** | **1,145** |

### 1.5 The number is a single-output best case — this is not recorded anywhere

The fixture builds a transaction with **one** output
(`complete-item-proof-fit.test.ts:272-274`, `buildTraceWithOutputs([…])`),
so `collection_proof.frontier` holds one peak and `collection_proof.siblings`
is empty. `bounded_collection_v1.verify_item`
(`onchain/aiken/lib/midgard/bounded-collection-v1.ak:123-152`) consumes both,
and both grow with the transaction's output count `K`:

- each frontier peak ≈ `d879 9f <height> 5820 <32> ff` ≈ 39 bytes,
- each sibling ≈ `5820 <32>` = 34 bytes,
- peaks ≈ `popcount(K)`, siblings ≈ `floor(log2 K)`.

So `C` grows by roughly 73 bytes per Merkle level and the frontier falls by
the same amount. Rough (unmeasured) consequence:

| outputs in the tx | added proof bytes | approximate stage-4 frontier |
| --- | --- | --- |
| 1 | 0 | 14,774 (pinned) |
| 256 | ≈ 580 | ≈ 14,190 |
| 16,384 (the profile guardrail) | ≈ 1,020 | ≈ 13,750 |

**Therefore "(14,774, 16,384]" is not a fixed interval; 14,774 is its upper
endpoint only for a one-output transaction.** I could not measure the exact
curve without running the fixture.

---

## 2. Three nested bounds — the briefed one is the loosest

### Bound 1 (loosest, on-chain machine): 16,384 — not violated

`onchain/aiken/lib/midgard/validation-machine-v1.ak:8990`
```
collection_proof.item_length <= max_serialized_output_preimage_bytes,
```
with `max_serialized_output_preimage_bytes: Int = 16384` at `:2079`. The
validator itself admits the full range. Good — this is not the constraint.

### Bound 2 (briefed): 14,774 — producer envelope guard

Derived in §1. This is a guard in the *evidence producer*, on an abstract
CBOR preimage, not on any real transaction.

### Bound 3 (tightest, and the one that actually governs): ≈ 8.3–8.8 KB

This is the finding the row does not record.

**(a) Stage 4 dispatches to the "non-output" semantic resolver.**
`demo/midgard-validation/src/validation-machine-data.ts:1014`
(`if (stage !== 5) return 0;`) maps a scriptSources stage-4
`transactionFieldItem` witness to semantic resolver **0**, which is
`onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-non-output-semantic-v1.ak`
→ `verify_script_sources_non_output_semantics_v1`
(`validation-machine-v1.ak:12353`, dispatching stage 4 at `:12378-12379`).
Note: `verify_script_sources_stage_four_semantics_v1` exists at
`validation-machine-v1.ak:12477` but **no validator references it** — it is
dead code; stage 4 really does run through the generic non-output resolver.

**(b) That resolver takes the auxiliary inline in its redeemer.**
`script-sources-non-output-semantic-v1.ak:13-20` —
`VerifyNonOutput { input_index, output_index, transition, auxiliary }`, where
`auxiliary: ValidationAuxiliaryWitnessV1` carries `item_cbor` whole.

**(c) The publication/reference carriage is hardwired to CanonicalDecode
only.** In `demo/midgard-fault-proofs/src/validation-dispute/submit.ts`:

- `encodeValidationSemanticResolutionRedeemerV1:3168-3181` — the reference
  route **throws** unless `resolverIndex === 0 && semanticResolverIndex === 1`;
- `:3219-3226` — everything else falls through to
  `semanticActionFieldsV1`, which for `resolverIndex === 8,
  semanticResolverIndex === 0` returns `[...base, auxiliary]` (auxiliary
  inline, whole);
- `:4034-4040` `isCompleteCanonicalItem` and `:3637-3647`
  `prepareCompleteItemByHash` are gated on the same `resolverIndex === 0 &&
  semanticResolverIndex === 1` pair, so the *prepare* transaction must also
  inline the whole item.

**(d) Consequence.** The stage-4 item must fit **direct carriage**, twice
(prepare tx and semantic-resolution tx), each ≤ 16,384 bytes complete and
signed. The profile's measured direct frontier for the structurally
equivalent complete-item redeemer is
`maxExactDirectCompleteItemBytes: 8_769` / `maxReliableDirectCompleteItemBytes:
8_273` (`demo/midgard-core/src/consensus-profile.ts:92-94`), against a
`maxReliableDirectCompleteItemProofTransactionBytes: 15_872` measurement.

Also note `selectValidationCompleteItemCarriageV1`
(`submit.ts:123-140`) **throws** above
`maxSinglePublicationCompleteItemBytes = 14_396`
(`consensus-profile.ts:55`) — so even the reference route, if it were
wired up, would stop at 14,396, still short of 16,384.

**Practical gap: roughly (8,769, 16,384], not (14,774, 16,384].** I could not
measure the exact stage-4 direct frontier without building a transaction.

---

## 3. Reachability and consequence

### 3.1 Is the range reachable? YES.

- Midgard's own consensus admission rule caps an output preimage at exactly
  16,384 bytes and rejects only *above* it:
  `demo/midgard-core/src/consensus-validation.ts:1087-1092`
  (`E_LEDGER_OUTPUT_SIZE`), with
  `MAX_LEDGER_OUTPUT_PREIMAGE_BYTES = MAX_L1_FAULT_PROOF_TX_BYTES`
  (`consensus-profile.ts:67`).
- No datum-size cap exists in the profile; only `maxOutputValueCborBytes:
  5_000` bounds the *Value* part. A ~15 KB inline datum is admissible, which
  is exactly how the fixture builds one
  (`complete-item-proof-fit.test.ts:94-118`).
- The capability floor decision *requires* this:
  `docs/midgard/decisions/0001-cardano-l1-transaction-capability-floor.md:77`
  records "Serialized output preimage … 16,384 … Equal byte floor", and
  `:116` "Midgard must accept at least 16,384 bytes of canonical transaction
  data".
- The **trace itself builds fine** at 16,384 — the test constructs it and only
  the *argument* throws (`complete-item-proof-fit.test.ts:271-281`). So
  both parties can commit descriptors; the failure is purely at evidence
  production.

### 3.2 Which side loses? The CHALLENGER. Class: SOUNDNESS.

The one-step resolution is a **challenger-only move**:

- `onchain/aiken/lib/midgard/validation-resolution-v1.ak:163-182`
  `prepare_semantic_resolution` requires
  `hash_machine_state(transition.claimed_successor) == state.challenger_successor_hash`;
- `:151-161` `challenger_wins_with_valid_successor` — the operator never
  submits a transition. The doc comment at `:147-150` says so explicitly
  ("revealing the operator's potentially large witness again would only
  consume proof-envelope capacity");
- `demo/midgard-fault-proofs/src/validation-dispute/submit.ts:3622-3626`
  gates the prepare on `inputDatum.fraud_prover === signer.paymentKeyHash`.

And a timeout in that state awards nobody:
`onchain/aiken/lib/midgard/validation-dispute-v1.ak:205-216` —
`ReadyForOneStep -> NeitherClaimValid`, and
`validators/fraud-proofs/validation-trace/game-v1.ak:137-149`
(`EnterChallengerTimeout`) and `timeout-v1.ak:44-78` both require
`timeout_winner(...) == ChallengerWins`, which `ReadyForOneStep` never
returns. So a stalled one-step is simply a **failed fraud proof** and the
challenged block stands.

**The attack.** A dishonest operator:
1. includes in a block a transaction with one output of, say, 12,000 bytes
   (legal per §3.1);
2. commits a validation trace that is honest up to the scriptSources stage-4
   fold step for that output, and *forges* that step (wrong `output_peaks`),
   propagating a wrong terminal state / verdict / ledger-delta root;
3. bisection (`validation-dispute-v1.ak:154-203`) converges deterministically
   on exactly that step — the honest challenger cannot steer it elsewhere,
   because deviating earlier would make the challenger's own step invalid and
   lose under `challenger_transition_is_valid`;
4. the challenger reaches `ReadyForOneStep` and **cannot build the argument**.

Result: an invalid block is finalized. **This is a SOUNDNESS break, not a
liveness break.** An honest operator is never harmed by it: a malicious
challenger who opens a dispute they cannot finish only burns their own
response windows, and the operator's bisection moves are cheap 32-byte state
hashes.

Mechanically the failure is a *completeness* failure of the evidence
producer; in rollup terms an honest challenger who cannot complete the proof
means invalid state finalizes, which this program must treat as soundness.

### 3.3 Correction to an existing artifact

`docs/exec-plans/evidence/necessity/ledger-output-incremental-proof-v1.md:67-76`
records the residual gap as "(14,774, 16,384]". Per §2 above that paragraph
**understates** the gap on two axes (deployed carriage ≈8.8 KB, not 14,774;
and 14,774 is only the one-output endpoint). It should be revised when this
row is closed.

---

## 4. Design options, ranked

### Option A — drop `item_cbor` from the stage-4 fold (RECOMMENDED)

**The claim: `item_cbor` is redundant at stage 4.** The fold's only output is

```
output_peaks := append_leaf(output_count, output_peaks,
                            output_item_leaf_hash(output_count, collection_proof.item_commitment))
```
(`validation-machine-v1.ak:9004-9011`). The triple
`(item_index, item_length, item_commitment)` is *already* uniquely determined
by authenticated data, because
`bounded_collection_v1.verify_item(output_commitment, collection_proof)`
(`bounded-collection-v1.ak:123-152`) hashes exactly that triple into the leaf
and then checks `commitment(field_index, item_count, frontier) ==
output_commitment`, where `output_commitment` comes from
`verify_native_tx_proof_source_v1(pre.transaction_id, …)`
(`validation-machine-v1.ak:8939-8945`). Stage 4 additionally pins
`field_index == 2`, `item_index == control.output_count`, and
`item_count == active_total_count` (`:8987-8989`). There is exactly one triple
that can pass.

The two checks that consume `item_cbor` —
`collection_proof.item_length == bytearray.length(item_cbor)` (`:8991`) and
`bounded_item_v1.from_bytes(...) == collection_proof.item_commitment`
(`:8992-8996`) — therefore only re-prove that the already-authenticated
commitment has a preimage. That is established elsewhere and unconditionally:

- **canonicalDecode** authenticates every output item's bytes against the same
  bounded-item commitment, and *does* have a chunk fallback
  (`validation-machine-v1.ak:1902-1928` admits both
  `TransactionFieldChunkWitness` and `TransactionFieldItemWitness`);
- **scriptSources stage 5** re-opens each output preimage incrementally via
  the `LedgerOutputProof` begin/step/finalize family
  (`script-sources-output-proof-{begin,step,finalize,finish}-semantic-v1.ak`),
  which is exactly the route `ledger-output-incremental-proof-v1.md` already
  justifies.

**What changes:** stage 4's auxiliary becomes a collection proof only
(≈150 bytes, output-size independent); `script_sources_stage_four` drops
`:8991` and `:8992-8996`; the TS producer at
`demo/midgard-validation/src/validation-machine.ts:3802-3809` stops attaching
`itemCbor`.

**Cost:** the frontier disappears entirely — stage-4 evidence becomes O(1) in
output size, and the existing direct-carriage route suffices with large
margin. No new resolver, no new control state, no extra trace steps, no
§3.2 necessity artifact needed (this is not a bounded fallback; it *removes*
carriage, which is what §3.2's "simplest authenticated representation that
fits" ordering wants).

**Risk:** it changes `script_sources_stage_four`, hence the applied validator
hashes and the blueprint sha256, which invalidates **all seven** existing
necessity artifacts under `docs/exec-plans/evidence/necessity/` per
GOAL_SPEC.md:458-461 and triggers the re-measurement cascade at
GOAL_SPEC.md:1085. That is the real price, and it is a scheduling cost, not a
correctness one.

**Evidence needed to justify:** (i) an Aiken test that stage 4 with a forged
`(item_length, item_commitment)` still rejects via `verify_item` alone;
(ii) a test that canonicalDecode + stage 5 jointly cover every byte of every
output for both chunked and complete carriage; (iii) equivalence + hostile
omission/duplication/reorder/substitution/trailing tests on the new shape;
(iv) re-measurement of the seven bound artifacts.

**Residual doubt I could not close without running anything:** whether any
Aiken test currently asserts that stage 4 rejects a *mismatched* `item_cbor`
(such a test would need rewriting, not deleting). I verified no other
validator consumes the stage-4 auxiliary — the only other
`TransactionFieldItemWitness` consumers are
`canonical-decode-item-{semantic,observe}-v1.ak`, both CanonicalDecode-only.

### Option B — chunk/paginate the stage-4 fold

Add a `TransactionFieldChunkWitness` branch to `script_sources_stage_four`,
mirroring `verify_canonical_decode_chunk`, with a per-item chunk cursor and
partial bounded-item frontier carried in `ScriptSourcesControlV1`.

**Cost:** a new semantic resolver validator; new control fields (which widen
*every* scriptSources witness, lowering every other stage's margin slightly);
up to `ceil(N/4095)` = 4 extra trace steps per large output. Bisection depth
is fine (`max_bisection_rounds = 32` ⇒ 2^32 steps,
`validation-dispute-v1.ak:7`). Requires a full §3.2 necessity artifact.

**Risk:** it is the largest change, and per §3.2 it is only admissible if a
simpler representation genuinely cannot fit — which Option A shows is not the
case. Adopting B without first refuting A would violate GOAL_SPEC.md:421-433
and :463-469 ("No bounded fallback may … turn an exceptional large-item
constraint into mandatory complexity for ordinary proofs").

**When B is right:** only if Option A's redundancy claim is refuted, i.e. if
some downstream stage genuinely needs stage 4 to have witnessed the bytes.

### Option B′ — extend reference carriage to resolver 8 (partial, cheap, complementary)

Widen `encodeValidationSemanticResolutionRedeemerV1:3168-3181`,
`isCompleteCanonicalItem:4034-4040`, and `prepareCompleteItemByHash:3637-3647`
to accept `resolverIndex === 8 / semanticResolverIndex === 0` with a
`transactionFieldItem` auxiliary, and add a by-reference redeemer variant to
`script-sources-non-output-semantic-v1.ak` (or a dedicated stage-4 validator
using the already-existing `verify_script_sources_stage_four_semantics_v1`).

**Cost:** small; reuses the deployed `proof_item_v1` publication machinery.
**Effect:** lifts Bound 3 from ≈8.8 KB to `maxSinglePublicationCompleteItemBytes`
= 14,396 — **but not to 16,384.** It closes the *unrecorded* part of the gap
and leaves the briefed part open. Worth doing regardless of A vs B, because it
is a strict improvement and it is where the deployed code is inconsistent with
the CanonicalDecode route. Needs its own §3.2 artifact (representation 2 of
the ordering, so a *light* one).

### Option C — constrain admissible outputs at the ledger layer

Lower `MAX_LEDGER_OUTPUT_PREIMAGE_BYTES` below the provable frontier so the
range is unreachable by construction.

**Blocked.** It directly contradicts
`docs/midgard/decisions/0001-cardano-l1-transaction-capability-floor.md:77`
("Equal byte floor") and `:116`, and GOAL_SPEC.md:145 ("retain and
authenticate maximum Cardano-capable dynamic content"). Rank last among
actionable options; record as refuted rather than deferred.

### Option D — accept and document

This is the status quo (`ledger-output-incremental-proof-v1.md:67-76`).
Not viable: §3 classes this as a soundness break, and a soundness break cannot
be closed by documentation. Retain only as the interim ledger entry while A or
B is implemented, and correct its numbers per §3.3.

### Ranking

1. **A** (drop `item_cbor`) — closes the gap completely, smallest surface,
   no new necessity artifact; price is the artifact re-measurement cascade.
2. **B′** (reference carriage for resolver 8) — do this regardless; cheap,
   fixes the deployed inconsistency, partial.
3. **B** (chunked fold) — correct but heavy; only if A is refuted.
4. **C** — refuted by the capability floor.
5. **D** — not a closure.

---

## 5. Draft §3.2 necessity artifact

Structure matched to `docs/exec-plans/evidence/necessity/script-source-hash-block-v1.md`
and `ledger-output-incremental-proof-v1.md`. **Every cell marked `MEASURE`
must be filled by a real run before this artifact is valid** — GOAL_SPEC.md:447
requires final applied/parameterized validators and GOAL_SPEC.md:439-442
forbids inferring fit from item length. I ran nothing, so I did not invent
numbers.

Intended path: `docs/exec-plans/evidence/necessity/script-sources-output-item-fold-v1.md`

---

```markdown
# §3.2 Necessity artifact — scriptSources output-item fold

## Binding

- Family / item: `script-sources-output-item-fold`
  (`TransactionFieldItemWitness` consumed by `script_sources_stage_four`
  through `verify_script_sources_non_output_semantics_v1`) / one complete
  ledger output item folded into `output_peaks`; maximum shape 16,384 bytes
  (`max_serialized_output_preimage_bytes`,
  `onchain/aiken/lib/midgard/validation-machine-v1.ak:2079`, equal to
  `maxLedgerOutputPreimageBytes`).
- Applied validator hashes measured: MEASURE
  (`script_sources_non_output_semantic_v1` applied on the measurement
  deployment), MEASURE (`proof_item_v1`); blueprint sha256 MEASURE.
  Any change invalidates this artifact (GOAL_SPEC.md §3.2).
- Parameter snapshot digests: consensus profile digest MEASURE; capability
  floor per
  `docs/midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`.
- Fixture: exact-size canonical output items generated in
  `demo/midgard-validation/tests/complete-item-proof-fit.test.ts`
  (deterministically regenerable), swept over output counts
  K ∈ {1, 2, 16, 256, 16384} because the collection proof grows with K.

## Measurements (§3.2 order — stop at the first representation that fits)

| Representation | Tx bytes / maxTxSize | Mem / limit·0.8 | CPU / limit·0.8 | Fee | Fits §3.3? |
| --- | --- | --- | --- | --- | --- |
| 1. Complete output item direct in the `VerifyNonOutput` prepare and semantic proof transactions | MEASURE — this is the currently deployed route (`submit.ts:3219-3226`, `semanticActionFieldsV1` resolver 8 / semantic 0 returns `[...base, auxiliary]`); the comparable canonical-decode direct frontier measures 8,769 exact / 8,273 reliable at a 15,872-byte proof transaction | MEASURE | MEASURE | MEASURE | NO above MEASURE bytes |
| 2. Complete output item as inline-datum publication + reference consumption | not deployed for resolver 8: `encodeValidationSemanticResolutionRedeemerV1` rejects the reference route unless `resolverIndex === 0 && semanticResolverIndex === 1` (`submit.ts:3168-3181`), and `isCompleteCanonicalItem` (`:4034-4040`) / `prepareCompleteItemByHash` (`:3637-3647`) are gated identically. Wiring it lifts the frontier to `maxSinglePublicationCompleteItemBytes` = 14,396 (`selectValidationCompleteItemCarriageV1`, `submit.ts:123-140`) | MEASURE | MEASURE | MEASURE | NO above 14,396 bytes |
| 3. Minimum multi-output publication + complete logical reconstruction | reconstruction would still have to recompute `bounded_item_v1.from_bytes` over every byte in one transition to re-derive `collection_proof.item_commitment`; it removes no step relative to 4 | — | — | — | superseded by 4 |
| 4. No item reveal at stage 4 — fold the already-authenticated `item_commitment` only | auxiliary becomes the collection proof alone (output-size independent, ≈150 bytes); byte authentication remains where it already occurs: canonicalDecode chunk/complete (`validation-machine-v1.ak:1902-1928`) and the stage-5 `LedgerOutputProof` traversal (`ledger-output-incremental-proof-v1.md`) | MEASURE | MEASURE | MEASURE | YES |

## Exact limiting constraint

Two constraints stack, and the outer one is not the item bound.

Byte fit: the stage-4 auxiliary carries the complete output as chunked
PlutusData bytes. For an item of N bytes the encoded term is
`E(N) = 2 + 66·floor(N/64) + pad(N mod 64)`. With the fold's fixed evidence
overhead C, `buildValidationOneStepArgumentV1`
(`demo/midgard-validation/src/validation-machine-data.ts:1466-1475`) rejects
once `C + E(N) >= minSupportedL1MaxTxBytes = 16,384`. For the one-output
fixture C = 1,145, giving the pinned frontier N = 14,774
(`complete-item-proof-fit.test.ts:331`); C grows by ≈73 bytes per Merkle
level of the outputs collection, so the frontier falls as the transaction's
output count rises.

Carriage fit: the deployed route has no publication/reference variant, so the
complete item must be inlined in both the prepare and the semantic-resolution
transaction, bounding it far below 14,774.

Dispute consequence: the one-step resolution is a challenger-only move
(`onchain/aiken/lib/midgard/validation-resolution-v1.ak:151-182`) and a
timeout at `ReadyForOneStep` awards nobody
(`validation-dispute-v1.ak:205-216`), so an unbuildable stage-4 argument
finalizes the challenged block.

## Why no simpler authenticated representation closes the gap

`collection_proof.item_commitment` and `collection_proof.item_length` are
already bound into the authenticated `outputs_hash` by
`bounded_collection_v1.verify_item`
(`onchain/aiken/lib/midgard/bounded-collection-v1.ak:123-152`), and stage 4
pins `field_index`, `item_index`, and `item_count`
(`validation-machine-v1.ak:8987-8989`). The fold's successor is therefore a
deterministic function of authenticated data with or without the item bytes.
Revealing the bytes re-proves only that the commitment has a preimage — which
canonicalDecode and the stage-5 traversal already establish for every output,
in bounded form. No representation that still reveals the bytes at stage 4 can
fit the envelope for the maximum shape; the representation that stops
revealing them fits trivially and is strictly simpler than any chunked
fallback.

## Preserved complete-item path

Every output continues to be carried as a complete item wherever it fits:
the canonical-decode producer emits `TransactionFieldItemWitness` at or below
`maxSinglePublicationCompleteItemBytes` (14,396) with its guard pinned by
`complete-item-carriage-policy.test.ts`, and the stage-5 output traversal
retains its complete-item path per
`docs/exec-plans/evidence/necessity/ledger-output-incremental-proof-v1.md`.
Both bind the same bounded-item commitment; equivalence and
omission/duplication/reorder/substitution/trailing rejection are exercised by
`demo/midgard-validation/tests/complete-item-equivalence.test.ts` and
`demo/midgard-validation/tests/complete-item-proof-fit-emulator.test.ts`.
```

---

## 6. What I could NOT determine without running anything

1. **The exact deployed stage-4 direct frontier.** I established that resolver
   8 / semantic 0 has direct-only carriage and that it is therefore bounded by
   a complete signed proof transaction ≤ 16,384. The nearest measured analogue
   is 8,769 exact / 8,273 reliable, but the `VerifyNonOutput` redeemer shape
   differs from `canonical_decode_item_semantic_v1`'s flat `Verify` shape by a
   few bytes of tagging, and the reference-script input set may differ.
   Requires building the actual transaction.
2. **The frontier-vs-output-count curve.** §1.5's table is arithmetic
   estimation from the encoder shapes, not measurement. The exact
   `popcount`/sibling counts per K come from
   `validation_merkle_v1.verify_membership` and need a run.
3. **Execution units.** Every Mem/CPU cell in the artifact is `MEASURE`. In
   particular I did not check whether `bounded_item_v1.from_bytes` over a
   ~15 KB item would itself breach the 0.8·limit ceilings — if it does, that
   is an *additional independent* reason the current shape cannot work, and it
   would strengthen Option A further.
4. **Whether any existing Aiken test pins stage-4 rejection of a mismatched
   `item_cbor`.** I confirmed no other *validator* consumes the stage-4
   auxiliary, but I did not enumerate `validation-machine-v1.test.ak`'s
   assertions (the file is large and I was asked not to run selectors).
5. **Whether `verify_script_sources_stage_four_semantics_v1`
   (`validation-machine-v1.ak:12477`) is intentionally reserved for a planned
   dedicated stage-4 validator** or is simply dead. Its existence suggests the
   split was once intended; that is a design signal, not a fact I verified
   against history.
