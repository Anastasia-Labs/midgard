# Canonical V1 validation-machine defect decision memo

Agent K. Read-only investigation. Repo `/home/gumbo/midgard-hub/midgard`,
branch `colll78/canonical-v1-watcher-l1-source-checkpoint`.
Nothing in the repo was modified. No `aiken` invocation was made.

Authority order applied: `GOAL_SPEC.md:1-40` §1 — protocol semantics are
governed by `technical-spec/` and the accepted decision/plan documents, with
current source as *implementation evidence only*; "source cannot silently
narrow required protocol capability" (`GOAL_SPEC.md` §1, conflict handling).

---

## Executive summary

| | Defect 1 (signatures handoff) | Defect 2 (rejection vs. delta root) |
|---|---|---|
| Severity | Completeness/liveness: phase-A native scripts unreachable | **Soundness: the entire one-step rejection surface is unprovable against an adversarial operator** |
| Was the authority decisive? | Yes (self-evident from source semantics + TS reference parity). Spec is silent on the sentinel, and does not need to speak. | Yes — `docs/consensus-profile-v1.md:268-271` is decisive on *where* the emptiness obligation lives. `technical-spec/` is silent on `ledger_delta_root` itself and says so explicitly (see §2.1). |
| Recommendation | `validation-machine-v1.ak:3128`: `0,` → `-1,` | Delete `validation-machine-v1.ak:2052`; re-home the obligation as a trace-endpoint clause in `validation-claim-v1.ak` |
| Confidence | **Very high (~99%)** | **High (~90%)** on removing line 2052; **medium-high (~80%)** on the exact form of the claim-level replacement clause — that part is an owner call. |
| Owner decision required? | No | Only on the optional/defensive replacement clause and on the doc-status downgrades |

Both defects are in the *deployed* machine and both are already recorded as
deliberate RED tests with in-file annotations forbidding the workaround
(`validation-machine-v1.test.ak:2865-2870` and `:2511-2519`). Those annotations
are consistent with my independent findings.

---

# DEFECT 1 — Signatures→PhaseANativeScripts handoff emits an unusable successor

## 1.1 What the source semantics say `result` means

`result` is an explicit **tri-state with a "not yet evaluated" sentinel**, and
that is unambiguous from three independent places in the deployed code:

1. **Domain constraint.** `onchain/aiken/lib/midgard/validation-machine-v1.ak:616-617`
   (inside `encode_phase_a_native_scripts_scan_witness`):
   ```
   expect result >= -1
   expect result <= 1
   ```
   and the same bounds in the binder, `:3379-3380`:
   ```
   control.result >= -1,
   control.result <= 1,
   ```

2. **Stage-conditioned meaning.** `validation-machine-v1.ak:3381-3391` (stage 0
   branch of `phase_a_native_control_is_bound`) — every "nothing has been read
   yet" field is pinned to its empty value and `result` is pinned to `-1`:
   ```
   if control.stage == 0 {
     and {
       !is_late_continuation,
       control.item_length == 0,
       control.item_commitment == #"",
       control.cursor == 0,
       control.stack_root == #"",
       control.stack_depth == 0,
       control.node_count == 0,
       control.result == -1,
     }
   } else { ... }
   ```
   and `:3406-3410`:
   ```
   if control.stage == 2 { control.result >= 0 } else { control.result == -1 },
   ```
   i.e. **only stage 2 (script evaluation complete) may carry a real verdict**;
   every other stage must carry the sentinel.

3. **Where a real verdict is produced and consumed.**
   Produced at `:3913-3917`, `:3974`, `:4388`, `:4677`, `:4901`, `:4997`,
   `:5084`, `:5145` — all of the form `result: if valid { 1 } else { 0 }`, all
   at `stage: 2`. Consumed at `:4013`:
   ```
   } else if control.result == 0 {
     rejected_successor_is_exact(pre, witness.claimed_successor,
       reject_native_script_invalid)
   ```
   So `0` at a completed stage means *"this native script evaluated false →
   reject the transaction"*. Emitting `0` at stage 0 therefore does not merely
   fail a well-formedness check — it asserts a verdict the machine has not
   computed. `-1` is the sentinel; `0` is a real "false" verdict.

4. **Canonical reset.** `reset_phase_a_native_control` (`:3509-3521`) sets
   `stage: 0 … result: -1`. The stage-1 emitter at `:13098` also emits `-1`.

## 1.2 Cross-language authority (decisive parity evidence)

The TypeScript reference builder — the parity partner named by
`GOAL_SPEC.md` §3 invariant 8 ("Canonical encoding is exact.
TypeScript/Aiken encoders … agree") — builds the *same* initial control with
the sentinel:

`demo/midgard-validation/src/validation-machine.ts:1918-1933`
(`resetPhaseANativeScriptsScanControl`):
```ts
stage: 0,
… itemLength: 0, itemCommitment: Buffer.alloc(0), cursor: 0,
stackRoot: Buffer.alloc(0), stackDepth: 0, nodeCount: 0,
result: -1,
```
and `:1934-1940` `initialPhaseANativeScriptsScanControl = reset…({
scriptCount: scriptWitnessesCollection.items.length === 0 ? 0 : -1, … })`.
The type is declared `readonly result: -1 | 0 | 1` at `:1892`.

That `scriptCount` expression matches Aiken `:3097-3103` exactly; `result` is
the **only** field on which the Aiken handoff diverges from the TS reference.
This is an Aiken-side transcription defect, not a semantic disagreement.

## 1.3 Does the normative spec speak to a "not yet evaluated" sentinel?

**No — and it does not need to.** A full sweep of `technical-spec/` found no
mention of `ledger_delta_root`, of the phase-A native scan control, or of any
per-instruction control-field encoding; the spec describes the *phases*
(`technical-spec/7-phase-two-validation/3-fraud-proofs-involved.tex:8-14`:
"The trace covers inline and reference source resolution, redeemer-purpose
binding, script-integrity binding, native-script evaluation, CEK execution
…") but delegates the work-witness encoding to the profile/implementation.
The nearest normative constraint is `GOAL_SPEC.md` §3 invariant 8 (exact
TS/Aiken agreement), which §1.2 settles.

I record explicitly: **the technical specification is silent on the phase-A
control encoding and on a not-yet-evaluated sentinel.** The resolution rests
on internal source semantics plus invariant-8 cross-language parity, both of
which point the same way with no residual ambiguity.

## 1.4 Recommended resolution

**Fix line 3128 to emit `-1`. Do not relax the checker.**

Relaxing `phase_a_native_control_is_bound` to accept `0` at stage 0 would (a)
diverge from the TS reference (invariant 8), (b) make two encodings of the
identical "nothing evaluated yet" control both bindable — a malleability that
lets an operator and a challenger commit different `work_root`s for the same
semantic state, manufacturing an artificial dispute boundary, and (c) collide
with the `result == 0 ⇒ reject` reading at `:4013`.

### Precise change

File: `onchain/aiken/lib/midgard/validation-machine-v1.ak`
Function: `verify_signatures_handoff` (`:3086-3136`)
Line: **3128** (the 16th positional argument, `result`, of
`encode_phase_a_native_scripts_scan_witness`)

Before:
```
3126:         0,
3127:         0,
3128:         0,
3129:         control.signer_count,
```
After:
```
3126:         0,
3127:         0,
3128:         -1,
3129:         control.signer_count,
```

No other line changes. Recommended (non-functional) accompaniment: add a
trailing comment naming the argument, since the call site is 19 bare
positional arguments and this defect is a direct consequence of that shape.

### Tests that must accompany it

1. **Positive, already written and currently RED — will turn green:**
   `onchain/aiken/lib/midgard/validation-machine-v1.test.ak:2792`
   `test signatures_accepts_an_empty_required_signer_and_witness_set()`.
   Its expected successor already pins `-1` at `:2870` with a comment
   (`:2865-2869`) forbidding the reverse "fix". Leave that comment's substance
   in place, updated to past tense.

2. **Negative control — mutation (GOAL_SPEC §3 invariant 9).** New test: the
   same handoff with the successor's `work_root` built over a stage-0 control
   carrying `result: 0` (and separately `result: 1`) must be rejected by
   `verify_signatures_handoff_semantics_v1`. This is the control that pins the
   sentinel and prevents the checker-relaxation regression.

3. **Negative control — continuation.** New test: a two-step chain proving the
   handoff successor is *usable* — take the `post` produced by the handoff and
   feed it as `pre` to the first phase-A native step, asserting it verifies.
   This is what the current defect actually breaks and what a `result`-only
   unit assertion would not catch.

4. **Cross-language parity.** Add the Signatures→PhaseANativeScripts handoff
   boundary to `onchain/aiken/lib/midgard/validation-one-step-cross-language.test.ak`.
   The defect survived because that boundary is not in the cross-language
   vector set; without this the same class of divergence recurs.

### Residual risk

Low. Risk is that other bare-positional call sites of
`encode_phase_a_native_scripts_scan_witness` carry the same class of
transcription error. I checked the two `result`-bearing emitters that the task
named (`:3520` reset, `:13098` stage-1) and both are correct; a full audit of
all call sites of the 19-argument encoder against the TS builder is
recommended as a follow-up but is not a blocker for this fix.

---

# DEFECT 2 — No rejection provable for a non-empty claimed ledger delta

## 2.1 What `ledger_delta_root` actually denotes — question (a)

**It is the operator's CLAIMED delta commitment for the transaction under
adjudication, immutable across the whole trace. It is never an accumulator.**

This is established by the complete production usage — the field appears in
exactly **eight** non-test lines in the whole `onchain/` tree:

| Site | Role |
|---|---|
| `validation-trace-v1.ak:77` | field of `ValidationMachineStateV1` |
| `validation-trace-v1.ak:163` | well-formedness: length 32 |
| `validation-trace-v1.ak:215` | committed into `encode_machine_state` → `hash_machine_state` → trace root |
| `validation-machine-v1.ak:386` | **immutable across every one-step transition** |
| `validation-claim-v1.ak:143` | **immutable across initial→terminal at the claim layer** |
| `validation-machine-v1.ak:17126` | *read* — each delta operation must be a member of `pre.ledger_delta_root` |
| `validation-machine-v1.ak:17560` | *read* — accepting terminal: reconstructed operation frontier must equal `pre.ledger_delta_root` |
| `validation-machine-v1.ak:2052` | **the defect** — rejecting terminal must *write* the empty commitment |

`validation-machine-v1.ak:17557-17560` (`ledger_delta_stage_three`) is the
decisive read:
```
validation_merkle_v1.frontier_commitment(
  control.operation_count,
  control.operation_peaks,
) == pre.ledger_delta_root,
```
The machine independently reconstructs the operation frontier in its work
control and *compares it against the pre-committed claim*. A value that is
checked-against is by construction a claim, not an accumulator. Line 2052 is
the sole site that treats it as writable, and it is the sole contradiction.

**Cross-language confirmation.** `demo/midgard-validation/src/validation-machine.ts`
computes the root **once**, outside the per-state loop
(`:1473-1475` `const ledgerDeltaRoot = hashMidgardValidationLedgerDeltaV1(
authenticatedLedgerOps)`), and stamps the same constant onto **every** state
including the terminal (`:7479` `ledgerDeltaRoot,` inside the state map). The
TS reference has no clearing step at all. Immutability is the reference
semantics.

**Vestigial-encoding evidence that line 2052 is accumulator-era residue.**
`validation_trace_v1.hash_ledger_delta` (`validation-trace-v1.ak:251-255`) —
a `blake2b_256` over a raw CBOR delta blob under a distinct domain — is used
by **zero** production lines and only by test fixtures. Production has moved
to `validation_merkle_v1.frontier_commitment` over operation leaves
(`validation-merkle-v1.ak:88-94`; TS `demo/midgard-core/src/validation-trace.ts:539-544`
confirms `hashMidgardValidationLedgerDeltaV1 ≡ commitMidgardValidationMerkleFrontierV1`,
so the two languages agree that `[] ↦ frontier_commitment(0,[])`). Line 2052
is the last surviving instruction from a design in which the machine built the
delta as it ran.

### What the normative technical specification says — and where it is silent

The spec **never defines a ledger delta as a data structure**. There are
exactly four occurrences of the word "delta" in the entire `technical-spec/`
tree, all informal prose; there are **zero** occurrences of `ledger_delta`,
`delta_root`, `utxo_delta`, or any equivalent field name.

The strongest and most relevant normative statement is the *opposite* of a
claimed-delta commitment, at the transition-step layer:

`technical-spec/1-ledger-state/1-block.tex:232-245`:
```latex
\T{TransitionStep} \coloneq \{ schema\_version, step\_index, event\_key,
    phase, pre\_utxos\_root, post\_utxos\_root \}
…
The base transition step does not include a claimed effect, read-set hash,
consumed-set hash, produced-set hash, or local-result commitment.
Given a source event, the corresponding pre-state root, and the corresponding
post-state root, a phase-specific one-step verifier can recompute whether that
event transforms the pre-state into the claimed post-state.
```

So: at the *block* layer the spec normatively forbids a claimed-effect
commitment and requires recomputation from pre/post roots.
`ledger_delta_root` lives one layer below (inside the validation machine
state) and the spec is **silent** about it. I record that silence explicitly
rather than inferring a normative rule.

## 2.2 What the protocol requires of a rejected transaction's delta — question (b)

The technical specification **is** decisive on the *obligation*, and the
accepted profile document **is** decisive on *where the obligation lives*.

### The obligation (technical-spec, normative)

`technical-spec/5-ledger-rules/2-custom-midgard-ledger-rules.tex:14-21`:
```latex
Every normal canonical V1 L2 transaction has the canonical \code{TxIsValid} tag.
A forced transaction carries the operator's claimed validity, which is checked
by the same deterministic validation machine used for normal transactions.
A valid forced transaction applies the accepted ledger delta; an invalid
forced transaction is an exact no-op. Either misclassification is
challengeable on L1.
```

`technical-spec/1-ledger-state/5-transaction-order-event.tex:63-73`:
```latex
The canonical V1 validation trace uses the same machine for normal and forced
transactions. A valid forced transaction therefore applies the exact accepted
ledger delta, while an invalid forced transaction commits an exact no-op.
An operator verdict that differs from the L1-computed terminal verdict is a
fault in either direction.

Canonical V1 contains no valid-forced rejection gate. The accepted-effectful
and rejected-no-op paths are both part of the canonical validation machine and
are bound to the forced source, terminal verdict, and resulting ledger root.
```

`technical-spec/1-ledger-state/6-transaction.tex:22-23` (normal-source rule):
```latex
Only accepted normal L2 transaction requests may appear in \code{transactions\_root}.
Invalid normal L2 transaction requests are excluded before commitment; they do
not become no-op source events in the transition trace.
```

So the obligation is **"an invalid transaction is an exact no-op"** — stated
about the *transition/effect*, never about a machine-state field.

### The placement (accepted decision doc — DECISIVE)

`docs/consensus-profile-v1.md:268-271` (§8, Transition binding):
```
An accepting terminal state derives the exact ordered delete/insert ledger
operations. The transition-trace one-step proof checks those operations
against the prior root. A rejecting terminal state derives no operations and
requires `pre_utxos_root == post_utxos_root`.
```

This is the authority that answers (b). A rejecting terminal **"derives no
operations"** — a property of what the terminal *emits*, not a mutation of
carried state — **and requires `pre_utxos_root == post_utxos_root`**, i.e. the
emptiness obligation is discharged **at the transition-binding layer**.

That is **option (ii)** in the task framing, and it is *already implemented
and already correct* in two places:

1. `onchain/aiken/lib/midgard/validation-machine-v1.ak:1111-1120` —
   `encode_terminal_rejection_witness` already commits the no-op directly:
   ```
   #"84"
     |> concat(cbor.serialise(2))              // tag 2 = rejection
     |> concat(encode_definite_bytes(rejection_code))
     |> concat(encode_definite_bytes(prior_ledger_root))  // post root = prior root
     |> concat(encode_definite_bytes(#"80"))              // EMPTY operation list
   ```
   The rejecting terminal's own work witness already says "post root = prior
   root, zero operations". Line 2052 is **redundant** with this.

2. `onchain/aiken/lib/midgard/validation-claim-v1.ak:396-400`:
   ```
   if descriptor.verdict == validation_trace_v1.Rejected {
     transition_step.pre_utxos_root == transition_step.post_utxos_root
   } else { True }
   ```
   This is verbatim the profile's `pre_utxos_root == post_utxos_root`
   requirement, already enforced at the claim layer.

3. The unilateral no-op faults exist and are independent of the machine:
   `onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak:1029-1051`
   (`validate_invalid_forced_transaction_no_op_transition`, asserting
   `pre_utxos_root != post_utxos_root` as the fault) and `:1005-1027` for
   withdrawals.

**Conclusion on (b): the answer is (ii), and the normative authority is
`docs/consensus-profile-v1.md:268-271`. The technical specification is silent
on any machine-state field but consistent with (ii); the profile document is
decisive.** Option (i) — "a non-empty claimed delta is itself the provable
fault" — is *not* supported by any authority and is analysed and rejected in
§2.3.

## 2.3 Soundness analysis of the candidate fixes — question (c)

First, the exploit, because it determines which properties matter.

### The exploit under the current code

The machine is the **challenger's** oracle. `validation-resolver-v1.ak:203-266`
finalises a dispute through
`validation_resolution_v1.challenger_wins_with_valid_successor(state,
challenger_evidence.transition.claimed_successor, verify_one_step(state.pre_state,
challenger_evidence))` — the challenger wins **only** by exhibiting a
one-step-valid successor from the agreed pre-state
(`validation-resolution-v1.ak:151-160`).

The agreed pre-state is drawn from the **operator's committed trace**, whose
`ledger_delta_root` the operator chooses freely (nothing binds it on a
non-accepting path). Now:

- For a **normal** L2 source, `validation-claim-v1.ak:288-296`
  (`source_binding_is_exact`) forces `descriptor.verdict == Accepted`. A
  dishonest operator including an invalid transaction therefore commits an
  `Accepted` descriptor with a real, **non-empty** claimed delta root.
- The true successor at the first failing instruction is a rejecting terminal.
- `rejected_successor_is_exact` (`:2052`) demands
  `post.ledger_delta_root == frontier_commitment(0, [])`;
  `immutable_context_matches` (`:386`), reached unconditionally via
  `structural_transition_is_valid` (`:485`) which `verify_one_step` applies,
  demands `pre.ledger_delta_root == post.ledger_delta_root`.
- Jointly unsatisfiable. **The challenger cannot produce any winning
  successor. The operator wins the dispute by default.**

This is a soundness break, not a liveness inconvenience, and it is gated
entirely on a field the *attacker* chooses. It affects **all 80 call sites**
of `rejected_successor_is_exact` in `validation-machine-v1.ak` — i.e. every
rejection code in every phase (`CanonicalDecode` through `LedgerDelta`).

Note the reason it was not caught: an *honest self-consistent* rejected trace
(TS-built, forced-invalid source) carries `ledgerDeltaRoot =
hashMidgardValidationLedgerDeltaV1([]) = frontier_commitment(0,[])` on **every**
state, so pre and post agree and line 2052 is satisfiable. The defect bites
**only** in the adversarial case — which is the machine's entire purpose.

### Fix A — exempt `ledger_delta_root` from `immutable_context_matches` on Terminal/Rejected successors

*Dishonest-operator escape:* it does close the exploit above.

*Valid-block false rejection (§3 invariant 9):* **it creates a new one.**
`validation-claim-v1.ak:129-144` (`immutable_context_matches` at the claim
layer, line `:143`) requires `initial.ledger_delta_root ==
terminal.ledger_delta_root` on *every* committed claim, and
`committed_claim_endpoints_and_source_are_valid:394` calls it unconditionally.
An honest operator's forced-invalid rejected trace built by the TS reference
carries the *same* root on both endpoints (`validation-machine.ts:7479`). Under
Fix A the machine would require a *cleared* terminal that the claim layer then
rejects — an honest, correct block becomes unclaimable. Fix A therefore
requires editing `validation-claim-v1.ak:143` too, weakening the trace-endpoint
binding, and diverging from the TS reference builder in violation of §3
invariant 8.

*Normal/forced symmetry (§3 invariant 7):* preserved in the machine, but the
claim layer breaks identically for both, so the asymmetry is between layers,
not between sources.

*Design cost:* it destroys the "immutable context" concept — `ledger_delta_root`
is grouped with `transaction_id`, `transaction_commitment`,
`validation_context_hash`, `source_kind`, `prior_ledger_root` precisely
because it is fixed input. **Reject Fix A.**

### Fix B — remove the clearing requirement from `rejected_successor_is_exact`

*Dishonest-operator escape:* **no new escape.** After removal, nothing on the
rejecting path reads `ledger_delta_root`, and nothing anywhere else in the
codebase reads it (the eight-site table in §2.1 is exhaustive). A bogus
non-empty claimed delta on a rejected transaction has **no downstream effect**
because the ledger does not move: `validation-claim-v1.ak:396-400` forces
`pre_utxos_root == post_utxos_root`, the rejection work witness itself commits
`post = prior_ledger_root` with an empty operation list
(`validation-machine-v1.ak:1111-1120`), and `proof.ak:1029-1051` makes any
actual root movement a unilateral fault. The accepting path's binding
(`:17557-17560`) is untouched.

*Valid-block false rejection:* none. Honest accepted traces never reach the
rejection rule. Honest rejected traces satisfy the (now weaker) rule
trivially. No honest block becomes challengeable.

*Normal/forced symmetry (§3 invariant 7):* preserved exactly — the rule is
source-kind-blind before and after, and the profile's rejecting-terminal rule
(`consensus-profile-v1.md:268-271`) is likewise source-blind. Critically, Fix B
*restores* symmetry: today the forced-invalid path works (honest empty root)
while the normal-invalid challenge path does not, which is precisely the
asymmetry invariant 7 forbids.

*Amended invariants 3-4:* Fix B adds no interactive machinery and removes no
non-interactive path. The unilateral no-op faults at `proof.ak:1005-1051`
remain the non-interactive route for a ledger-movement violation, as invariant
3 requires.

**Recommend Fix B.**

### Fix C (option (i) in the task framing) — compare claimed-vs-empty at the rejection terminal

I.e. require `pre.ledger_delta_root == frontier_commitment(0, [])` and treat a
non-empty claimed delta on a rejected transaction as the fault.

**This does not fix the defect.** The predicate is still unsatisfiable from
exactly the adversarial pre-states that matter — the operator sets the field
non-empty and the challenger still cannot construct a winning successor. It
reproduces the same soundness hole in a different syntactic position.

To make Fix C work one would have to add a *new* non-interactive fault family
("claimed delta non-empty on a rejected transaction"). GOAL_SPEC §3 invariant
3 permits a new family only where a violation is provable from retained
evidence and the family is necessary; here the violation is *inert* — the
ledger cannot move — so the family would carry no fund-safety or
state-correction content. It is exactly the kind of unnecessary surface
invariants 3-4 exclude. **Reject Fix C.**

### Recommended precise change

**Primary (required).**
File: `onchain/aiken/lib/midgard/validation-machine-v1.ak`
Function: `rejected_successor_is_exact` (`:2041-2059`)

Before (`:2046-2058`):
```
  and {
    post.phase == validation_trace_v1.Terminal,
    post.verdict == validation_trace_v1.Rejected,
    post.rejection_code_hash == validation_trace_v1.hash_rejection_code(
      rejection_code,
    ),
    post.ledger_delta_root == validation_merkle_v1.frontier_commitment(0, []),
    post.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      pre.program_counter + 1,
      encode_terminal_rejection_witness(rejection_code, pre.prior_ledger_root),
    ),
  }
```
After — **delete line 2052 only**:
```
  and {
    post.phase == validation_trace_v1.Terminal,
    post.verdict == validation_trace_v1.Rejected,
    post.rejection_code_hash == validation_trace_v1.hash_rejection_code(
      rejection_code,
    ),
    post.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      pre.program_counter + 1,
      encode_terminal_rejection_witness(rejection_code, pre.prior_ledger_root),
    ),
  }
```
Add a comment recording *why* the clause is absent, so it is not
"restored" by a future reader: the no-op obligation is discharged by
`encode_terminal_rejection_witness`'s empty operation list, by
`validation-claim-v1.ak`'s `pre_utxos_root == post_utxos_root` clause, and by
the unilateral no-op faults in `proof.ak` — per
`docs/consensus-profile-v1.md:268-271`.

If `validation_merkle_v1` becomes unused in that scope, do **not** remove the
import blindly — it is used at `:17123`, `:17557`, so no import change is
needed.

**Secondary (recommended, owner's call).**
File: `onchain/aiken/lib/midgard/validation-claim-v1.ak`
Function: `committed_claim_endpoints_and_source_are_valid`, existing clause at
`:396-400`.

Before:
```
    if descriptor.verdict == validation_trace_v1.Rejected {
      transition_step.pre_utxos_root == transition_step.post_utxos_root
    } else {
      True
    },
```
After:
```
    if descriptor.verdict == validation_trace_v1.Rejected {
      and {
        transition_step.pre_utxos_root == transition_step.post_utxos_root,
        terminal.ledger_delta_root == validation_merkle_v1.frontier_commitment(0, []),
      }
    } else {
      True
    },
```
(`terminal.ledger_delta_root == initial.ledger_delta_root` already holds via
`:143`, so either endpoint may be used.)

Rationale, and why it is a *choice* rather than a requirement: the clause is
not load-bearing for soundness (§2.3, Fix B analysis — the value is inert).
It is load-bearing for **§3 invariant 8 (exact TS/Aiken agreement)**: the TS
builder will never produce a rejected trace with a non-empty delta root
(`validation-machine.ts:1455-1461` fails with "a rejected transaction must
commit an exact ledger no-op" when `ledgerOps.length !== 0`), so without this
clause the Aiken-accepted set is strictly larger than the TS-producible set at
the trace-endpoint level. I recommend adding it; the alternative — leaving the
Aiken side permissive and noting the divergence — is defensible but weakens
the parity claim the Goal rests on.

Note the correct placement: this is a **trace-endpoint** obligation, checked
once on the committed claim, **not** a per-step obligation. That is exactly the
distinction the defect got wrong.

### Tests that must accompany it

*Positive:*

1. `onchain/aiken/lib/midgard/validation-machine-v1.test.ak:2466`
   `test static_rules_prove_a_network_mismatch_is_an_exact_no_op()` — already
   written, currently RED, with the workaround explicitly forbidden at
   `:2511-2519`. Turns green. **The forbidding comment must be preserved in
   substance** (rewritten to explain why the pre-state root is deliberately
   non-empty), or the regression re-enters silently.

2. **New — the exploit test.** Prove a rejection one-step from a pre-state
   whose `ledger_delta_root` is a *realistic non-empty* frontier commitment
   over ≥1 operation (not `hash_ledger_delta(#"80")`, not
   `frontier_commitment(0,[])`), asserting `verify_one_step` accepts and that
   the post carries the *same* non-empty root. Do this for at least one
   rejection code in each of an early phase (`StaticLedgerRules`), a mid phase
   (`PhaseANativeScripts` or `ScriptSources`), and `LedgerDelta`, since all 80
   sites share the predicate.

*Negative controls (§3 invariant 9 — mutation and valid-block rejection):*

3. **Mutation control.** Same pre-state; successor mutated to carry
   `frontier_commitment(0,[])` (i.e. the old cleared shape) must be
   **rejected** by `verify_one_step` — this is the control that pins
   immutability and prevents someone "restoring" Fix A later.

4. **Mutation control.** Successor mutated to any third delta root must be
   rejected (immutability in both directions).

5. **Valid-block rejection control.** A well-formed *accepted* trace over a
   transaction with a genuine non-empty delta must still reach
   `ledger_delta_stage_three` and terminate `Accepted`, and must **not** be
   provable as a rejection — confirming Fix B did not make honest blocks
   challengeable. Pair with a claim-layer test that an honest rejected forced
   trace passes `committed_claim_is_valid` and an honest accepted trace is
   unaffected.

6. **Claim-layer control** (if the secondary change is adopted): a committed
   `Rejected` descriptor whose terminal carries a non-empty delta root must be
   rejected by `committed_claim_endpoints_and_source_are_valid`; and the
   honest empty-root rejected claim must pass.

7. **Normal/forced symmetry control (§3 invariant 7).** Run the §2.3 exploit
   scenario end-to-end for both `source_kind: Normal` and `source_kind:
   Forced` with identical transaction and prior state, asserting identical
   machine behaviour.

8. **Dispute-level regression.** In
   `demo/midgard-fault-proofs/tests/validation-dispute-submit.test.ts` and/or
   `demo/midgard-sdk/tests/validation-dispute.test.ts`, add the case a
   challenger actually faces: operator commits `Accepted` with a non-empty
   delta root over a transaction that in truth rejects; the challenger must
   win the one-step resolution. This is the test whose absence let a
   soundness hole ship, and unit-level machine tests alone do not cover it.

### Residual risk

- **The claim layer does not bind `initial.ledger_delta_root` to anything for
  a non-accepting trace.** After Fix B that is harmless (the value is inert),
  but it means the field's *only* enforcement is the accepting path. If any
  future consumer starts reading `ledger_delta_root` off a committed trace
  without re-deriving it, the analysis in §2.3 must be redone. Worth an
  explicit code comment at `validation-trace-v1.ak:77`.
- **Out of scope but noticed:** the *accepted* terminal's operation frontier is
  compared to `post_utxos_root` only via
  `proof.ak:1272-1292` (`validate_accepted_transaction_transition_mismatch`, a
  fault proof), not as a positive claim-time obligation. That is a separate
  question about whether the accepted delta binding is complete; it is not
  affected by either fix here and I did not investigate it.
- `hash_ledger_delta` (`validation-trace-v1.ak:251-255`) is dead production
  code retained only by test fixtures. It should be removed under GOAL_SPEC §3
  invariant 13 ("Remove obsolete branches; do not reserve dormant protocol
  surface"), but removing it will touch ~20 test fixtures and is best done as
  a separate change.

## 2.4 Coverage-matrix / family impact — question (d)

**Yes. In practice the entire one-step *rejection* surface of the canonical V1
validation machine is unprovable against an adversarial operator**, which
means every family whose fault proof bottoms out in "the machine rejects this
transaction" is currently unprovable. Concretely:

| Doc + line | Row / family | Claimed status today | Reality under this defect |
|---|---|---|---|
| `docs/consensus-profile-v1.md:554-563` | `requiredProofFamilies`: `"validation-machine-one-step"` | listed as required | the rejecting half is unprovable whenever the operator's claimed delta root is non-empty |
| `docs/consensus-profile-v1.md:554-563` | `"forced-transaction-verdict-mismatch"` | listed as required | provable only in the operator-says-invalid direction; the operator-says-valid / machine-rejects direction is blocked — directly contradicting `technical-spec/1-ledger-state/5-transaction-order-event.tex:66-67` "a fault in either direction" |
| `docs/fault-proofs/coverage-matrix.md:130` (§5) | Plutus/MidgardV1 script execution — "D-S5 is represented in canonical V1" | 🔶 | a failing script's rejection terminal is unprovable; the row overstates |
| `docs/fault-proofs/coverage-matrix.md:155` (§7) | Forced-tx inclusion & classification — "deterministic accepted-effectful or rejected-no-op traces" | 🔶 | the rejected-no-op half is provable only from an already-empty pre-state |
| `docs/fault-proofs/coverage-matrix.md:166` (§8) | One-step L2 tx transition (UTxO delta) | 🟠 Partial | the invalid-transaction direction is blocked |
| `docs/fault-proofs/coverage-matrix.md:390` (§14 F7) | `l2-tx-mistag` | ❌ | already ❌; unchanged, but this defect is a second reason |
| `docs/fault-proofs/catalogue-status.md:57-67`, esp. `:63` | `InvalidOneStepTransition` — status `REAL` / "✅ canonical V1 Aiken/TypeScript paths" | REAL / ✅ | **the ✅ is not currently earned**; this is the most direct overstatement and the clearest AC-X13 exposure |

None of these rows is *wrong about intent*; they are wrong about executable
status, which `GOAL_SPEC.md` §1 ("a prose claim or status table never outranks
a failing or missing executable check") makes the governing consideration.

## 2.5 Is the six-place test workaround spec-legal? — the AC-X13 question

**Answer: it is a real protocol scenario, but as the *only* framing it is
unrepresentative, and using it to make rejection tests pass is exactly the
practice AC-X13 and §3 invariant 6 forbid.**

The workaround is exactly six occurrences of
`ledger_delta_root: validation_merkle_v1.frontier_commitment(0, [])` in
`onchain/aiken/lib/midgard/validation-machine-v1.test.ak`, in two shapes:

- *pre-state fixtures* pinned to the empty commitment: `:838` (ResolveInputs
  fixture), `:7221` (ScriptIntegrity fixture), `:10689` (ValueAndMint
  `invalid_pre`);
- *terminal helpers* that override the successor's root to empty:
  `:575` (the shared `exact_rejection_post` helper), `:4083`, `:6967` — each
  of which only verifies because the corresponding `pre` is also empty.

(For contrast, `:7467` and `:10068` set a genuine non-empty
`frontier_commitment(operation_frontier.count, operation_frontier.peaks)` —
those are the LedgerDelta-phase tests, which need a real delta and therefore
could not use the workaround.)

Is an empty claimed delta *spec-legal*? Yes, in one narrow case: a genuinely
invalid **forced** transaction, whose honest TS-built trace carries
`hashMidgardValidationLedgerDeltaV1([]) = frontier_commitment(0,[])` on every
state (`validation-machine.ts:1455-1475`, `:7479`). So these fixtures do model
a scenario the protocol admits.

But that is *not* the scenario the rejection machinery exists for. The
governing case — an operator claiming `Accepted` (mandatory for every normal
L2 source, `validation-claim-v1.ak:288-296`) over a transaction that truly
rejects — **always** has a non-empty claimed delta, because a real transaction
removes inputs and adds outputs. The six fixtures therefore exercise the one
special case in which the defect is invisible, and exercise none of the cases
in which the machine must work.

Against the invariants:
- **§3 invariant 6 "No placeholder semantics"** — using an empty-delta framing
  to claim the rejection path closed is placeholder framing.
- **AC-X13** (`GOAL_SPEC.md:1278-1280`) — "No acceptance claim relies solely on
  documentation, a synthetic helper, **representative framing**, an
  emulator-only limit …". A pre-state pinned to the empty commitment purely so
  the successor rule can be satisfied is representative framing in the precise
  sense named.
- **§3 invariant 9 "Soundness is symmetric"** — none of the six has a negative
  control over a non-empty delta root, which is why the contradiction survived.

The repository's own annotations at `validation-machine-v1.test.ak:2511-2519`
("Do NOT make this green by setting `pre.ledger_delta_root` to
`frontier_commitment(0, [])` -- that is the workaround that hid the defect")
already state this conclusion; my independent analysis agrees with it.

**Required follow-up, independent of which fix is chosen:** after the fix, the
six sites must be re-based onto realistic non-empty delta roots, keeping *one*
empty-root case explicitly labelled as the honest forced-invalid scenario. If
they are left as-is, the tests will pass under Fix B but will still not
demonstrate the property that was broken.

---

# Documents requiring update

Protocol-semantics documents (accepted decision/plan layer):

1. `docs/consensus-profile-v1.md` — §8 transition binding (`:268-271`) should
   state explicitly that the rejecting-terminal no-op obligation is discharged
   at the transition-binding/claim layer and that the validation-machine state
   carries the claimed delta immutably. Also `:192-195`, whose enumeration of
   committed machine-state fields ("phase, program counter, immutable
   transaction/source commitment, prior ledger root, work-stack roots,
   accumulated execution units, and current verdict") **omits the ledger delta
   root entirely** — that omission should be corrected regardless of the fix,
   since the field is in `encode_machine_state` (`validation-trace-v1.ak:215`)
   and therefore consensus-relevant. Check whether the compiled profile
   digest/`requiredProofFamilies` block (`:554-563`) needs rebinding.
2. `technical-spec/` — **no normative change is required**, since the spec
   never defines the field. Optional clarification in
   `technical-spec/1-ledger-state/5-transaction-order-event.tex` or
   `7-phase-two-validation/` that the machine's claimed-delta commitment is
   immutable and that the no-op obligation is a transition-step property. If
   the owner does add it, note that
   `technical-spec/1-ledger-state/1-block.tex:244` ("does not include a
   claimed effect …") is about `TransitionStep`, not the machine state, and the
   two must not be conflated.

Status / coverage documents (all currently overstate executable status):

3. `docs/fault-proofs/coverage-matrix.md` — rows at `:130` (§5), `:155` (§7),
   `:166` (§8); §13 catalogue identifiers (`:342-364`), §14 F7 (`:390`).
4. `docs/fault-proofs/catalogue-status.md` — §1 table (`:17-30`) and §2 table
   (`:57-67`), especially `:63` `InvalidOneStepTransition` (status `REAL` /
   ✅), and §6 required-but-missing list (`:136-149`).
5. `docs/fault-proofs/execution-plan.md` — D-S9 (`:104`), W-C8 (`:134`),
   W-T8 (`:184`).
6. `docs/fault-proofs/README.md:67-72` — the "exact rejected no-op … Wrong
   verdicts, wrong roots, and either source-phase misclassification direction
   are represented" claim.
7. `docs/exec-plans/cardano-capability-proof-completion.md` — `:132`
   ("spent/produced ledger-delta construction"), `:142-144`, `:154`
   ("accepted transaction paired with the wrong ledger delta").
8. `docs/exec-plans/canonical-v1-format-registry.md:165-166` — V15/V16
   ledger-delta format rows; V16 is described as "Accepted transition work
   witness", which should be reconciled with the rejection-terminal witness
   shape.
9. `GOAL_PROGRESS.md` — record both defects, the resolutions, and the
   corrected status of the affected families; per `GOAL_SPEC.md:10-12` any
   `GOAL_SPEC.md` amendment requires re-recording its SHA-256 baseline (no
   `GOAL_SPEC.md` amendment is implied by these fixes).

Cross-language / test artifacts:

10. `onchain/aiken/lib/midgard/validation-machine-v1.test.ak` — the six
    workaround sites (§2.5) and the two annotated RED tests (`:2466`, `:2792`).
11. `onchain/aiken/lib/midgard/validation-one-step-cross-language.test.ak` —
    add the Signatures→PhaseANativeScripts boundary (defect 1) and a rejection
    boundary from a non-empty delta root (defect 2).
12. `demo/midgard-fault-proofs/tests/validation-dispute-submit.test.ts` and
    `demo/midgard-sdk/tests/validation-dispute.test.ts` — add the
    challenger-wins-against-a-non-empty-delta scenario.

No TypeScript **source** change is implied by either fix: the TS reference
(`demo/midgard-validation/src/validation-machine.ts`) is already correct on
both points (`:1933` sentinel; `:1455-1461` + `:7479` immutable delta root).
In both defects it is the Aiken side that diverges from the reference.

---

# Confidence and what the owner must decide

**Defect 1 — very high confidence (~99%), no owner decision needed.**
`validation-machine-v1.ak:3128` → `-1`. Established by three independent
in-source semantics and exact TS-reference parity. The spec is silent and does
not need to speak.

**Defect 2 — high confidence (~90%) that line 2052 must be deleted.**
`docs/consensus-profile-v1.md:268-271` is decisive that the rejecting
terminal's obligation is "derives no operations" plus `pre_utxos_root ==
post_utxos_root` at the transition-binding layer — not a mutation of a
machine-state field — and both halves are already implemented and correct.
The technical specification is **silent** on `ledger_delta_root`; I have not
manufactured a normative claim from that silence. The residual 10% is that
some intent behind line 2052 exists in a place I did not find; nothing in
`technical-spec/`, `docs/`, the TS reference, or the eight production usages
supports it, and the vestigial `hash_ledger_delta` helper is affirmative
evidence that it is accumulator-era residue.

**Owner must decide two things, neither of which blocks the primary fix:**
1. Whether to add the secondary trace-endpoint clause in
   `validation-claim-v1.ak` (§2.3). Recommended for §3 invariant 8 parity;
   not required for soundness. Tradeoff: adding it keeps the Aiken-accepted
   set equal to the TS-producible set at the cost of one more consensus-
   relevant clause; omitting it leaves a documented, inert permissiveness.
2. How far to downgrade the status rows in §"Documents requiring update"
   items 3-6 in the interim — in particular whether
   `catalogue-status.md:63` `InvalidOneStepTransition` reverts from ✅ to a
   blocked state until the new negative controls are green.
