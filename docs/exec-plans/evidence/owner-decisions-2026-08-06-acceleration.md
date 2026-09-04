# Owner decisions — 2026-08-06 — delivery acceleration amendment

Provenance: on 2026-08-06 the repository owner (Philip DiSarro) directed
that the Goal program's execution process be streamlined for delivery
speed — "get rid of low value verification / evidence bookkeeping … that
don't justify the slowdown they produce" — and set a delivery target of
**2026-08-20**. This file is the durable record of that direction and of
the specific process amendments it authorizes. It amends execution
process only. It does not amend `GOAL_SPEC.md` §3 invariants, §12
acceptance criteria, §14, or §15, and it does not weaken any fail-closed
release gate.

Measured baseline motivating the amendment (2026-07-24 → 2026-08-05,
461 commits): ~36% of commits were ledger/evidence/checkpoint
bookkeeping, ~24% fix/rework, ~18% feature, ~16% tests. Manifest
completion stood at 49/186 rows done (26.3% unweighted, 22.9%
size-weighted); straight-line projection at that overhead was ~41 days.

---

## D1 — Delivery target and priority order

Delivery target **2026-08-20**. Remaining work is prioritized:

1. Close the in-flight rows first: `C21-AUDIT`, `F40`, `F41`, the `C26`
   remainder, `NODE-DEPOSIT-DA-OUTBOX`.
2. Fan out §9.3 proof-family closures (35 rows, explicitly independently
   assignable) across the standing concurrent lanes of D5.
3. §8.2–§8.4 capability remainder in parallel where path leases allow.
4. §8.5 release evidence and registry batch promotion (D3).
5. §8.6 bounded deployment and §10.4–§10.5 watcher acceptance last,
   under the §0.2 `releaseCommit`/evidence-commit discipline.

The two validation-machine defects characterized in
`vm-defect-decision-memo.md` and the C26 datum-probe replacement from
`c26-cml-investigation.md` are the top of the fix queue; the VM defect-2
replacement-clause form still requires its own decision round and is
**not** decided here.

## D2 — Batched ledger recording

Supersedes per-subtask ledger prose. Evidence is recorded once per
coherent integration batch (typically one lane-day or one integration
commit), not once per subtask. Task-queue `Focused verification` cells
and validation-ledger entries may cite the exact command, exit status,
and material counts, or a committed evidence artifact path, instead of
inline narrative. §4.2's required sections are unchanged; §5.2 item 7 is
satisfied by the batch entry covering the task. What counts as `PASS` is
unchanged: final-tree executable evidence, never prose.

## D3 — Batched registry promotion

Format-registry rows are promoted in batches: one focused verification
run and one ledger entry per batch of rows, replacing per-row promotion
entries. The strict release gate remains fail-closed; an `UNVERIFIED`
row still blocks release exactly as before. Only the recording
granularity changes.

## D4 — Pre-authorized decision classes

Extending the pattern of `owner-decisions-2026-08-05.md` Decision 1, the
following outcome classes are pre-authorized and need no owner
round-trip:

- (a) status flips (including `LOCAL_PASS` and `PASS`) for rows whose
  manifest `focusedCommands` pass verbatim against the final tree;
- (b) structural-N/A closures that match an already-adjudicated
  precedent class (the Q47/Q49 pattern), citing the precedent;
- (c) registry-row promotion under D3 where the row's required evidence
  commands pass.

Owner (or explicitly delegated) decision rounds remain required for:
protocol-semantics changes, anything touching a §3 invariant or §12
acceptance wording, scope changes, conflicts between normative sources,
and the VM defect-2 replacement clause.

## D5 — Standing concurrency through delivery

The 2026-08-04 wave-scoped delegation override becomes standing through
2026-08-20: up to four concurrent implementation lanes plus the parent
integration lane, with bounded delegation permitted inside a lane. Path
leases, §5.1 serialization-sensitive-surface ownership, dependency
gates, and the parent's exclusive ownership of shared surfaces,
integration, blueprint regeneration, and commits are all unchanged. The
parent-never-idle-waits rule (2026-07-30) remains in force.

## D6 — Duplicate-replay reduction

Full parent replay of an agent lane's suites is required only when the
lane touched a §5.1 serialization-sensitive surface or a consensus
codec. Otherwise the lane's recorded evidence (exact command, exit
status, counts, revision) is accepted, with parent spot-replay of at
least one batch in five. A spot-replay mismatch immediately restores
full-replay for that lane's past and future batches.

## D7 — Checkpoint cadence

Checkpoint freezes are taken per integration batch and at least daily,
not per task. Freeze narratives record deltas from the previous freeze
only. Superseding-section chains in the ledger are closed by one
consolidated section per day.

---

## Explicitly not waived

- §12 acceptance criteria, §14, §15, and the completion decision.
- Fail-closed behavior of every release gate, including the 132-row
  format-registry strict verification.
- §5.2 test classes (positive, adjacent-boundary, mutation, malformed,
  fail-closed) and cross-language vectors on consensus surfaces.
- §4.3 commit discipline and §0.2 release/evidence-commit binding.
- Provenance rules: the unprovenanced 246-row "(live-verified on
  preprod)" bulk edit remains excluded from every commit and preserved
  in the working tree pending adjudication.

## Feasibility and fallback

The bookkeeping cuts (D2, D3, D6, D7) are projected to recover roughly
1.3–1.5× throughput; reaching 2026-08-20 additionally requires the D5
lanes to sustain roughly 3× aggregate throughput on the §9.3 backlog.
Checkpoint: if by **2026-08-12** fewer than half of the §9.3 rows are
closed, the owner is asked for a scope decision round on §12 (descoping
is an owner spec amendment and is not pre-authorized here).
