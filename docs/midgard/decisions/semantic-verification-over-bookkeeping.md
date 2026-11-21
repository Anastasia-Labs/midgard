# Verify behavior at real boundaries

Status: Accepted; records owner-directed verification simplification.

Recorded: 2026-09-07. Decisions: 2026-08-01, 2026-08-28, 2026-08-29,
and the 2026-09-07 format-registry retirement.

## Context

The canonical capability program grew task manifests, progress diaries,
per-question evidence files, and copies of source/plan hashes. These became
mutually stale and produced repeated rebinding work. The owner retired this
bookkeeping after it failed to justify its maintenance cost.

## Decision

Use source, focused behavioral tests, domain documentation, open decisions,
and a human-readable completion report to establish implementation and
acceptance. A progress diary is not a task database, a release gate, or a
machine-parsed authority. Git already records document and source history.

Delete duplicated task/closure manifests, aggregate task harnesses, per-family
bookkeeping artifacts, and the obsolete format-registry citation gate. Their
retirement does not waive protocol requirements or prove unfinished work done.
A proof family's local verification comes from its Aiken selectors, emulator
lifecycle, and actual SDK/CLI wiring; live acceptance still needs live evidence.

Retain checks and identities that cross a real trust boundary: applied validator
hashes, ABI parity, protocol parameters, deployment/release identity, measured
Cardano transaction and execution budgets, and reproducible acceptance results.
Retained necessity measurements are invalid for current acceptance when their
source, compiler, blueprint, parameters, or fixture identity changes. Historical
evidence survives only where a current executable consumer requires it or an
active decision still needs its rationale. Move that rationale into an ADR and
remove the obsolete record. Comment mentions, unique provenance, and earlier
retention instructions alone do not justify keeping an archive.

## Consequences

Do not rebuild a task database by parsing Markdown, count an optimistic status
row as coverage, or update a digest as a substitute for remeasurement. The
[documentation policy](../../DOCUMENTATION_POLICY.md) defines evidence handling;
[GOAL_SPEC](../../exec-plans/GOAL_SPEC.md) preserves the unfinished acceptance
contract. The live Header ABI gate is
[verify-canonical-v1-header-v1-abi.mjs](../../../demo/scripts/verify-canonical-v1-header-v1-abi.mjs).

Original owner directions and their historical inventories are recoverable at
`81a511e15:docs/exec-plans/GOAL_PROGRESS.md` (Decisions). Some mechanisms listed
as retained in the August 28 entry were retired by later rulings; the inventory
in that diary is not a current gate list.
