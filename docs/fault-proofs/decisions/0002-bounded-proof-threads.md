# 0002 — Bounded non-interactive proof threads

- Status: Accepted; implemented in the current catalogue
- Scope: deterministic fault families and both applicable verdict directions
- Recorded: 2026-09-07, extracted from delivered plans and checked against source;
  this date is not a new protocol acceptance or measurement receipt.

## Context

Aiken compiles reachable branches into each applied validator. A broad category
can exceed the L1 publication envelope even if each selected branch is cheap.
A rejection can also require proving a universal property, which cannot be
established by presenting one favorable item.

## Decision

Deploy narrow families, each binding one authenticated subject shape, decisive
predicate or inseparable predicate family, resumable state shape, and maximum
evidence frontier. Share pure rules, canonical encoders, scanners, carriage
planners, and tests behind that boundary. Share a downstream scan only after
source-specific binders reduce their evidence to the same authenticated state.

Authenticate the source, transaction identity, direction, typed rejection reason,
and exact coordinate before evaluating the predicate. Normal transaction leaves
are acceptance claims and carry validity code zero. Forced rejections carry typed
reasons; a coarse error code cannot substitute for a typed reason and coordinate.
For wrongful rejection, prove the full contradiction of that exact reason.
In particular, value conservation requires an exhaustive input/output/mint union
fold and Ada/fee equality; absence of one counterexample is insufficient.

Deterministic statements over retained public authenticated evidence remain
single-party even when their computation spans multiple resumable transactions.
`PlutusExecutionFailed` uses the interactive execution dispute. Production
classification refuses missing direct-family installations instead of silently
routing deterministic reasons to an interactive fallback.

Persist authenticated cursor/checkpoint state and signed submission intent.
Recovery reopens public L1 state and retained evidence and verifies that they
identify the same source and continuation. Cancellation is explicit; a restart
must not silently cancel or change a claim.

## Alternatives and consequences

A single universal validator would save catalogue entries but multiply reachable
code and evidence shapes. Splitting physical validators costs transactions and
requires complete publication, execution, recovery, and challenge-window evidence.
Both costs are preferable to weaker predicates, smaller undocumented input bounds,
or raised emulator limits.

The source catalogue, manifest, and applied blueprint are one deployment identity.
Changes to validator parameters must update builders and both-polarity emulator
scenarios together. A source installation is not release acceptance.

## Authorities and verification

- [Typed reason disposition](../../../demo/midgard-fault-proofs/src/workflow/reason-disposition.ts)
- [Catalogue](../../../demo/midgard-sdk/src/fraud-proof/catalogue.ts)
- [Family reference](../family-reference.md)
- [Rejection reasons](../rejection-reason-catalogue-v1.md)
- [Remaining acceptance](../execution-plan.md)

Maximum-shape registered lifecycles must reach permanent proof mint and removal,
refuse honest/substituted evidence, and cover applicable cancellation and recovery.
Keep blueprint-bound fit ledgers alongside those executable checks.
