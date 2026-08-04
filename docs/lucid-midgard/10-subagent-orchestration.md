# Subagent Orchestration

This project should use subagents for focused research and drafting, but final
design authority must remain centralized. The builder touches consensus-facing
transaction bytes and validation assumptions, so fragmented ownership is risky.

## Roles

Use these roles during design and implementation:

- Native codec agent: owns transaction format, CBOR, hashes, tx id, output
  protection, and golden vectors.
- Validation agent: owns Phase A/Phase B rule mapping and reject-code parity.
- API agent: owns fluent API shape and lifecycle ergonomics.
- Provider/wallet agent: owns node provider, local provider, signer adapters,
  and submission/status semantics.
- Script agent: owns scripts, redeemers, purpose indexing, datums, and budgets.
- Testing agent: owns test matrix, fixtures, property tests, and integration
  harness.
- Review agent: checks for production-L2 violations, hidden compatibility
  paths, and validation drift.

## Wave 1: Context

Agents gather evidence only. Outputs should be line-referenced summaries, not
patches. Required questions:

- What does the current implementation accept?
- What does it reject?
- Which docs are stale?
- Which tests define behavior?

## Wave 2: Design Drafts

Agents draft docs in disjoint files:

- API agent: `01-public-api.md`
- Codec agent: `02-native-transaction-model.md`
- State/API agent: `03-builder-state-machine.md`
- Provider/wallet agent: `04-provider-and-wallet.md`
- Script agent: `06-scripts-redeemers-and-indexing.md`
- Testing agent: `09-testing-and-conformance.md`

The lead integrates language and resolves contradictions.

## Wave 3: Task Decomposition

Agents convert accepted docs into implementation tasks with:

- Scope.
- Dependencies.
- Deliverables.
- Acceptance criteria.
- Required tests.
- Explicit non-goals.

No task should modify both consensus-sensitive encoding and unrelated API
ergonomics in the same patch.

## Wave 4: Implementation

Implementation agents should receive disjoint write scopes:

- Package scaffold and exports.
- Codec and core types.
- Provider and wallet adapters.
- Builder state and completion.
- Script/redeemer support.
- Validation/test fixtures.

Workers must be told that others may be editing the repo. They must not revert
unrelated changes.

## Wave 5: Review

Before merging implementation:

- Codec reviewer compares bytes against node decode.
- Validation reviewer runs Phase A/B fixtures.
- API reviewer checks examples and type ergonomics.
- Security reviewer searches for compatibility shims, silent rewrites, and
  local validation bypasses.

The lead accepts or rejects changes only after conformance tests pass.
