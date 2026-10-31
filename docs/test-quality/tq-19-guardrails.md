# TQ-19 — Add guardrails for confirmed test defects

Status: Proposed
Last reviewed: 2026-09-07

- **Audit sections**: all
- **Rules**: R12
- **Blocked by**: the relevant task among TQ-01–18

## Purpose

Guardrails should prevent demonstrated defects without banning valid testing
techniques. Apply the corrected rules in [principles.md](principles.md). The
candidate searches in TQ-01–18 are not suitable as blanket lint rules.

## Proposed guardrails

| Contract                                                      | Appropriate check                                                                                            |
| ------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------ |
| Required evidence has current provenance                      | Consumer validates the required build and measurement basis; historical fixtures are explicitly classified   |
| Required suites execute                                       | Enumerate required lanes and prerequisites; missing mandatory fixtures fail clearly; document optional lanes |
| A refusal establishes its promised cause                      | Structured codes or available evaluator identity/purpose, with honest limits and explicit exceptions         |
| An assertion reads only a local value against itself          | A narrow AST check for identical stable operands, with reviewed exceptions                                   |
| Production behavior does not change merely to satisfy tests   | Review semantic test-only branches and package export boundaries, not every `NODE_ENV` or `ForTest` spelling |
| Harness substitutions do not masquerade as validator coverage | Record affected validators and the suites allowed to substitute them                                         |
| Consolidation preserves fit evidence                          | Compare required family rows, live producer/consumer coverage, and the applicable budget contracts           |
| Test names describe exercised behavior                        | Review against the actual call path and assertions                                                           |

Repeated calls to the same function can check determinism; they are not identical
stable values. Runtime hash shape checks and `toBeDefined()` are not guaranteed
by TypeScript. Shared assertion helpers are legitimate. Injected test
constructors, diagnostic logging, single-row tables, explicit timeout overrides,
and source checks for declared architecture contracts may all be appropriate.
Do not ban these shapes solely because they appeared in the audit.

## What to do

1. Land a narrow rule with the corrected behavior it protects, or use an
   explicit baseline to prevent new violations while existing ones are reviewed.
   A baseline still prevents growth; it is not equivalent to no rule.
2. Prefer existing ESLint facilities for syntax/import constraints. Use a
   repository gate when the contract actually requires cross-file knowledge.
   Keep behavioral tests where syntax cannot establish the property.
3. Give exceptions a reason and a maintained owner or review responsibility.
   An optional suite needs a runnable command and prerequisites, not just a
   warning. A required suite cannot satisfy its contract by warning and skipping.
4. Verify each new gate with small valid and invalid fixtures. No temporary
   commit or scratch branch is required just to demonstrate a lint failure.
5. Document only rules actually implemented and distinguish them from proposals.
   Avoid claiming that every test smell can or should be mechanically detected.

## Acceptance criteria

Each implemented rule has a bounded contract, correct positive and negative
fixtures, a required invocation, and documented exceptions. Valid runtime shape,
determinism, interaction, and artifact-integrity tests remain allowed. Report
which proposals remain manual review criteria rather than claiming total
mechanical enforcement. Use coverage metrics as supporting evidence when useful;
they alone do not establish suite effectiveness.
