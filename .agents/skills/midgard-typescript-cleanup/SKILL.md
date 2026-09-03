---
name: midgard-typescript-cleanup
description: Use when scanning or refactoring Midgard TypeScript for redundant checks, assertions, normalization, defensive code, duplicate helpers, or dead compatibility code while preserving production L2 correctness. Especially useful for split-scope worker cleanup passes across demo/midgard-node, demo/lucid-midgard, demo/midgard-core, demo/midgard-sdk, demo/midgard-validation, and demo/midgard-fault-proofs packages.
---

# Midgard TypeScript Cleanup

Use this skill from `/home/gumbo/midgard-hub/midgard` for cleanup passes whose goal is to reduce redundant TypeScript without weakening protocol safety.

## Hard Rules

- Read root `AGENTS.md` first and follow the production-grade L2 directive.
- Treat correctness, safety, liveness, and auditability as stronger than LOC reduction.
- Do not remove validation at external, persistence, API, CLI, database, Cardano, serialization, or trust boundaries unless an equivalent canonical validation remains closer to the boundary and tests prove it.
- For `demo/midgard-node`, do not preserve or introduce legacy compatibility, alias IDs, fallback formats, or migration shims for abandoned pre-launch shapes.
- Do not remove useful comments unless the code they describe is removed.
- In multi-worker runs, only edit the assigned ownership scope. You are not alone in the codebase; do not revert or overwrite edits made by others.

## Candidate Triage

Prefer high-confidence candidates:

- Duplicate unknown-error formatting that can use `formatUnknownError` or an existing local helper.
- Duplicate hex/hash/address normalization that can use canonical helpers from `@al-ft/midgard-core` or a package-local shared helper.
- Guards that are impossible after schema parsing, typed constructors, non-empty collection builders, or prior canonical validation in the same call path.
- Repeated local assertions that duplicate one canonical constructor or parser and add no boundary-specific context.
- Dead helpers with no callers in the assigned scope and no exported/public API obligation.
- Demo-node compatibility or fallback paths that conflict with the no-pre-launch-legacy rule.

Reject or defer candidates when:

- The check protects a hostile input boundary, persistence recovery, database integrity, on-chain data, transaction finalization, operator/admin authorization, or observability.
- The proof depends on a broad architectural assumption that is not enforced in code.
- Removing it would make an error less diagnosable at an operational boundary.
- Tests are missing for the success and rejection path and adding them would exceed the requested scope.

## Workflow

1. Lock scope: record the assigned files/packages and the non-overlap expectation.
2. Search with `rg` for repeated patterns before editing:

```bash
rg -n "instanceof Error|String\\(|JSON\\.stringify\\(|isHex|hex|assert|invariant|normalize|sanitize|legacy|fallback|alias|compat|TODO" <scope>
```

3. For each candidate, prove the redundancy from code, not intent. Identify the canonical upstream validation or helper and the affected call path.
4. Patch only high-confidence reductions. If the cleanup needs a shared helper, put it at the narrowest existing shared boundary already used by the scope.
5. Keep behavior strict. Do not add compatibility switches or fallback modes to preserve old runtime objects.
6. Run targeted verification first, then broader verification when the touched surface is shared:

```bash
cd /home/gumbo/midgard-hub/midgard/demo
pnpm run typecheck
pnpm run test
```

Use narrower package scripts first when available and the change is scoped.

## Final Report

Report:

- Files changed and the exact redundancy removed.
- The canonical validation/helper that now owns the behavior.
- Tests or typechecks run.
- Any candidates deliberately left in place because they protect a boundary, improve observability, or lacked proof.

If no safe candidate exists, say so and list the highest-confidence inspected areas.
