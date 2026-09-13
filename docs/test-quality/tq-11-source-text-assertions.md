# TQ-11 — Match source inspections to the contract they enforce

Status: Proposed
Last reviewed: 2026-09-07 (semantic source review)

- **Audit section**: §3
- **Rules**: R2, R5, R12 in [principles](principles.md)

## Problem and boundary

Searching source text for a call does not prove that the call executes, and
textual order does not necessarily establish effect order. Conversely, source
and artifact inspections can directly enforce a contract: package exports,
dependency restrictions, generated ABI layouts, document structure, or a build
manifest. Reading `package.json`, Aiken source, or declarations is not by
itself a reason to delete a test.

Prefer an executable oracle when the claim concerns runtime behavior. Keep an
inspection when the inspected representation is the actual interface, and
choose a parser or machine-readable artifact that is robust to irrelevant
formatting changes.

## Focused candidates

- [Auxiliary witness schema checks](../../demo/midgard-sdk/tests/validation-auxiliary-witness.test.ts)
  use indentation-sensitive extraction of `Data.Object` field names. Preserve
  the cross-language constructor/field-order obligation. Compare encoded
  schema vectors or structured definitions where possible, and verify that a
  field-order change still fails.
- Node commitment, publication, and lease tests that infer ordering from
  `indexOf` source positions: evaluate whether recorded effects or controlled
  concurrent execution can establish the intended order or exclusion.
  Another test mentioning the same service is not automatically equivalent.
- Source scans for forbidden imports or APIs: a syntax-aware dependency rule
  may be more accurate, but it must cover the same files and run in the
  appropriate CI lane before the old guard is removed.
- The historical `lucid-midgard/tests/api-export-snapshot.test.ts` is absent
  from the current tree. Its original built-declaration and export-set checks
  represented a public API contract. A replacement must preserve detection of
  removed or unintentionally added exports; source-layout criticism alone
  does not establish equivalent coverage.
- `local-validation-shared.test.ts`, cited in the original audit, is absent
  from the current tracked suite. It is not remaining deletion work.

The Aiken field-opening checks and generated wire vectors also protect ABI
semantics. Their use of source or generated artifacts must be judged against
that purpose rather than put on a blanket exemption list after a general ban.

## Work and acceptance

1. Record the claim, current inspection, and proposed replacement per changed
   case. Separate runtime behavior, static architecture, and artifact contracts.
2. Establish replacement coverage before deleting the old check. A harmless
   formatting change should pass when formatting is irrelevant; a change to
   the protected ABI, dependency, export, or behavior must fail.
3. Preserve required CI execution and generated-artifact consumers. Do not
   delete an entire mixed suite because one assertion is weak.
4. Report focused checks and any source assumptions not yet validated. There
   is no acceptance target of zero `readFileSync` calls or zero source tests.
