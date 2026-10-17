# Selected design sketch

Status: design only, 2026-10-16. Names below illustrate intended interfaces;
they are not new exported APIs or claims of completed migration.

## Shared completion: narrowed scope

Keep the existing API and role-specific wallet selection. The only behavior
change is refusal of an explicitly empty wallet-input pool inside the shared
completion helper, before Lucid is invoked, through its existing error mapper.

```ts
const completed =
  yield *
  completeTxWithLocalUPLCEvalProgram(draft, mapCompletionError, walletInputs);
```

Omitted inputs retain wallet discovery. Nonempty inputs remain the explicit
selection pool. Empty explicit inputs are refused. Local evaluation stays on.
An empty preset would otherwise call `wallet.getUtxos()`, which may itself be
restricted by an override or wallet API; provider fallback is not universal.

The helper does not acquire reservation, signing or submission responsibilities.
Existing owners retain action/freshness checks and mandatory-input collection.
No new capability types, role services or provider/selected policy union is
proposed. Earlier transaction-specific refusals may remain for their diagnostics.

Focused tests use the real Lucid emulator to prove empty-input refusal before
completion, successful explicit selection without wallet discovery, and
unchanged discovery when inputs are omitted. The regression must fail against
the original helper before the guard is added.

## Deployment data

```ts
// ExpectedContract and ExpectedFamily come from an independent protocol contract,
// checked against the blueprint. They are not inferred from entries below.
type DeploymentMember = Readonly<{
  family: ExpectedFamily;
  purpose: ScriptPurpose;
  referenceRole: ReferenceRole | null;
  manifestPosition: number;
  publicationPosition: number | null;
}>;
declare const members: Readonly<Record<ExpectedContract, DeploymentMember>>;
declare const recipes: Readonly<Record<ExpectedContract, AppliedRecipe>>;
```

Core owns declarative member identity; SDK owns recipe functions; node/watcher
consume projections. Member-local positions generate ordered views without
three separately maintained name arrays. Admission checks completeness,
uniqueness, purpose and positions; exact identity/order comparisons detect
permutations. The independent expected universe also catches a yield omitted
from every derived list at once. This is the decisive improvement over merely
replacing arrays with a record inferred from those same arrays.

First alias the equal maps. Then implement one family round trip. Split the
logical catalogue into family-local files only when that improves editing;
generation is a later implementation choice, not a prerequisite.

## Package execution

```ts
type PackageTask = Readonly<{
  command: readonly [string, ...string[]];
  environment: Readonly<Record<string, string>>;
  prerequisites: readonly Capability[];
  outputs: readonly OwnedOutput[];
}>;
```

Use a validated package-owned declaration, consumed by ordinary package scripts
and contrib. The owning executor resolves typed capabilities through existing
build/native/blueprint/database owners. Container selection consumes that same
package graph plus build-time edges. Installed boundary checks consume actual
package exports instead of maintaining another export list.

The interface hides preparation and execution details rather than requiring
callers to coordinate them. Preserve current runtime tracing and refusal until
parity and clean-run tests establish equivalent coverage. No arbitrary dynamic
plugin discovery or second orchestrator is required.

## Alternatives and accepted tradeoffs

- A universal transaction service could hide more sequencing but would compete
  with existing journals, cancellation and reservation owners. Keep their
  behavior and enforce shared completion mechanics below them.
- Separate role services are useful only as existing owner interfaces; new
  wrapper layers and wholesale migration are not justified by this evidence.
- A single handwritten giant registry removes duplication but makes family
  editing awkward. One logical authority can have multiple physical files.
- A new verification platform would duplicate already implemented contributors,
  receipts, gates and campaigns. Extend those owners and their feature maps.

The wallet guard is an immediate hardening fix. Deployment metadata is the
highest-priority architectural improvement. Acceptance criteria, full scope and
implementation order are in [the prioritized report](REPORT.md).
