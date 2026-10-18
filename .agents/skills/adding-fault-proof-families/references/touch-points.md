# Fault-proof family touch points

Every surface a family touches, by package, with what fails when it is
missing. Read this before step 2 of the skill, and
whenever a `GAP` line from `family-checklist.mjs` names a file you do not
recognise.

Legacy line references below are a 2026-09-25 snapshot. The deployment metadata
owners and projections were rechecked on 2026-10-17 and use paths without line
numbers. The template throughout is
`mintItemNonCanonical` (catalogue ID `00000036`, four steps). It was added in
`29ecaaae4` (2026-09-11, 75 files), which predates the family application
registry (`625d00b38`, 2026-09-19); read that commit for the Aiken, SDK and
node shape, and the current tree for the registry.

"Typecheck" in the enforcement column means a `satisfies Record<Category, …>`
table, or an exhaustive `switch` without a `default`, that fails to compile
when your category is absent. It runs in the package's CI typecheck step.

## Aiken (`onchain/aiken`)

| Surface                                                                | Enforced by                                                                                                        |
| ---------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| `lib/midgard/fraud-proofs/<kebab>/rule.ak`, scan modules, `step-0N.ak` | `[ci: Aiken CI/Compile and run the Aiken test suite with the pinned fork]` for the module's own tests              |
| `lib/midgard/fraud-proofs/<kebab>/rule.test.ak`                        | same; a focused run must collect a nonzero count (see [aiken-contract-build](../../aiken-contract-build/SKILL.md)) |
| `validators/fraud-proofs/<kebab>/step-0N.ak`                           | `family-checklist.mjs` `sdk-chain` (a file for each blueprint title the SDK family names)                          |
| `plutus.json`                                                          | generated; never commit it. Build it with the command in [verification.md](verification.md)                        |

## SDK (`demo/midgard-sdk`)

The catalogue order and IDs, reference-script token names and deployment
recipes are projections of core; add nothing for them here. A family member's
recipe reads its validator from the member's first `familyPaths` entry in the
SDK family chain, and the SDK typecheck fails when that path does not name a
validator of the member's purpose.

| Surface                                                                                      | Enforced by                                                                                  |
| -------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------- |
| `src/common.ts:248` `FraudProofs` (one `SpendingValidator` per category)                     | typecheck of `contracts/build.ts`, which builds the record                                   |
| `src/fraud-proof/contracts/families/<kebab>.ts` (`deploymentValidatorTitle`, `build<Name>Chain`) | `family-checklist.mjs` `sdk-chain` checks builder linkage and core member title files; real construction remains a test obligation |
| `src/fraud-proof/contracts/build.ts` (import, chain call, result field, `FraudProofs` field) | `family-checklist.mjs` `sdk-chain`; typecheck                                                |
| `src/fraud-proof/contracts/index.ts` (re-exports), `types.ts:114` (`FaultProofContracts` field) | `[review]`; once the `types.ts` field exists, every `FaultProofContractChains` record fails typecheck until it has the entry |

## Core (`demo/midgard-core/src/deployment-manifest-identity/`)

Deployment identity has one source: add one member file in core and register
the family in the core tables below. The protocol ABI is independent of the
member descriptions. The checklist's
`deployment-members` check compares every reached member with that ABI, so a
yield omitted from all derived views remains a gap. Member ownership uses the
explicit family, including aliases and auxiliary scripts whose names have a
different prefix. [script: .agents/skills/adding-fault-proof-families/scripts/family-checklist.mjs]

| Owner | Surface and enforcement |
| ----- | ----------------------- |
| `members/<kebab>.ts` (the one new file), spread into `members.data.ts` | Family, purpose, reference role, compiler title, ordered `familyPaths` (the first is the SDK recipe's path) and explicit manifest/publication/role positions; `verifyDeploymentMembers` admits complete metadata, and core/node tests pin behavior |
| `catalogue-roles.protocol-categories.ts` | Category order (append at the end) and an unused 8-hex ID; `catalogue-order`, `catalogue-id` (format, uniqueness); SDK, node and watcher read these |
| `catalogue-roles.deployment-manifest-contract-names.ts` | Independent positional contract ABI; `deployment-members` compares reached member keys |
| `reference-script-tokens.ts` | Stable token names, the only copy; `reference-scripts` checks every family member's role |
| `reference-script-contracts.ts`, `catalogue-roles.deployment-manifest-fraud-proof-contract-by-category.ts` | Derived from the members; nothing to edit |

The checklist reads the exact aggregate and member files, follows only its
spreads into `defineDeploymentFamily` declarations, and refuses unsupported
expressions. SDK builder/title evidence follows the family entry's explicit
facet imports/exports; unused siblings supply none. It checks source presence;
the runtime admission, real applied-script binding, order and restoration tests
own correctness. [script: .agents/skills/adding-fault-proof-families/scripts/family-checklist.mjs]

## Node (`demo/midgard-node`)

| Surface | Enforced by |
| ------- | ----------- |
| `src/deployable-scripts.publication-order.ts` | Uses core order and SDK recipes; nothing to edit |
| `src/deployment-manifest.deployment-manifest-reference-script-contract-by-role.ts`, `src/deployment-manifest.validate-fraud-proof-catalogue.ts` | Core projections; nothing to edit |
| `src/services/always-succeeds.ts` | The `SDK.FaultProofContractChains` record requires the family; these scripts do not prove real validator wiring |
| `src/services/midgard-contracts.midgard-contracts-from-deployment-manifest.ts` | Family loader; the total `SDK.FaultProofContractChains` record and node manifest roundtrip tests check restoration |
| `tests/deployment-members.test.ts` | Real blueprint/application binding, auxiliary-yield omissions and swapped real step bytes |

## DA committee node (`demo/da-committee-node`)

| Surface                                                              | Enforced by                                                                           |
| -------------------------------------------------------------------- | ------------------------------------------------------------------------------------- |
| `tests/helpers/deployment-fixture.ts` (per-category field, line 222) | `[ci: Midgard Node CI/Verify DA committee transport and admission]` (typecheck, test) |

## Fault proofs (`demo/midgard-fault-proofs`)

| Surface                                                                                                                                    | Enforced by                                                                                             |
| ------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------- |
| `src/<kebab>/` (scan, replay, submit steps, cancel, workflow) and `src/index.ts` re-export                                                 | `[review]`                                                                                              |
| `src/runtime.ts:614` `FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY` and the `categoryLabel` / `buildOneCategoryFaultProofContracts` switches | typecheck                                                                                               |
| `src/workflow/classification.ts:56` `FRAUD_PROOF_CLASSIFICATION_RULES` (catalogue order)                                                   | module throws at load; `tests/workflow.test.ts:346`; `family-checklist.mjs` `classification-rule`       |
| `src/workflow/adapters.ts:124` `workflowAdapterRegistrationRows` (catalogue order)                                                         | `validateWorkflowAdapterCoverage` (`adapters.ts:795`); `family-checklist.mjs` `adapter-registration`    |
| `src/workflow/complete-replay.ts` `completeReplayer([...])` entry                                                                          | `[review]`                                                                                              |
| `src/workflow/replay-prerequisite.ts:112` `directTransactionCategories` (direct transaction families)                                      | `[review]`                                                                                              |
| `src/workflow/actuation-permit.ts` (only for bespoke terminal handling, as mint-item has)                                                  | `[review]`                                                                                              |
| `src/workflow/linear-family-spec.ts` `LINEAR_FAMILY_CATEGORIES` or `family-definitions.ts` `CURSOR_FAMILY_DEFINITIONS`                     | `family-checklist.mjs` `family-definition` (informational)                                              |
| `src/workflow/family-application-registry.ts` `FAMILY_APPLICATION_REGISTRY`                                                                | typecheck; `tests/family-application-registry.test.ts:312`; `family-checklist.mjs` `application-record` |
| `tests/support/emulator/contracts.ts` `real<Name>` flag (line 186 for mint-item) and `buildFamilyContracts` call                           | `[review]`                                                                                              |
| `tests/support/emulator/validators.ts` `scaffoldChain(appendedFamilyFallback, N)` entry (line 472)                                         | typecheck                                                                                               |
| `tests/<kebab>-lifecycle.test.ts` and `tests/support/<kebab>-*.ts`                                                                         | `family-checklist.mjs` `tests` (file name only); both polarities are `[review]`                         |

## Watcher (`demo/midgard-watcher`)

| Surface                                                                  | Enforced by                                                                                                                |
| ------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------- |
| `src/runtime/deployment-identity.catalogue-category-to-contract.ts` `CATALOGUE_CATEGORY_TO_CONTRACT` | Core projection; nothing to edit |
| `src/funding/prover-funding-recovery.ts:190` category branches           | `[review]`; only a family with bespoke funding recovery needs one                                                          |
| `src/fault-proofs/fault-proof-application.ts`                            | derives from `FAMILY_APPLICATION_REGISTRY`; nothing to add                                                                 |

## Node tools (`demo/midgard-node-tools/devnet/watcher-journeys`)

| Surface                                                                                                                                | Enforced by                                                                   |
| -------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------- |
| `catalogue.ts:21` `JOURNEY_FIXTURE_OWNERS` (`transaction`, `script` or `history`)                                                      | typecheck (`satisfies Record<JourneyCategory, JourneyFixtureOwner>`, line 76) |
| `catalogue.test.ts` owner counts (51 families, lines 18–41)                                                                            | runs in no CI job; run it by hand ([verification.md](verification.md))        |
| `transaction-fixtures.ts`, `transaction-source-cases.ts`, `transaction-proof-material.ts`, `script-fixtures.ts`, `history-fixtures.ts` | `[review]`; pick the file that matches the owner                              |

## Docs

The maintenance rule in `docs/fault-proofs/README.md` (lines 50–55) names the
catalogue status, coverage matrix, testing status, execution plan and
public-testnet readiness checklist. All are `[review]`.

- `docs/fault-proofs/catalogue-status.md` (`family-checklist.mjs`
  `catalogue-status-row` checks only that a row exists)
- `docs/fault-proofs/coverage-matrix.md`
- `docs/fault-proofs/testing-status.md`
- `docs/fault-proofs/execution-plan.md`
- `docs/public_testnet_readiness.md`
- `docs/fault-proofs/family-reference.md` (a section per family)
- `docs/spec/midgard-tx.md` and `technical-spec/5-ledger-rules/` when the
  family changes a ledger rule's statement
