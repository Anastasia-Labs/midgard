# Naming and Versioning Policy

Status: Active

Applies to every TypeScript, Aiken, script, and document name in this
repository. `AGENTS.md` points here; the rules below are the reason the tree
looks the way it does after the 2026-09 rename.

## Version at the boundary, not per identifier

Midgard has not launched. `AGENTS.md` already says that undeployed versions
have no compatibility contract: V1 is replaced in place, obsolete branches are
removed, and development state is wiped rather than migrated. A version
suffix on an identifier therefore carries no information until a second
version coexists with it in the tree.

- **Identifiers and file names carry no version.** `HeaderV1`,
  `buildTransitionTraceResultV1`, `WATCHER_CONFIG_V1_SCHEMA_VERSION`,
  `validation-machine-v1.ak`, and `rule-bundle-v1.ts` are `Header`,
  `buildTransitionTraceResult`, `WATCHER_CONFIG_SCHEMA_VERSION`,
  `validation-machine.ak`, and `rule-bundle.ts`.
- **Wire, storage, and manifest boundaries do carry a version, as data.** A
  schema-version string (`"midgard-watcher-config-v1"`), a CBOR domain tag, a
  database migration, a blueprint or deployment-manifest field, a
  consensus-profile identifier, and the evidence artifacts that pin them keep
  their version because a peer, a database, or a chain reads it. The version
  lives in the value, never in the name of the constant that holds it.
- **The seam appears when the second version ships.** When a genuinely
  different shape must coexist with a shipped one, introduce the suffix on
  both sides at that moment (`HeaderV1` and `HeaderV2`), at the boundary
  module that has to tell them apart. Do not pre-suffix in anticipation.
- **External versions are not ours.** `PlutusV2`, `CostModelPlutusV1`, and
  similar library or Cardano names keep their spelling.

## Production is the default

Strict production behavior is the repository default (`AGENTS.md`), so a
`production-` prefix says nothing. Do not prefix production code; mark the
exception instead.

- A module is `fault-proof-application.ts`, not
  `production-fault-proof-application-v1.ts`.
- Where two variants genuinely coexist, name the distinction, not the
  default. The fault-proof families keep a runner that reconstructs its
  evidence exclusively from authenticated L1 and retained DA beside one that
  accepts caller-supplied evidence for tests and local tooling; those are
  `authenticated-workflow.ts` and `workflow.ts` (likewise `authenticated-replay`
  and `replay`). Demo, benchmark, emulator, and test-only variants carry
  `demo-`, `bench-`, `emulator-`, or `test-`.

## Tickets, goal identifiers, and review rows belong in comments

Issue numbers, Goal-program identifiers (`W25`, `Q35`, `C21`, `RF-021`), and
review-row labels change meaning as the program moves. They never appear in a
file name or an identifier. A file is named for what it does
(`replay-authority-fixtures.ts`, `semantic-resolver-arity-gate.test.ts`); the
ticket that motivated it is the first line of its doc comment, where it can be
read, searched, and retired without a rename.

## Spelling

- Files: `kebab-case`. TypeScript identifiers: `PascalCase` for types and
  classes, `camelCase` for values, `SCREAMING_SNAKE_CASE` for constants.
  Aiken: `snake_case` values and modules, `PascalCase` types (see
  `CONTRIBUTING.md`).
- A module's file name is its subject; a directory groups modules by
  concern, not by language construct (no `types/`, `utils/`, or `helpers/`
  directories, no `-utils` suffixes).
- Evidence artifacts and plan documents under `docs/` keep the names their
  verifiers and cross-references pin; they are records, not code.
