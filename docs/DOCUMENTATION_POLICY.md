# Documentation and Plan Policy

Status: Active

Last reviewed: 2026-09-07 (maintenance and lifecycle scope)

Midgard documentation is part of the protocol safety surface. A stale claim
about finality, proof coverage, data availability, recovery, or transaction
ordering can cause an operator or integrator to make an unsafe decision.

## Document classes

Use the document form that matches the reader's need:

- Tutorial: a learning path for a new reader.
- How-to: a goal-oriented procedure with prerequisites and verification.
- Reference: exact commands, APIs, schemas, parameters, and failure behavior.
- Explanation: design rationale, trust assumptions, and tradeoffs.
- Plan: proposed or active work with acceptance evidence.
- Status: a dated, evidence-backed implementation/readiness snapshot.
- ADR: a concise architectural decision, its context, alternatives, and
  consequences.
- Verification artifact: an input or result needed by a named current check
  or release requirement, with its producing identity and reproduction command.

Do not combine a future design, current implementation reference, and operating
runbook without visibly separating them.

## Source hierarchy

There is no single artifact that is authoritative for every kind of claim:

1. The technical specification is the normative **design target** for protocol
   semantics.
2. A `docs/spec/` component specification is **implementation-normative** for
   the concrete detail it covers — exact types, byte-level encodings,
   constants, and the security properties stated with them. On that detail it
   wins over the technical specification, which remains the protocol-level
   design target; a divergence is a technical-specification erratum, not
   grounds to reopen the component specification. See `docs/spec/README.md`
   for the authority rule, its scope, and the amendment process.
3. Checked-in code, configuration, generated blueprints, and tests describe the
   **implemented behavior** at a particular revision.
4. Passing acceptance tests and retained, reproducible artifacts describe the
   **verified behavior**.
5. `docs/public_testnet_readiness.md` owns launch/readiness claims.
6. `docs/fault-proofs/` owns the current proof-coverage and proof-binding audit.

When these disagree, document the divergence. Do not make the implementation
sound conformant by silently rewriting an unapproved protocol rule, and do not
describe a design target as deployed behavior.

## Security and normative language

Use `MUST`, `MUST NOT`, `REQUIRED`, `SHOULD`, `SHOULD NOT`, and `MAY` with the
meanings in BCP 14 (RFC 2119 and RFC 8174) only when they are capitalized.

Every externally visible security claim must state its scope and assumptions,
including the applicable protocol version or deployment, L1 settlement and
rollback policy, DA retrieval/retention model, enabled proof families, challenge
deadline, and economics where relevant. Prefer “the design requires” or “the
implementation currently does” over categorical claims such as “impossible,”
“cannot,” or “inherits L1 security.”

## Plan lifecycle

Every plan must have, near its title:

- `Status`: Proposed, Active, Blocked, Implemented, Superseded, or Historical.
- `Last reviewed`: an ISO date.
- The implementation boundary and explicit non-goals.
- Dependencies and decisions that can change the approach.
- Acceptance criteria tied to checked-in tests, commands, or durable artifacts.

The header requirement applies to plans, not every Markdown file. A formatting
change is not a semantic review: date only the scope actually checked.

When work is delivered or superseded, move lasting rationale into an ADR in
`docs/midgard/decisions/` or `docs/fault-proofs/decisions/`, and current procedures
into the maintained reference or runbook. Remove the completed plan and update
inbound references. Git preserves the execution history. A source comment,
inbound link, unique provenance, or obsolete plan instruction is not a reason
to retain the old document.

For a partially delivered plan, remove completed task detail and keep the
outstanding scope, dependencies, and acceptance criteria. Implementation alone
does not close a release or verification gate.

## Evidence retention

Retain an evidence artifact only when its owner can name the current verifier,
release requirement, or unresolved decision that needs the artifact itself.
State that consumer and what it reads or establishes beside the artifact.
A file merely mentioned in a comment or written by an optional reporting mode
has no automatic retention requirement.

For each old document or embedded historical section:

1. Identify the current consumer and whether it uses the artifact as input,
   verifies its contents, or needs its observations for an open requirement.
2. Extract lasting rationale into the existing ADR for the concern. Keep current
   procedures and unresolved acceptance criteria in their maintained references.
3. Delete the historical narrative, duplicate measurement table, or unused
   snapshot; update links and source comments. Use Git for previous revisions.
4. Preserve inputs required by live checks, including stale baselines that a
   check intentionally rejects. Keep the mismatch visible until remeasurement;
   never delete a failing input or weaken its check to make hygiene pass.

An active proposal does not need an executable consumer: its current purpose is
the decision being considered. This is distinct from retaining the history of
a decision already implemented or superseded.

## Evidence requirements

- Commands must state their working directory and required environment.
- Paths, CLI commands, exported APIs, routes, configuration keys, and generated
  validator entries should be checked automatically where practical.
- Evidence must be checked in or have a durable URI, content hash, revision, and
  reproduction command. Ignored local logs alone do not substantiate a claim.
- A dated review is stale after a relevant semantic change until revalidated.

## Keeping documentation current

- Keep exact inventories in one maintained reference and link to it elsewhere.
  The catalogue inventory is `docs/fault-proofs/catalogue-status.md`; its IDs,
  installation rows, and counts are checked against SDK/watcher source by
  `pnpm --dir docs-site run check:facts`. That gate also checks the SDK language
  advertisement and selected public CLI/workspace facts.
- When adding a source-derived documentation check, include its source paths in
  the docs CI triggers. Run `check:facts` and `check:links` after changes;
  link validity alone does not establish semantic correctness.
- Record deployment hashes and measurements together with their source revision,
  compiler, build flags, parameters, and producing command in acceptance artifacts.
  Avoid copying generated blueprint hashes into multiple current-state pages.
- When a bound identity changes, mark the old evidence invalid for current
  acceptance until the measurements are reproduced. Keep an old receipt only
  while a named current check or release requirement needs it; Git preserves
  earlier revisions. Replacing a digest does not revalidate measured behavior.
- Preserve required evidence and extract unique rationale before retiring a
  plan. Before deletion, check non-Markdown consumers, package manifests,
  generated metadata, and CI in addition to inbound links.
- Prefer named modules and symbols to unmaintained line-number copies of code.
  A document should explain the boundary, rationale, or workflow that makes the
  source easier to use, rather than duplicate easily inspected implementation.

## Protocol review checklist

Before approving a protocol plan or public security document, review:

- trust boundaries, privileged roles, key custody, and upgrade authority;
- deterministic L1-to-L2 derivation, ordering, and versioning;
- Cardano chain-point identity, confirmation depth, rollback, and provider
  disagreement handling;
- DA commitment binding, permissionless retrieval, retention, and the remedy for
  unavailable data;
- deposits, forced inclusion, withdrawals, custody conservation, and a liveness
  path when operators stop;
- complete proof coverage for every enabled transition, sound proof binding,
  proof deadlines, and valid-block non-challengeability;
- bond, slash, reward, fee, and maximum-extractable-value assumptions;
- crash recovery, replay/idempotency, migrations, disaster recovery, and
  protocol upgrades;
- adversarial, rollback, restart, concurrency, and preproduction acceptance
  evidence.

Correctness, safety, and liveness take precedence over performance and
convenience.
