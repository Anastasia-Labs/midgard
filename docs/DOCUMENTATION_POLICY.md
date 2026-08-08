# Documentation and Plan Policy

Status: Active

Last reviewed: 2026-08-08

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
- Historical: retained decision context that must not be executed as current
  guidance.

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
5. `public_testnet_readiness.md` owns launch/readiness claims.
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

An implemented or superseded plan is not a runbook. Add a banner pointing to the
current implementation/reference documentation and keep old paths only when they
are necessary to explain a historical decision.

## Evidence requirements

- Commands must state their working directory and required environment.
- Paths, CLI commands, exported APIs, routes, configuration keys, and generated
  validator entries should be checked automatically where practical.
- Evidence must be checked in or have a durable URI, content hash, revision, and
  reproduction command. Ignored local logs alone do not substantiate a claim.
- A dated review is stale after a relevant semantic change until revalidated.

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
