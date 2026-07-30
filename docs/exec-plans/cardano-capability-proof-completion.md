# Cardano-capability proof completion and bounded activation

- **Status:** Active; mandatory before canonical V1 activation
- **Created:** 2026-07-25
- **Predecessor:** `canonical-v1-consolidation.md`
- **Decision authority:**
  `../midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`
- **Safety rule:** unsupported, unproven, malformed, unavailable, stale, or
  deployment-mismatched behavior fails closed

## Objective

Make the sole pre-launch canonical V1 surface independently verifiable on
Cardano L1 without reducing Midgard capability below Cardano's applicable
transaction capability.

The completed release supports:

- valid effectful normal and forced transactions;
- invalid and no-op forced transactions with their specified outcomes;
- normal/forced transaction classification and misclassification challenges;
- spend and reference inputs;
- native and Plutus scripts, redeemers, script credentials, inline datums,
  reference scripts, protected outputs, and observers;
- output Values, mint, and burn; and
- every supported transaction shape admitted by the target Cardano parameter
  snapshot.

Proof layout may require more Cardano transactions. User-facing transaction
capability may not be reduced to make a proof fit one transaction.
Proof transaction count is orthogonal to interactivity: a prover-only proof may
be an ordered multi-step, multi-transaction computation-thread chain.
"Interactive" means that sound resolution intrinsically requires an opposing
party's authenticated response or a withholding deadline, not merely that the
complete proof exceeds one L1 transaction.

## Non-negotiable invariants

1. Every fault that one prover can establish from retained public authenticated
   evidence remains single-party. Double spend, nonexistent input, invalid
   range, and every other fault with a self-contained L1 witness do not enter
   the interactive validation game, even when their proof requires multiple
   computation-thread steps or L1 transactions.
2. The validation game is used only where soundness intrinsically requires
   competing authenticated execution traces, an adversarial response, or a
   withholding deadline. Each interactive family records executable necessity
   evidence for that claim. The bisection hub does not contain the complete
   validation machine.
3. Every validator and every individual revealed field, item, chunk, or
   one-step argument fits the live L1 transaction byte and execution envelopes
   with measured margin.
4. Reference scripts are real Cardano reference scripts. A proof design may
   authenticate and scan their content in bounded pieces; it may not replace
   them with zero-byte placeholders or disable their use.
5. Values have no arbitrary independent 1 KiB cap. The applicable bound is
   derived from the Cardano `maxValueSize` and complete-transaction rules.
6. Transaction capacity is the sum of bounded dynamic content plus fixed
   canonical content. There is no independent 8 KiB Midgard transaction cap.
7. Normal and forced transactions execute the same deterministic validation
   semantics and produce the same ledger deltas for the same source
   transaction and prior state.
8. Exact protocol parameters, profile digest, program/rule commitments,
   validator hashes, DA framing, and release evidence are deployment-bound.
9. Missing DA, proof material, a resolver, a comparison, or release evidence
   blocks activation; it never selects a smaller compatibility profile.

## Architecture

### 1. Authenticated bounded content

The native transaction commitment binds both the canonical transaction
identity and typed content commitments for every dynamic collection or blob.
DA retains the complete canonical bytes. Proofs reveal only a bounded unit:

- ordered collection item plus count/index proof;
- byte chunk plus offset/length proof;
- nested Value policy/asset item;
- datum or redeemer data node;
- script-envelope or program-material node; or
- incremental CBOR scan state.

Each fold state commits its domain, source transaction, field, cursor, total
length/count, prior accumulator, and successor accumulator. A chunk or item
cannot be replayed across fields, transactions, profiles, or deployments.
Finalization checks the exact committed count/length and canonical terminal
scan state, so omission, duplication, reordering, and trailing bytes fail.

No independently submitted preimage may reach 16 KiB. Its generated maximum is
the live L1 `maxTxSize` minus the measured transaction and proof overhead for
the concrete instruction.

### 2. Small dispute hub and resolver chain

The validation dispute is split into:

1. a small bisection hub for opening, authenticated midpoint reveals,
   deadlines, withholding timeouts, and one-step boundary authentication; and
2. instruction-specific resolver validators.

When one transition remains, the hub authenticates the agreed pre-state and
both committed successors, selects the resolver required by the pre-state
instruction, and routes the computation-thread NFT to that exact
deployment-bound script hash. The resolver accepts only its narrowly typed
evidence, evaluates both claimed successors, and can finalize only when the
challenger's successor is uniquely correct.

Resolver hashes are built first and are embedded as one exact ordered set in
the hub parameters. Unknown phases, indices, duplicate hashes, absent
resolvers, or wrong routing fail closed. No resolver imports or decodes the
complete auxiliary-witness sum type.

If one semantic instruction remains too large, it is replaced by finer
authenticated microsteps. Raising Cardano limits in an emulator or publishing
an undeployable monolith is forbidden.

### 3. Validation trace decomposition

Whole-field decoding is replaced by bounded scan/fold instructions:

- canonical envelope and field-directory binding;
- input and reference-input item scans;
- output/address/datum/reference-script scans;
- required signer and witness scans;
- Value and mint/burn policy/asset scans;
- script-source and redeemer scans;
- native-script node evaluation;
- phase-A purpose and authorization scans;
- script-integrity construction;
- Plutus context construction;
- CEK core execution;
- execution-unit aggregation; and
- spent/produced ledger-delta construction.

The TypeScript constructor deterministically emits every state, authenticated
item/chunk proof, midpoint proof, and final narrow resolver argument. Aiken and
TypeScript cross-language vectors cover every instruction.

### 4. Forced execution and classification

Forced publication, material availability, ordering, validation, and ledger
application are separate authenticated stages. A due valid effectful forced
transaction cannot be skipped, rejected, or converted to a no-op. An invalid
forced transaction consumes its exact forced-order effect without applying an
invalid ledger delta. A specified no-op remains a no-op.

The transition trace retains enough source, prior-root, validation-descriptor,
verdict, rejection, and delta data to prove:

- valid forced transaction omitted or rejected;
- invalid forced transaction executed;
- no-op forced transaction given effects;
- normal transaction classified as forced;
- forced transaction classified as normal; and
- accepted transaction paired with the wrong ledger delta.

### 5. Capability-floor derivation

A machine-readable target-network parameter snapshot drives boundary fixture
generation. For each applicable Cardano rule, the release records:

- Cardano parameter and effective epoch;
- corresponding Midgard rule;
- derived maximum shape/count rather than an unrelated round-number cap;
- accepting boundary fixtures and adjacent rejecting fixtures;
- complete normal and forced proof paths; and
- measured L1 bytes, memory, CPU, fees, transaction count, and duration.

The parity checker rejects unknown mappings, stale/pending parameter changes,
lower Midgard capability, or missing measurements.

## Work sequence and gates

### P0 — Freeze and baseline

- Preserve the canonical V1 consolidation and exact format registry.
- Keep the release-evidence digest unset.
- Record current compiled sizes and prove that the monolithic dispute and
  transition-trace validators are undeployable.

**Gate:** no activation or deployment claim uses the monolithic scripts.

### P1 — Split the dispute control plane

- Remove one-step execution from the bisection hub.
- Add exact resolver routing and one-step boundary state.
- Implement small boundary and finalization helpers.
- Rebuild SDK contracts, manifest identity, catalogue, publication order, and
  tests for the full chain.

**Gate:** every parameterized hub/control validator fits a real 16,384-byte
Cardano publication transaction with margin.

### P2 — Incremental transaction commitments

- Add typed item/chunk commitments and exact counts/lengths.
- Replace whole-field caps and whole-list decoders in challenged transitions.
- Add canonical terminal scan checks and DA reconstruction agreement.

**Gate:** maximum Cardano-capable transaction content is committed and can be
revealed through individually bounded proof steps.

### P3 — Narrow resolvers and full semantics

- Implement and size every resolver/microstep.
- Complete scripts, redeemers, reference inputs/scripts, script credentials,
  protected outputs, observers, Values, mint/burn, and native-script
  semantics.
- Demonstrate the target Cardano aggregate script-execution floor.

**Gate:** no supported accepted transition or rejection reason lacks an L1
one-step verifier.

### P4 — Forced and misclassification proof completion

- Execute valid effectful forced transactions.
- Complete invalid/no-op outcomes and all classification fault proofs.
- Retain and test existing single-party proof paths.

**Gate:** the coverage matrix has no missing or partial fund-safety row for the
canonical surface.

### P5 — Release evidence

- Query trusted target-network effective and pending parameters.
- Generate maximum-shape parity fixtures.
- Measure every script and complete proof path.
- Construct every measured publication, resolution, and settlement transaction
  from the actual applied/parameterized validators and concrete target-network
  transaction shape. Representative framing and emulator-only measurements are
  diagnostic evidence only and cannot satisfy the proof-fit gate.
- Generate a machine-checkable Cardano-to-Midgard capability parity report.
  Unknown mappings, missing boundary paths, or incomplete measurements keep
  the release-evidence digest unset.
- Bind the snapshot, profile, DA framing, rule/program commitments, and
  validator hashes into the release-evidence digest.
- Generate or check profile documentation from the compiled profile values.
  Stale documented limits, features, schema identities, or proof families fail
  CI.

**Gate:** the fail-closed release gate opens only for the exact measured
artifacts.

### P6 — Bounded deployment and acceptance

- Stop and remove only the obsolete Midgard development topology/state named
  by the deployment runbook.
- Rebuild and publish the exact release.
- Update the live runbook to the exact canonical V1 schema/profile identities,
  and make its validator reject stale Midgard manifest, schema, profile, and
  version strings. Syntactic runbook validation alone is insufficient.
- Run functional E2E acceptance, fault-proof drills, forced execution, restart,
  recovery, and withholding paths.
- Run the stress gate only after functional acceptance is green.

**Resource gate:** an enforced exclusive-topology lock and preflight must prove
that no other Midgard topology is running before Docker launch. Run one Midgard
topology at a time and one node unless a named multi-participant test requires
more. Every service has explicit container memory/CPU/PID limits; builds and
test workers remain serialized; preflight checks container count and available
memory before every launch; failed disposable resources are torn down
immediately. Documentation or operator convention alone does not satisfy this
gate. Existing unrelated Cardano and application containers are not modified.

### Iteration and review hygiene

- Make coherent checkpoint commits between proof phases so subsequent
  diagnosis starts from a clean worktree and review can isolate the new phase.
- Stage only intended source, tests, specifications, plans, and generated
  protocol artifacts. Logs, probe output, disposable databases, and temporary
  diagnostics remain untracked and excluded.
- `git diff --check` must be clean at every checkpoint and at release.

**Gate:** no live deployment/debugging phase starts from an unreviewable mixed
worktree, known whitespace errors, or staged disposable runtime evidence.

## Verification evidence

The completion report must name exact commands and results for:

- TypeScript lint, typecheck, build, and bounded unit/integration suites;
- Aiken formatting, build, focused checks, cross-language vectors, compiled
  byte sizes, and execution budgets;
- construction of the actual parameterized publication, resolution, and
  settlement transactions against the live `maxTxSize`, not representative
  framing or only emulator evaluation;
- DA/database exact-schema tests;
- capability parity and adjacent-boundary corpus;
- generated/checked profile documentation and semantic stale-version runbook
  validation;
- exclusive-topology/resource-preflight evidence;
- normal/forced valid/invalid/no-op and misclassification matrices;
- single-party and interactive fault-proof drills;
- release-evidence digest reproduction;
- clean deployment and restart/recovery;
- functional E2E; and
- bounded E2E stress.

The goal is incomplete while any release gate is bypassed, any applicable
Cardano capability is lower, any supported transition is not independently
disputable, or any required E2E evidence is stale.
