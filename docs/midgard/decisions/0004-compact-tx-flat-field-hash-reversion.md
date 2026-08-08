# 0004 — Compact-tx flat field-hash reversion

- **Status:** ACCEPTED — owner-decided across the wayfinder map
  [#552](https://github.com/Anastasia-Labs/midgard/issues/552) sessions,
  2026-08-06 through 2026-08-07; recorded 2026-08-08.
- **Owner/approver:** repository owner (Philip DiSarro).
- **Scope:** rationale only. This record explains **why** the nine compact-tx
  per-field commitments revert from the counted bounded-collection Merkle
  scheme to flat blake2b-256 field hashes, and why now. The format itself —
  the MidgardTx type, canonical encoding, the nine flat commitments, the
  enveloped preimage grammar, and the three-tier carriage convention — is
  defined once, normatively, in `docs/spec/midgard-tx.md`. This record
  deliberately restates none of it; where the two ever appear to disagree,
  the spec document wins.
- **Decision trail:** wayfinder map
  [#552](https://github.com/Anastasia-Labs/midgard/issues/552) and its twelve
  closed decision tickets (#553–#564), collapsed into spec
  [#565](https://github.com/Anastasia-Labs/midgard/issues/565). The
  execution-order authority is the sequencing resolution on
  [#563](https://github.com/Anastasia-Labs/midgard/issues/563).

## Decision

All nine compact-transaction per-field commitments are redefined as flat
blake2b-256 hashes over the raw canonical enveloped field-preimage bytes,
replacing the counted bounded-collection Merkle commitments
(`bounded_collection_v1`). Hybrid and partial-counted formats are ruled out.
Disputes recover per-item access by authenticating a field's full preimage
once against its flat hash and then walking or slicing it, with preimages
reaching dispute transactions through a three-tier publication-carriage
ladder. The reversion executes now, inside the active Goal program, with a
GOAL_SPEC amendment binding the new spec document at scheme altitude
(delivery target August 28 — owner override, 2026-08-07, of the earlier
~mid-September target from #553).

## Why

### 1. Industry survey: nobody commits per-field Merkle roots in transactions

`docs/research/l2-tx-commitment-survey-2026-08-06.md` surveyed the production
optimistic L2s with live or historical fraud-proof paths (Fuel v1, Fuel
v2/Ignition, Optimism/OP Stack, Arbitrum Nitro/BoLD). **Zero of them commit
per-field Merkle roots inside individual transactions.** The finest
commitment-time granularity observed anywhere is the whole transaction (Fuel
v1's Merkle leaves). Fine-grained structure, where it exists at all, is
derived at dispute time by disputants (OP/Arbitrum bisection and
memory-Merkleization), or obtained by revealing the whole bounded object and
parsing it on-chain (Fuel v1: flat commit, full reveal, boundary walk — the
VM-less precedent closest to Midgard's shape). The counted scheme taxed every
honest transaction to subsidize a dispute path that no production system has
needed subsidized.

### 2. Honest-path cost: the counted scheme is 19–36× a flat hash

Node-side benchmarks of the current JS implementation measured counted-scheme
commitment construction at **19–36× the cost of a flat hash, ~760 µs per
typical transaction**, paid on every L2 transaction in the node, DA, watcher,
and builder hot paths — the paths every honest participant runs
continuously. The counted structure also spread codec twins, proof-step
witness idioms, and per-item Merkle machinery across roughly 46 Aiken modules
and 15 TypeScript modules that must stay in lockstep.

### 3. Dispute-side measurement: the flat primitives fit the budget

The #556 prototype bench (fork runner, PlutusV3 cost model) measured the
flat-format dispute primitives at the capability maxima:

- **The flat commitment check is effectively free**: one blake2b-256 over a
  maximal two-chunk preimage measured 1,341 mem / 17.4M CPU — ~0.01% of the
  memory budget.
- **Top-level access fits under the enveloped grammar**: walking the maximal
  296-item spend-inputs field to the last item measured 6.2M mem; arithmetic
  fixed-stride access measured ~2K mem per slice.
- **What does not fit** — raw (non-enveloped) item walks and intra-item
  access at the maxima (16.9M–161.9M mem) — is exactly the design space the
  dispute-machine decisions (#557: offset-and-slice access, Value bookmark,
  Canonical-Data Acceptor, checkpointable pushdown discipline) and the
  carriage decision (#558: three-tier ladder) close, without taxing the
  honest path.

The counted scheme's only genuine consumer — per-item access at dispute
time — is therefore serviceable from flat preimages within the execution
budget; the per-transaction Merkle subsidy bought nothing the dispute side
needs.

### 4. Timing: revert now, inside the Goal

Deciding late would have compounded the cost: the counted scheme's ABI
surface was still spreading through in-flight proof-family lanes (quiesced
under #562 until the amendment lands), and the identity cascade
(blueprint → catalogue → manifests → ABI freeze → re-measurement) is the
program's most expensive path — it must be paid exactly once, at the end,
not incrementally (#563). Reverting inside the Goal keeps one amendment, one
cascade, and one re-measurement campaign.

## Consequences (headline only; authorities named)

- `docs/spec/midgard-tx.md` becomes the implementation-normative format
  authority (the first document of the `docs/spec/` layer; authority rule in
  `docs/spec/README.md`).
- `GOAL_SPEC.md` is amended at scheme altitude to bind that document by
  reference; counted-scheme evidence rows are superseded, never deleted (§3
  invariant 14), with dispositions ledgered in `GOAL_PROGRESS.md`.
- Necessity-artifact dispositions (dissolve / re-derive / stand) are recorded
  on [#560](https://github.com/Anastasia-Labs/midgard/issues/560); the
  consolidated re-measurement list rides the #563 phase plan at the single
  declared 13.2M-mem execution-budget basis.
- The datum canonicity predicate re-pins to `serialiseData`'s image,
  restoring L1 parity ([#564](https://github.com/Anastasia-Labs/midgard/issues/564)).

## Superseded

This record supersedes the counted bounded-collection commitment direction
wherever prior documents, artifacts, or evidence rows assume it. Those rows
are retired in place with superseding notes — retained as historical
provenance, never deleted.
