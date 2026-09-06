# Installed transition trace replay

The watcher installs `transitionTrace` in canonical catalogue order. Its
manifest-bound constructor checks all nine spending scripts, fifteen rewarding
scripts and three shared witnesses before starting a computation thread.
The accepted/deposit transition cuts and bounds remain those in
[size-plans/transition-trace-accepted-transaction-v1.md](size-plans/transition-trace-accepted-transaction-v1.md)
and [size-plans/transition-trace-deposit-v1.md](size-plans/transition-trace-deposit-v1.md).

## Replay authority

`createTransitionTraceEventAuthority` binds a local raw L1 observation source
to the deployment. Each classification captures the authenticated hub oracle,
its exact event addresses, address coverage and complete histories for the
discovered event NFTs. Copied handles and caller-authored event verdicts have
no authority. Every committed L1 event requires matching retained raw coverage;
missing coverage fails rather than producing a healthy verdict.

`replayTransitionTraceFromRetainedHistory` requires that handle and an admitted
historical corpus tied to the exact current canonical evidence object. The
predecessor trie contains canonical output descriptors under fixed-index spend
keys. Replay derives membership, nonmembership, deletion and insertion proofs
from that trie. It stops at the first false transition because later claimed
pre-roots no longer describe the honest ledger.

Normal and forced operator acceptance claims open the exact retained endpoint
states, validation context and terminal work witness against the operator's
counted validation trace. Honest replay never constructs operator membership.
The retained chronological states use the coordinates documented in
[retained-validation-trace.md](retained-validation-trace.md).

## Durable transitions and publications

`runOrResumeManifestBoundTransitionTraceWorkflow` reopens the history and raw
L1 authority on every invocation. The journal retains current and predecessor
payload envelopes, exact selected proof, selected event out-ref and raw L1
snapshot. These bytes support recovery and audit; fresh admission supplies the
replay authority. A different selected proof, source, event or header refuses
resume.

The route selects one of eight finals. Accepted and deposit finals retain their
thread NFT while scanning; the cursor admits only the registered successor or
the permanent proof token. Proof/output byte publications use fixed 4,096-byte
chunks. Typed datum evidence uses the shared structured Data carrier, preserving
original map order. Each publication has its own signed intent and authenticated
L1 recovery before the consuming step can proceed. Funding excludes datum-bearing
outputs, script references and tokens so later evidence cannot be spent as fees.

After removal, the live header observation remains unavailable. Recovery uses
`observeRetainedHeader`, which opens the unique NFT mint and exact state-queue
output from release-final history and verifies the linked-list key and header
hash. It does not assert that a removed header is live. The existing raw L1
terminal verifier still checks the permanent proof and removal economics.

`transition-trace-workflow-fit-ledger.json` records complete signed installed
transactions, including publications, under the normal pinned testnet build.
The lifecycle test recreates the constructor and directory journal after each
pending boundary and advances an actual emulator block before reconciliation.

## Verification receipt

Pinned normal testnet blueprint:
`57310dd3c0864ffd39e9cf11ec2ce2e316c2a960e6ea1b0d73c734d13691c4de`.

- Installed classifier and recovery: 5/5 cases (deposit fraud/honest, 12,000-byte
  deposit datum, retained accepted transaction fraud/honest).
- Installed watcher application and canonical category roster: 3/3.
- Challenger, descriptor-trie replay and header classifier: 61/61.
- Transition timing/count subvariant registered lifecycles: 6/6, including
  submitted-valid forced bytes with an adjudicated-invalid committed source
  and an on-chain refusal for a changed exact rejection reason.
- Guarded Aiken `midgard/fraud_proofs/transition_trace/proof.{..}`: 95/95.
- Canonical action/publication prerequisite regressions: 14/14; retained-header
  raw history derivation: 9/9; SDK publication funding and carriage door: 6/6.
- SDK, fault-proofs and watcher `tsc --noEmit` passed. Touched TypeScript lint
  and format checks passed.

The installed ledger contains 497 successful complete signed transactions,
including 337 publications: maximum 14,903 bytes, 5,249,103 memory units and
2,367,328,446 CPU units. Its minimum publication reserve is 969 bytes.
Ledger digest:
`00996fff9d649c96686516ff88bc09262ed2cdf39ebf12af1b9ce58eada26ef1`.
The forced timing ledger separately records 116 transactions and 94 publications.
The accepted/deposit direct-family maximum fixtures remain in
`size-plans/transition-trace-fit-ledger.json`; this installed receipt measures
its own stated shapes rather than claiming every direct maximum was rerun here.

Shared integration checkpoint: normal testnet all-contract build at `f5ea519a`
produced blueprint `4247d99fd581a54edf952f93b6c9d4dcf9e6eaa66e52592dae71a7dfb73523e5`. FP typecheck passed.
Shared combined suite passed 68/68: ScriptSources boundaries 25, discovery 18,
TransitionTrace forced-source subvariants 6, and CEK context planner 19. The
forced-window fit ledger was remeasured against this combined blueprint.
